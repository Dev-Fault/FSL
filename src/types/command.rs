use core::fmt;
use std::{
    collections::VecDeque,
    ops::Range,
    pin::Pin,
    sync::{Arc, atomic::Ordering},
};

use crate::{
    InterpreterData,
    error::CommandError,
    types::{FslType, value::Value},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ArgPos {
    Index(usize),
    OptionalIndex(usize),
    Range(Range<usize>),
    AnyFrom(usize),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgRule {
    pub position: ArgPos,
    pub valid_types: &'static [FslType],
}

impl ArgRule {
    pub const fn new(position: ArgPos, valid_types: &'static [FslType]) -> Self {
        Self {
            position,
            valid_types,
        }
    }
}

pub type InterpreterFut =
    Pin<Box<dyn Future<Output = Result<Value, CommandError>> + Send + 'static>>;

pub type InterpeterFn =
    Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync + 'static>;

#[derive(Clone)]
pub struct Handler(InterpeterFn);

impl<F, Fut> From<F> for Handler
where
    F: Fn(Command, Arc<InterpreterData>) -> Fut + Send + Sync + 'static,
    Fut: Future<Output = Result<Value, CommandError>> + Send + 'static,
{
    fn from(func: F) -> Self {
        Self(Arc::new(move |command: Command, vars| {
            Box::pin(func(command, vars))
        }))
    }
}

impl Handler {
    pub fn handle(&self, command: Command, data: Arc<InterpreterData>) -> InterpreterFut {
        self.0(command, data)
    }
}

#[derive(Clone)]
pub struct CommandDefinition {
    label: &'static str,
    arg_rules: &'static [ArgRule],
    handler: Option<Handler>,
}

impl fmt::Debug for CommandDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.label)
            .finish()
    }
}

impl CommandDefinition {
    pub fn new(label: &'static str, arg_rules: &'static [ArgRule], handler: Handler) -> Self {
        Self {
            label,
            arg_rules,
            handler: Some(handler),
        }
    }
}

pub struct Command {
    label: String,
    arg_rules: &'static [ArgRule],
    args: VecDeque<Value>,
    handler: Option<Handler>,
}

impl From<CommandDefinition> for Command {
    fn from(value: CommandDefinition) -> Self {
        Self {
            label: value.label.to_string(),
            arg_rules: value.arg_rules,
            args: VecDeque::new(),
            handler: value.handler,
        }
    }
}

impl Command {
    pub fn mem_size(&self) -> Option<usize> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size.checked_add(arg.mem_size()?)?;
        }
        Some(size)
    }

    pub fn new(label: String, arg_rules: &'static [ArgRule], handler: Handler) -> Self {
        Self {
            args: VecDeque::new(),
            handler: Some(handler),
            label,
            arg_rules,
        }
    }

    pub fn get_label(&self) -> &str {
        &self.label
    }

    pub fn get_rules(&self) -> &'static [ArgRule] {
        self.arg_rules
    }

    pub fn set_args(&mut self, args: VecDeque<Value>) {
        self.args = args;
    }

    pub fn take_args(self) -> VecDeque<Value> {
        self.args
    }

    pub fn get_args(&self) -> &VecDeque<Value> {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut VecDeque<Value> {
        &mut self.args
    }

    fn validate_arg_range(
        &self,
        arg_rule: &ArgRule,
        range: &Range<usize>,
    ) -> Result<(), CommandError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.as_type();
            if !arg_rule.valid_types.contains(&fsl_type) {
                return Err(CommandError::WrongArgType(format!(
                    "Arg {} of command {} cannot be of type {}\nValid types are {:?}",
                    i,
                    self.get_label().to_string(),
                    fsl_type.as_str(),
                    arg_rule.valid_types
                )));
            }
        }
        Ok(())
    }

    fn validate_args(&self) -> Result<(), CommandError> {
        let mut max_args = 0;
        for arg_rule in self.arg_rules {
            match &arg_rule.position {
                ArgPos::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.validate_arg_range(arg_rule, &range)?;
                        }
                        None => {
                            return Err(CommandError::WrongArgCount(format!(
                                "Arg {} of command {} must be present and be of type {:?}",
                                i,
                                self.get_label(),
                                arg_rule.valid_types
                            )));
                        }
                    }
                }
                ArgPos::Range(range) => {
                    max_args = if max_args < range.end {
                        range.end
                    } else {
                        max_args
                    };
                    if self.args.len() < range.start {
                        return Err(CommandError::WrongArgCount(format!(
                            "Command {} must have at least {} arguments and only {} were given",
                            self.get_label(),
                            range.start,
                            self.args.len(),
                        )));
                    } else if self.args.len() > range.end {
                        return Err(CommandError::WrongArgCount(format!(
                            "Command {} must have no more than {} arguments and {} were given",
                            self.get_label(),
                            range.end,
                            self.args.len(),
                        )));
                    } else {
                        self.validate_arg_range(arg_rule, range)?;
                    }
                }
                ArgPos::None => {
                    if self.args.len() > 0 {
                        return Err(CommandError::WrongArgCount(format!(
                            "Command {} does not take any arguments",
                            self.get_label()
                        )));
                    }
                }
                ArgPos::AnyFrom(i) => {
                    max_args = usize::MAX;
                    let range = Range::from(*i..self.args.len());
                    self.validate_arg_range(arg_rule, &range)?;
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    if let Some(_) = self.args.get(*i) {
                        self.validate_arg_range(arg_rule, &range)?;
                    }
                }
            }
        }

        if self.args.len() > max_args {
            return Err(CommandError::WrongArgCount(format!(
                "Command {} expected {} args but got {}",
                self.get_label(),
                max_args,
                self.args.len()
            )));
        }
        Ok(())
    }

    const EXECUTE_EXPECT: &str = "Command should always have executor before being consumed";
    pub async fn execute(mut self, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
        self.validate_args()?;

        let return_flag = data.flags.return_flag.load(Ordering::Relaxed);
        let break_flag = data.flags.break_flag.load(Ordering::Relaxed);
        let continue_flag = data.flags.continue_flag.load(Ordering::Relaxed);

        if return_flag || break_flag || continue_flag {
            return Ok(Value::None);
        }

        data.push_call(&self.label).await;

        let handler = self.handler.take().expect(Self::EXECUTE_EXPECT);
        match handler.handle(self, data.clone()).await {
            Ok(value) => {
                data.pop_call().await;
                Ok(value)
            }
            Err(e) => Err(e),
        }
    }
}

impl PartialEq for Command {
    fn eq(&self, other: &Self) -> bool {
        self.get_label() == other.get_label()
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.get_label())
            .finish()
    }
}

impl Clone for Command {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            handler: self.handler.clone(),
            label: self.label.clone(),
            arg_rules: self.arg_rules,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserCommand {
    pub label: String,
    pub parameters: VecDeque<String>,
    pub commands: Vec<Command>,
}
