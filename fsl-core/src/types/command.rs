use core::fmt;
use std::{
    borrow::Cow,
    collections::VecDeque,
    ops::Range,
    sync::{Arc, atomic::Ordering},
};

use futures::future::BoxFuture;

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
    pub const fn none() -> Self {
        Self {
            position: ArgPos::None,
            valid_types: &[],
        }
    }
}

pub type InterpreterFut<'c> = BoxFuture<'c, Result<Value<'c>, CommandError>>;

pub type InterpreterFn =
    Arc<dyn for<'c> Fn(Command<'c>, Arc<InterpreterData<'c>>) -> InterpreterFut<'c> + Send + Sync>;

#[derive(Clone)]
pub struct Handler(InterpreterFn);

impl Handler {
    pub fn new<F>(func: F) -> Self
    where
        F: for<'c> Fn(Command<'c>, Arc<InterpreterData<'c>>) -> InterpreterFut<'c>
            + Send
            + Sync
            + 'static,
    {
        Self(Arc::new(func))
    }

    pub fn handle<'c>(
        &self,
        command: Command<'c>,
        data: Arc<InterpreterData<'c>>,
    ) -> InterpreterFut<'c> {
        (self.0)(command, data)
    }
}

#[derive(Clone)]
pub struct CommandDefinition {
    label: &'static str,
    arg_rules: &'static [ArgRule],
    handler: Handler,
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
            handler: handler,
        }
    }
}

pub struct Command<'c> {
    label: &'c str,
    arg_rules: &'static [ArgRule],
    args: VecDeque<Value<'c>>,
    handler: Handler,
}

impl<'c> From<&CommandDefinition> for Command<'c> {
    fn from(value: &CommandDefinition) -> Self {
        Self {
            label: value.label,
            arg_rules: value.arg_rules,
            args: VecDeque::new(),
            handler: value.handler.clone(),
        }
    }
}

impl<'c> Command<'c> {
    pub fn mem_size(&self) -> Option<usize> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size.checked_add(arg.mem_size()?)?;
        }
        Some(size)
    }

    pub fn new(label: &'c str, arg_rules: &'static [ArgRule], handler: Handler) -> Self {
        Self {
            args: VecDeque::new(),
            handler: handler,
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

    pub fn set_args(&mut self, args: VecDeque<Value<'c>>) {
        self.args = args;
    }

    pub fn take_args(self) -> VecDeque<Value<'c>> {
        self.args
    }

    pub fn pop_front_arg(&mut self) -> Option<Value<'c>> {
        self.args.pop_front()
    }

    pub fn get_args(&self) -> &VecDeque<Value<'c>> {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut VecDeque<Value<'c>> {
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

    pub async fn execute(self, data: Arc<InterpreterData<'c>>) -> Result<Value<'c>, CommandError> {
        self.validate_args()?;

        let return_flag = data.flags.return_flag.load(Ordering::Relaxed);
        let break_flag = data.flags.break_flag.load(Ordering::Relaxed);
        let continue_flag = data.flags.continue_flag.load(Ordering::Relaxed);

        if return_flag || break_flag || continue_flag {
            return Ok(Value::None);
        }

        data.push_call(self.label).await;

        let handler = self.handler.clone();
        match handler.handle(self, data.clone()).await {
            Ok(value) => {
                data.pop_call().await;
                Ok(value)
            }
            Err(e) => Err(e),
        }
    }
}

impl<'c> PartialEq for Command<'c> {
    fn eq(&self, other: &Self) -> bool {
        self.get_label() == other.get_label()
    }
}

impl<'c> fmt::Debug for Command<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.get_label())
            .finish()
    }
}

impl<'c> Clone for Command<'c> {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            handler: self.handler.clone(),
            label: self.label,
            arg_rules: self.arg_rules,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserCommand<'c> {
    pub label: Cow<'c, str>,
    pub parameters: VecDeque<Cow<'c, str>>,
    pub commands: Vec<Command<'c>>,
}

impl<'c> UserCommand<'c> {
    pub fn declaration(label: Cow<'c, str>) -> Self {
        Self {
            label,
            parameters: VecDeque::new(),
            commands: Vec::new(),
        }
    }

    pub fn definition(
        label: Cow<'c, str>,
        parameters: VecDeque<Cow<'c, str>>,
        commands: Vec<Command<'c>>,
    ) -> Self {
        Self {
            label,
            parameters,
            commands,
        }
    }
}
