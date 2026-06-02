use core::fmt;
use std::{collections::VecDeque, ops::Range, sync::Arc};

use futures::future::BoxFuture;
use tokio::sync::Mutex;

use crate::{
    InterpreterData,
    data::UserDefinitions,
    error::{
        ExecutionError,
        ExpectedArgs::{self},
        RuntimeError, ToExecutionError,
    },
    source_str::SourceStr,
    span::Span,
    types::{
        argument::{ArgPos, ArgRule, Argument},
        value::Value,
    },
};

pub type InterpreterFut = BoxFuture<'static, Result<Value, ExecutionError>>;

pub type InterpreterFn = Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync>;

#[derive(Clone)]
pub struct Handler(InterpreterFn);

impl Handler {
    pub fn new<F>(func: F) -> Self
    where
        F: Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync + 'static,
    {
        Self(Arc::new(func))
    }

    pub fn handle(&self, command: Command, data: Arc<InterpreterData>) -> InterpreterFut {
        (self.0)(command, data)
    }
}

#[derive(Clone)]
pub struct CommandDef {
    label: &'static str,
    arg_rules: &'static [ArgRule],
    handler: Handler,
}

impl fmt::Debug for CommandDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.label)
            .finish()
    }
}

impl CommandDef {
    pub fn new(label: &'static str, arg_rules: &'static [ArgRule], handler: Handler) -> Self {
        Self {
            label,
            arg_rules,
            handler,
        }
    }
}

pub struct Command {
    label: SourceStr,
    arg_rules: &'static [ArgRule],
    args: VecDeque<Argument>,
    handler: Handler,
    pub span: Span,
}

impl Command {
    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size
                .checked_add(arg.mem_size().await?)
                .ok_or(RuntimeError::Overflow)?;
        }
        Ok(size)
    }

    pub fn new(
        label: SourceStr,
        arg_rules: &'static [ArgRule],
        handler: Handler,
        span: Span,
    ) -> Self {
        Self {
            args: VecDeque::new(),
            handler,
            label,
            arg_rules,
            span,
        }
    }

    pub fn from_def(value: &CommandDef, span: Span) -> Self {
        Self {
            label: SourceStr::Static(value.label),
            arg_rules: value.arg_rules,
            args: VecDeque::new(),
            handler: value.handler.clone(),
            span,
        }
    }

    pub fn get_label(&self) -> SourceStr {
        self.label.clone()
    }

    pub fn get_rules(&self) -> &'static [ArgRule] {
        self.arg_rules
    }

    pub fn set_args(&mut self, args: VecDeque<Argument>) {
        self.args = args;
    }

    pub fn take_args(&mut self) -> VecDeque<Argument> {
        std::mem::take(&mut self.args)
    }

    pub fn pop_front_arg(&mut self) -> Option<Argument> {
        self.args.pop_front()
    }

    pub fn get_args(&self) -> &VecDeque<Argument> {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut VecDeque<Argument> {
        &mut self.args
    }

    async fn validate_arg_range(
        &self,
        arg_rule: &ArgRule,
        range: &Range<usize>,
        data: Arc<InterpreterData>,
    ) -> Result<(), ExecutionError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.type_of(data.clone()).await;
            if !arg_rule.valid_types.contains(&fsl_type) {
                return Err(RuntimeError::WrongArgType {
                    command_label: self.get_label().to_string(),
                    arg_number: i,
                    fsl_type,
                    expected: arg_rule.valid_types,
                }
                .to_exec(arg.span, data.clone()));
            }
        }
        Ok(())
    }

    async fn validate_args(&self, data: Arc<InterpreterData>) -> Result<(), ExecutionError> {
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
                            self.validate_arg_range(arg_rule, &range, data.clone())
                                .await?;
                        }
                        None => {
                            return Err(RuntimeError::MissingArg {
                                command_label: self.get_label().to_string(),
                                arg_number: *i,
                            }
                            .to_exec(self.span, data.clone()));
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
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.get_label().to_string(),
                            expected: ExpectedArgs::AtLeast(range.start),
                            got: self.args.len(),
                        }
                        .to_exec(self.span, data.clone()));
                    } else if self.args.len() > range.end {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.get_label().to_string(),
                            expected: ExpectedArgs::AtMost(range.end),
                            got: self.args.len(),
                        }
                        .to_exec(self.span, data.clone()));
                    } else {
                        self.validate_arg_range(arg_rule, range, data.clone())
                            .await?;
                    }
                }
                ArgPos::None => {
                    if !self.args.is_empty() {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.get_label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .to_exec(self.span, data.clone()));
                    }
                }
                ArgPos::AnyFrom(i) => {
                    max_args = usize::MAX;
                    let range = *i..self.args.len();
                    self.validate_arg_range(arg_rule, &range, data.clone())
                        .await?;
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    if self.args.get(*i).is_some() {
                        self.validate_arg_range(arg_rule, &range, data.clone())
                            .await?;
                    }
                }
            }
        }

        if self.args.len() > max_args {
            return Err(RuntimeError::WrongArgCount {
                command_label: self.get_label().to_string(),
                expected: ExpectedArgs::Exactly(max_args),
                got: self.args.len(),
            }
            .to_exec(self.span, data.clone()));
        }
        Ok(())
    }

    pub async fn execute(self, data: Arc<InterpreterData>) -> Result<Value, ExecutionError> {
        self.validate_args(data.clone()).await?;

        if data.should_execute().await {
            return Ok(Value::None);
        }

        data.push_call(self.label.clone()).await;

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
            span: self.span,
        }
    }
}

#[derive(Debug, Default)]
pub struct UserDef {
    pub label: SourceStr,
    pub parameters: Mutex<VecDeque<SourceStr>>,
    pub commands: Mutex<Vec<Command>>,
    pub local_defs: Arc<Mutex<UserDefinitions>>,
}

impl UserDef {
    pub fn declaration(label: SourceStr) -> Self {
        Self {
            label,
            ..Default::default()
        }
    }

    pub async fn define(&self, parameters: VecDeque<SourceStr>, commands: Vec<Command>) {
        let mut old_parameters = self.parameters.lock().await;
        *old_parameters = parameters;
        let mut old_commands = self.commands.lock().await;
        *old_commands = commands;
    }
}
