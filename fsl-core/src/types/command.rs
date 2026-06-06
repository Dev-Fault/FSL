use core::fmt;
use std::{collections::VecDeque, ops::Range, sync::Arc};

use futures::future::BoxFuture;

use crate::{
    InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    source_str::SourceStr,
    span::Span,
    types::{argument::Argument, value::Value},
};

pub type InterpreterFut = BoxFuture<'static, Result<Value, SpannedError>>;

pub type InterpreterFn = Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync>;

pub enum InterpreterResult {
    Sync(Result<Value, SpannedError>),
    Async(BoxFuture<'static, Result<Value, SpannedError>>),
}

#[derive(Clone)]
pub enum Handler {
    SyncStatic(fn(Command, Arc<InterpreterData>) -> Result<Value, SpannedError>),
    Static(fn(Command, Arc<InterpreterData>) -> InterpreterFut),
    Dynamic(InterpreterFn),
}

impl Handler {
    pub fn handle(&self, command: Command, data: Arc<InterpreterData>) -> InterpreterResult {
        match self {
            Handler::Static(f) => InterpreterResult::Async(f(command, data)),
            Handler::Dynamic(f) => InterpreterResult::Async(f(command, data)),
            Handler::SyncStatic(f) => InterpreterResult::Sync(f(command, data)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ArgPos {
    Index(usize),
    OptionalIndex(usize),
    Range(Range<usize>),
    AnyFrom(usize),
    None,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ArgRule {
    Resolved(ArgPos),
    Unresolved(ArgPos),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ExpectedArgs {
    None,
    Exactly(usize),
    AtLeast(usize),
    AtMost(usize),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum CommandSignature {
    Rules(&'static [ArgRule]),
    Count(ExpectedArgs),
    AnyArgs,
}

#[derive(Clone)]
pub struct CommandDef {
    label: &'static str,
    signature: &'static CommandSignature,
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
    pub fn new(
        label: &'static str,
        signature: &'static CommandSignature,
        handler: Handler,
    ) -> Self {
        Self {
            label,
            signature,
            handler,
        }
    }
}

pub struct Command {
    label: SourceStr,
    signature: &'static CommandSignature,
    args: VecDeque<Argument>,
    handler: Handler,
    pub span: Span,
}

impl Eq for Command {}

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
        signature: &'static CommandSignature,
        handler: Handler,
        span: Span,
    ) -> Self {
        Self {
            args: VecDeque::new(),
            handler,
            label,
            signature,
            span,
        }
    }

    pub fn from_def(value: &CommandDef, span: Span) -> Self {
        Self {
            label: SourceStr::Static(value.label),
            signature: value.signature,
            args: VecDeque::new(),
            handler: value.handler.clone(),
            span,
        }
    }

    pub fn label(&self) -> SourceStr {
        self.label.clone()
    }

    pub fn signature(&self) -> &'static CommandSignature {
        self.signature
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

    async fn resolve_args(
        &mut self,
        rule: &ArgRule,
        range: &Range<usize>,
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        for i in range.start..range.end {
            if let ArgRule::Resolved(_) = rule {
                let span = self.args[i].span;
                let arg = std::mem::replace(&mut self.args[i], Argument::new(Value::None, span));
                let value = arg.as_raw(data.clone()).await?;
                self.args[i] = Argument::new(value, span);
            }
        }
        Ok(())
    }

    async fn validate_arg_rules(
        &mut self,
        rules: &'static [ArgRule],
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        let mut max_args = 0;
        for rule in rules {
            let pos = match rule {
                ArgRule::Resolved(arg_pos) => arg_pos,
                ArgRule::Unresolved(arg_pos) => arg_pos,
            };

            match &pos {
                ArgPos::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.resolve_args(rule, &range, data.clone()).await?;
                        }
                        None => {
                            return Err(RuntimeError::MissingArg {
                                command_label: self.label().to_string(),
                                arg_number: *i,
                            }
                            .span(self.span));
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
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::AtLeast(range.start),
                            got: self.args.len(),
                        }
                        .span(self.span));
                    } else if self.args.len() > range.end {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::AtMost(range.end),
                            got: self.args.len(),
                        }
                        .span(self.span));
                    } else {
                        self.resolve_args(rule, range, data.clone()).await?;
                    }
                }
                ArgPos::None => {
                    if !self.args.is_empty() {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .span(self.span));
                    }
                }
                ArgPos::AnyFrom(i) => {
                    max_args = usize::MAX;
                    let range = *i..self.args.len();
                    self.resolve_args(rule, &range, data.clone()).await?;
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    if self.args.get(*i).is_some() {
                        self.resolve_args(rule, &range, data.clone()).await?;
                    }
                }
            }
        }
        if self.args.len() > max_args {
            return Err(RuntimeError::WrongArgCount {
                command_label: self.label().to_string(),
                expected: ExpectedArgs::Exactly(max_args),
                got: self.args.len(),
            }
            .span(self.span));
        }
        Ok(())
    }

    pub async fn validate_args(&mut self, data: Arc<InterpreterData>) -> Result<(), SpannedError> {
        match self.signature {
            CommandSignature::Rules(rules) => self.validate_arg_rules(rules, data).await,
            CommandSignature::Count(expected) => match expected {
                ExpectedArgs::None => {
                    if !self.args.is_empty() {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .span(self.span));
                    }
                    Ok(())
                }
                ExpectedArgs::Exactly(i) => {
                    if self.args.len() != *i {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .span(self.span));
                    }
                    Ok(())
                }
                ExpectedArgs::AtLeast(i) => {
                    if self.args.len() < *i {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .span(self.span));
                    }
                    Ok(())
                }
                ExpectedArgs::AtMost(i) => {
                    if self.args.len() > *i {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .span(self.span));
                    }
                    Ok(())
                }
            },
            CommandSignature::AnyArgs => Ok(()),
        }
    }

    pub fn execute(self, data: Arc<InterpreterData>) -> InterpreterResult {
        if data.should_execute() {
            return InterpreterResult::Sync(Ok(Value::None));
        }

        let handler = self.handler.clone();
        handler.handle(self, data.clone())
    }
}

#[macro_export]
macro_rules! execute_command {
    ($command:expr, $data:expr) => {{
        let mut command = $command;
        let result = command.validate_args($data.clone()).await;
        if let Err(_) = result {
            result.map(|_| Value::None)
        } else {
            match command.execute($data) {
                crate::types::command::InterpreterResult::Sync(value) => value,
                crate::types::command::InterpreterResult::Async(pin) => pin.await,
            }
        }
    }};
}

impl PartialEq for Command {
    fn eq(&self, other: &Self) -> bool {
        self.label() == other.label()
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.label())
            .finish()
    }
}

impl Clone for Command {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            handler: self.handler.clone(),
            label: self.label.clone(),
            signature: self.signature,
            span: self.span,
        }
    }
}
