use core::fmt;
use std::{collections::VecDeque, ops::Range, sync::Arc};

use futures::{TryFutureExt, future::BoxFuture};

use crate::{
    InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    source_str::SourceStr,
    span::Span,
    types::{argument::Argument, value::Value},
};

use super::argument::ArgumentKind;

pub type InterpreterFut = BoxFuture<'static, Result<Value, SpannedError>>;

pub type InterpreterFn = Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync>;

pub enum InterpreterResult<T, E> {
    Sync(Result<T, E>),
    Async(BoxFuture<'static, Result<T, E>>),
}

impl<T, E> InterpreterResult<T, E> {
    pub fn map_err<F, O>(self, op: O) -> InterpreterResult<T, F>
    where
        O: FnOnce(E) -> F + Send + 'static,
        E: 'static,
        T: 'static,
    {
        match self {
            InterpreterResult::Sync(r) => InterpreterResult::Sync(r.map_err(op)),
            InterpreterResult::Async(pin) => {
                InterpreterResult::Async(Box::pin(async { pin.map_err(op).await }))
            }
        }
    }
}

impl<T, E> InterpreterResult<T, E> {
    pub fn and_then_result<U, E2>(
        self,
        f: impl FnOnce(T) -> InterpreterResult<U, E2> + Send + 'static,
    ) -> InterpreterResult<U, E2>
    where
        E: Into<E2> + 'static + Send,
        E2: 'static + Send,
        U: 'static + Send,
        T: 'static + Send,
    {
        match self {
            InterpreterResult::Sync(Ok(value)) => f(value),
            InterpreterResult::Sync(Err(e)) => InterpreterResult::Sync(Err(e.into())),
            InterpreterResult::Async(pin) => InterpreterResult::Async(Box::pin(async {
                crate::await_result!(f(pin.await.map_err(Into::into)?))
            })),
        }
    }
}

#[macro_export]
macro_rules! await_result {
    ($expr:expr) => {
        match $expr {
            $crate::types::command::InterpreterResult::Sync(value) => value,
            $crate::types::command::InterpreterResult::Async(pin) => pin.await,
        }
    };
}

#[derive(Clone)]
pub enum Handler {
    SyncStatic(fn(Command, Arc<InterpreterData>) -> Result<Value, SpannedError>),
    AsyncStatic(fn(Command, Arc<InterpreterData>) -> InterpreterFut),
    AsyncDynamic(InterpreterFn),
}

impl Handler {
    pub fn handle(
        &self,
        command: Command,
        data: Arc<InterpreterData>,
    ) -> InterpreterResult<Value, SpannedError> {
        match self {
            Handler::AsyncStatic(f) => InterpreterResult::Async(f(command, data)),
            Handler::AsyncDynamic(f) => InterpreterResult::Async(f(command, data)),
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
    should_resolve_args: bool,
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
            should_resolve_args: false,
        }
    }

    pub fn from_def(value: &CommandDef, span: Span) -> Self {
        Self {
            label: SourceStr::Static(value.label),
            signature: value.signature,
            args: VecDeque::new(),
            handler: value.handler.clone(),
            span,
            should_resolve_args: false,
        }
    }

    pub fn label(&self) -> SourceStr {
        self.label.clone()
    }

    pub fn signature(&self) -> &'static CommandSignature {
        self.signature
    }

    pub fn set_args(&mut self, args: VecDeque<Argument>) -> Result<(), SpannedError> {
        for arg in &args {
            match &arg.kind {
                ArgumentKind::Value(value) => match value {
                    Value::List(_) | Value::Map(_) | Value::Var(_) | Value::Command(_) => {
                        self.should_resolve_args = true;
                        break;
                    }
                    _ => {}
                },
                ArgumentKind::Accessor(_) => {
                    self.should_resolve_args = true;
                    break;
                }
            }
        }
        self.args = args;
        self.validate_args()?;
        Ok(())
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

    #[inline(always)]
    async fn resolve_arg_range(
        &mut self,
        rule: &ArgRule,
        range: &Range<usize>,
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        for i in range.start..range.end {
            if let ArgRule::Resolved(_) = rule {
                let value = self.args[i].take_value();
                let value = await_result!(value.as_raw(data.clone())).span_err(self.span)?;
                self.args[i].replace_value(value);
            }
        }
        Ok(())
    }

    pub async fn resolve_args(&mut self, data: Arc<InterpreterData>) -> Result<(), SpannedError> {
        if self.should_resolve_args
            && let CommandSignature::Rules(rules) = self.signature
        {
            for rule in *rules {
                let pos = match rule {
                    ArgRule::Resolved(arg_pos) => arg_pos,
                    ArgRule::Unresolved(arg_pos) => arg_pos,
                };

                match &pos {
                    ArgPos::Index(i) => {
                        let range = *i..*i + 1;
                        if let Some(_) = self.args.get(*i) {
                            self.resolve_arg_range(rule, &range, data.clone()).await?;
                        }
                    }
                    ArgPos::Range(range) => {
                        self.resolve_arg_range(rule, range, data.clone()).await?;
                    }
                    ArgPos::None => {}
                    ArgPos::AnyFrom(i) => {
                        let range = *i..self.args.len();
                        self.resolve_arg_range(rule, &range, data.clone()).await?;
                    }
                    ArgPos::OptionalIndex(i) => {
                        let range = *i..*i + 1;
                        if self.args.get(*i).is_some() {
                            self.resolve_arg_range(rule, &range, data.clone()).await?;
                        }
                    }
                }
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    fn rules_count_check(&self, rules: &'static [ArgRule]) -> Result<(), SpannedError> {
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
                    if let None = self.args.get(*i) {
                        return Err(RuntimeError::MissingArg {
                            command_label: self.label().to_string(),
                            arg_number: *i,
                        }
                        .span(self.span));
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
                ArgPos::AnyFrom(_) => {
                    max_args = usize::MAX;
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
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

    pub fn validate_args(&mut self) -> Result<(), SpannedError> {
        match self.signature {
            CommandSignature::Rules(rules) => self.rules_count_check(rules),
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

    pub fn execute(self, data: Arc<InterpreterData>) -> InterpreterResult<Value, SpannedError> {
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
        let result = command.resolve_args($data.clone()).await;
        if let Err(_) = result {
            result.map(|_| Value::None)
        } else {
            $crate::await_result!(command.execute($data))
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
            should_resolve_args: self.should_resolve_args,
        }
    }
}
