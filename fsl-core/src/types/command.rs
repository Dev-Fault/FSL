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

#[macro_export]
macro_rules! try_result {
    ($result:expr ) => {{
        match $result {
            Ok(result) => result,
            Err(e) => {
                return InterpreterResult::Sync(Err(e));
            }
        }
    }};
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
    pub args: VecDeque<Argument>,
    handler: Handler,
    pub span: Span,
    needs_resolving: bool,
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
            needs_resolving: false,
        }
    }

    pub fn from_def(value: &CommandDef, span: Span) -> Self {
        Self {
            label: SourceStr::Static(value.label),
            signature: value.signature,
            args: VecDeque::new(),
            handler: value.handler.clone(),
            span,
            needs_resolving: false,
        }
    }

    pub fn label(&self) -> SourceStr {
        self.label.clone()
    }

    pub fn signature(&self) -> &'static CommandSignature {
        self.signature
    }

    pub fn set_args(&mut self, args: VecDeque<Argument>) -> Result<(), SpannedError> {
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

    pub(crate) fn should_resolve_args(&self) -> bool {
        self.needs_resolving
    }

    pub(crate) fn resolve_args(
        mut self,
        data: Arc<InterpreterData>,
    ) -> InterpreterResult<Self, SpannedError> {
        let mut async_ops: Vec<_> = Vec::new();
        for (i, arg) in self.args.iter_mut().enumerate() {
            if arg.needs_resolving {
                let value = arg.take_value();
                let value = match value.as_raw(data.clone()) {
                    InterpreterResult::Sync(value) => match value {
                        Ok(value) => value,
                        Err(e) => return InterpreterResult::Sync(Err(e.span(self.span))),
                    },
                    InterpreterResult::Async(pin) => {
                        async_ops.push((i, pin));
                        continue;
                    }
                };
                arg.replace_value(value);
            }
        }
        if async_ops.len() > 0 {
            InterpreterResult::Async(Box::pin(async move {
                for (i, pin) in async_ops {
                    let arg = &mut self.args[i];
                    let value = pin.await.span_err(self.span)?;
                    arg.replace_value(value);
                }
                Ok(self)
            }))
        } else {
            InterpreterResult::Sync(Ok(self))
        }
    }

    fn rules_count_check(&mut self, rules: &'static [ArgRule]) -> Result<(), SpannedError> {
        let mut max_args = 0;
        for rule in rules {
            let pos = match rule {
                ArgRule::Resolved(arg_pos) => arg_pos,
                ArgRule::Unresolved(arg_pos) => arg_pos,
            };

            match pos {
                ArgPos::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    match self.args.get_mut(*i) {
                        None => {
                            return Err(RuntimeError::MissingArg {
                                command_label: self.label().to_string(),
                                arg_number: *i,
                            }
                            .span(self.span));
                        }
                        Some(arg) => {
                            arg.needs_resolving = true && arg.not_resolved();
                            self.needs_resolving = arg.needs_resolving | self.needs_resolving
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
                        for i in range.start..range.end {
                            if let ArgRule::Resolved(_) = rule {
                                self.args[i].needs_resolving = true && self.args[i].not_resolved();
                                self.needs_resolving =
                                    self.args[i].needs_resolving | self.needs_resolving
                            }
                        }
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
                ArgPos::AnyFrom(p) => {
                    max_args = usize::MAX;
                    for i in *p..self.args.len() {
                        if let ArgRule::Resolved(_) = rule {
                            self.args[i].needs_resolving = true && self.args[i].not_resolved();
                            self.needs_resolving =
                                self.args[i].needs_resolving | self.needs_resolving
                        }
                    }
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    if let Some(arg) = self.args.get_mut(*i) {
                        arg.needs_resolving = true && arg.not_resolved();
                        self.needs_resolving = arg.needs_resolving | self.needs_resolving
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

    #[inline(always)]
    fn handle(self, data: Arc<InterpreterData>) -> InterpreterResult<Value, SpannedError> {
        if data.should_execute() {
            return InterpreterResult::Sync(Ok(Value::None));
        }

        let handler = self.handler.clone();
        handler.handle(self, data.clone())
    }

    pub fn execute(self, data: Arc<InterpreterData>) -> InterpreterResult<Value, SpannedError> {
        if self.should_resolve_args() {
            match self.resolve_args(data.clone()) {
                InterpreterResult::Sync(command) => {
                    let command = crate::try_result!(command);
                    command.handle(data)
                }
                InterpreterResult::Async(pin) => InterpreterResult::Async(Box::pin(async move {
                    let command = pin.await?;
                    await_result!(command.handle(data))
                })),
            }
        } else {
            self.handle(data)
        }
    }
}

#[macro_export]
macro_rules! execute_command {
    ($command:expr, $data:expr) => {{ $crate::await_result!($command.execute($data)) }};
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
            needs_resolving: self.needs_resolving,
        }
    }
}
