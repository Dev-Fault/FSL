use core::fmt;
use std::{collections::VecDeque, ops::Range, sync::Arc};

use futures::future::BoxFuture;

use crate::{
    InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    source_str::SourceStr,
    span::Span,
    types::{
        argument::Argument,
        value::{Value, ValueError},
    },
};

pub type InterpreterFut = BoxFuture<'static, Result<Value, SpannedError>>;

pub type InterpreterFn = Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync>;

pub enum PotentialFuture<T, E> {
    Sync(T),
    Async(BoxFuture<'static, Result<T, E>>),
}

impl<T: 'static, E: 'static> PotentialFuture<T, E> {
    pub fn map<F, T2>(self, f: F) -> PotentialFuture<T2, E>
    where
        F: FnOnce(T) -> T2 + Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => PotentialFuture::Sync(f(t)),
            PotentialFuture::Async(t) => {
                PotentialFuture::Async(Box::pin(async move { Ok(f(t.await?)) }))
            }
        }
    }

    pub fn map_err<F, E2>(self, f: F) -> PotentialFuture<T, E2>
    where
        F: FnOnce(E) -> E2 + Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => PotentialFuture::Sync(t),
            PotentialFuture::Async(t) => {
                PotentialFuture::Async(Box::pin(async move { t.await.map_err(f) }))
            }
        }
    }

    pub fn map_result<F, T2, E2>(self, f: F) -> PotentialFutureResult<T2, E2>
    where
        F: FnOnce(T) -> PotentialFutureResult<T2, E2> + Send + 'static,
        T: Send + 'static,
        E: Into<E2> + Send + 'static,
        E2: Send + 'static,
        T2: Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => f(t),
            PotentialFuture::Async(t) => Ok(PotentialFuture::Async(Box::pin(async move {
                match f(t.await.map_err(|e| e.into())?)? {
                    PotentialFuture::Sync(value) => Ok(value),
                    PotentialFuture::Async(pin) => pin.await,
                }
            }))),
        }
    }
}

pub type PotentialFutureResult<T, E> = Result<PotentialFuture<T, E>, E>;

pub trait SpannedPotentialFutureResult<T> {
    fn span_future(self, span: Span) -> PotentialFutureResult<T, SpannedError>;
}

impl<T: 'static, E: ToSpannedError + 'static> SpannedPotentialFutureResult<T>
    for PotentialFutureResult<T, E>
{
    fn span_future(self, span: Span) -> PotentialFutureResult<T, SpannedError> {
        match self {
            Ok(pf) => Ok(pf.map_err(move |e| e.span(span))),
            Err(_) => self
                .map(|pf| pf.map_err(move |e| e.span(span)))
                .map_err(|e| e.span(span)),
        }
    }
}

#[macro_export]
macro_rules! execute_command {
    ($command:expr, $data:expr) => {{
        match $command.execute($data) {
            Ok(o) => match o {
                $crate::types::command::PotentialFuture::Sync(t) => Ok(t),
                $crate::types::command::PotentialFuture::Async(pin) => pin.await,
            },
            Err(e) => Err(e),
        }
    }};
}

#[macro_export]
macro_rules! potential_future {
    ($expr:expr) => {
        match $expr {
            $crate::types::command::PotentialFuture::Sync(value) => value,
            $crate::types::command::PotentialFuture::Async(pin) => pin.await?,
        }
    };
}

#[macro_export]
macro_rules! try_result {
    ($result:expr ) => {{
        match $result {
            Ok(result) => result,
            Err(e) => {
                return PotentialFuture::Sync(Err(e));
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
    ) -> PotentialFutureResult<Value, SpannedError> {
        match self {
            Handler::AsyncStatic(f) => Ok(PotentialFuture::Async(f(command, data))),
            Handler::AsyncDynamic(f) => Ok(PotentialFuture::Async(f(command, data))),
            Handler::SyncStatic(f) => Ok(PotentialFuture::Sync(f(command, data)?)),
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

    fn rules_count_check(&mut self, rules: &'static [ArgRule]) -> Result<(), SpannedError> {
        let mut max_args = 0;
        for rule in rules {
            let resolve;
            let pos = match rule {
                ArgRule::Resolved(arg_pos) => {
                    resolve = true;
                    arg_pos
                }
                ArgRule::Unresolved(arg_pos) => {
                    resolve = false;
                    arg_pos
                }
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
                            arg.needs_resolving = resolve && arg.not_resolved();
                            self.needs_resolving |= arg.needs_resolving;
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
                                let arg = &mut self.args[i];
                                arg.needs_resolving = resolve && arg.not_resolved();
                                self.needs_resolving |= arg.needs_resolving;
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
                            let arg = &mut self.args[i];
                            arg.needs_resolving = resolve && arg.not_resolved();
                            self.needs_resolving |= arg.needs_resolving;
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
                        arg.needs_resolving = resolve && arg.not_resolved();
                        self.needs_resolving |= arg.needs_resolving;
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

    pub(crate) fn resolve_args(
        mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Self, SpannedError> {
        enum PendingOp {
            NeedsRaw(BoxFuture<'static, Result<Value, SpannedError>>),
            AlreadyRaw(BoxFuture<'static, Result<Value, ValueError>>),
        }
        let mut async_ops: Vec<_> = Vec::new();
        for (i, arg) in self.args.iter_mut().enumerate() {
            if arg.needs_resolving {
                let span = arg.span;
                let kind = std::mem::take(&mut arg.kind);
                let value = kind.into_value(span, data.clone());
                let value = match value {
                    PotentialFuture::Sync(value) => value,
                    PotentialFuture::Async(pin) => {
                        async_ops.push((i, (PendingOp::NeedsRaw(pin))));
                        continue;
                    }
                };
                let value = match value.as_raw(data.clone()).span(arg.span)? {
                    PotentialFuture::Sync(value) => value,
                    PotentialFuture::Async(pin) => {
                        async_ops.push((i, (PendingOp::AlreadyRaw(pin))));
                        continue;
                    }
                };
                arg.replace_value(value);
            }
        }
        if async_ops.len() > 0 {
            Ok(PotentialFuture::Async(Box::pin(async move {
                for (i, op) in async_ops {
                    let span = self.args[i].span;
                    match op {
                        PendingOp::NeedsRaw(pin) => {
                            let value = pin.await?;
                            let value =
                                potential_future!(value.as_raw(data.clone()).span_future(span)?);
                            let arg = &mut self.args[i];
                            arg.replace_value(value);
                        }
                        PendingOp::AlreadyRaw(pin) => {
                            let arg = &mut self.args[i];
                            let value = pin.await.span(span)?;
                            arg.replace_value(value);
                        }
                    }
                }
                Ok(self)
            })))
        } else {
            Ok(PotentialFuture::Sync(self))
        }
    }

    #[inline(always)]
    fn handle(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Value, SpannedError> {
        if data.should_execute() {
            return Ok(PotentialFuture::Sync(Value::None));
        }

        let handler = self.handler.clone();
        handler.handle(self, data.clone())
    }

    pub fn execute(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Value, SpannedError> {
        if self.should_resolve_args() {
            match self.resolve_args(data.clone())? {
                PotentialFuture::Sync(command) => {
                    let command = command;
                    command.handle(data)
                }
                PotentialFuture::Async(pin) => Ok(PotentialFuture::Async(Box::pin(async move {
                    let command = pin.await?;
                    Ok(potential_future!(command.handle(data)?))
                }))),
            }
        } else {
            self.handle(data)
        }
    }
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
