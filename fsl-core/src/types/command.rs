use core::fmt;
use std::{ops::Range, sync::Arc};

use futures::future::BoxFuture;

use crate::{
    InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    potential_future,
    potential_futures::{PotentialFuture, PotentialFutureResult},
    source_str::SourceStr,
    span::Span,
    types::{
        ValueType,
        argument::{Argument, ArgumentKind},
        value::Value,
    },
};

pub type InterpreterFut = BoxFuture<'static, Result<Value, SpannedError>>;

pub type InterpreterFn = Arc<dyn Fn(Command, Arc<InterpreterData>) -> InterpreterFut + Send + Sync>;

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
pub enum ArgRule<T: std::fmt::Debug> {
    Literal(T),
    Raw(T),
    Mutable(T),
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
    Positional(&'static [ArgRule<ArgPos>]),
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
    pub args: Vec<Argument>,
    handler: Handler,
    pub span: Span,
    needs_resolving: bool,
}
impl Eq for Command {}

impl Command {
    pub fn mem_size(&self) -> Result<usize, RuntimeError> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size
                .checked_add(arg.mem_size()?)
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
            args: Vec::new(),
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
            args: Vec::new(),
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

    pub fn set_args(
        &mut self,
        args: Vec<Argument>,
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        self.args = args;
        self.validate_args(data)?;
        Ok(())
    }

    pub(crate) fn should_resolve_args(&self) -> bool {
        self.needs_resolving
    }

    /// Tags argument with rule and returns if the argument will need resolving or not
    fn tag_arg<T: std::fmt::Debug>(
        rule: &ArgRule<T>,
        arg: &mut Argument,
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        match rule {
            ArgRule::Literal(_) => {
                // Don't resolve already literal values (better for performance)
                if let ArgumentKind::Value(value) = &arg.kind {
                    if value.is_literal() {
                        return Ok(());
                    }
                }
                arg.resolve_to = ArgRule::Literal(());
            }
            ArgRule::Raw(_) => arg.resolve_to = ArgRule::Raw(()),
            ArgRule::Mutable(_) => {
                if let ArgumentKind::Accessor(_) = &arg.kind {
                    arg.resolve_to = ArgRule::Mutable(());
                } else if !arg.is_type(ValueType::Var, data)? {
                    arg.resolve_to = ArgRule::Mutable(());
                }
            }
        }
        Ok(())
    }

    fn rules_count_check(
        &mut self,
        rules: &'static [ArgRule<ArgPos>],
        data: Arc<InterpreterData>,
    ) -> Result<(), SpannedError> {
        let mut max_args = 0;
        for rule in rules {
            let pos = match rule {
                ArgRule::Literal(arg_pos) => arg_pos,
                ArgRule::Raw(arg_pos) => arg_pos,
                ArgRule::Mutable(arg_pos) => arg_pos,
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
                            Self::tag_arg(rule, arg, data.clone())?;
                            self.needs_resolving |= !matches!(arg.resolve_to, ArgRule::Raw(_));
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
                            let arg = &mut self.args[i];
                            Self::tag_arg(rule, arg, data.clone())?;
                            self.needs_resolving |= !matches!(arg.resolve_to, ArgRule::Raw(_));
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
                        let arg = &mut self.args[i];
                        Self::tag_arg(rule, arg, data.clone())?;
                        self.needs_resolving |= !matches!(arg.resolve_to, ArgRule::Raw(_));
                    }
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    if let Some(arg) = self.args.get_mut(*i) {
                        Self::tag_arg(rule, arg, data.clone())?;
                        self.needs_resolving |= !matches!(arg.resolve_to, ArgRule::Raw(_));
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

    pub fn validate_args(&mut self, data: Arc<InterpreterData>) -> Result<(), SpannedError> {
        match self.signature {
            CommandSignature::Positional(rules) => self.rules_count_check(rules, data),
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
            Literal(BoxFuture<'static, Result<Value, SpannedError>>),
            Command(BoxFuture<'static, PotentialFutureResult<Value, SpannedError>>),
            RootCommand(BoxFuture<'static, Result<Value, SpannedError>>),
        }
        let mut async_ops: Vec<_> = Vec::new();
        for (i, arg) in self.args.iter_mut().enumerate() {
            match arg.resolve_to {
                ArgRule::Literal(_) => {
                    let value = match arg.to_inner(data.clone())? {
                        PotentialFuture::Sync(value) => value,
                        PotentialFuture::Async(pin) => {
                            async_ops.push((i, (PendingOp::Literal(pin))));
                            continue;
                        }
                    };
                    arg.replace_value(value);
                }
                ArgRule::Raw(_) => {
                    continue;
                }
                ArgRule::Mutable(_) => {
                    let span = arg.span;
                    if let ArgumentKind::Accessor(accessor) = &mut arg.kind {
                        if accessor.root.value.is_command() {
                            let root_value = std::mem::take(&mut accessor.root.value);
                            let command = root_value.to_command().span(span)?;
                            match command.execute(data.clone())? {
                                PotentialFuture::Sync(value) => {
                                    accessor.root.value = value;
                                }
                                PotentialFuture::Async(pin) => {
                                    async_ops.push((
                                        i,
                                        PendingOp::RootCommand(Box::pin(async move {
                                            let value = pin.await?;
                                            Ok(value)
                                        })),
                                    ));
                                }
                            }
                        } else {
                            arg.resolve_to = ArgRule::Raw(());
                        }
                    } else {
                        let value = arg.take_value();
                        if value.is_command() {
                            let data_clone = data.clone();
                            async_ops.push((
                                i,
                                PendingOp::Command(Box::pin(async move {
                                    let command = value.to_command().span(span)?;
                                    Ok(command.execute(data_clone)?)
                                })),
                            ));
                            continue;
                        }
                        arg.replace_value(value);
                    }
                }
            }
        }
        if async_ops.len() > 0 {
            Ok(PotentialFuture::Async(Box::pin(async move {
                for (i, op) in async_ops {
                    match op {
                        PendingOp::Literal(pin) => {
                            let arg = &mut self.args[i];
                            let value = pin.await?;
                            arg.replace_value(value);
                        }
                        PendingOp::Command(pin) => {
                            let arg = &mut self.args[i];
                            let value = potential_future!(pin.await?);
                            arg.replace_value(value);
                        }
                        PendingOp::RootCommand(pin) => {
                            let arg = &mut self.args[i];
                            let value = pin.await?;
                            match &mut arg.kind {
                                ArgumentKind::Value(_) => unreachable!(),
                                ArgumentKind::Accessor(accessor) => {
                                    accessor.root.value = value;
                                }
                            }
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
