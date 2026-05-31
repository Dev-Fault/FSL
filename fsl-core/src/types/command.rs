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
        FslType,
        list::List,
        map::Map,
        value::{FslValue, Value, ValueResult},
    },
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
            handler: handler,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub value: Value,
    pub span: Span,
}

impl Argument {
    pub async fn mut_with<F, R>(
        &mut self,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: FnOnce(&mut Value) -> Result<R, ExecutionError>,
    {
        if let Value::Var(label) = &self.value {
            let vars = data.vars.read().await;
            vars.with_mut(&label, f)
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))?
        } else {
            f(&mut self.value)
        }
    }
}

impl FslValue<Argument, ExecutionError> for Argument {
    fn as_type(&self) -> FslType {
        self.value.as_type()
    }

    async fn as_literal_type(&self, data: Arc<InterpreterData>) -> Result<FslType, RuntimeError> {
        self.value.as_literal_type(data).await
    }

    fn is_type(&self, fsl_type: FslType) -> bool {
        self.value.is_type(fsl_type)
    }

    fn mem_size(&self) -> Result<usize, RuntimeError> {
        self.value.mem_size()
    }

    fn equal(&self, other: &Argument, data: Arc<InterpreterData>) -> Result<bool, ExecutionError> {
        self.value
            .equal(&other.value, data.clone())
            .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
    }

    fn soft_equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        self.value
            .soft_equal(&other.value, data.clone())
            .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
    }

    fn as_int(self, data: Arc<InterpreterData>) -> ValueResult<i64, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_int(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_usize(self, data: Arc<InterpreterData>) -> ValueResult<usize, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_usize(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_float(self, data: Arc<InterpreterData>) -> ValueResult<f64, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_float(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_bool(self, data: Arc<InterpreterData>) -> ValueResult<bool, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_bool(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_var_label(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_var_label(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_text(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_text(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_list(self, data: Arc<InterpreterData>) -> ValueResult<List, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_list(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_map(self, data: Arc<InterpreterData>) -> ValueResult<Map, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_map(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_number(self, data: Arc<InterpreterData>) -> ValueResult<Argument, ExecutionError> {
        Box::pin(async move {
            let number = self.value.as_number(data.clone()).await;
            // dbg!(self.span.clone());
            let number = number.map(|v| Self::new(v, self.span.clone()));
            let number = number.map_err(|e| e.to_exec(self.span.clone(), data.source.clone()));
            number
        })
    }

    fn as_raw_checked(
        self,
        data: Arc<InterpreterData>,
        valid_types: &'static [FslType],
    ) -> ValueResult<Argument, ExecutionError> {
        Box::pin(async move {
            let raw = self.value.as_raw_checked(data.clone(), valid_types).await;
            let raw = raw.map(|v| Self::new(v, self.span.clone()));
            let raw = raw.map_err(|e| e.to_exec(self.span.clone(), data.source.clone()));
            raw
        })
    }

    fn as_raw(self, data: Arc<InterpreterData>) -> ValueResult<Argument, ExecutionError> {
        Box::pin(async move {
            let raw = self.value.as_raw(data.clone()).await;
            let raw = raw.map(|v| Self::new(v, self.span.clone()));
            let raw = raw.map_err(|e| e.to_exec(self.span.clone(), data.source.clone()));
            raw
        })
    }

    fn as_list_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<usize>, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_list_key(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_map_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<SourceStr>, ExecutionError> {
        Box::pin(async move {
            self.value
                .as_map_key(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
        })
    }

    fn as_command(self, data: Arc<InterpreterData>) -> Result<Command, ExecutionError> {
        self.value
            .as_command(data.clone())
            .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
    }

    fn get_var_label(&self, data: Arc<InterpreterData>) -> Result<SourceStr, ExecutionError> {
        self.value
            .get_var_label(data.clone())
            .map_err(|e| e.to_exec(self.span.clone(), data.source.clone()))
    }

    fn get_command_label(&self) -> Option<&str> {
        self.value.get_command_label()
    }
}

impl Argument {
    pub fn new(value: Value, span: Span) -> Self {
        Self { value, span }
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
    pub fn mem_size(&self) -> Result<usize, RuntimeError> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size
                .checked_add(arg.value.mem_size()?)
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
            handler: handler,
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

    pub fn get_label(&self) -> &str {
        &self.label
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

    fn validate_arg_range(
        &self,
        arg_rule: &ArgRule,
        range: &Range<usize>,
        data: Arc<InterpreterData>,
    ) -> Result<(), ExecutionError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.value.as_type();
            if !arg_rule.valid_types.contains(&fsl_type) {
                return Err(RuntimeError::WrongArgType {
                    command_label: self.get_label().to_string(),
                    arg_number: i,
                    fsl_type,
                    expected: arg_rule.valid_types,
                }
                .to_exec(arg.span.clone(), data.source.clone()));
            }
        }
        Ok(())
    }

    fn validate_args(&self, data: Arc<InterpreterData>) -> Result<(), ExecutionError> {
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
                            self.validate_arg_range(arg_rule, &range, data.clone())?;
                        }
                        None => {
                            return Err(RuntimeError::MissingArg {
                                command_label: self.get_label().to_string(),
                                arg_number: *i,
                            }
                            .to_exec(self.span.clone(), data.source.clone()));
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
                        .to_exec(self.span.clone(), data.source.clone()));
                    } else if self.args.len() > range.end {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.get_label().to_string(),
                            expected: ExpectedArgs::AtMost(range.end),
                            got: self.args.len(),
                        }
                        .to_exec(self.span.clone(), data.source.clone()));
                    } else {
                        self.validate_arg_range(arg_rule, range, data.clone())?;
                    }
                }
                ArgPos::None => {
                    if self.args.len() > 0 {
                        return Err(RuntimeError::WrongArgCount {
                            command_label: self.get_label().to_string(),
                            expected: ExpectedArgs::None,
                            got: self.args.len(),
                        }
                        .to_exec(self.span.clone(), data.source.clone()));
                    }
                }
                ArgPos::AnyFrom(i) => {
                    max_args = usize::MAX;
                    let range = Range::from(*i..self.args.len());
                    self.validate_arg_range(arg_rule, &range, data.clone())?;
                }
                ArgPos::OptionalIndex(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    if let Some(_) = self.args.get(*i) {
                        self.validate_arg_range(arg_rule, &range, data.clone())?;
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
            .to_exec(self.span.clone(), data.source.clone()));
        }
        Ok(())
    }

    pub async fn execute(self, data: Arc<InterpreterData>) -> Result<Value, ExecutionError> {
        self.validate_args(data.clone())?;

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
            span: self.span.clone(),
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
