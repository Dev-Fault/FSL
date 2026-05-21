use core::fmt;
use std::{borrow::Cow, collections::VecDeque, ops::Range, sync::Arc};

use futures::future::BoxFuture;
use tokio::sync::Mutex;

use crate::{
    InterpreterData,
    data::UserDefinitions,
    error::CommandError,
    parser::Span,
    types::{
        FslType,
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
pub struct Argument<'c> {
    pub value: Value<'c>,
    pub span: Span<'c>,
}

impl<'c> FslValue<'c> for Argument<'c> {
    fn as_type(&self) -> FslType {
        self.value.as_type()
    }

    fn as_literal_type(&self, data: Arc<InterpreterData>) -> FslType {
        self.value.as_literal_type(data)
    }

    fn is_type(&self, fsl_type: FslType) -> bool {
        self.value.is_type(fsl_type)
    }

    fn mem_size(&self) -> Option<usize> {
        self.value.mem_size()
    }

    fn equal(&self, other: &Value) -> Result<bool, CommandError> {
        self.value.equal(other)
    }

    fn as_int(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, i64, CommandError> {
        self.value.as_int(data)
    }

    fn as_usize(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, usize, CommandError> {
        self.value.as_usize(data)
    }

    fn as_float(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, f64, CommandError> {
        self.value.as_float(data)
    }

    fn as_bool(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, bool, CommandError> {
        self.value.as_bool(data)
    }

    fn as_var_label(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Cow<'c, str>, CommandError> {
        self.value.as_var_label(data)
    }

    fn as_text(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Cow<'c, str>, CommandError> {
        self.value.as_text(data)
    }

    fn as_list(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Vec<Value<'c>>, CommandError> {
        self.value.as_list(data)
    }

    fn as_map(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, std::collections::HashMap<Cow<'c, str>, Value<'c>>, CommandError> {
        self.value.as_map(data)
    }

    fn as_number(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, Value<'c>, CommandError> {
        self.value.as_number(data)
    }

    fn as_raw(
        self,
        data: Arc<InterpreterData<'c>>,
        valid_types: &'static [FslType],
    ) -> ValueResult<'c, Value<'c>, CommandError> {
        self.value.as_raw(data, valid_types)
    }

    fn as_raw_unchecked(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Value<'c>, CommandError> {
        self.value.as_raw_unchecked(data)
    }

    fn as_key(
        self,
        data: Arc<InterpreterData<'c>>,
        key_types: &'static [FslType],
    ) -> ValueResult<'c, Vec<Value<'c>>, CommandError> {
        self.value.as_key(data, key_types)
    }

    fn as_command(self) -> Result<Command<'c>, CommandError> {
        self.value.as_command()
    }

    fn get_var_label(&self) -> Result<Cow<'c, str>, CommandError> {
        self.value.get_var_label()
    }

    fn get_command_label(&self) -> Option<&str> {
        self.value.get_command_label()
    }

    fn get_var_value(&self, data: Arc<InterpreterData<'c>>) -> Result<Value<'c>, CommandError> {
        self.value.get_var_value(data)
    }
}

impl<'c> Argument<'c> {
    pub fn new(value: Value<'c>, span: Span<'c>) -> Self {
        Self { value, span }
    }
}

pub struct Command<'c> {
    label: &'c str,
    arg_rules: &'static [ArgRule],
    args: VecDeque<Argument<'c>>,
    handler: Handler,
    span: Span<'c>,
}

impl<'c> Command<'c> {
    pub fn mem_size(&self) -> Option<usize> {
        let mut size = size_of::<Command>();
        for arg in &self.args {
            size = size.checked_add(arg.value.mem_size()?)?;
        }
        Some(size)
    }

    pub fn new(
        label: &'c str,
        arg_rules: &'static [ArgRule],
        handler: Handler,
        span: Span<'c>,
    ) -> Self {
        Self {
            args: VecDeque::new(),
            handler: handler,
            label,
            arg_rules,
            span,
        }
    }

    pub fn from_def(value: &CommandDef, span: Span<'c>) -> Self {
        Self {
            label: value.label,
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

    pub fn set_args(&mut self, args: VecDeque<Argument<'c>>) {
        self.args = args;
    }

    pub fn take_args(self) -> VecDeque<Argument<'c>> {
        self.args
    }

    pub fn pop_front_arg(&mut self) -> Option<Argument<'c>> {
        self.args.pop_front()
    }

    pub fn get_args(&self) -> &VecDeque<Argument<'c>> {
        &self.args
    }

    pub fn get_args_mut(&mut self) -> &mut VecDeque<Argument<'c>> {
        &mut self.args
    }

    fn validate_arg_range(
        &self,
        arg_rule: &ArgRule,
        range: &Range<usize>,
    ) -> Result<(), CommandError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.value.as_type();
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

        if data.should_execute().await {
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
            span: self.span,
        }
    }
}

#[derive(Debug, Default)]
pub struct UserDef<'c> {
    pub label: Cow<'c, str>,
    pub parameters: Mutex<VecDeque<Cow<'c, str>>>,
    pub commands: Mutex<Vec<Command<'c>>>,
    pub local_defs: Arc<Mutex<UserDefinitions<'c>>>,
}

impl<'c> UserDef<'c> {
    pub fn declaration(label: Cow<'c, str>) -> Self {
        Self {
            label,
            ..Default::default()
        }
    }

    pub async fn define(&self, parameters: VecDeque<Cow<'c, str>>, commands: Vec<Command<'c>>) {
        let mut old_parameters = self.parameters.lock().await;
        *old_parameters = parameters;
        let mut old_commands = self.commands.lock().await;
        *old_commands = commands;
    }
}
