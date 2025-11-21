use core::fmt;
use std::{
    collections::{HashMap, VecDeque},
    ops::Range,
    pin::Pin,
    sync::{Arc, Mutex},
};

use async_recursion::async_recursion;

use crate::{EXIT, ErrorContext, FslError, FslInterpreter, InterpreterData};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FslType {
    Int,
    Float,
    Bool,
    Text,
    List,
    Var,
    Command,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(String),
    List(Vec<Value>),
    Var(String),
    Command(Command),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgPos {
    Index(usize),
    OptionalIndex(usize),
    Range(Range<usize>),
    AnyAfter(usize),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgRule {
    pub position: ArgPos,
    pub valid_types: &'static [FslType],
}

impl FslType {
    pub fn as_str(&self) -> &str {
        match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
    }
}

impl ArgRule {
    pub const fn new(position: ArgPos, valid_types: &'static [FslType]) -> Self {
        Self {
            position,
            valid_types,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CommandSpec {
    label: &'static str,
    arg_rules: &'static [ArgRule],
}

impl CommandSpec {
    pub fn new(label: &'static str, arg_rules: &'static [ArgRule]) -> Self {
        Self { label, arg_rules }
    }
}

pub trait CommandFn: Send + Sync {
    fn execute(
        &self,
        command: Command,
        interpreter: Arc<InterpreterData>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, FslError>> + Send + '_>>;
}

impl<F, Fut> CommandFn for F
where
    F: Fn(Command, Arc<InterpreterData>) -> Fut + Send + Sync,
    Fut: Future<Output = Result<Value, FslError>> + Send + 'static,
{
    fn execute(
        &self,
        command: Command,
        data: Arc<InterpreterData>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, FslError>> + Send + '_>> {
        Box::pin(self(command, data))
    }
}

pub type Executor = Option<Arc<dyn CommandFn>>;

pub struct Command {
    command_spec: CommandSpec,
    args: VecDeque<Value>,
    executor: Executor,
}

impl Command {
    pub fn new(command_spec: CommandSpec, executor: Executor) -> Self {
        Self {
            command_spec,
            args: VecDeque::new(),
            executor,
        }
    }

    pub fn get_label(&self) -> &'static str {
        self.command_spec.label
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

    fn validate_arg_range(&self, arg_rule: &ArgRule, range: &Range<usize>) -> Result<(), FslError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.as_type();
            if !arg_rule.valid_types.contains(&fsl_type) {
                return Err(FslError::IncorrectArgs(ErrorContext::new(
                    self.get_label().to_string(),
                    format!(
                        "Arg {} of command {} cannot be of type {}\nValid types are {:?}",
                        i,
                        self.get_label().to_string(),
                        fsl_type.as_str(),
                        arg_rule.valid_types
                    ),
                )));
            }
        }
        Ok(())
    }

    fn validate_args(&self) -> Result<(), FslError> {
        let mut max_args = 0;
        for arg_rule in self.command_spec.arg_rules {
            match &arg_rule.position {
                ArgPos::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.validate_arg_range(arg_rule, &range)?;
                        }
                        None => {
                            return Err(FslError::IncorrectArgs(ErrorContext::new(
                                self.get_label().to_string(),
                                format!(
                                    "Arg {} of command {} must be present and be of type {:?}",
                                    i,
                                    self.get_label(),
                                    arg_rule.valid_types
                                ),
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
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!(
                                "Command {} must have at least {} arguments and only {} were given",
                                self.get_label(),
                                range.start,
                                self.args.len(),
                            ),
                        )));
                    } else if self.args.len() > range.end {
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!(
                                "Command {} must have no more than {} arguments and {} were given",
                                self.get_label(),
                                range.end,
                                self.args.len(),
                            ),
                        )));
                    } else {
                        self.validate_arg_range(arg_rule, range)?;
                    }
                }
                ArgPos::None => {
                    if self.args.len() > 0 {
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!("Command {} does not take any arguments", self.get_label()),
                        )));
                    }
                }
                ArgPos::AnyAfter(i) => {
                    max_args = usize::MAX;
                    let range = Range::from(*i..self.args.len());
                    self.validate_arg_range(arg_rule, &range)?;
                }
                ArgPos::OptionalIndex(i) => {
                    let range = *i..*i;
                    if let Some(_) = self.args.get(*i) {
                        self.validate_arg_range(arg_rule, &range)?;
                    }
                }
            }
        }

        if self.args.len() > max_args {
            return Err(FslError::IncorrectArgs(ErrorContext::new(
                self.get_label().to_string(),
                format!(
                    "Command {} expected {} args but got {}",
                    self.get_label(),
                    max_args,
                    self.args.len()
                ),
            )));
        }
        Ok(())
    }
    const EXECUTE_EXPECT: &str = "Command should always have executor before being consumed";
    pub async fn execute(mut self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if self.get_label() == EXIT {
            return Err(FslError::ProgramExited());
        }

        self.validate_args()?;

        let executor = self.executor.take().expect(Self::EXECUTE_EXPECT);
        Ok(executor.execute(self, data).await?)
    }

    pub async fn execute_clone(&self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if self.get_label() == EXIT {
            return Err(FslError::ProgramExited());
        }

        self.validate_args()?;

        Ok(self
            .executor
            .clone()
            .expect(Self::EXECUTE_EXPECT)
            .execute(self.clone(), data)
            .await?)
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
            command_spec: self.command_spec,
            args: self.args.clone(),
            executor: self.executor.clone(),
        }
    }
}

fn gen_invalid_conversion_error(from: FslType, to: FslType) -> FslError {
    FslError::InvalidValueConversion(ErrorContext::new(
        "".into(),
        format!(
            "Cannot convert from type {} to type {}",
            from.as_str(),
            to.as_str()
        ),
    ))
}

fn gen_failed_parse_error(from: FslType, to: FslType) -> FslError {
    FslError::FailedValueParse(ErrorContext::new(
        "".into(),
        format!(
            "Failed to parse type {} to type {}",
            from.as_str(),
            to.as_str()
        ),
    ))
}

impl Value {
    pub fn eq(&self, other: &Value) -> Result<bool, FslError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(*a == *b),
            (Value::Float(a), Value::Float(b)) => Ok(*a == *b),
            (Value::Bool(a), Value::Bool(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Text(b)) => Ok(*a == *b),
            (Value::List(a_list), Value::List(b_list)) => {
                if a_list.len() != b_list.len() {
                    return Ok(false);
                } else {
                    for i in 0..a_list.len() {
                        if !a_list[i].eq(&b_list[i])? {
                            return Ok(false);
                        }
                    }
                }
                Ok(true)
            }
            (Value::None, Value::None) => Ok(true),
            _ => Err(FslError::InvalidComparison(format!(
                "Cannot compare {} with {}",
                self.as_type().as_str(),
                other.as_type().as_str()
            ))),
        }
    }

    pub fn as_type(&self) -> FslType {
        match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Var(_) => FslType::Var,
            Value::Command(_) => FslType::Command,
            Value::None => FslType::None,
        }
    }

    pub fn is_type(&self, fsl_type: FslType) -> bool {
        return self.as_type() == fsl_type;
    }

    #[async_recursion]
    pub async fn as_int(self, data: Arc<InterpreterData>) -> Result<i64, FslError> {
        let to_type = FslType::Int;
        match self {
            Value::Int(value) => Ok(value.clone()),
            Value::Float(value) => Ok(value.clone() as i64),
            Value::Var(label) => data.vars.get_value(&label)?.as_int(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_int(data.clone())
                    .await
            }
            Value::Text(value) => match value.parse::<i64>() {
                Ok(value) => Ok(value),
                Err(_) => Err(gen_failed_parse_error(FslType::Text, to_type)),
            },
            Value::Bool(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::List(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::None => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
        }
    }

    #[async_recursion]
    pub async fn as_float(self, data: Arc<InterpreterData>) -> Result<f64, FslError> {
        let to_type = FslType::Float;
        match self {
            Value::Int(value) => Ok(value.clone() as f64),
            Value::Float(value) => Ok(value.clone()),
            Value::Var(label) => data.vars.get_value(&label)?.as_float(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_float(data.clone())
                    .await
            }
            Value::Text(value) => match value.parse::<f64>() {
                Ok(value) => Ok(value),
                Err(_) => Err(gen_failed_parse_error(FslType::Text, to_type)),
            },
            Value::Bool(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::List(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::None => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
        }
    }

    #[async_recursion]
    pub async fn as_text(self, data: Arc<InterpreterData>) -> Result<String, FslError> {
        match self {
            Value::Int(value) => Ok(value.to_string()),
            Value::Float(value) => Ok(value.to_string()),
            Value::Text(value) => Ok(value),
            Value::Bool(value) => Ok(value.to_string()),
            Value::List(values) => {
                let mut output = String::new();
                output.push('[');
                let empty = values.is_empty();
                for value in values {
                    output.push_str(&format!("{}, ", value.as_text(data.clone()).await?));
                }
                if !empty {
                    output.pop();
                    output.pop();
                }
                output.push(']');
                Ok(output)
            }
            Value::Var(label) => data.vars.get_value(&label)?.as_text(data).await,
            Value::Command(command) => {
                command
                    .execute_clone(data.clone())
                    .await?
                    .as_text(data.clone())
                    .await
            }
            Value::None => Err(gen_invalid_conversion_error(self.as_type(), FslType::Text)),
        }
    }

    #[async_recursion]
    pub async fn as_bool(self, data: Arc<InterpreterData>) -> Result<bool, FslError> {
        let to_type = FslType::Bool;
        match self {
            Value::Int(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Float(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Text(value) => match value.parse::<bool>() {
                Ok(value) => Ok(value),
                Err(_) => Err(gen_failed_parse_error(FslType::Text, to_type)),
            },
            Value::Bool(value) => Ok(value.clone()),
            Value::List(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Var(label) => data.vars.get_value(&label)?.as_bool(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_bool(data.clone())
                    .await
            }
            Value::None => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
        }
    }

    #[async_recursion]
    pub async fn as_list(self, data: Arc<InterpreterData>) -> Result<Vec<Value>, FslError> {
        let to_type = FslType::List;
        match self {
            Value::Int(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Float(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Text(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::Bool(_) => Err(gen_invalid_conversion_error(self.as_type(), to_type)),
            Value::List(values) => {
                let mut computed_values = Vec::with_capacity(values.len());
                for value in values {
                    if value.is_type(FslType::Command) {
                        computed_values.push(value.as_command()?.execute(data.clone()).await?);
                    } else {
                        computed_values.push(value);
                    }
                }
                Ok(computed_values)
            }
            Value::Var(label) => data.vars.get_value(&label)?.as_list(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_list(data.clone())
                    .await
            }
            Value::None => todo!(),
        }
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if self.is_type(FslType::Var) {
            Ok(self.get_var_value(data.clone())?)
        } else if self.is_type(FslType::Command) {
            Ok(self.as_command()?.execute(data.clone()).await?)
        } else if self.is_type(FslType::List) {
            Ok(Value::List(self.as_list(data.clone()).await?))
        } else {
            Ok(self.clone())
        }
    }

    pub fn as_command(self) -> Result<Command, FslError> {
        if let Value::Command(command) = self {
            Ok(command)
        } else {
            Err(FslError::InvalidValueConversion(ErrorContext::new(
                "".into(),
                "cannot convert command into another value".into(),
            )))
        }
    }

    pub fn get_var_label(&self) -> Result<&str, FslError> {
        if let Value::Var(label) = self {
            Ok(label)
        } else {
            Err(FslError::InvalidVarLabel(ErrorContext::new(
                "".into(),
                format!("{} is not a valid var label", self.to_string()),
            )))
        }
    }

    pub fn get_var_value(&self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if let Value::Var(label) = self {
            match data.vars.get_value(label) {
                Ok(value) => match value {
                    Value::Var(_) => value.get_var_value(data),
                    _ => Ok(value),
                },
                Err(e) => Err(e),
            }
        } else {
            panic!(
                "Should not be called on non {} types",
                FslType::Var.as_str()
            );
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Float(value) => value.to_string(),
            Value::Text(value) => value.to_string(),
            Value::Bool(value) => value.to_string(),
            Value::List(values) => format!("{:?}", values),
            Value::Var(value) => value.to_string(),
            Value::Command(command) => command.get_label().to_string(),
            Value::None => "".to_string(),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Text(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Text(value.to_string())
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Text(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::List(value)
    }
}

impl From<Command> for Value {
    fn from(value: Command) -> Self {
        Value::Command(value)
    }
}
