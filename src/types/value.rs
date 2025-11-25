use std::{
    num::{ParseFloatError, ParseIntError},
    sync::Arc,
};

use async_recursion::async_recursion;

use crate::{
    InterpreterData,
    types::{
        FslType,
        command::{Command, CommandError},
    },
};

#[derive(Debug, Clone)]
pub enum ValueError {
    InvalidComparison(String),
    InvalidConversion(String),
    FailedParse(String),
    NonExistantVar(String),
    InvalidVarName(String),
    InvalidVarValue(String),
    NegativeIndex(String),
    VarMemoryLimitReached,
    CommandExecutionFailed(String),
}

impl ValueError {
    pub fn to_string(self) -> String {
        match self {
            ValueError::InvalidComparison(error_text) => error_text,
            ValueError::InvalidConversion(error_text) => error_text,
            ValueError::FailedParse(error_text) => error_text,
            ValueError::NonExistantVar(error_text) => error_text,
            ValueError::InvalidVarName(error_text) => error_text,
            ValueError::NegativeIndex(error_text) => error_text,
            ValueError::InvalidVarValue(error_text) => error_text,
            ValueError::CommandExecutionFailed(error_text) => error_text,
            ValueError::VarMemoryLimitReached => "interpreter var memory limit reached".into(),
        }
    }
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

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

impl Value {
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

    pub fn mem_size(&self) -> Option<usize> {
        match &self {
            Value::Int(_) => Some(size_of::<Value>()),
            Value::Float(_) => Some(size_of::<Value>()),
            Value::Bool(_) => Some(size_of::<Value>()),
            Value::Text(str) => Some(size_of::<Value>().checked_add(str.capacity())?),
            Value::List(list) => {
                let mut size: usize = size_of::<Value>();
                for element in list {
                    size = size.checked_add(element.mem_size()?)?;
                }
                Some(size)
            }
            Value::Var(str) => Some(size_of::<Value>().checked_add(str.capacity())?),
            Value::Command(command) => Some(size_of::<Value>().checked_add(command.mem_size()?)?),
            Value::None => Some(size_of::<Value>()),
        }
    }

    pub fn eq(&self, other: &Value) -> Result<bool, ValueError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(*a == *b),
            (Value::Float(a), Value::Int(b)) => Ok(*a == *b as f64),
            (Value::Int(a), Value::Float(b)) => Ok(*a as f64 == *b),
            (Value::Float(a), Value::Float(b)) => Ok(*a == *b),
            (Value::Bool(a), Value::Bool(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Text(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Int(b)) => Ok(a.parse::<i64>()? == *b),
            (Value::Int(a), Value::Text(b)) => Ok(*a == b.parse::<i64>()?),
            (Value::Text(a), Value::Float(b)) => Ok(a.parse::<f64>()? == *b),
            (Value::Float(a), Value::Text(b)) => Ok(*a == b.parse::<f64>()?),
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
            _ => Err(ValueError::InvalidComparison(format!(
                "cannot compare {} with {}",
                self.as_type().as_str(),
                other.as_type().as_str()
            ))),
        }
    }

    #[async_recursion]
    pub async fn as_int(self, data: Arc<InterpreterData>) -> Result<i64, ValueError> {
        let to_type = FslType::Int;
        match self {
            Value::Int(value) => Ok(value),
            Value::Float(value) => Ok(value as i64),
            Value::Var(label) => data.vars.clone_value(&label)?.as_int(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_int(data.clone())
                    .await
            }
            Value::Text(value) => match value.parse::<i64>() {
                Ok(value) => Ok(value),
                Err(_) => Err(FslType::Text.gen_parse_err(to_type)),
            },
            Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::List(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::None => Err(self.gen_conversion_err_to_type(to_type)),
        }
    }

    pub async fn as_usize(self, data: Arc<InterpreterData>) -> Result<usize, ValueError> {
        let integer = self.as_int(data).await?;
        if integer < 0 {
            Err(ValueError::NegativeIndex(format!(
                "cannot use a negative value as an index"
            )))
        } else {
            Ok(integer as usize)
        }
    }

    #[async_recursion]
    pub async fn as_float(self, data: Arc<InterpreterData>) -> Result<f64, ValueError> {
        let to_type = FslType::Float;
        match self {
            Value::Int(value) => Ok(value as f64),
            Value::Float(value) => Ok(value),
            Value::Var(label) => data.vars.clone_value(&label)?.as_float(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_float(data.clone())
                    .await
            }
            Value::Text(value) => match value.parse::<f64>() {
                Ok(value) => Ok(value),
                Err(_) => Err(FslType::Text.gen_parse_err(to_type)),
            },
            Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::List(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::None => Err(self.gen_conversion_err_to_type(to_type)),
        }
    }

    #[async_recursion]
    pub async fn as_bool(self, data: Arc<InterpreterData>) -> Result<bool, ValueError> {
        let to_type = FslType::Bool;
        match self {
            Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Text(value) => match value.parse::<bool>() {
                Ok(value) => Ok(value),
                Err(_) => Err(FslType::Text.gen_parse_err(to_type)),
            },
            Value::Bool(value) => Ok(value),
            Value::List(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Var(label) => data.vars.clone_value(&label)?.as_bool(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_bool(data.clone())
                    .await
            }
            Value::None => Err(self.gen_conversion_err_to_type(to_type)),
        }
    }

    #[async_recursion]
    pub async fn as_text(self, data: Arc<InterpreterData>) -> Result<String, ValueError> {
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
            Value::Var(label) => data.vars.clone_value(&label)?.as_text(data).await,
            Value::Command(command) => {
                command
                    .execute_clone(data.clone())
                    .await?
                    .as_text(data.clone())
                    .await
            }
            Value::None => Err(self.gen_conversion_err_to_type(FslType::Text)),
        }
    }

    #[async_recursion]
    pub async fn as_list(self, data: Arc<InterpreterData>) -> Result<Vec<Value>, ValueError> {
        let to_type = FslType::List;
        match self {
            Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Text(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type)),
            Value::List(mut values) => {
                for value in values.iter_mut() {
                    if value.is_type(FslType::Command) {
                        let new_value = std::mem::take(value)
                            .as_command()?
                            .execute(data.clone())
                            .await?;
                        *value = new_value
                    }
                }
                Ok(values)
            }
            Value::Var(label) => data.vars.clone_value(&label)?.as_list(data).await,
            Value::Command(command) => {
                command
                    .execute(data.clone())
                    .await?
                    .as_list(data.clone())
                    .await
            }
            Value::None => Err(self.gen_conversion_err_to_type(to_type)),
        }
    }

    pub async fn as_raw(
        self,
        data: Arc<InterpreterData>,
        valid_types: &[FslType],
    ) -> Result<Value, ValueError> {
        let fsl_type = self.as_type();
        let value;
        if self.is_type(FslType::Var) {
            value = self.get_var_value(data.clone())?;
        } else if self.is_type(FslType::Command) {
            value = self.as_command()?.execute(data.clone()).await?;
        } else if self.is_type(FslType::List) {
            value = Value::List(self.as_list(data.clone()).await?);
        } else {
            value = self
        }
        if valid_types.contains(&value.as_type()) {
            Ok(value)
        } else {
            Err(fsl_type.gen_conversion_err_to_types(valid_types))
        }
    }

    pub fn as_command(self) -> Result<Command, ValueError> {
        if let Value::Command(command) = self {
            Ok(command)
        } else {
            Err(ValueError::InvalidConversion(
                "failed to convert value into command".into(),
            ))
        }
    }

    pub fn get_var_label(&self) -> Result<&str, ValueError> {
        if let Value::Var(label) = self {
            Ok(label)
        } else {
            Err(ValueError::InvalidVarName(format!(
                "{} is not a valid var name",
                self.to_string()
            )))
        }
    }

    pub fn get_var_value(&self, data: Arc<InterpreterData>) -> Result<Value, ValueError> {
        if let Value::Var(label) = self {
            match data.vars.clone_value(label) {
                Ok(value) => match value {
                    Value::Var(_) => value.get_var_value(data),
                    _ => Ok(value),
                },
                Err(e) => Err(e),
            }
        } else {
            panic!(
                "should not be called on non {} types",
                FslType::Var.as_str()
            );
        }
    }

    fn gen_conversion_err_to_type(&self, to: FslType) -> ValueError {
        ValueError::InvalidConversion(format!(
            "cannot convert from type {} to type {}",
            self.as_type().as_str(),
            to.as_str(),
        ))
    }

    #[allow(dead_code)]
    fn gen_conversion_err_to_types(&self, to: &[FslType]) -> ValueError {
        ValueError::InvalidConversion(format!(
            "cannot convert self type {} to type {:?}",
            self.as_type().as_str(),
            to,
        ))
    }

    #[allow(dead_code)]
    fn gen_parse_err(&self, to: FslType) -> ValueError {
        ValueError::FailedParse(format!(
            "failed to parse type {} to type {}",
            self.as_type().as_str(),
            to.as_str()
        ))
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

impl From<ParseIntError> for ValueError {
    fn from(_: ParseIntError) -> Self {
        ValueError::FailedParse("failed to convert text to int (whole number)".into())
    }
}

impl From<ParseFloatError> for ValueError {
    fn from(_: ParseFloatError) -> Self {
        ValueError::FailedParse("failed to convert text to float (decimal)".into())
    }
}

impl From<CommandError> for ValueError {
    fn from(value: CommandError) -> Self {
        ValueError::CommandExecutionFailed(value.to_string())
    }
}
