use std::{
    borrow::Cow,
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
    pin::Pin,
    sync::Arc,
};

use crate::{
    InterpreterData,
    commands::NUMBER,
    error::{ExecutionError, ValueError},
    types::{FslType, command::Command},
};
pub type FslMap<'c> = HashMap<Cow<'c, str>, Value<'c>>;
pub type ValueResult<'c, T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send + 'c>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'c> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(Cow<'c, str>),
    List(Vec<Value<'c>>),
    Map(FslMap<'c>),
    Var(Cow<'c, str>),
    Command(Box<Command<'c>>),
    None,
}

impl<'c> Default for Value<'c> {
    fn default() -> Self {
        Self::None
    }
}

impl<'c> Value<'c> {
    pub fn from_command(command: Command<'c>) -> Self {
        Self::Command(Box::new(command))
    }

    pub fn as_type(&self) -> FslType {
        match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Map(_) => FslType::Map,
            Value::Var(_) => FslType::Var,
            Value::Command(_) => FslType::Command,
            Value::None => FslType::None,
        }
    }

    pub fn as_literal_type(&self, data: Arc<InterpreterData>) -> FslType {
        match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Map(_) => FslType::Map,
            Value::Var(label) => data.vars.get_type(label),
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
            Value::Text(str) => Some(size_of::<Value>().checked_add(str.len())?),
            Value::List(list) => {
                let mut size: usize = size_of::<Value>().checked_add(size_of::<Vec<Value>>())?;
                for element in list {
                    size = size.checked_add(element.mem_size()?)?;
                }
                Some(size)
            }
            Value::Map(map) => {
                let mut size: usize =
                    size_of::<Value>().checked_add(size_of::<HashMap<String, Value>>())?;
                for key_value_pair in map {
                    let key = key_value_pair.0;
                    let value = key_value_pair.1;
                    size = size.checked_add(key.len())?;
                    size = size.checked_add(value.mem_size()?)?;
                }
                Some(size)
            }
            Value::Var(var) => Some(size_of::<Value>().checked_add(var.len())?),
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

    pub fn as_int(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, i64, ExecutionError> {
        Box::pin(async {
            let to_type = FslType::Int;
            match self {
                Value::Int(value) => Ok(value),
                Value::Float(value) => Ok(value as i64),
                Value::Var(label) => data.vars.get(&label)?.as_int(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_int(data.clone())
                        .await
                }
                Value::Text(value) => match value.trim().parse::<i64>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(FslType::Text.gen_parse_err(to_type).into()),
                },
                Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub async fn as_usize(self, data: Arc<InterpreterData<'c>>) -> Result<usize, ExecutionError> {
        let integer = self.as_int(data).await?;
        if integer < 0 {
            Err(
                ValueError::NegativeIndex(format!("cannot use a negative value as an index"))
                    .into(),
            )
        } else {
            Ok(integer as usize)
        }
    }

    pub fn as_float(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, f64, ExecutionError> {
        Box::pin(async {
            let to_type = FslType::Float;
            match self {
                Value::Int(value) => Ok(value as f64),
                Value::Float(value) => Ok(value),
                Value::Var(label) => data.vars.get(&label)?.as_float(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_float(data.clone())
                        .await
                }
                Value::Text(value) => match value.parse::<f64>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(FslType::Text.gen_parse_err(to_type).into()),
                },
                Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub fn as_bool(self, data: Arc<InterpreterData<'c>>) -> ValueResult<'c, bool, ExecutionError> {
        Box::pin(async {
            let to_type = FslType::Bool;
            match self {
                Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Text(value) => match value.parse::<bool>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(FslType::Text.gen_parse_err(to_type).into()),
                },
                Value::Bool(value) => Ok(value),
                Value::List(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Var(label) => data.vars.get(&label)?.as_bool(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_bool(data.clone())
                        .await
                }
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub fn as_var_label(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Cow<'c, str>, ExecutionError> {
        Box::pin(async move {
            let to_type = FslType::Var;
            match self {
                Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Var(label) => Ok(label),
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_var_label(data.clone())
                        .await
                }
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub fn as_text(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Cow<'c, str>, ExecutionError> {
        Box::pin(async {
            match self {
                Value::Int(value) => Ok(Cow::Owned(value.to_string())),
                Value::Float(value) => Ok(Cow::Owned(value.to_string())),
                Value::Text(value) => Ok(value),
                Value::Bool(value) => Ok(Cow::Owned(value.to_string())),
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
                    Ok(Cow::Owned(output))
                }
                Value::Map(map) => {
                    let mut output = String::new();
                    output.push('[');
                    let empty = map.is_empty();
                    for (key, value) in map {
                        output.push_str(&format!(
                            "{}: {}, ",
                            key,
                            value.as_text(data.clone()).await?
                        ));
                    }
                    if !empty {
                        output.pop();
                        output.pop();
                    }
                    output.push(']');
                    Ok(Cow::Owned(output))
                }
                Value::Var(label) => data.vars.get(&label)?.as_text(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_text(data.clone())
                        .await
                }
                Value::None => Err(self.gen_conversion_err_to_type(FslType::Text).into()),
            }
        })
    }

    pub fn as_list(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Vec<Value<'c>>, ExecutionError> {
        Box::pin(async {
            let to_type = FslType::List;
            match self {
                Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
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
                Value::Map(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Var(label) => data.vars.get(&label)?.as_list(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_list(data.clone())
                        .await
                }
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub fn as_map(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, HashMap<Cow<'c, str>, Value<'c>>, ExecutionError> {
        Box::pin(async {
            let to_type = FslType::Map;
            match self {
                Value::Int(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.gen_conversion_err_to_type(to_type).into()),
                Value::Map(mut map) => {
                    for (_, value) in map.iter_mut() {
                        if value.is_type(FslType::Command) {
                            let new_value = std::mem::take(value)
                                .as_command()?
                                .execute(data.clone())
                                .await?;
                            *value = new_value
                        }
                    }
                    Ok(map)
                }
                Value::Var(label) => data.vars.get(&label)?.as_map(data).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_map(data.clone())
                        .await
                }
                Value::None => Err(self.gen_conversion_err_to_type(to_type).into()),
            }
        })
    }

    pub fn as_number(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Value<'c>, ExecutionError> {
        Box::pin(async move {
            match self {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(n) => Ok(Value::Float(n)),
                Value::Text(n) => match n.parse::<f64>() {
                    Ok(n) => Ok(Value::Float(n)),
                    Err(_) => match n.parse::<i64>() {
                        Ok(n) => Ok(Value::Int(n)),
                        Err(_) => Err(Value::Text(n)
                            .gen_conversion_err_to_types(&[FslType::Int, FslType::Float])
                            .into()),
                    },
                },
                Value::Var(label) => data.vars.get(&label)?.as_number(data.clone()).await,
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_number(data.clone())
                        .await
                }
                _ => Err(self.gen_conversion_err_to_types(NUMBER).into()),
            }
        })
    }

    /// Converts value into it's most raw state ensuring result is of valid type
    pub fn as_raw(
        self,
        data: Arc<InterpreterData<'c>>,
        valid_types: &'static [FslType],
    ) -> ValueResult<'c, Value<'c>, ExecutionError> {
        Box::pin(async move {
            let value;
            match self.as_type() {
                FslType::Int => value = self,
                FslType::Float => value = self,
                FslType::Bool => value = self,
                FslType::Text => value = self,
                FslType::List => {
                    value = Value::List(self.as_list(data.clone()).await?);
                }
                FslType::Map => {
                    value = Value::Map(self.as_map(data.clone()).await?);
                }
                FslType::Var => {
                    let result = self.get_var_value(data.clone())?;
                    value = result.as_raw(data.clone(), valid_types).await?;
                }
                FslType::Command => {
                    let result = self.as_command()?.execute(data.clone()).await?;
                    value = result.as_raw(data.clone(), valid_types).await?;
                }
                FslType::None => value = self,
            }

            if valid_types.contains(&value.as_type()) {
                Ok(value)
            } else {
                Err(value.gen_conversion_err_to_types(valid_types).into())
            }
        })
    }

    /// Converts value into it's most raw state without checking what the result type it is
    pub fn as_raw_unchecked(
        self,
        data: Arc<InterpreterData<'c>>,
    ) -> ValueResult<'c, Value<'c>, ExecutionError> {
        Box::pin(async move {
            let value;
            match self.as_type() {
                FslType::Int => value = self,
                FslType::Float => value = self,
                FslType::Bool => value = self,
                FslType::Text => value = self,
                FslType::List => {
                    value = Value::List(self.as_list(data.clone()).await?);
                }
                FslType::Map => {
                    value = Value::Map(self.as_map(data.clone()).await?);
                }
                FslType::Var => {
                    let result = self.get_var_value(data.clone())?;
                    value = result.as_raw_unchecked(data.clone()).await?;
                }
                FslType::Command => {
                    let result = self.as_command()?.execute(data.clone()).await?;
                    value = result.as_raw_unchecked(data.clone()).await?;
                }
                FslType::None => value = self,
            }

            Ok(value)
        })
    }

    // Attempts to convert value to a value that can be used to access indices in a map or list
    pub async fn as_key(
        self,
        data: Arc<InterpreterData<'c>>,
        key_types: &'static [FslType],
    ) -> Result<Vec<Value<'c>>, ExecutionError> {
        let accesor = self.as_raw(data.clone(), key_types).await?;

        match accesor {
            Value::List(values) => Ok(values),
            _ => Ok(vec![accesor]),
        }
    }

    pub fn as_command(self) -> Result<Command<'c>, ValueError> {
        if let Value::Command(command) = self {
            Ok(*command)
        } else {
            Err(ValueError::InvalidConversion(
                "failed to convert value into command".into(),
            ))
        }
    }

    pub fn get_var_label(&self) -> Result<Cow<'c, str>, ValueError> {
        if let Value::Var(label) = self {
            Ok(label.clone())
        } else {
            Err(ValueError::InvalidVarName(format!(
                "{} is not a valid var name",
                self.to_string()
            )))
        }
    }

    pub fn get_command_label(&self) -> Option<&str> {
        if let Value::Command(command) = self {
            Some(command.get_label())
        } else {
            None
        }
    }

    pub fn get_var_value(&self, data: Arc<InterpreterData<'c>>) -> Result<Value<'c>, ValueError> {
        if let Value::Var(label) = self {
            match data.vars.get(label) {
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
            "cannot convert value \"{}\" from type {} to type {}",
            self.to_string(),
            self.as_type().as_str(),
            to.as_str(),
        ))
    }

    fn gen_conversion_err_to_types(&self, to: &[FslType]) -> ValueError {
        ValueError::InvalidConversion(format!(
            "cannot convert value \"{}\" from type {} to type {:?}",
            self.to_string(),
            self.as_type().as_str(),
            to,
        ))
    }
}

impl<'c> ToString for Value<'c> {
    fn to_string(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Float(value) => value.to_string(),
            Value::Text(value) => value.to_string(),
            Value::Bool(value) => value.to_string(),
            Value::List(values) => format!("{:?}", values),
            Value::Map(map) => format!("{:?}", map),
            Value::Var(value) => value.to_string(),
            Value::Command(command) => command.get_label().to_string(),
            Value::None => "".to_string(),
        }
    }
}

impl<'c> From<i64> for Value<'c> {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl<'c> From<usize> for Value<'c> {
    fn from(value: usize) -> Self {
        Value::Int(value as i64)
    }
}

impl<'c> From<f64> for Value<'c> {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl<'c> From<Cow<'c, str>> for Value<'c> {
    fn from(value: Cow<'c, str>) -> Self {
        Value::Text(value)
    }
}

impl<'c> From<String> for Value<'c> {
    fn from(value: String) -> Self {
        Value::Text(Cow::Owned(value))
    }
}

impl<'c> From<&'c str> for Value<'c> {
    fn from(value: &'c str) -> Self {
        Value::Text(Cow::Borrowed(value))
    }
}

impl<'c> From<char> for Value<'c> {
    fn from(value: char) -> Self {
        Value::Text(Cow::Owned(value.to_string()))
    }
}

impl<'c> From<bool> for Value<'c> {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl<'c> From<Vec<Value<'c>>> for Value<'c> {
    fn from(value: Vec<Value<'c>>) -> Self {
        Value::List(value)
    }
}

impl<'c> From<Command<'c>> for Value<'c> {
    fn from(value: Command<'c>) -> Self {
        Value::Command(Box::new(value))
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
