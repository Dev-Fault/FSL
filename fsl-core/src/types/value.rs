use std::{pin::Pin, sync::Arc};

use crate::{
    FslInterpreter, InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    potential_future,
    source_str::SourceStr,
    span::Span,
    types::{
        LIST_KEY, MAP_KEY, ValueType,
        argument::Argument,
        command::{Command, PotentialFuture, PotentialFutureResult},
        list::List,
        map::{FslMap, Map},
    },
};
pub type ValueResult<T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(SourceStr),
    List(List),
    Map(Map),
    Var(SourceStr),
    Command(Box<Command>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Value {
    pub fn into_str(self) -> Result<SourceStr, RuntimeError> {
        match self {
            Value::Int(i) => Ok(SourceStr::Owned(i.to_string())),
            Value::Float(f) => Ok(SourceStr::Owned(f.to_string())),
            Value::Bool(b) => Ok(SourceStr::Owned(b.to_string())),
            Value::Map(map) => Ok(SourceStr::Owned(map.to_string())),
            Value::List(list) => Ok(SourceStr::Owned(list.to_string())),
            Value::Text(source_str) => Ok(source_str),
            Value::Var(label) => Ok(label),
            _ => Err(self.conversion_err(&[ValueType::Text])),
        }
    }

    pub fn into_int(self) -> Result<i64, RuntimeError> {
        match &self {
            Value::Int(i) => Ok(*i),
            Value::Text(source_str) => match FslInterpreter::parse_number(&source_str)? {
                Value::Int(i) => Ok(i),
                _ => Err(self.conversion_err(&[ValueType::Int])),
            },
            _ => Err(self.conversion_err(&[ValueType::Int])),
        }
    }

    pub fn into_float(self) -> Result<f64, RuntimeError> {
        match &self {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            Value::Text(source_str) => match FslInterpreter::parse_number(&source_str)? {
                Value::Float(f) => Ok(f),
                Value::Int(i) => Ok(i as f64),
                _ => Err(self.conversion_err(&[ValueType::Float])),
            },
            _ => Err(self.conversion_err(&[ValueType::Float])),
        }
    }

    pub fn into_number(self) -> Result<Number, RuntimeError> {
        match &self {
            Value::Int(i) => Ok(Number::Int(*i)),
            Value::Float(f) => Ok(Number::Float(*f)),
            Value::Text(source_str) => match FslInterpreter::parse_number(&source_str)? {
                Value::Int(i) => Ok(Number::Int(i)),
                Value::Float(f) => Ok(Number::Float(f)),
                _ => Err(self.conversion_err(&[ValueType::Float])),
            },
            _ => Err(self.conversion_err(&[ValueType::Int, ValueType::Float])),
        }
    }

    pub fn into_bool(self) -> Result<bool, RuntimeError> {
        if let Self::Bool(b) = self {
            Ok(b)
        } else {
            Err(self.conversion_err(&[ValueType::Bool]))
        }
    }

    pub fn into_usize(self) -> Result<usize, RuntimeError> {
        if let Self::Int(i) = self {
            Ok(i as usize)
        } else {
            Err(self.conversion_err(&[ValueType::Int]))
        }
    }

    pub fn into_list_indexer(self) -> Result<Vec<usize>, RuntimeError> {
        match self {
            Value::Int(i) => Ok(vec![i as usize]),
            Value::List(values) => {
                let values = values.take();
                let mut indices = Vec::with_capacity(values.len());
                for value in values {
                    indices.push(value.into_usize()?);
                }
                Ok(indices)
            }
            _ => Err(self.conversion_err(&[ValueType::Int, ValueType::List])),
        }
    }

    pub fn into_map_indexer(self) -> Result<Vec<SourceStr>, RuntimeError> {
        match self {
            Value::Text(source_str) => Ok(vec![source_str]),
            Value::List(values) => {
                let values = values.take();
                let mut indices = Vec::with_capacity(values.len());
                for value in values {
                    indices.push(value.into_str()?);
                }
                Ok(indices)
            }
            _ => Err(self.conversion_err(&[ValueType::Int, ValueType::List])),
        }
    }

    pub fn into_list(self) -> Result<List, RuntimeError> {
        if let Self::List(list) = self {
            Ok(list)
        } else {
            Err(self.conversion_err(&[ValueType::List]))
        }
    }

    pub fn into_map(self) -> Result<Map, RuntimeError> {
        if let Self::Map(map) = self {
            Ok(map)
        } else {
            Err(self.conversion_err(&[ValueType::List]))
        }
    }

    pub fn into_var(self) -> Result<SourceStr, RuntimeError> {
        if let Self::Var(label) = self {
            Ok(label)
        } else {
            Err(self.conversion_err(&[ValueType::List]))
        }
    }

    pub fn into_command(self) -> Result<Box<Command>, RuntimeError> {
        if let Self::Command(command) = self {
            Ok(command)
        } else {
            Err(self.conversion_err(&[ValueType::List]))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueError {
    Runtime(RuntimeError),
    Execution(SpannedError),
}

impl ToSpannedError for ValueError {
    fn span(self, span: Span) -> SpannedError {
        match self {
            ValueError::Runtime(runtime_error) => runtime_error.span(span),
            ValueError::Execution(execution_error) => execution_error,
        }
    }
}

impl From<RuntimeError> for ValueError {
    fn from(value: RuntimeError) -> Self {
        Self::Runtime(value)
    }
}

impl From<SpannedError> for ValueError {
    fn from(value: SpannedError) -> Self {
        Self::Execution(value)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

impl Value {
    pub fn to_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Text(_) => ValueType::Text,
            Value::Bool(_) => ValueType::Bool,
            Value::List(_) => ValueType::List,
            Value::Map(_) => ValueType::Map,
            Value::Var(_) => ValueType::Var,
            Value::Command(_) => ValueType::Command,
            Value::None => ValueType::None,
        }
    }

    pub fn to_inner_type(&self, data: Arc<InterpreterData>) -> Result<ValueType, RuntimeError> {
        Ok(match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Text(_) => ValueType::Text,
            Value::Bool(_) => ValueType::Bool,
            Value::List(_) => ValueType::List,
            Value::Map(_) => ValueType::Map,
            Value::Var(label) => data.vars.read().get_type(label)?,
            Value::Command(_) => ValueType::Command,
            Value::None => ValueType::None,
        })
    }

    pub fn is_var(&self) -> bool {
        match self {
            Value::Var(_) => true,
            _ => false,
        }
    }

    pub fn is_type(&self, fsl_type: ValueType) -> bool {
        self.to_type() == fsl_type
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Value::Var(_) => false,
            Value::Command(_) => false,
            Value::List(_) => false,
            Value::Map(_) => false,

            Value::Int(_) => true,
            Value::Float(_) => true,
            Value::Bool(_) => true,
            Value::Text(_) => true,
            Value::None => true,
        }
    }

    pub fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self {
            Value::Int(_) => Ok(size_of::<Value>()),
            Value::Float(_) => Ok(size_of::<Value>()),
            Value::Bool(_) => Ok(size_of::<Value>()),
            Value::Text(str) => size_of::<Value>()
                .checked_add(str.len())
                .ok_or(RuntimeError::Overflow),
            Value::List(list) => {
                let mut size: usize = size_of::<Value>();
                for element in list.iter() {
                    size = size
                        .checked_add(element.mem_size()?)
                        .ok_or(RuntimeError::Overflow)?;
                }
                Ok(size)
            }
            Value::Map(map) => {
                let mut size: usize = size_of::<Value>();
                for key_value_pair in map.iter() {
                    let key = key_value_pair.0;
                    let value = key_value_pair.1;
                    size = size.checked_add(key.len()).ok_or(RuntimeError::Overflow)?;
                    size = size
                        .checked_add(value.mem_size()?)
                        .ok_or(RuntimeError::Overflow)?;
                }
                Ok(size)
            }
            Value::Var(var) => size_of::<Value>()
                .checked_add(var.len())
                .ok_or(RuntimeError::Overflow),
            Value::Command(command) => size_of::<Value>()
                .checked_add(command.mem_size()?)
                .ok_or(RuntimeError::Overflow),
            Value::None => Ok(size_of::<Value>()),
        }
    }

    pub fn equal(&self, other: &Value, _: Arc<InterpreterData>) -> Result<bool, ValueError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(*a == *b),
            (Value::Float(a), Value::Int(b)) => Ok(*a == *b as f64),
            (Value::Int(a), Value::Float(b)) => Ok(*a as f64 == *b),
            (Value::Float(a), Value::Float(b)) => Ok(*a == *b),
            (Value::Bool(a), Value::Bool(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Text(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Int(b)) => Ok(a
                .parse::<i64>()
                .map_err(|_| self.conversion_err_to_type(ValueType::Int))?
                == *b),
            (Value::Int(a), Value::Text(b)) => Ok(*a
                == b.parse::<i64>()
                    .map_err(|_| self.conversion_err_to_type(ValueType::Int))?),
            (Value::Text(a), Value::Float(b)) => Ok(a
                .parse::<f64>()
                .map_err(|_| self.conversion_err_to_type(ValueType::Float))?
                == *b),
            (Value::Float(a), Value::Text(b)) => Ok(*a
                == b.parse::<f64>()
                    .map_err(|_| self.conversion_err_to_type(ValueType::Float))?),
            (Value::List(a), Value::List(b)) => Ok(a == b),
            (Value::Map(a), Value::Map(b)) => Ok(a == b),
            (Value::None, Value::None) => Ok(true),
            _ => Err(RuntimeError::InvalidComparison {
                a: self.to_string(),
                b: other.to_string(),
            }
            .into()),
        }
    }

    // Identical to equal except that it coerces list elements to the same type
    // ["1", "2", "3"].eq([1, 2, 3]) would be true
    // Much slower than equal
    pub fn soft_equal(
        &self,
        other: &Value,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ValueError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(*a == *b),
            (Value::Float(a), Value::Int(b)) => Ok(*a == *b as f64),
            (Value::Int(a), Value::Float(b)) => Ok(*a as f64 == *b),
            (Value::Float(a), Value::Float(b)) => Ok(*a == *b),
            (Value::Bool(a), Value::Bool(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Text(b)) => Ok(*a == *b),
            (Value::Text(a), Value::Int(b)) => Ok(a
                .parse::<i64>()
                .map_err(|_| self.conversion_err_to_type(ValueType::Int))?
                == *b),
            (Value::Int(a), Value::Text(b)) => Ok(*a
                == b.parse::<i64>()
                    .map_err(|_| self.conversion_err_to_type(ValueType::Int))?),
            (Value::Text(a), Value::Float(b)) => Ok(a
                .parse::<f64>()
                .map_err(|_| self.conversion_err_to_type(ValueType::Float))?
                == *b),
            (Value::Float(a), Value::Text(b)) => Ok(*a
                == b.parse::<f64>()
                    .map_err(|_| self.conversion_err_to_type(ValueType::Float))?),
            (Value::List(a), Value::List(b)) => {
                if a == b {
                    return Ok(true);
                }
                if a.len() != b.len() {
                    return Ok(false);
                }
                for i in 0..a.len() {
                    if !a[i].soft_equal(&b[i], data.clone())? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Value::Map(a), Value::Map(b)) => Ok(a == b),
            (Value::None, Value::None) => Ok(true),
            _ => Err(RuntimeError::InvalidComparison {
                a: self.to_string(),
                b: other.to_string(),
            }
            .into()),
        }
    }

    pub fn to_int(self, data: Arc<InterpreterData>) -> PotentialFutureResult<i64, ValueError> {
        let to_type = ValueType::Int;
        match self {
            Value::Int(value) => Ok(PotentialFuture::Sync(value)),
            Value::Float(value) => Ok(PotentialFuture::Sync(value as i64)),
            Value::Text(ref value) => match value.trim().parse::<i64>() {
                Ok(value) => Ok(PotentialFuture::Sync(value)),
                Err(_) => Err(self.conversion_err_to_type(to_type).into()),
            },
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                Err(self.conversion_err_to_type(to_type).into())
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|v| v.to_int(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_int(data)
            }
        }
    }

    pub fn to_usize(self, data: Arc<InterpreterData>) -> PotentialFutureResult<usize, ValueError> {
        Ok(self.to_int(data)?.map(|i| i as usize))
    }

    pub fn to_float(self, data: Arc<InterpreterData>) -> PotentialFutureResult<f64, ValueError> {
        let to_type = ValueType::Int;
        match self {
            Value::Int(value) => Ok(PotentialFuture::Sync(value as f64)),
            Value::Float(value) => Ok(PotentialFuture::Sync(value)),
            Value::Text(ref value) => match value.trim().parse::<f64>() {
                Ok(value) => Ok(PotentialFuture::Sync(value)),
                Err(_) => Err(self.conversion_err_to_type(to_type).into()),
            },
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                Err(self.conversion_err_to_type(to_type).into())
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_float(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_float(data)
            }
        }
    }

    pub fn to_bool(self, data: Arc<InterpreterData>) -> PotentialFutureResult<bool, ValueError> {
        let to_type = ValueType::Bool;
        match self {
            Value::Int(_) => Err(self.conversion_err_to_type(to_type).into()),
            Value::Float(_) => Err(self.conversion_err_to_type(to_type).into()),
            Value::Text(ref value) => match value.trim().parse::<bool>() {
                Ok(value) => Ok(PotentialFuture::Sync(value)),
                Err(_) => Err(self.conversion_err_to_type(to_type).into()),
            },
            Value::Bool(value) => Ok(PotentialFuture::Sync(value)),
            Value::List(_) | Value::Map(_) | Value::None => {
                Err(self.conversion_err_to_type(to_type).into())
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_bool(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_bool(data)
            }
        }
    }

    pub fn to_var(
        self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<SourceStr, ValueError> {
        let to_type = ValueType::Var;
        match self {
            Value::Int(_)
            | Value::Float(_)
            | Value::Text(_)
            | Value::Bool(_)
            | Value::List(_)
            | Value::Map(_)
            | Value::None => Err(self.conversion_err_to_type(to_type).into()),
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_var(data)),
            Value::Var(label) => Ok(PotentialFuture::Sync(label)),
        }
    }

    pub fn to_text(
        self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<SourceStr, ValueError> {
        let to_type = ValueType::Text;
        match self {
            Value::Int(value) => Ok(PotentialFuture::Sync(value.to_string().into())),
            Value::Float(value) => Ok(PotentialFuture::Sync(value.to_string().into())),
            Value::Text(value) => Ok(PotentialFuture::Sync(value)),
            Value::Bool(value) => Ok(PotentialFuture::Sync(value.to_string().into())),
            Value::List(value) => Ok(PotentialFuture::Async(Box::pin(async move {
                Ok(value.resolve(data).await?.to_string().into())
            }))),
            Value::Map(value) => Ok(PotentialFuture::Async(Box::pin(async move {
                Ok(value.resolve(data).await?.to_string().into())
            }))),
            Value::None => Err(self.conversion_err_to_type(to_type).into()),
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_text(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_text(data)
            }
        }
    }

    pub fn to_list(self, data: Arc<InterpreterData>) -> PotentialFutureResult<List, ValueError> {
        let to_type = ValueType::Var;
        match self {
            Value::Int(_)
            | Value::Float(_)
            | Value::Text(_)
            | Value::Bool(_)
            | Value::Map(_)
            | Value::None => Err(self.conversion_err_to_type(to_type).into()),
            Value::List(list) => Ok(PotentialFuture::Async(Box::pin(async move {
                Ok(list.resolve(data).await?)
            }))),
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_list(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_list(data)
            }
        }
    }

    pub fn to_map(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Map, ValueError> {
        let to_type = ValueType::Var;
        match self {
            Value::Int(_)
            | Value::Float(_)
            | Value::Text(_)
            | Value::Bool(_)
            | Value::List(_)
            | Value::None => Err(self.conversion_err_to_type(to_type).into()),
            Value::Map(map) => Ok(PotentialFuture::Async(Box::pin(async move {
                Ok(map.resolve(data).await?)
            }))),
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_map(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_map(data)
            }
        }
    }

    pub fn to_number(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Value, ValueError> {
        let to_type = ValueType::Int;
        match self {
            Value::Int(_) => Ok(PotentialFuture::Sync(self)),
            Value::Float(_) => Ok(PotentialFuture::Sync(self)),
            Value::Text(ref value) => {
                match (value.trim().parse::<i64>(), value.trim().parse::<f64>()) {
                    (Ok(_), Ok(_)) | (Ok(_), Err(_)) | (Err(_), Ok(_)) => {
                        return Ok(PotentialFuture::Sync(self));
                    }
                    (Err(_), Err(_)) => {
                        return Err(self.conversion_err_to_type(to_type).into());
                    }
                }
            }
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                Err(self.conversion_err_to_type(to_type).into())
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_number(data)),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_number(data)
            }
        }
    }

    /// Reduces value to base type and checks the resulting type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    pub fn to_inner_checked(
        self,
        valid_types: &'static [ValueType],
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, ValueError> {
        match self {
            Value::Int(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Sync(self))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::Float(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Sync(self))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::Bool(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Sync(self))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::Text(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Sync(self))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::List(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Async(Box::pin(async move {
                        let result = self.to_list(data.clone())?;
                        let list = potential_future!(result);
                        Ok(Value::List(list))
                    })))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::Map(_) => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Async(Box::pin(async move {
                        let result = self.to_map(data.clone())?;
                        let map = potential_future!(result);
                        Ok(Value::Map(map))
                    })))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_inner_checked(valid_types, data)
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_inner(data)),
            Value::None => {
                if valid_types.contains(&self.to_type()) {
                    Ok(PotentialFuture::Sync(self))
                } else {
                    Err(self.conversion_err(valid_types).into())
                }
            }
        }
    }

    /// Reduces value to base type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    pub fn to_inner(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Value, ValueError> {
        match self {
            Value::Int(_) => Ok(PotentialFuture::Sync(self)),
            Value::Float(_) => Ok(PotentialFuture::Sync(self)),
            Value::Bool(_) => Ok(PotentialFuture::Sync(self)),
            Value::Text(_) => Ok(PotentialFuture::Sync(self)),
            Value::List(_) => Ok(PotentialFuture::Async(Box::pin(async move {
                let result = self.to_list(data.clone())?;
                let list = potential_future!(result);
                Ok(Value::List(list))
            }))),
            Value::Map(_) => Ok(PotentialFuture::Async(Box::pin(async move {
                let result = self.to_map(data.clone())?;
                let map = potential_future!(result);
                Ok(Value::Map(map))
            }))),
            Value::Var(label) => {
                let value = data.vars.read().get_clone(&label)?;
                value.to_inner(data)
            }
            Value::Command(command) => command
                .execute(data.clone())?
                .map_result(|r| r.to_inner(data)),
            Value::None => Ok(PotentialFuture::Sync(self)),
        }
    }

    // Attempts to convert value to a value that can be used to access indices in a map or list
    pub fn to_list_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> ValueResult<Vec<usize>, ValueError> {
        Box::pin(async move {
            let accesor = potential_future!(self.to_inner_checked(LIST_KEY, data.clone())?);

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(potential_future!(value.to_usize(data.clone())?));
                    }
                    Ok(indices)
                }
                _ => Ok(vec![potential_future!(accesor.to_usize(data.clone())?)]),
            }
        })
    }

    pub fn to_map_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> ValueResult<Vec<SourceStr>, ValueError> {
        Box::pin(async move {
            let accesor = potential_future!(self.to_inner_checked(MAP_KEY, data.clone())?);

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(potential_future!(value.to_text(data.clone())?));
                    }
                    Ok(indices)
                }
                _ => Ok(vec![potential_future!(accesor.to_text(data.clone())?)]),
            }
        })
    }

    pub fn to_command(self, _: Arc<InterpreterData>) -> Result<Command, ValueError> {
        if let Value::Command(command) = self {
            Ok(*command)
        } else {
            Err(RuntimeError::InvalidConversion {
                from: self.to_string(),
                to: vec![ValueType::Command],
            }
            .into())
        }
    }

    pub fn as_var_label(&self, _: Arc<InterpreterData>) -> Result<SourceStr, ValueError> {
        if let Value::Var(label) = self {
            Ok(label.clone())
        } else {
            Err(RuntimeError::NotAVar {
                value: self.to_string(),
            }
            .into())
        }
    }

    pub fn as_command_label(&self) -> Result<SourceStr, ValueError> {
        match self {
            Value::Command(command) => Ok(command.label()),
            _ => Err(self.conversion_err_to_type(ValueType::Command).into()),
        }
    }

    pub fn to_arg(self, span: Span) -> Argument {
        Argument::new(self, span)
    }
}

impl Value {
    pub fn from_command(command: Command) -> Self {
        Self::Command(Box::new(command))
    }

    fn conversion_err_to_type(&self, to: ValueType) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: vec![to],
        }
    }

    pub fn conversion_err(&self, to: &[ValueType]) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: to.to_vec(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Value::Int(value) => value.to_string(),
            Value::Float(value) => value.to_string(),
            Value::Text(value) => value.to_string(),
            Value::Bool(value) => value.to_string(),
            Value::List(values) => format!("{}", values),
            Value::Map(map) => format!("{}", map),
            Value::Var(value) => value.to_string(),
            Value::Command(command) => command.label().to_string(),
            Value::None => "".to_string(),
        };
        write!(f, "{}", out)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Int(value as i64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Text(SourceStr::Owned(value))
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Text(SourceStr::Owned(value.to_string()))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::List(List::Unresolved(Arc::new(value)))
    }
}

impl From<Command> for Value {
    fn from(value: Command) -> Self {
        Value::Command(Box::new(value))
    }
}

impl From<FslMap> for Value {
    fn from(value: FslMap) -> Self {
        Value::Map(Map::Unresolved(Arc::new(value)))
    }
}
