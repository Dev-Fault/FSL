use std::{future::ready, pin::Pin, sync::Arc};

use async_recursion::async_recursion;
use futures::future::Either;

use crate::{
    InterpreterData, await_result,
    error::{RuntimeError, SpannedError, ToSpannedError},
    execute_command,
    source_str::SourceStr,
    span::Span,
    types::{
        FslType, LIST_KEY, MAP_KEY,
        argument::Argument,
        command::{Command, InterpreterResult},
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
    pub fn to_type(&self) -> FslType {
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

    pub async fn to_inner_type(&self, data: Arc<InterpreterData>) -> Result<FslType, RuntimeError> {
        Ok(match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Map(_) => FslType::Map,
            Value::Var(label) => data.vars.read().await.get_type(label).await?,
            Value::Command(_) => FslType::Command,
            Value::None => FslType::None,
        })
    }

    pub fn is_var(&self) -> bool {
        match self {
            Value::Var(_) => true,
            _ => false,
        }
    }

    pub fn is_type(&self, fsl_type: FslType) -> bool {
        self.to_type() == fsl_type
    }

    #[async_recursion]
    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
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
                        .checked_add(element.mem_size().await?)
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
                        .checked_add(value.mem_size().await?)
                        .ok_or(RuntimeError::Overflow)?;
                }
                Ok(size)
            }
            Value::Var(var) => size_of::<Value>()
                .checked_add(var.len())
                .ok_or(RuntimeError::Overflow),
            Value::Command(command) => size_of::<Value>()
                .checked_add(command.mem_size().await?)
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
                .map_err(|_| self.conversion_err_to_type(FslType::Int))?
                == *b),
            (Value::Int(a), Value::Text(b)) => Ok(*a
                == b.parse::<i64>()
                    .map_err(|_| self.conversion_err_to_type(FslType::Int))?),
            (Value::Text(a), Value::Float(b)) => Ok(a
                .parse::<f64>()
                .map_err(|_| self.conversion_err_to_type(FslType::Float))?
                == *b),
            (Value::Float(a), Value::Text(b)) => Ok(*a
                == b.parse::<f64>()
                    .map_err(|_| self.conversion_err_to_type(FslType::Float))?),
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
                .map_err(|_| self.conversion_err_to_type(FslType::Int))?
                == *b),
            (Value::Int(a), Value::Text(b)) => Ok(*a
                == b.parse::<i64>()
                    .map_err(|_| self.conversion_err_to_type(FslType::Int))?),
            (Value::Text(a), Value::Float(b)) => Ok(a
                .parse::<f64>()
                .map_err(|_| self.conversion_err_to_type(FslType::Float))?
                == *b),
            (Value::Float(a), Value::Text(b)) => Ok(*a
                == b.parse::<f64>()
                    .map_err(|_| self.conversion_err_to_type(FslType::Float))?),
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

    pub fn to_int(self, data: Arc<InterpreterData>) -> InterpreterResult<i64, ValueError> {
        let to_type = FslType::Int;
        match self {
            Value::Int(value) => InterpreterResult::Sync(Ok(value)),
            Value::Float(value) => InterpreterResult::Sync(Ok(value as i64)),
            Value::Text(ref value) => match value.trim().parse::<i64>() {
                Ok(value) => InterpreterResult::Sync(Ok(value)),
                Err(_) => InterpreterResult::Sync(Err(self.conversion_err_to_type(to_type).into())),
            },
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                InterpreterResult::Sync(Err(self.conversion_err_to_type(to_type).into()))
            }
            Value::Command(command) => command
                .execute(data.clone())
                .and_then_result(|r| r.to_int(data)),
            Value::Var(label) => InterpreterResult::Async(Box::pin(async move {
                let value = data.vars.read().await.get_clone(&label).await?;
                await_result!(value.to_int(data.clone()))
            })),
        }
    }

    pub fn to_usize(self, data: Arc<InterpreterData>) -> InterpreterResult<usize, ValueError> {
        self.to_int(data)
            .and_then_result(|v| InterpreterResult::Sync(Ok(v as usize)))
    }

    pub fn to_float(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<f64, ValueError>> + Send {
        let to_type = FslType::Int;
        match self {
            Value::Int(value) => {
                return Either::Left(ready(Ok(value as f64)));
            }
            Value::Float(value) => {
                return Either::Left(ready(Ok(value)));
            }
            Value::Text(ref value) => match value.trim().parse::<f64>() {
                Ok(value) => {
                    return Either::Left(ready(Ok(value)));
                }
                Err(_) => {
                    return Either::Left(ready(Err(self.conversion_err_to_type(to_type).into())));
                }
            },
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                return Either::Left(ready(Err(self.conversion_err_to_type(to_type).into())));
            }
            Value::Var(_) | Value::Command(_) => {}
        }

        Either::Right(Box::pin(async move {
            match self {
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_float(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_float(data.clone()).await
                }
                _ => unreachable!(),
            }
        }))
    }

    pub fn to_bool(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<bool, ValueError>> + Send {
        let to_type = FslType::Int;
        match self {
            Value::Bool(value) => return Either::Left(ready(Ok(value))),
            Value::Text(ref value) => match value.trim().parse::<bool>() {
                Ok(value) => {
                    return Either::Left(ready(Ok(value)));
                }
                Err(_) => {
                    return Either::Left(ready(Err(self.conversion_err_to_type(to_type).into())));
                }
            },
            Value::Float(_) | Value::Int(_) | Value::List(_) | Value::Map(_) | Value::None => {
                return Either::Left(ready(Err(self.conversion_err_to_type(to_type).into())));
            }
            Value::Var(_) | Value::Command(_) => {}
        }

        Either::Right(Box::pin(async move {
            match self {
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_bool(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_bool(data.clone()).await
                }
                _ => unreachable!(),
            }
        }))
    }

    pub fn to_var(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<SourceStr, ValueError>> + Send {
        let to_type = FslType::Var;
        match self {
            Value::Int(_)
            | Value::Float(_)
            | Value::Text(_)
            | Value::Bool(_)
            | Value::List(_)
            | Value::Map(_)
            | Value::None => Either::Left(ready(Err(self.conversion_err_to_type(to_type).into()))),
            Value::Var(label) => Either::Left(ready(Ok(label))),
            Value::Command(command) => Either::Right(Box::pin(async move {
                let result = execute_command!(command, data.clone())?;
                result.to_var(data.clone()).await
            })),
        }
    }

    pub fn to_text(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<SourceStr, ValueError>> + Send {
        match self {
            Value::Text(value) => {
                return Either::Left(ready(Ok(value)));
            }
            Value::Int(value) => {
                return Either::Left(ready(Ok(value.to_string().into())));
            }
            Value::Float(value) => {
                return Either::Left(ready(Ok(value.to_string().into())));
            }
            Value::Bool(value) => {
                return Either::Left(ready(Ok(value.to_string().into())));
            }
            Value::None => {
                return Either::Left(ready(Err(self
                    .conversion_err_to_type(FslType::Text)
                    .into())));
            }
            Value::Var(_) | Value::Command(_) | Value::List(_) | Value::Map(_) => {}
        }

        Either::Right(Box::pin(async move {
            match self {
                Value::List(list) => {
                    let list = list.resolve(data.clone()).await?;
                    Ok(list.to_string().into())
                }
                Value::Map(map) => {
                    let map = map.resolve(data.clone()).await?;
                    Ok(map.to_string().into())
                }
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_text(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_text(data.clone()).await
                }
                _ => unreachable!(),
            }
        }))
    }

    pub fn to_list(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<List, ValueError>> + Send {
        Box::pin(async move {
            let to_type = FslType::List;
            match self {
                Value::Int(_)
                | Value::Float(_)
                | Value::Text(_)
                | Value::Bool(_)
                | Value::Map(_)
                | Value::None => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(list) => Ok(list.resolve(data).await?),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_list(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_list(data.clone()).await
                }
            }
        })
    }

    pub fn to_map(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<Map, ValueError>> + Send {
        Box::pin(async move {
            let to_type = FslType::Map;
            match self {
                Value::Int(_)
                | Value::Float(_)
                | Value::Text(_)
                | Value::Bool(_)
                | Value::List(_)
                | Value::None => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(map) => Ok(map.resolve(data).await?),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_map(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_map(data.clone()).await
                }
            }
        })
    }

    pub fn to_number(
        self,
        data: Arc<InterpreterData>,
    ) -> impl Future<Output = Result<Value, ValueError>> + Send {
        let to_type = FslType::Int;
        match self {
            Value::Int(_) => {
                return Either::Left(ready(Ok(self)));
            }
            Value::Float(_) => {
                return Either::Left(ready(Ok(self)));
            }
            Value::Text(ref value) => {
                match (value.trim().parse::<i64>(), value.trim().parse::<f64>()) {
                    (Ok(_), Ok(_)) | (Ok(_), Err(_)) | (Err(_), Ok(_)) => {
                        return Either::Left(ready(Ok(self)));
                    }
                    (Err(_), Err(_)) => {
                        return Either::Left(ready(Err(self
                            .conversion_err_to_type(to_type)
                            .into())));
                    }
                }
            }
            Value::Bool(_) | Value::List(_) | Value::Map(_) | Value::None => {
                return Either::Left(ready(Err(self.conversion_err_to_type(to_type).into())));
            }
            Value::Var(_) | Value::Command(_) => {}
        }

        Either::Right(Box::pin(async move {
            match self {
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.to_number(data.clone()).await
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.to_number(data.clone()).await
                }
                _ => unreachable!(),
            }
        }))
    }

    /// Reduces value to base type and checks the resulting type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    pub fn as_raw_checked(
        self,
        valid_types: &'static [FslType],
        data: Arc<InterpreterData>,
    ) -> ValueResult<Value, ValueError> {
        Box::pin(async move {
            let value = match self {
                Value::Int(_) => self,
                Value::Float(_) => self,
                Value::Bool(_) => self,
                Value::Text(_) => self,
                Value::List(_) => Value::List(self.to_list(data.clone()).await?),
                Value::Map(_) => Value::Map(self.to_map(data.clone()).await?),
                Value::Var(label) => {
                    let var = data.vars.read().await.get_clone(&label).await?;
                    var.as_raw_checked(valid_types, data.clone()).await?
                }
                Value::Command(command) => {
                    let result = execute_command!(command, data.clone())?;
                    result.as_raw_checked(valid_types, data.clone()).await?
                }
                Value::None => self,
            };

            if valid_types.contains(&value.to_type()) {
                Ok(value)
            } else {
                Err(value.conversion_err(valid_types).into())
            }
        })
    }

    /// Reduces value to base type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    pub fn as_raw(self, data: Arc<InterpreterData>) -> InterpreterResult<Value, ValueError> {
        match self {
            Value::Int(_) => InterpreterResult::Sync(Ok(self)),
            Value::Float(_) => InterpreterResult::Sync(Ok(self)),
            Value::Bool(_) => InterpreterResult::Sync(Ok(self)),
            Value::Text(_) => InterpreterResult::Sync(Ok(self)),
            Value::List(_) => InterpreterResult::Async(Box::pin(async move {
                Ok(Value::List(self.to_list(data.clone()).await?))
            })),
            Value::Map(_) => InterpreterResult::Async(Box::pin(async move {
                Ok(Value::Map(self.to_map(data.clone()).await?))
            })),
            Value::Var(label) => InterpreterResult::Async(Box::pin(async move {
                let var = data.vars.read().await.get_clone(&label).await?;
                await_result!(var.as_raw(data.clone()))
            })),
            Value::Command(command) => command
                .execute(data.clone())
                .and_then_result(|r| r.as_raw(data)),
            Value::None => InterpreterResult::Sync(Ok(self)),
        }
    }

    // Attempts to convert value to a value that can be used to access indices in a map or list
    pub fn to_list_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> ValueResult<Vec<usize>, ValueError> {
        Box::pin(async move {
            let accesor = self.as_raw_checked(LIST_KEY, data.clone()).await?;

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(await_result!(value.to_usize(data.clone()))?);
                    }
                    Ok(indices)
                }
                _ => Ok(vec![await_result!(accesor.to_usize(data.clone()))?]),
            }
        })
    }

    pub fn to_map_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> ValueResult<Vec<SourceStr>, ValueError> {
        Box::pin(async move {
            let accesor = self.as_raw_checked(MAP_KEY, data.clone()).await?;

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(value.to_text(data.clone()).await?);
                    }
                    Ok(indices)
                }
                _ => Ok(vec![(accesor.to_text(data.clone()).await?)]),
            }
        })
    }

    pub fn to_command(self, _: Arc<InterpreterData>) -> Result<Command, ValueError> {
        if let Value::Command(command) = self {
            Ok(*command)
        } else {
            Err(RuntimeError::InvalidConversion {
                from: self.to_string(),
                to: vec![FslType::Command],
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
            _ => Err(self.conversion_err_to_type(FslType::Command).into()),
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

    fn conversion_err_to_type(&self, to: FslType) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: vec![to],
        }
    }

    pub fn conversion_err(&self, to: &[FslType]) -> RuntimeError {
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
