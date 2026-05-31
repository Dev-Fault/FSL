use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    pin::Pin,
    sync::Arc,
};

use bytes::Bytes;

use crate::{
    InterpreterData,
    error::{ExecutionError, RuntimeError, ToExecutionError},
    libraries::standard::{LIST_KEY, MAP_KEY},
    source_str::SourceStr,
    span::Span,
    standard::NUMBER,
    types::{FslType, command::Command},
};
pub type FslMap = HashMap<SourceStr, Value>;
pub type ValueResult<T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send>>;
pub type ArgResult<'c, T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send + 'c>>;

#[derive(Debug, Clone, PartialEq)]
pub enum List {
    Resolved(Arc<Vec<Value>>),
    Unresolved(Arc<Vec<Value>>),
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        let arc_vec = match self {
            List::Resolved(vec) => vec,
            List::Unresolved(vec) => vec,
        };

        let vec = Arc::unwrap_or_clone(arc_vec);
        vec.into_iter()
    }
}

impl List {
    pub async fn resolve(self, data: Arc<InterpreterData>) -> Result<List, ValueError> {
        match self {
            List::Resolved(_) => Ok(self),
            List::Unresolved(arc) => {
                let mut values = Arc::unwrap_or_clone(arc);
                for value in values.iter_mut() {
                    *value = std::mem::take(value).as_raw(data.clone()).await?;
                }
                Ok(List::Resolved(Arc::new(values)))
            }
        }
    }

    pub fn take(self) -> Vec<Value> {
        match self {
            List::Resolved(values) => Arc::unwrap_or_clone(values),
            List::Unresolved(values) => Arc::unwrap_or_clone(values),
        }
    }

    pub fn get_nested(
        &self,
        indices: &[usize],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.source.clone())),
            [i] => {
                let result = self.get(*i).cloned();
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds)
                        .map_err(|e| e.to_exec(span, data.source.clone())),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotIndexable)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn get_nested_mut(
        &mut self,
        indices: &[usize],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<&mut Value, ExecutionError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.source.clone())),
            [i] => match self.get_mut(*i) {
                Some(i) => Ok(i),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_mut(rest, data, span),
                Some(_) => Err(RuntimeError::NotIndexable)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn remove_nested(
        &mut self,
        indices: &[usize],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.source.clone())),
            [i] => match self.get(*i) {
                Some(_) => Ok(self.remove(*i)),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.remove_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotIndexable)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn insert_nested(
        &mut self,
        indices: &[usize],
        value_to_insert: Value,
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<(), ExecutionError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.source.clone())),
            [i] => {
                if *i <= self.len() {
                    self.insert(*i, value_to_insert);
                    Ok(())
                } else {
                    Err(RuntimeError::IndexOutOfBounds)
                        .map_err(|e| e.to_exec(span, data.source.clone()))
                }
            }
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => {
                    inner_list.insert_nested(rest, value_to_insert, data, span)
                }
                Some(_) => Err(RuntimeError::NotIndexable)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
                None => Err(RuntimeError::IndexOutOfBounds)
                    .map_err(|e| e.to_exec(span, data.source.clone())),
            },
        }
    }
}

impl Deref for List {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        match self {
            List::Resolved(values) => values,
            List::Unresolved(values) => values,
        }
    }
}

impl DerefMut for List {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            List::Resolved(values) => Arc::make_mut(values),
            List::Unresolved(values) => Arc::make_mut(values),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Map {
    Resolved(Arc<FslMap>),
    Unresolved(Arc<FslMap>),
}

impl Map {
    pub async fn resolve(self, data: Arc<InterpreterData>) -> Result<Map, ValueError> {
        match self {
            Map::Resolved(_) => Ok(self),
            Map::Unresolved(arc) => {
                let mut map = Arc::unwrap_or_clone(arc);
                for (_, value) in map.iter_mut() {
                    *value = std::mem::take(value).as_raw(data.clone()).await?;
                }
                Ok(Map::Resolved(Arc::new(map)))
            }
        }
    }

    pub fn take(self) -> FslMap {
        match self {
            Map::Resolved(map) => Arc::unwrap_or_clone(map),
            Map::Unresolved(map) => Arc::unwrap_or_clone(map),
        }
    }

    pub fn set_nested(
        &mut self,
        keys: &[SourceStr],
        value: Value,
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.source.clone())),
            [key] => match self.get(&*key) {
                Some(_) => {
                    let return_value = self.insert(key.clone(), value);
                    Ok(return_value.unwrap_or(Value::None))
                }
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.set_nested(rest, value, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn remove_nested(
        &mut self,
        keys: &[SourceStr],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.source.clone())),
            [key] => {
                let return_value = self.remove(&*key);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.remove_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn insert_nested(
        &mut self,
        keys: &[SourceStr],
        value: Value,
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.source.clone())),
            [key] => {
                let return_value = self.insert(key.clone(), value);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.insert_nested(rest, value, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
        }
    }

    pub fn get_nested(
        &self,
        keys: &[SourceStr],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.source.clone())),
            [key] => match self.get(&*key).cloned() {
                Some(value) => Ok(value),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
            [key, rest @ ..] => match self.get(&*key) {
                Some(Value::Map(inner_map)) => inner_map.get_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.source.clone())),
            },
        }
    }
}

impl Deref for Map {
    type Target = FslMap;

    fn deref(&self) -> &Self::Target {
        match self {
            Map::Resolved(map) => map,
            Map::Unresolved(map) => map,
        }
    }
}

impl DerefMut for Map {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Map::Resolved(map) => Arc::make_mut(map),
            Map::Unresolved(map) => Arc::make_mut(map),
        }
    }
}

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
    Execution(ExecutionError),
}

impl ToExecutionError for ValueError {
    fn to_exec(self, span: Span, source: Bytes) -> ExecutionError {
        match self {
            ValueError::Runtime(runtime_error) => runtime_error.to_exec(span, source),
            ValueError::Execution(execution_error) => execution_error,
        }
    }
}

impl From<RuntimeError> for ValueError {
    fn from(value: RuntimeError) -> Self {
        Self::Runtime(value)
    }
}

impl From<ExecutionError> for ValueError {
    fn from(value: ExecutionError) -> Self {
        Self::Execution(value)
    }
}

#[allow(async_fn_in_trait)]
pub trait FslValue<T, E> {
    fn as_type(&self) -> FslType;

    async fn as_literal_type(&self, data: Arc<InterpreterData>) -> Result<FslType, RuntimeError>;

    fn is_type(&self, fsl_type: FslType) -> bool;

    fn mem_size(&self) -> Result<usize, RuntimeError>;

    fn equal(&self, other: &T, data: Arc<InterpreterData>) -> Result<bool, E>;

    fn as_int(self, data: Arc<InterpreterData>) -> ValueResult<i64, E>;

    fn as_usize(self, data: Arc<InterpreterData>) -> ValueResult<usize, E>;

    fn as_float(self, data: Arc<InterpreterData>) -> ValueResult<f64, E>;

    fn as_bool(self, data: Arc<InterpreterData>) -> ValueResult<bool, E>;

    fn as_var_label(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, E>;

    fn as_text(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, E>;

    fn as_list(self, data: Arc<InterpreterData>) -> ValueResult<List, E>;

    fn as_map(self, data: Arc<InterpreterData>) -> ValueResult<Map, E>;

    fn as_number(self, data: Arc<InterpreterData>) -> ValueResult<T, E>;

    /// Converts value into it's most raw state ensuring result is of valid type
    fn as_raw_checked(
        self,
        data: Arc<InterpreterData>,
        valid_types: &'static [FslType],
    ) -> ValueResult<T, E>;

    /// Converts value into it's most raw state without checking what the result type it is
    fn as_raw(self, data: Arc<InterpreterData>) -> ValueResult<T, E>;

    fn as_list_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<usize>, E>;

    fn as_map_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<SourceStr>, E>;

    fn as_command(self, data: Arc<InterpreterData>) -> Result<Command, E>;

    fn get_var_label(&self, data: Arc<InterpreterData>) -> Result<SourceStr, E>;

    fn get_command_label(&self) -> Option<&str>;
}

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

impl FslValue<Value, ValueError> for Value {
    fn as_type(&self) -> FslType {
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

    async fn as_literal_type(&self, data: Arc<InterpreterData>) -> Result<FslType, RuntimeError> {
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

    fn is_type(&self, fsl_type: FslType) -> bool {
        return self.as_type() == fsl_type;
    }

    fn mem_size(&self) -> Result<usize, RuntimeError> {
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

    fn equal(&self, other: &Value, data: Arc<InterpreterData>) -> Result<bool, ValueError> {
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
            (Value::List(a_list), Value::List(b_list)) => {
                if a_list.len() != b_list.len() {
                    return Ok(false);
                } else {
                    for i in 0..a_list.len() {
                        if !a_list[i].equal(&b_list[i], data.clone())? {
                            return Ok(false);
                        }
                    }
                }
                Ok(true)
            }
            (Value::None, Value::None) => Ok(true),
            _ => Err(RuntimeError::InvalidComparison {
                a: self.to_string(),
                b: other.to_string(),
            }
            .into()),
        }
    }

    fn as_int(self, data: Arc<InterpreterData>) -> ValueResult<i64, ValueError> {
        Box::pin(async move {
            let to_type = FslType::Int;
            match self {
                Value::Int(value) => Ok(value),
                Value::Float(value) => Ok(value as i64),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_int(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_int(data.clone())
                        .await
                }
                Value::Text(ref value) => match value.trim().parse::<i64>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(self.conversion_err_to_type(to_type).into()),
                },
                Value::Bool(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_usize(self, data: Arc<InterpreterData>) -> ValueResult<usize, ValueError> {
        Box::pin(async move {
            let integer = self.as_int(data).await?;
            if integer < 0 {
                Err(RuntimeError::NegativeIndex.into())
            } else {
                Ok(integer as usize)
            }
        })
    }

    fn as_float(self, data: Arc<InterpreterData>) -> ValueResult<f64, ValueError> {
        Box::pin(async move {
            let to_type = FslType::Float;
            match self {
                Value::Int(value) => Ok(value as f64),
                Value::Float(value) => Ok(value),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_float(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_float(data.clone())
                        .await
                }
                Value::Text(ref value) => match value.parse::<f64>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(self.conversion_err_to_type(to_type).into()),
                },
                Value::Bool(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_bool(self, data: Arc<InterpreterData>) -> ValueResult<bool, ValueError> {
        Box::pin(async move {
            let to_type = FslType::Bool;
            match self {
                Value::Int(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Text(ref value) => match value.parse::<bool>() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(self.conversion_err_to_type(to_type).into()),
                },
                Value::Bool(value) => Ok(value),
                Value::List(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_bool(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_bool(data.clone())
                        .await
                }
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_var_label(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, ValueError> {
        Box::pin(async move {
            let to_type = FslType::Var;
            match self {
                Value::Int(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Var(label) => Ok(label),
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_var_label(data.clone())
                        .await
                }
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_text(self, data: Arc<InterpreterData>) -> ValueResult<SourceStr, ValueError> {
        Box::pin(async move {
            match self {
                Value::Int(value) => Ok(value.to_string().into()),
                Value::Float(value) => Ok(value.to_string().into()),
                Value::Text(value) => Ok(value),
                Value::Bool(value) => Ok(value.to_string().into()),
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?;
                    let mut output = String::new();
                    output.push('[');
                    let empty = values.is_empty();
                    for value in values.take() {
                        output.push_str(&format!("{}, ", value.as_text(data.clone()).await?));
                    }
                    if !empty {
                        output.pop();
                        output.pop();
                    }
                    output.push(']');
                    Ok(output.into())
                }
                Value::Map(map) => {
                    let map = map.resolve(data.clone()).await?;
                    let mut output = String::new();
                    output.push('[');
                    let empty = map.is_empty();
                    for (key, value) in map.take() {
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
                    Ok(output.into())
                }
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_text(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_text(data.clone())
                        .await
                }
                Value::None => Err(self.conversion_err_to_type(FslType::Text).into()),
            }
        })
    }

    fn as_list(self, data: Arc<InterpreterData>) -> ValueResult<List, ValueError> {
        Box::pin(async move {
            let to_type = FslType::List;
            match self {
                Value::Int(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(list) => Ok(list.resolve(data).await?),
                Value::Map(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_list(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_list(data.clone())
                        .await
                }
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_map(self, data: Arc<InterpreterData>) -> ValueResult<Map, ValueError> {
        Box::pin(async move {
            let to_type = FslType::Map;
            match self {
                Value::Int(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Float(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Text(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Bool(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::List(_) => Err(self.conversion_err_to_type(to_type).into()),
                Value::Map(map) => Ok(map.resolve(data).await?),
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_map(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_map(data.clone())
                        .await
                }
                Value::None => Err(self.conversion_err_to_type(to_type).into()),
            }
        })
    }

    fn as_number(self, data: Arc<InterpreterData>) -> ValueResult<Value, ValueError> {
        Box::pin(async move {
            match self {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(n) => Ok(Value::Float(n)),
                Value::Text(n) => match n.parse::<i64>() {
                    Ok(n) => Ok(Value::Int(n)),
                    Err(_) => match n.parse::<f64>() {
                        Ok(n) => Ok(Value::Float(n)),
                        Err(_) => Err(Value::Text(n)
                            .conversion_err_to_types(&[FslType::Int, FslType::Float])
                            .into()),
                    },
                },
                Value::Var(label) => {
                    let value = data.vars.read().await.get_clone(&label).await?;
                    value.as_number(data.clone()).await
                }
                Value::Command(command) => {
                    command
                        .execute(data.clone())
                        .await?
                        .as_number(data.clone())
                        .await
                }
                _ => Err(self.conversion_err_to_types(NUMBER).into()),
            }
        })
    }

    /// Reduces value to base type and checks the resulting type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    fn as_raw_checked(
        self,
        data: Arc<InterpreterData>,
        valid_types: &'static [FslType],
    ) -> ValueResult<Value, ValueError> {
        Box::pin(async move {
            let value = match self {
                Value::Int(_) => self,
                Value::Float(_) => self,
                Value::Bool(_) => self,
                Value::Text(_) => self,
                Value::List(_) => Value::List(self.as_list(data.clone()).await?),
                Value::Map(_) => Value::Map(self.as_map(data.clone()).await?),
                Value::Var(label) => {
                    let var = data.vars.read().await.get_clone(&label).await?;
                    var.as_raw(data.clone()).await?
                }
                Value::Command(command) => {
                    let result = command.execute(data.clone()).await?;
                    result.as_raw(data.clone()).await?
                }
                Value::None => self,
            };

            if valid_types.contains(&value.as_type()) {
                Ok(value)
            } else {
                Err(value.conversion_err_to_types(valid_types).into())
            }
        })
    }

    /// Reduces value to base type
    /// Recursively reduces inner values of lists and maps to their most base type
    /// Expensive, use as_base if list/map inner values do not need to be reduced
    fn as_raw(self, data: Arc<InterpreterData>) -> ValueResult<Value, ValueError> {
        Box::pin(async move {
            Ok(match self {
                Value::Int(_) => self,
                Value::Float(_) => self,
                Value::Bool(_) => self,
                Value::Text(_) => self,
                Value::List(_) => Value::List(self.as_list(data.clone()).await?),
                Value::Map(_) => Value::Map(self.as_map(data.clone()).await?),
                Value::Var(label) => {
                    let var = data.vars.read().await.get_clone(&label).await?;
                    var.as_raw(data.clone()).await?
                }
                Value::Command(command) => {
                    let result = command.execute(data.clone()).await?;
                    result.as_raw(data.clone()).await?
                }
                Value::None => self,
            })
        })
    }

    // Attempts to convert value to a value that can be used to access indices in a map or list
    fn as_list_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<usize>, ValueError> {
        Box::pin(async move {
            let accesor = self.as_raw_checked(data.clone(), LIST_KEY).await?;

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(value.as_usize(data.clone()).await?);
                    }
                    Ok(indices)
                }
                _ => Ok(vec![accesor.as_usize(data.clone()).await?]),
            }
        })
    }

    fn as_map_key(self, data: Arc<InterpreterData>) -> ValueResult<Vec<SourceStr>, ValueError> {
        Box::pin(async move {
            let accesor = self.as_raw_checked(data.clone(), MAP_KEY).await?;

            match accesor {
                Value::List(values) => {
                    let values = values.resolve(data.clone()).await?.take();
                    let mut indices = Vec::with_capacity(values.len());
                    for value in values {
                        indices.push(value.as_text(data.clone()).await?);
                    }
                    Ok(indices)
                }
                _ => Ok(vec![(accesor.as_text(data.clone()).await?)]),
            }
        })
    }

    fn as_command(self, _: Arc<InterpreterData>) -> Result<Command, ValueError> {
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

    fn get_var_label(&self, _: Arc<InterpreterData>) -> Result<SourceStr, ValueError> {
        if let Value::Var(label) = self {
            Ok(label.clone())
        } else {
            Err(RuntimeError::NotAVar {
                value: self.to_string(),
            }
            .into())
        }
    }

    fn get_command_label(&self) -> Option<&str> {
        if let Value::Command(command) = self {
            Some(command.get_label())
        } else {
            None
        }
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

    pub fn conversion_err_to_types(&self, to: &[FslType]) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: to.to_vec(),
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
            Value::Map(map) => format!("{:?}", map),
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
