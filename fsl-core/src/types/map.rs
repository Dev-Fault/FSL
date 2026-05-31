use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    data::InterpreterData,
    error::{ExecutionError, RuntimeError, ToExecutionError},
    source_str::SourceStr,
    span::Span,
    types::value::{FslValue, Value, ValueError},
};

pub type FslMap = HashMap<SourceStr, Value>;

#[derive(Debug, Clone)]
pub enum Map {
    Resolved(Arc<FslMap>),
    Unresolved(Arc<FslMap>),
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(self.inner(), other.inner()) || **self == **other
    }
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

    pub fn inner(&self) -> &Arc<FslMap> {
        match self {
            Map::Resolved(map) => map,
            Map::Unresolved(map) => map,
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
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.clone())),
            [key] => match self.get(&*key) {
                Some(_) => {
                    let return_value = self.insert(key.clone(), value);
                    Ok(return_value.unwrap_or(Value::None))
                }
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
            },
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.set_nested(rest, value, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
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
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.clone())),
            [key] => {
                let return_value = self.remove(&*key);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.remove_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
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
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.clone())),
            [key] => {
                let return_value = self.insert(key.clone(), value);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(&*key) {
                Some(Value::Map(inner_map)) => inner_map.insert_nested(rest, value, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
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
            [] => Err(RuntimeError::MissingKey).map_err(|e| e.to_exec(span, data.clone())),
            [key] => match self.get(&*key).cloned() {
                Some(value) => Ok(value),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
            },
            [key, rest @ ..] => match self.get(&*key) {
                Some(Value::Map(inner_map)) => inner_map.get_nested(rest, data, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .to_exec(span, data.clone())),
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
