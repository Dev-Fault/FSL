use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    potential_futures::{PotentialFuture, PotentialFutureResult},
    source_str::SourceStr,
    span::Span,
    types::value::{Value, ValueError},
};

pub type FslMap = HashMap<SourceStr, Value>;

#[derive(Debug, Clone)]
pub enum Map {
    Resolved(Arc<FslMap>),
    Unresolved(Arc<FslMap>),
}

impl std::fmt::Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let map = match self {
            Map::Resolved(map) => map,
            Map::Unresolved(map) => map,
        };
        write!(f, "[")?;
        for (i, (key, value)) in map.iter().enumerate() {
            if i < map.len() - 1 {
                write!(f, "{}: {}, ", &*key, value.to_string())?;
            } else {
                write!(f, "{}: {}", &*key, value.to_string())?;
            }
        }
        write!(f, "]")
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(self.inner(), other.inner()) || **self == **other
    }
}

impl Map {
    pub fn resolve(self, data: Arc<InterpreterData>) -> PotentialFutureResult<Map, ValueError> {
        match self {
            Map::Resolved(_) => Ok(PotentialFuture::Sync(self)),
            Map::Unresolved(arc) => {
                let mut pending_ops: Vec<_> = Vec::new();
                let mut map = Arc::unwrap_or_clone(arc);
                for (key, value) in map.iter_mut() {
                    let inner_value = std::mem::take(value).to_inner(data.clone())?;
                    match inner_value {
                        PotentialFuture::Sync(resolved_value) => {
                            *value = resolved_value;
                        }
                        PotentialFuture::Async(pin) => {
                            pending_ops.push((key.clone(), pin));
                        }
                    }
                }
                if pending_ops.len() > 0 {
                    Ok(PotentialFuture::Async(Box::pin(async move {
                        for (key, op) in pending_ops {
                            map.insert(key, op.await?);
                        }
                        Ok(Map::Resolved(Arc::new(map)))
                    })))
                } else {
                    Ok(PotentialFuture::Sync(Map::Resolved(Arc::new(map))))
                }
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
        span: Span,
    ) -> Result<Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey.span(span)),
            [key] => match self.get(key) {
                Some(_) => {
                    let return_value = self.insert(key.clone(), value);
                    Ok(return_value.unwrap_or(Value::None))
                }
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
            [key, rest @ ..] => match self.get_mut(key) {
                Some(Value::Map(inner_map)) => inner_map.set_nested(rest, value, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
        }
    }

    pub fn remove_nested(&mut self, keys: &[SourceStr], span: Span) -> Result<Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).span(span),
            [key] => {
                let return_value = self.remove(key);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(key) {
                Some(Value::Map(inner_map)) => inner_map.remove_nested(rest, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
        }
    }

    pub fn insert_nested(
        &mut self,
        keys: &[SourceStr],
        value: Value,
        span: Span,
    ) -> Result<Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).span(span),
            [key] => {
                let return_value = self.insert(key.clone(), value);
                Ok(return_value.unwrap_or(Value::None))
            }
            [key, rest @ ..] => match self.get_mut(key) {
                Some(Value::Map(inner_map)) => inner_map.insert_nested(rest, value, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
        }
    }

    pub fn get_nested_clone(&self, keys: &[SourceStr], span: Span) -> Result<Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).span(span),
            [key] => match self.get(key).cloned() {
                Some(value) => Ok(value),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
            [key, rest @ ..] => match self.get(key) {
                Some(Value::Map(inner_map)) => inner_map.get_nested_clone(rest, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
        }
    }

    pub fn get_nested(&self, keys: &[SourceStr], span: Span) -> Result<&Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).span(span),
            [key] => match self.get(key) {
                Some(value) => Ok(value),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
            [key, rest @ ..] => match self.get(key) {
                Some(Value::Map(inner_map)) => inner_map.get_nested(rest, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
        }
    }

    pub fn get_nested_mut(
        &mut self,
        keys: &[SourceStr],
        span: Span,
    ) -> Result<&mut Value, SpannedError> {
        match keys {
            [] => Err(RuntimeError::MissingKey).span(span),
            [key] => match self.get_mut(key) {
                Some(value) => Ok(value),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
            },
            [key, rest @ ..] => match self.get_mut(key) {
                Some(Value::Map(inner_map)) => inner_map.get_nested_mut(rest, span),
                Some(_) => Err(RuntimeError::NotAMap {
                    key: key.to_string(),
                }
                .span(span)),
                None => Err(RuntimeError::NonExistantKey {
                    key: key.to_string(),
                }
                .span(span)),
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
