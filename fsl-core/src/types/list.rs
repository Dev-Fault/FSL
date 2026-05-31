use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    data::InterpreterData,
    error::{ExecutionError, RuntimeError, ToExecutionError},
    span::Span,
    types::value::{FslValue, Value, ValueError},
};

#[derive(Debug, Clone)]
pub enum List {
    Resolved(Arc<Vec<Value>>),
    Unresolved(Arc<Vec<Value>>),
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(self.inner(), other.inner()) || **self == **other
    }
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

    pub fn inner(&self) -> &Arc<Vec<Value>> {
        match self {
            List::Resolved(values) => values,
            List::Unresolved(values) => values,
        }
    }

    pub fn get_nested(
        &self,
        indices: &[usize],
        data: Arc<InterpreterData>,
        span: Span,
    ) -> Result<Value, ExecutionError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.clone())),
            [i] => {
                let result = self.get(*i).cloned();
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds)
                        .map_err(|e| e.to_exec(span, data.clone())),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested(rest, data, span),
                Some(_) => {
                    Err(RuntimeError::NotIndexable).map_err(|e| e.to_exec(span, data.clone()))
                }
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
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
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.clone())),
            [i] => match self.get_mut(*i) {
                Some(i) => Ok(i),
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_mut(rest, data, span),
                Some(_) => {
                    Err(RuntimeError::NotIndexable).map_err(|e| e.to_exec(span, data.clone()))
                }
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
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
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.clone())),
            [i] => match self.get(*i) {
                Some(_) => Ok(self.remove(*i)),
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.remove_nested(rest, data, span),
                Some(_) => {
                    Err(RuntimeError::NotIndexable).map_err(|e| e.to_exec(span, data.clone()))
                }
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
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
            [] => Err(RuntimeError::MissingIndex).map_err(|e| e.to_exec(span, data.clone())),
            [i] => {
                if *i <= self.len() {
                    self.insert(*i, value_to_insert);
                    Ok(())
                } else {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
            }
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => {
                    inner_list.insert_nested(rest, value_to_insert, data, span)
                }
                Some(_) => {
                    Err(RuntimeError::NotIndexable).map_err(|e| e.to_exec(span, data.clone()))
                }
                None => {
                    Err(RuntimeError::IndexOutOfBounds).map_err(|e| e.to_exec(span, data.clone()))
                }
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
