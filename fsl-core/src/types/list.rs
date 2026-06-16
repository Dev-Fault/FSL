use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError},
    potential_futures::{PotentialFuture, PotentialFutureResult},
    span::Span,
    types::value::{Value, ValueError},
};

#[derive(Debug, Clone)]
pub enum List {
    Resolved(Arc<Vec<Value>>),
    Unresolved(Arc<Vec<Value>>),
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let values = match self {
            List::Resolved(values) => values,
            List::Unresolved(values) => values,
        };
        write!(f, "[")?;
        for (i, value) in values.iter().enumerate() {
            if i < values.len() - 1 {
                write!(f, "{}, ", value.to_string())?;
            } else {
                write!(f, "{}", value.to_string())?;
            }
        }
        write!(f, "]")
    }
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
    pub fn resolve(self, data: Arc<InterpreterData>) -> PotentialFutureResult<List, ValueError> {
        match self {
            List::Resolved(_) => Ok(PotentialFuture::Sync(self)),
            List::Unresolved(arc) => {
                let mut pending_ops: Vec<_> = Vec::new();
                let mut values = Arc::unwrap_or_clone(arc);
                for (i, value) in values.iter_mut().enumerate() {
                    let inner_value = std::mem::take(value).to_inner(data.clone())?;
                    match inner_value {
                        PotentialFuture::Sync(resolved_value) => {
                            *value = resolved_value;
                        }
                        PotentialFuture::Async(pin) => {
                            pending_ops.push((i, pin));
                        }
                    }
                }
                if pending_ops.len() > 0 {
                    Ok(PotentialFuture::Async(Box::pin(async move {
                        for (i, op) in pending_ops {
                            values[i] = op.await?;
                        }
                        Ok(List::Resolved(Arc::new(values)))
                    })))
                } else {
                    Ok(PotentialFuture::Sync(List::Resolved(Arc::new(values))))
                }
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

    pub fn get_nested_clone(&self, indices: &[usize], span: Span) -> Result<Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => {
                let result = self.get(*i).cloned();
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds).span(span),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_clone(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
        }
    }

    pub fn get_nested(&self, indices: &[usize], span: Span) -> Result<&Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => {
                let result = self.get(*i);
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds).span(span),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
        }
    }

    pub fn get_nested_mut(
        &mut self,
        indices: &[usize],
        span: Span,
    ) -> Result<&mut Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => match self.get_mut(*i) {
                Some(i) => Ok(i),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_mut(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
        }
    }

    pub fn set_nested(
        &mut self,
        indices: &[usize],
        value: Value,
        span: Span,
    ) -> Result<Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => match self.get_mut(*i) {
                Some(i) => {
                    let old = std::mem::take(i);
                    *i = value;
                    Ok(old)
                }
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.set_nested(rest, value, span),
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
        }
    }

    pub fn remove_nested(&mut self, indices: &[usize], span: Span) -> Result<Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => match self.get(*i) {
                Some(_) => Ok(self.remove(*i)),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.remove_nested(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
            },
        }
    }

    pub fn insert_nested(
        &mut self,
        indices: &[usize],
        value_to_insert: Value,
        span: Span,
    ) -> Result<(), SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span(span),
            [i] => {
                if *i <= self.len() {
                    self.insert(*i, value_to_insert);
                    Ok(())
                } else {
                    Err(RuntimeError::IndexOutOfBounds).span(span)
                }
            }
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => {
                    inner_list.insert_nested(rest, value_to_insert, span)
                }
                Some(_) => Err(RuntimeError::NotIndexable).span(span),
                None => Err(RuntimeError::IndexOutOfBounds).span(span),
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
