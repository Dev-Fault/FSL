use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::{
    await_result,
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError},
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
    pub async fn resolve(self, data: Arc<InterpreterData>) -> Result<List, ValueError> {
        match self {
            List::Resolved(_) => Ok(self),
            List::Unresolved(arc) => {
                let mut values = Arc::unwrap_or_clone(arc);
                for value in values.iter_mut() {
                    *value = await_result!(std::mem::take(value).as_raw(data.clone()))?;
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

    pub fn get_nested_clone(&self, indices: &[usize], span: Span) -> Result<Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => {
                let result = self.get(*i).cloned();
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_clone(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
        }
    }

    pub fn get_nested(&self, indices: &[usize], span: Span) -> Result<&Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => {
                let result = self.get(*i);
                match result {
                    Some(value) => Ok(value),
                    None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
                }
            }
            [i, rest @ ..] => match self.get(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
        }
    }

    pub fn get_nested_mut(
        &mut self,
        indices: &[usize],
        span: Span,
    ) -> Result<&mut Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => match self.get_mut(*i) {
                Some(i) => Ok(i),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.get_nested_mut(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
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
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => match self.get_mut(*i) {
                Some(i) => {
                    let old = std::mem::take(i);
                    *i = value;
                    Ok(old)
                }
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.set_nested(rest, value, span),
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
        }
    }

    pub fn remove_nested(&mut self, indices: &[usize], span: Span) -> Result<Value, SpannedError> {
        match indices {
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => match self.get(*i) {
                Some(_) => Ok(self.remove(*i)),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
            },
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => inner_list.remove_nested(rest, span),
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
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
            [] => Err(RuntimeError::MissingIndex).span_err(span),
            [i] => {
                if *i <= self.len() {
                    self.insert(*i, value_to_insert);
                    Ok(())
                } else {
                    Err(RuntimeError::IndexOutOfBounds).span_err(span)
                }
            }
            [i, rest @ ..] => match self.get_mut(*i) {
                Some(Value::List(inner_list)) => {
                    inner_list.insert_nested(rest, value_to_insert, span)
                }
                Some(_) => Err(RuntimeError::NotIndexable).span_err(span),
                None => Err(RuntimeError::IndexOutOfBounds).span_err(span),
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
