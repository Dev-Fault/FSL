use std::sync::Arc;

use crate::{
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    potential_futures::{PotentialFuture, PotentialFutureResult, SpannedPotentialFutureResult},
    source_str::SourceStr,
    span::Span,
    types::{
        ValueType,
        command::{ArgRule, Command},
        list::List,
        map::Map,
        value::{Number, Value},
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct AccessorSegment {
    pub value: Value,
    pub span: Span,
}

impl AccessorSegment {
    pub fn new(value: Value, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Accessor {
    pub root: AccessorSegment,
    pub segments: Vec<AccessorSegment>,
}

pub enum Indexer {
    List(Vec<usize>),
    Text(usize),
    Map(Vec<SourceStr>),
}

impl Indexer {
    pub fn to_list_indexer(self) -> Result<Vec<usize>, RuntimeError> {
        match self {
            Indexer::List(items) => Ok(items),
            _ => Err(RuntimeError::InvalidIndex {
                being_indexed: ValueType::List,
                should_be: ValueType::Int,
            }),
        }
    }

    pub fn to_map_indexer(self) -> Result<Vec<SourceStr>, RuntimeError> {
        match self {
            Indexer::Map(items) => Ok(items),
            _ => Err(RuntimeError::InvalidIndex {
                being_indexed: ValueType::Map,
                should_be: ValueType::Text,
            }),
        }
    }

    pub fn to_text_indexer(self) -> Result<usize, RuntimeError> {
        match self {
            Indexer::Text(i) => Ok(i),
            _ => Err(RuntimeError::InvalidIndex {
                being_indexed: ValueType::Text,
                should_be: ValueType::Int,
            }),
        }
    }
}

impl Accessor {
    pub fn new(root: AccessorSegment, segments: Vec<AccessorSegment>) -> Self {
        Self { root, segments }
    }

    fn value_from_indexer(
        mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Value, SpannedError> {
        match self.to_indexer(span, data.clone())? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::Text(text) => match text.chars().nth(i) {
                            Some(ch) => Ok(Value::from(ch)),
                            None => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                        },
                        _ => Err(value.conversion_err(&[ValueType::Text]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.root.value;
                    let text = value.into_str().span(self.root.span)?;
                    match text.chars().nth(i) {
                        Some(ch) => Ok(Value::from(ch)),
                        None => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                    }
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::List(list) => list.get_nested_clone(&indexer, span),
                        _ => Err(value.conversion_err(&[ValueType::List]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.root.value;
                    let root = value.into_list().span(self.root.span)?;
                    root.get_nested_clone(&indexer, span)
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::Map(map) => map.get_nested_clone(&indexer, span),
                        _ => Err(value.conversion_err(&[ValueType::Map]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.root.value;
                    let root = value.into_map().span(self.root.span)?;
                    root.get_nested_clone(&indexer, span)
                }
            },
        }
    }

    pub fn into_value(
        mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, SpannedError> {
        if self.root.value.is_type(ValueType::Command) {
            let command = std::mem::take(&mut self.root.value);
            let command = command.to_command().span(span)?;
            match command.execute(data.clone()) {
                Ok(o) => match o {
                    PotentialFuture::Sync(value) => {
                        return Ok(PotentialFuture::Sync({
                            self.root.value = value;
                            self.value_from_indexer(span, data)?
                        }));
                    }
                    PotentialFuture::Async(pin) => {
                        return Ok(PotentialFuture::Async(Box::pin(async move {
                            let value = pin.await?;
                            self.root.value = value;
                            self.value_from_indexer(span, data)
                        })));
                    }
                },
                Err(e) => return Err(e),
            }
        }
        Ok(PotentialFuture::Sync(self.value_from_indexer(span, data)?))
    }

    fn modify_char<F, R>(
        value: &mut Value,
        text: SourceStr,
        i: usize,
        span: Span,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch, span)?;
                let replacement = ch.into_str().span(span)?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                *value = Value::Text(SourceStr::Owned(string));
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span)),
        }
    }

    fn with_char<F, R>(text: &SourceStr, i: usize, span: Span, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch, span)?;
                let replacement = ch.into_str().span(span)?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span)),
        }
    }

    pub fn with_mut<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone())? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with_mut(&var, span, |value, span| match value {
                        Value::Text(text) => {
                            let text = text.clone();
                            Self::modify_char(value, text, i, span, f)
                        }
                        _ => Err(value.conversion_err(&[ValueType::Text]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.take_root().value;
                    let text = value.into_str().span(self.root.span)?;
                    Self::modify_char(&mut self.root.value, text, i, self.root.span, f)
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with_mut(&var, span, |value, span| match value {
                        Value::List(list) => {
                            let value = list.get_nested_mut(&indexer, span)?;
                            f(value, span)
                        }
                        _ => Err(value.conversion_err(&[ValueType::List]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.take_root().value;
                    let mut root = value.into_list().span(self.root.span)?;
                    let value = root.get_nested_mut(&indexer, span)?;
                    f(value, span)
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with_mut(&var, span, |value, span| match value {
                        Value::Map(map) => {
                            let value = map.get_nested_mut(&indexer, span)?;
                            f(value, span)
                        }
                        _ => Err(value.conversion_err(&[ValueType::Map]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.take_root().value;
                    let mut root = value.into_map().span(self.root.span)?;
                    let value = root.get_nested_mut(&indexer, span)?;
                    f(value, span)
                }
            },
        }
    }

    fn with<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone())? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::Text(text) => Self::with_char(&text, i, span, f),
                        _ => Err(value.conversion_err(&[ValueType::Text]).span(span)),
                    })
                }
                Err(_) => {
                    let value = std::mem::take(&mut self.root.value);
                    let text = value.into_str().span(self.root.span)?;
                    let r = Self::with_char(&text, i, self.root.span, f)?;
                    self.root.value = Value::Text(text);
                    Ok(r)
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::List(list) => {
                            let value = list.get_nested(&indexer, span)?;
                            f(value, span)
                        }
                        _ => Err(value.conversion_err(&[ValueType::List]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.root.value.clone();
                    let root = value.into_list().span(self.root.span)?;
                    let value = root.get_nested(&indexer, span)?;
                    f(value, span)
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read();
                    vars.with(&var, span, |value, span| match value {
                        Value::Map(map) => {
                            let value = map.get_nested(&indexer, span)?;
                            f(value, span)
                        }
                        _ => Err(value.conversion_err(&[ValueType::Map]).span(span)),
                    })
                }
                Err(_) => {
                    let value = self.root.value.clone();
                    let root = value.into_map().span(self.root.span)?;
                    let value = root.get_nested(&indexer, span)?;
                    f(value, span)
                }
            },
        }
    }

    fn take_root(&mut self) -> AccessorSegment {
        let replacement = AccessorSegment::new(Value::None, self.root.span);
        let previous = std::mem::replace(&mut self.root, replacement);
        previous
    }

    fn resolve_segments(
        &mut self,
        inner_type: ValueType,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match inner_type {
            ValueType::Map => {
                let mut indexer = Vec::with_capacity(self.segments.len());
                for segment in &self.segments {
                    let key = match &segment.value {
                        Value::Text(text) => text.clone(),
                        Value::Var(label) => label.clone(),
                        _ => segment.value.clone().into_str().span(segment.span)?,
                    };
                    indexer.push(key);
                }
                Ok(Indexer::Map(indexer))
            }
            ValueType::List => {
                let mut indexer = Vec::with_capacity(self.segments.len());
                for segment in &self.segments {
                    let key = match &segment.value {
                        Value::Int(i) => Ok(*i as usize),
                        Value::Var(label) => {
                            let value = data.vars.read().get_clone(&label).span(segment.span)?;
                            value.into_usize().span(segment.span)
                        }
                        _ => Err(segment
                            .value
                            .conversion_err(&[ValueType::Int])
                            .span(segment.span)),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::List(indexer))
            }
            ValueType::Text => {
                let segment = match self.segments.as_slice() {
                    [segment] => Ok(segment),
                    _ => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                }?;
                let i = match &segment.value {
                    Value::Int(i) => Ok(*i as usize),
                    Value::Var(label) => {
                        let value = data.vars.read().get_clone(&label).span(segment.span)?;
                        value.into_usize().span(segment.span)
                    }
                    _ => Err(segment
                        .value
                        .conversion_err(&[ValueType::Int])
                        .span(segment.span)),
                }?;
                Ok(Indexer::Text(i))
            }
            _ => Err(RuntimeError::NotIndexable.span(self.root.span)),
        }
    }

    fn to_indexer(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match self
            .root
            .value
            .to_inner_type(data.clone())
            .span(self.root.span)?
        {
            ValueType::Text => Ok(Self::resolve_segments(self, ValueType::Text, data)?),
            ValueType::List => Ok(Self::resolve_segments(self, ValueType::List, data)?),
            ValueType::Map => Ok(Self::resolve_segments(self, ValueType::Map, data)?),
            _ => Err(RuntimeError::NotIndexable.span(span)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentKind {
    Value(Value),
    Accessor(Accessor),
}

impl Default for ArgumentKind {
    fn default() -> Self {
        Self::Value(Value::None)
    }
}

impl ArgumentKind {
    pub fn into_value(
        self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, SpannedError> {
        match self {
            ArgumentKind::Value(value) => Ok(PotentialFuture::Sync(value)),
            ArgumentKind::Accessor(path) => match path.into_value(span, data)? {
                PotentialFuture::Sync(value) => Ok(PotentialFuture::Sync(value)),
                PotentialFuture::Async(pin) => Ok(PotentialFuture::Async(Box::pin(async move {
                    let value = pin.await?;
                    Ok(value)
                }))),
            },
        }
    }

    pub fn with_mut<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => match value {
                Value::Var(label) => {
                    let vars = data.vars.read();
                    vars.with_mut(label, span, f)
                }
                _ => f(value, span),
            },
            ArgumentKind::Accessor(path) => path.with_mut(span, data, f),
        }
    }

    pub fn with_inner<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => match value {
                Value::Var(label) => {
                    let vars = data.vars.read();
                    vars.with(label, span, f)
                }
                _ => f(value, span),
            },
            ArgumentKind::Accessor(path) => path.with(span, data, f),
        }
    }

    pub fn with<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => f(value, span),
            ArgumentKind::Accessor(path) => path.with(span, data, f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub kind: ArgumentKind,
    pub span: Span,
    pub(crate) resolve_to: ArgRule<()>,
}

impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span
    }
}

impl Argument {
    pub fn new(value: Value, span: Span) -> Self {
        Self {
            kind: ArgumentKind::Value(value),
            span,
            resolve_to: ArgRule::Raw(()),
        }
    }

    pub fn new_path(path: Accessor, span: Span) -> Self {
        Self {
            kind: ArgumentKind::Accessor(path),
            span,
            resolve_to: ArgRule::Raw(()),
        }
    }

    pub fn with<F, R>(&mut self, data: Arc<InterpreterData>, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        self.kind.with_inner(self.span, data.clone(), f)
    }

    pub fn with_mut<F, R>(&mut self, data: Arc<InterpreterData>, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        let kind = std::mem::take(&mut self.kind);
        self.kind = kind;
        self.kind.with_mut(self.span, data.clone(), f)
    }

    pub fn into_value(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        kind.into_value(self.span, data)
    }

    pub fn into_indexer(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<(Argument, Indexer), SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        match kind {
            ArgumentKind::Value(_) => Err(RuntimeError::NotIndexable.span(self.span)),
            ArgumentKind::Accessor(mut accessor) => {
                let indexer = accessor.to_indexer(self.span, data)?;
                let root = Argument::new(accessor.root.value, accessor.root.span);
                Ok((root, indexer))
            }
        }
    }

    pub fn take_value(&mut self) -> Value {
        match &mut self.kind {
            ArgumentKind::Value(value) => std::mem::take(value),
            ArgumentKind::Accessor(acessor) => std::mem::take(&mut acessor.root.value),
        }
    }

    pub fn replace_value(&mut self, value: Value) {
        match &mut self.kind {
            ArgumentKind::Value(v) => {
                *v = value;
            }
            ArgumentKind::Accessor(accessor) => {
                accessor.root.value = value;
            }
        }
    }
}

impl Argument {
    pub fn to_type(&mut self, data: Arc<InterpreterData>) -> Result<ValueType, SpannedError> {
        self.kind
            .with(self.span, data, |value, _| Ok(value.to_type()))
    }

    pub fn to_inner_type(&mut self, data: Arc<InterpreterData>) -> Result<ValueType, SpannedError> {
        self.with(data.clone(), |value, span| {
            value.to_inner_type(data).span(span)
        })
    }

    pub fn is_type(
        &mut self,
        fsl_type: ValueType,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, |value, _| Ok(value.is_type(fsl_type)))
    }

    pub fn is_literal(&mut self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, |value, _| Ok(value.is_literal()))
    }

    pub fn is_var(&mut self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, |value, _| Ok(value.is_var()))
    }

    pub fn is_command(&mut self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, |value, _| Ok(value.is_command()))
    }

    pub fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self.kind {
            ArgumentKind::Value(value) => value.mem_size(),
            ArgumentKind::Accessor(_) => Ok(0),
        }
    }

    pub fn to_int(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<i64, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_int(data)).span_future(self.span)
    }

    pub fn to_usize(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<usize, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value
            .map_result(|v| v.to_usize(data))
            .span_future(self.span)
    }

    pub fn to_float(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<f64, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value
            .map_result(|v| v.to_float(data))
            .span_future(self.span)
    }

    pub fn to_bool(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<bool, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_bool(data)).span_future(self.span)
    }

    pub fn to_var(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<SourceStr, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_var(data)).span_future(self.span)
    }

    pub fn to_text(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<SourceStr, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_text(data)).span_future(self.span)
    }

    pub fn to_list(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<List, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_list(data)).span_future(self.span)
    }

    pub fn to_map(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Map, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value.map_result(|v| v.to_map(data)).span_future(self.span)
    }

    pub fn to_number(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Argument, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let span = self.span;
        let value = kind.into_value(self.span, data.clone())?;
        value
            .map_result(move |v| {
                v.to_number(data)
                    .map(|pf| pf.map(move |v| Argument::new(v, span)))
            })
            .span_future(span)
    }

    pub fn to_inner_checked(
        &mut self,
        valid_types: &'static [ValueType],
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value
            .map_result(move |v| v.to_inner_checked(valid_types, data))
            .span_future(self.span)
    }

    pub fn to_inner(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Value, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        value
            .map_result(move |v| v.to_inner(data))
            .span_future(self.span)
    }

    pub fn to_command(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> PotentialFutureResult<Command, SpannedError> {
        let kind = std::mem::take(&mut self.kind);
        let value = kind.into_value(self.span, data.clone())?;
        let span = self.span;
        match value {
            PotentialFuture::Sync(v) => Ok(PotentialFuture::Sync(v.to_command().span(self.span)?)),
            PotentialFuture::Async(pin) => Ok(PotentialFuture::Async(Box::pin(async move {
                let v = pin.await?;
                Ok(v.to_command().span(span)?)
            }))),
        }
    }

    pub fn as_var_label(&mut self, data: Arc<InterpreterData>) -> Result<SourceStr, SpannedError> {
        self.kind.with(self.span, data.clone(), |value, span| {
            value.as_var_label(data).span(span)
        })
    }

    pub fn as_command_label(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, SpannedError> {
        self.kind.with(self.span, data.clone(), |value, span| {
            value.as_command_label().span(span)
        })
    }
}

impl Argument {
    pub fn into_str(&mut self) -> Result<SourceStr, SpannedError> {
        self.take_value().into_str().span(self.span)
    }

    pub fn into_usize(&mut self) -> Result<usize, SpannedError> {
        self.take_value().into_usize().span(self.span)
    }

    pub fn into_list_indexer(&mut self) -> Result<Vec<usize>, SpannedError> {
        self.take_value().into_list_indexer().span(self.span)
    }

    pub fn into_map_indexer(&mut self) -> Result<Vec<SourceStr>, SpannedError> {
        self.take_value().into_map_indexer().span(self.span)
    }

    pub fn into_list(&mut self) -> Result<List, SpannedError> {
        self.take_value().into_list().span(self.span)
    }

    pub fn into_map(&mut self) -> Result<Map, SpannedError> {
        self.take_value().into_map().span(self.span)
    }

    pub fn into_var(&mut self) -> Result<SourceStr, SpannedError> {
        self.take_value().into_var().span(self.span)
    }

    pub fn into_command(&mut self) -> Result<Box<Command>, SpannedError> {
        self.take_value().into_command().span(self.span)
    }

    pub fn into_int(&mut self) -> Result<i64, SpannedError> {
        self.take_value().into_int().span(self.span)
    }

    pub fn into_float(&mut self) -> Result<f64, SpannedError> {
        self.take_value().into_float().span(self.span)
    }

    pub fn into_number(&mut self) -> Result<Number, SpannedError> {
        self.take_value().into_number().span(self.span)
    }

    pub fn into_bool(&mut self) -> Result<bool, SpannedError> {
        self.take_value().into_bool().span(self.span)
    }
}
