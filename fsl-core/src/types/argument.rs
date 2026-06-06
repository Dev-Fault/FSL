use std::{future::ready, sync::Arc};

use async_recursion::async_recursion;
use futures::future::Either;

use crate::{
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    execute_command,
    source_str::SourceStr,
    span::Span,
    types::{FslType, command::Command, list::List, map::Map, value::Value},
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
                being_indexed: FslType::List,
                should_be: FslType::Int,
            }),
        }
    }

    pub fn to_map_indexer(self) -> Result<Vec<SourceStr>, RuntimeError> {
        match self {
            Indexer::Map(items) => Ok(items),
            _ => Err(RuntimeError::InvalidIndex {
                being_indexed: FslType::Map,
                should_be: FslType::Text,
            }),
        }
    }

    pub fn to_text_indexer(self) -> Result<usize, RuntimeError> {
        match self {
            Indexer::Text(i) => Ok(i),
            _ => Err(RuntimeError::InvalidIndex {
                being_indexed: FslType::Text,
                should_be: FslType::Int,
            }),
        }
    }
}

impl Accessor {
    pub fn new(root: AccessorSegment, segments: Vec<AccessorSegment>) -> Self {
        Self { root, segments }
    }

    pub async fn into_value(
        mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Value, SpannedError> {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::Text(text) => match text.chars().nth(i) {
                            Some(ch) => Ok(Value::from(ch)),
                            None => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                        },
                        _ => Err(value.conversion_err(&[FslType::Text]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let text = self
                        .root
                        .value
                        .to_text(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    match text.chars().nth(i) {
                        Some(ch) => Ok(Value::from(ch)),
                        None => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                    }
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::List(list) => list.get_nested_clone(&indexer, span),
                        _ => Err(value.conversion_err(&[FslType::List]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let root = self
                        .root
                        .value
                        .clone()
                        .to_list(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    root.get_nested_clone(&indexer, span)
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::Map(map) => map.get_nested_clone(&indexer, span),
                        _ => Err(value.conversion_err(&[FslType::Map]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let root = self
                        .root
                        .value
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    root.get_nested_clone(&indexer, span)
                }
            },
        }
    }

    async fn modify_char<F, R>(
        value: &mut Value,
        text: SourceStr,
        i: usize,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch, span).await?;
                let replacement = ch.clone().to_text(data.clone()).await.span_err(span)?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                *value = Value::Text(SourceStr::Owned(string));
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span)),
        }
    }

    async fn with_char<F, R>(
        text: &SourceStr,
        i: usize,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch, span).await?;
                let replacement = ch.clone().to_text(data.clone()).await.span_err(span)?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span)),
        }
    }

    pub async fn with_mut<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with_mut(&var, span, async |value, span| match value {
                        Value::Text(text) => {
                            let text = text.clone();
                            Self::modify_char(value, text, i, span, data, f).await
                        }
                        _ => Err(value.conversion_err(&[FslType::Text]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let text = self
                        .take_root()
                        .value
                        .to_text(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    Self::modify_char(&mut self.root.value, text, i, self.root.span, data, f).await
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with_mut(&var, span, async |value, span| match value {
                        Value::List(list) => {
                            let value = list.get_nested_mut(&indexer, span)?;
                            f(value, span).await
                        }
                        _ => Err(value.conversion_err(&[FslType::List]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let mut root = self
                        .take_root()
                        .value
                        .to_list(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    let value = root.get_nested_mut(&indexer, span)?;
                    f(value, span).await
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with_mut(&var, span, async |value, span| match value {
                        Value::Map(map) => {
                            let value = map.get_nested_mut(&indexer, span)?;
                            f(value, span).await
                        }
                        _ => Err(value.conversion_err(&[FslType::Map]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let mut root = self
                        .take_root()
                        .value
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    let value = root.get_nested_mut(&indexer, span)?;
                    f(value, span).await
                }
            },
        }
    }

    async fn with<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(i) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::Text(text) => Self::with_char(&text, i, span, data, f).await,
                        _ => Err(value.conversion_err(&[FslType::Text]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    let root_span = self.root.span;
                    let text = std::mem::take(&mut self.root.value);
                    let text = text.to_text(data.clone()).await.span_err(root_span)?;
                    let r = Self::with_char(&text, i, self.root.span, data, f).await?;
                    self.root.value = Value::Text(text);
                    Ok(r)
                }
            },
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::List(list) => {
                            let value = list.get_nested(&indexer, span)?;
                            f(value, span).await
                        }
                        _ => Err(value.conversion_err(&[FslType::List]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    // list is arc so cloning is cheap
                    let root = self
                        .root
                        .value
                        .clone()
                        .to_list(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    let value = root.get_nested(&indexer, span)?;
                    f(value, span).await
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, async |value, span| match value {
                        Value::Map(map) => {
                            let value = map.get_nested(&indexer, span)?;
                            f(value, span).await
                        }
                        _ => Err(value.conversion_err(&[FslType::Map]).span(span)),
                    })
                    .await
                }
                Err(_) => {
                    // map is arc so cloning is cheap
                    let root = self
                        .root
                        .value
                        .clone()
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span)?;
                    let value = root.get_nested(&indexer, span)?;
                    f(value, span).await
                }
            },
        }
    }

    fn take_root(&mut self) -> AccessorSegment {
        let replacement = AccessorSegment::new(Value::None, self.root.span);
        let previous = std::mem::replace(&mut self.root, replacement);
        previous
    }

    async fn resolve_segments(
        &mut self,
        inner_type: FslType,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match inner_type {
            FslType::Map => {
                let mut indexer = Vec::with_capacity(self.segments.len());
                for segment in &self.segments {
                    let key = match &segment.value {
                        Value::Text(text) => Ok(text.clone()),
                        Value::Var(label) => Ok(label.clone()),
                        _ => segment
                            .value
                            .clone()
                            .to_text(data.clone())
                            .await
                            .span_err(segment.span),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::Map(indexer))
            }
            FslType::List => {
                let mut indexer = Vec::with_capacity(self.segments.len());
                for segment in &self.segments {
                    let key = match segment.value {
                        Value::Int(i) => Ok(i as usize),
                        Value::Var(_) => segment
                            .value
                            .clone()
                            .to_usize(data.clone())
                            .await
                            .span_err(segment.span),
                        _ => Err(segment
                            .value
                            .conversion_err(&[FslType::Int])
                            .span(segment.span)),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::List(indexer))
            }
            FslType::Text => {
                let segment = match self.segments.as_slice() {
                    [segment] => Ok(segment),
                    _ => Err(RuntimeError::IndexOutOfBounds.span(self.root.span)),
                }?;
                let i = match segment.value {
                    Value::Int(i) => Ok(i as usize),
                    Value::Var(_) => segment
                        .value
                        .clone()
                        .to_usize(data.clone())
                        .await
                        .span_err(segment.span),
                    _ => Err(segment
                        .value
                        .conversion_err(&[FslType::Int])
                        .span(segment.span)),
                }?;
                Ok(Indexer::Text(i))
            }
            FslType::Command => {
                let value = std::mem::take(&mut self.root.value);
                let command = value.to_command(data.clone()).span_err(self.root.span)?;
                let value = execute_command!(command, data.clone())?;
                self.root.value = value;
                self.to_indexer(self.root.span, data).await
            }
            _ => Err(RuntimeError::NotIndexable.span(self.root.span)),
        }
    }

    #[async_recursion]
    async fn to_indexer(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match self
            .root
            .value
            .to_inner_type(data.clone())
            .await
            .span_err(self.root.span)?
        {
            FslType::Text => Ok(Self::resolve_segments(self, FslType::Text, data).await?),
            FslType::List => Ok(Self::resolve_segments(self, FslType::List, data).await?),
            FslType::Map => Ok(Self::resolve_segments(self, FslType::Map, data).await?),
            FslType::Command => {
                let value = std::mem::take(&mut self.root.value);
                let command = value.to_command(data.clone()).span_err(self.root.span)?;
                let value = execute_command!(command, data.clone())?;
                self.root.value = value;
                self.to_indexer(span, data).await
            }
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
    ) -> impl Future<Output = Result<Value, SpannedError>> {
        match self {
            ArgumentKind::Value(value) => {
                return Either::Left(ready(Ok(value)));
            }
            ArgumentKind::Accessor(path) => {
                return Either::Right(path.into_value(span, data));
            }
        }
    }

    pub async fn into_indexer(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match self {
            ArgumentKind::Value(_) => Err(RuntimeError::NotIndexable.span(span)),
            ArgumentKind::Accessor(accessor) => Ok(accessor.to_indexer(span, data).await?),
        }
    }

    pub async fn with_mut<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => match value {
                Value::Var(label) => {
                    let vars = data.vars.read().await;
                    vars.with_mut(label, span, f).await
                }
                _ => f(value, span).await,
            },
            ArgumentKind::Accessor(path) => path.with_mut(span, data, f).await,
        }
    }

    pub async fn with_inner<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => match value {
                Value::Var(label) => {
                    let vars = data.vars.read().await;
                    vars.with(label, span, f).await
                }
                _ => f(value, span).await,
            },
            ArgumentKind::Accessor(path) => path.with(span, data, f).await,
        }
    }

    pub async fn with<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => f(value, span).await,
            ArgumentKind::Accessor(path) => path.with(span, data, f).await,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    kind: ArgumentKind,
    pub span: Span,
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
        }
    }

    pub fn new_path(path: Accessor, span: Span) -> Self {
        Self {
            kind: ArgumentKind::Accessor(path),
            span,
        }
    }

    pub async fn with<F, R>(&mut self, data: Arc<InterpreterData>, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        if let ArgumentKind::Value(Value::Command(_)) = &mut self.kind {
            let value = self.take_value();
            let command = value.to_command(data.clone()).span_err(self.span)?;
            let value = execute_command!(command, data.clone())?;
            self.kind = ArgumentKind::Value(value);
        }
        self.kind.with_inner(self.span, data.clone(), f).await
    }

    pub async fn with_mut<F, R>(
        &mut self,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        let mut kind = std::mem::take(&mut self.kind);
        if let ArgumentKind::Value(Value::Command(command)) = kind {
            let value = execute_command!(command, data.clone())?;
            kind = ArgumentKind::Value(value);
        }
        self.kind = kind;
        self.kind.with_mut(self.span, data.clone(), f).await
    }

    pub async fn into_value(self, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
        self.kind.into_value(self.span, data).await
    }

    pub async fn into_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<(Argument, Indexer), SpannedError> {
        match self.kind {
            ArgumentKind::Value(_) => Err(RuntimeError::NotIndexable.span(self.span)),
            ArgumentKind::Accessor(mut accessor) => {
                let indexer = accessor.to_indexer(self.span, data).await?;
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
    pub async fn to_type(&mut self, data: Arc<InterpreterData>) -> Result<FslType, SpannedError> {
        self.kind
            .with(self.span, data, async |value, _| Ok(value.to_type()))
            .await
    }

    pub async fn to_inner_type(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<FslType, SpannedError> {
        self.with(data.clone(), async |value, span| {
            value.to_inner_type(data).await.span_err(span)
        })
        .await
    }

    pub async fn is_type(
        &mut self,
        fsl_type: FslType,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(
                self.span,
                data,
                async |value, _| Ok(value.is_type(fsl_type)),
            )
            .await
    }

    pub async fn is_var(&mut self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, async |value, _| Ok(value.is_var()))
            .await
    }

    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self.kind {
            ArgumentKind::Value(value) => value.mem_size().await,
            ArgumentKind::Accessor(_) => Ok(0),
        }
    }

    pub async fn equal(
        &mut self,
        other: &mut Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |l_value, _| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value, span| {
                        l_value.equal(r_value, data).span_err(span)
                    })
                    .await
            })
            .await
    }

    pub async fn soft_equal(
        &mut self,
        other: &mut Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |l_value, _| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value, span| {
                        l_value.soft_equal(r_value, data.clone()).span_err(span)
                    })
                    .await
            })
            .await
    }

    pub async fn to_int(self, data: Arc<InterpreterData>) -> Result<i64, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_int(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_usize(self, data: Arc<InterpreterData>) -> Result<usize, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_usize(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_float(self, data: Arc<InterpreterData>) -> Result<f64, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_float(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_bool(self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_bool(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_var(self, data: Arc<InterpreterData>) -> Result<SourceStr, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_var(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_text(self, data: Arc<InterpreterData>) -> Result<SourceStr, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_text(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_list(self, data: Arc<InterpreterData>) -> Result<List, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_list(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_map(self, data: Arc<InterpreterData>) -> Result<Map, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_map(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_number(self, data: Arc<InterpreterData>) -> Result<Argument, SpannedError> {
        let number = self
            .kind
            .into_value(self.span, data.clone())
            .await?
            .to_number(data.clone())
            .await;
        let number = number.map(|v| Self::new(v, self.span));

        number.span_err(self.span)
    }

    pub async fn as_raw_checked(
        self,
        valid_types: &'static [FslType],
        data: Arc<InterpreterData>,
    ) -> Result<Value, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_raw_checked(valid_types, data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_raw(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_list_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<Vec<usize>, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_list_indexer(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_map_indexer(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<Vec<SourceStr>, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_map_indexer(data.clone())
            .await
            .span_err(self.span)
    }

    pub async fn to_command(self, data: Arc<InterpreterData>) -> Result<Command, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_command(data.clone())
            .span_err(self.span)
    }

    pub async fn as_var_label(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |value, span| {
                value.as_var_label(data).span_err(span)
            })
            .await
    }

    pub async fn as_command_label(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |value, span| {
                value.as_command_label().span_err(span)
            })
            .await
    }
}
