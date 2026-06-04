use std::{future::ready, ops::Range, sync::Arc};

use async_recursion::async_recursion;
use futures::future::Either;

use crate::{
    data::InterpreterData,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    source_str::SourceStr,
    span::Span,
    types::{FslType, command::Command, list::List, map::Map, value::Value},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ArgPos {
    Index(usize),
    OptionalIndex(usize),
    Range(Range<usize>),
    AnyFrom(usize),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgRule {
    pub position: ArgPos,
    pub valid_types: &'static [FslType],
}

impl ArgRule {
    pub const fn new(position: ArgPos, valid_types: &'static [FslType]) -> Self {
        Self {
            position,
            valid_types,
        }
    }
    pub const fn none() -> Self {
        Self {
            position: ArgPos::None,
            valid_types: &[],
        }
    }
}

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

enum Indexer {
    List(Vec<usize>),
    Text(Vec<usize>),
    Map(Vec<SourceStr>),
}

impl Accessor {
    pub fn new(root: AccessorSegment, segments: Vec<AccessorSegment>) -> Self {
        Self { root, segments }
    }

    pub async fn into_value(
        self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Value, SpannedError> {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(indexer) => {
                let i = *indexer.get(0).unwrap();
                match self.root.value.as_var_label(data.clone()) {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.with(&var, span, data.clone(), async |value| match value {
                            Value::Text(text) => match text.chars().nth(i) {
                                Some(ch) => Ok(Value::from(ch)),
                                None => {
                                    Err(RuntimeError::IndexOutOfBounds.span(self.root.span, data))
                                }
                            },
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Text])
                                .span(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let text = self
                            .root
                            .value
                            .to_text(data.clone())
                            .await
                            .span_err(self.root.span, data.clone())?;
                        match text.chars().nth(i) {
                            Some(ch) => Ok(Value::from(ch)),
                            None => Err(RuntimeError::IndexOutOfBounds.span(self.root.span, data)),
                        }
                    }
                }
            }
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, data.clone(), async |value| match value {
                        Value::List(list) => list.get_nested_clone(&indexer, data, span),
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::List])
                            .span(span, data)),
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
                        .span_err(self.root.span, data.clone())?;
                    root.get_nested_clone(&indexer, data, span)
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, data.clone(), async |value| match value {
                        Value::Map(map) => map.get_nested_clone(&indexer, data, span),
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::Map])
                            .span(span, data)),
                    })
                    .await
                }
                Err(_) => {
                    let root = self
                        .root
                        .value
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span, data.clone())?;
                    root.get_nested_clone(&indexer, data, span)
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
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch).await?;
                let replacement = ch
                    .clone()
                    .to_text(data.clone())
                    .await
                    .span_err(span, data.clone())?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                *value = Value::Text(SourceStr::Owned(string));
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span, data)),
        }
    }

    async fn with_char<F, R>(
        text: SourceStr,
        i: usize,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, SpannedError>,
    {
        let mut string = text.to_string();
        match string.chars().nth(i) {
            Some(char) => {
                let mut ch = Value::from(char);
                let result = f(&mut ch).await?;
                let replacement = ch
                    .clone()
                    .to_text(data.clone())
                    .await
                    .span_err(span, data.clone())?;
                string.replace_range(i..i + char.len_utf8(), &replacement);
                Ok(result)
            }
            None => Err(RuntimeError::IndexOutOfBounds.span(span, data)),
        }
    }

    pub async fn modify<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(indexer) => {
                let i = *indexer.get(0).unwrap();
                match self.root.value.as_var_label(data.clone()) {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::Text(text) => {
                                let text = text.clone();
                                Self::modify_char(value, text, i, span, data, f).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Text])
                                .span(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let text = self
                            .take_root()
                            .value
                            .to_text(data.clone())
                            .await
                            .span_err(self.root.span, data.clone())?;
                        Self::modify_char(&mut self.root.value, text, i, self.root.span, data, f)
                            .await
                    }
                }
            }
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.modify(&var, span, data.clone(), async |value| match value {
                        Value::List(list) => {
                            let value = list.get_nested_mut(&indexer, data, span)?;
                            f(value).await
                        }
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::List])
                            .span(span, data)),
                    })
                    .await
                }
                Err(_) => {
                    let mut root = self
                        .take_root()
                        .value
                        .to_list(data.clone())
                        .await
                        .span_err(self.root.span, data.clone())?;
                    let value = root.get_nested_mut(&indexer, data, span)?;
                    f(value).await
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.modify(&var, span, data.clone(), async |value| match value {
                        Value::Map(map) => {
                            let value = map.get_nested_mut(&indexer, data, span)?;
                            f(value).await
                        }
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::Map])
                            .span(span, data)),
                    })
                    .await
                }
                Err(_) => {
                    let mut root = self
                        .take_root()
                        .value
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span, data.clone())?;
                    let value = root.get_nested_mut(&indexer, data, span)?;
                    f(value).await
                }
            },
        }
    }

    async fn with<F, R>(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, SpannedError>,
    {
        match self.to_indexer(span, data.clone()).await? {
            Indexer::Text(indexer) => {
                let i = *indexer.get(0).unwrap();
                match self.root.value.as_var_label(data.clone()) {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.with(&var, span, data.clone(), async |value| match value {
                            Value::Text(text) => {
                                let text = text.clone();
                                Self::with_char(text, i, span, data, f).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Text])
                                .span(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let text = self
                            .root
                            .value
                            .clone()
                            .to_text(data.clone())
                            .await
                            .span_err(self.root.span, data.clone())?;
                        Self::with_char(text, i, self.root.span, data, f).await
                    }
                }
            }
            Indexer::List(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, data.clone(), async |value| match value {
                        Value::List(list) => {
                            let value = list.get_nested(&indexer, data, span)?;
                            f(value).await
                        }
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::List])
                            .span(span, data)),
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
                        .span_err(self.root.span, data.clone())?;
                    let value = root.get_nested(&indexer, data, span)?;
                    f(value).await
                }
            },
            Indexer::Map(indexer) => match self.root.value.as_var_label(data.clone()) {
                Ok(var) => {
                    let data_clone = data.clone();
                    let vars = data_clone.vars.read().await;
                    vars.with(&var, span, data.clone(), async |value| match value {
                        Value::Map(map) => {
                            let value = map.get_nested(&indexer, data, span)?;
                            f(value).await
                        }
                        _ => Err(value
                            .conversion_err_to_types(&[FslType::Map])
                            .span(span, data)),
                    })
                    .await
                }
                Err(_) => {
                    let root = self
                        .root
                        .value
                        .clone()
                        .to_map(data.clone())
                        .await
                        .span_err(self.root.span, data.clone())?;
                    let value = root.get_nested(&indexer, data, span)?;
                    f(value).await
                }
            },
        }
    }

    fn take_root(&mut self) -> AccessorSegment {
        let replacement = AccessorSegment::new(Value::None, self.root.span);
        let previous = std::mem::replace(&mut self.root, replacement);
        previous
    }

    #[async_recursion]
    async fn to_indexer(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Indexer, SpannedError> {
        match self
            .root
            .value
            .to_inner_type(data.clone())
            .await
            .span_err(self.root.span, data.clone())?
        {
            FslType::Text => {
                let mut indexer = Vec::with_capacity(self.segments.len());
                for segment in &self.segments {
                    let key = match segment.value {
                        Value::Int(i) => Ok(i as usize),
                        Value::Var(_) => segment
                            .value
                            .clone()
                            .to_usize(data.clone())
                            .await
                            .span_err(segment.span, data.clone()),
                        _ => Err(segment
                            .value
                            .conversion_err_to_types(&[FslType::Int])
                            .span(segment.span, data.clone())),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::Text(indexer))
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
                            .span_err(segment.span, data.clone()),
                        _ => Err(segment
                            .value
                            .conversion_err_to_types(&[FslType::Int])
                            .span(segment.span, data.clone())),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::List(indexer))
            }
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
                            .span_err(segment.span, data.clone()),
                    }?;
                    indexer.push(key);
                }
                Ok(Indexer::Map(indexer))
            }
            FslType::Command => {
                let value = self
                    .root
                    .value
                    .clone()
                    .to_command(data.clone())
                    .span_err(self.root.span, data.clone())?
                    .execute(data.clone())
                    .await?;
                let root = AccessorSegment::new(value, span);
                let path = Accessor::new(root, self.segments.clone());
                path.to_indexer(span, data).await
            }
            _ => Err(RuntimeError::NotIndexable.span(span, data)),
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

    pub async fn modify<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => {
                if let Value::Var(label) = value {
                    let vars = data.vars.read().await;
                    vars.modify(label, span, data.clone(), f).await
                } else {
                    f(value).await
                }
            }
            ArgumentKind::Accessor(path) => path.modify(span, data, f).await,
        }
    }

    pub async fn with_inner<F, R>(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => {
                if let Value::Var(label) = value {
                    let vars = data.vars.read().await;
                    vars.with(label, span, data.clone(), f).await
                } else {
                    f(value).await
                }
            }
            ArgumentKind::Accessor(path) => path.with(span, data, f).await,
        }
    }

    pub async fn with<F, R>(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, SpannedError>,
    {
        match self {
            ArgumentKind::Value(value) => f(value).await,
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

    pub async fn with<F, R>(&self, data: Arc<InterpreterData>, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, SpannedError>,
    {
        self.kind.with_inner(self.span, data.clone(), f).await
    }

    pub async fn modify<F, R>(
        &mut self,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, SpannedError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, SpannedError>,
    {
        self.kind.modify(self.span, data.clone(), f).await
    }

    pub async fn into_value(self, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
        self.kind.into_value(self.span, data).await
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
    pub async fn to_type(&self, data: Arc<InterpreterData>) -> Result<FslType, SpannedError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.to_type()))
            .await
    }

    pub async fn to_inner_type(&self, data: Arc<InterpreterData>) -> Result<FslType, SpannedError> {
        self.with(data.clone(), async |value| {
            value
                .to_inner_type(data.clone())
                .await
                .span_err(self.span, data)
        })
        .await
    }

    pub async fn is_type(
        &self,
        fsl_type: FslType,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.is_type(fsl_type)))
            .await
    }

    pub async fn is_var(&self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.is_var()))
            .await
    }

    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self.kind {
            ArgumentKind::Value(value) => value.mem_size().await,
            ArgumentKind::Accessor(_) => Ok(0),
        }
    }

    pub async fn equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |l_value| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value| {
                        l_value
                            .equal(r_value, data.clone())
                            .span_err(self.span, data)
                    })
                    .await
            })
            .await
    }

    pub async fn soft_equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |l_value| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value| {
                        l_value
                            .soft_equal(r_value, data.clone())
                            .span_err(self.span, data)
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
            .span_err(self.span, data.clone())
    }

    pub async fn to_usize(self, data: Arc<InterpreterData>) -> Result<usize, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_usize(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_float(self, data: Arc<InterpreterData>) -> Result<f64, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_float(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_bool(self, data: Arc<InterpreterData>) -> Result<bool, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_bool(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_var(self, data: Arc<InterpreterData>) -> Result<SourceStr, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_var(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_text(self, data: Arc<InterpreterData>) -> Result<SourceStr, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_text(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_list(self, data: Arc<InterpreterData>) -> Result<List, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_list(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_map(self, data: Arc<InterpreterData>) -> Result<Map, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_map(data.clone())
            .await
            .span_err(self.span, data.clone())
    }

    pub async fn to_number(self, data: Arc<InterpreterData>) -> Result<Argument, SpannedError> {
        let number = self
            .kind
            .into_value(self.span, data.clone())
            .await?
            .to_number(data.clone())
            .await;
        let number = number.map(|v| Self::new(v, self.span));

        number.span_err(self.span, data.clone())
    }

    pub async fn as_raw_checked(
        self,
        data: Arc<InterpreterData>,
        valid_types: &'static [FslType],
    ) -> Result<Argument, SpannedError> {
        let raw = self
            .kind
            .into_value(self.span, data.clone())
            .await?
            .as_raw_checked(valid_types, data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.span_err(self.span, data.clone())
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Argument, SpannedError> {
        let raw = self
            .kind
            .into_value(self.span, data.clone())
            .await?
            .as_raw(data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.span_err(self.span, data.clone())
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
            .span_err(self.span, data.clone())
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
            .span_err(self.span, data.clone())
    }

    pub async fn to_command(self, data: Arc<InterpreterData>) -> Result<Command, SpannedError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .to_command(data.clone())
            .span_err(self.span, data.clone())
    }

    pub async fn as_var_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |value| {
                value.as_var_label(data.clone()).span_err(self.span, data)
            })
            .await
    }

    pub async fn as_command_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, SpannedError> {
        self.kind
            .with(self.span, data.clone(), async |value| {
                value.as_command_label().span_err(self.span, data)
            })
            .await
    }
}
