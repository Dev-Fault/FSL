use std::{ops::Range, sync::Arc};

use crate::{
    data::InterpreterData,
    error::{ExecutionError, RuntimeError, ToExecutionError},
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
pub struct PathArgument {
    pub head: SourceStr,
    pub body: Vec<SourceStr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentKind {
    Value(Value),
    Path(PathArgument),
}

impl Default for ArgumentKind {
    fn default() -> Self {
        Self::Value(Value::None)
    }
}

impl ArgumentKind {
    pub async fn into_value(self, span: Span, data: Arc<InterpreterData>) -> Value {
        match self {
            ArgumentKind::Value(value) => value,
            ArgumentKind::Path(path) => {
                let vars = data.vars.read().await;

                vars.modify(
                    &path.head,
                    span,
                    data.clone(),
                    async |map_var| match map_var {
                        Value::Map(map) => {
                            Ok(map.get_nested_clone(&path.body, data.clone(), span)?)
                        }
                        _ => Err(RuntimeError::NotAMap {
                            key: path.head.to_string(),
                        }
                        .to_exec(span, data.clone())),
                    },
                )
                .await
                .unwrap()
            }
        }
    }

    pub async fn modify<F, R>(
        &mut self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, ExecutionError>,
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
            ArgumentKind::Path(path) => {
                let data_clone = data.clone();
                let vars = data_clone.vars.write().await;
                vars.modify(
                    &path.head,
                    span,
                    data.clone(),
                    async |map_var| match map_var {
                        Value::Map(map) => {
                            let value = map.get_nested_mut(&path.body, data, span)?;
                            f(value).await
                        }
                        _ => Err(RuntimeError::NotAMap {
                            key: path.head.to_string(),
                        }
                        .to_exec(span, data)),
                    },
                )
                .await
            }
        }
    }

    pub async fn with_inner<F, R>(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, ExecutionError>,
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
            ArgumentKind::Path(path) => {
                let data_clone = data.clone();
                let vars = data_clone.vars.write().await;
                vars.modify(
                    &path.head,
                    span,
                    data.clone(),
                    async |map_var| match map_var {
                        Value::Map(map) => {
                            let value = map.get_nested(&path.body, data, span)?;
                            f(value).await
                        }
                        _ => Err(RuntimeError::NotAMap {
                            key: path.head.to_string(),
                        }
                        .to_exec(span, data)),
                    },
                )
                .await
            }
        }
    }

    pub async fn with<F, R>(
        &self,
        span: Span,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, ExecutionError>,
    {
        match self {
            ArgumentKind::Value(value) => f(value).await,
            ArgumentKind::Path(path) => {
                let data_clone = data.clone();
                let vars = data_clone.vars.write().await;
                vars.modify(
                    &path.head,
                    span,
                    data.clone(),
                    async |map_var| match map_var {
                        Value::Map(map) => {
                            let value = map.get_nested(&path.body, data, span)?;
                            f(value).await
                        }
                        _ => Err(RuntimeError::NotAMap {
                            key: path.head.to_string(),
                        }
                        .to_exec(span, data)),
                    },
                )
                .await
            }
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

    pub async fn with<F, R>(&self, data: Arc<InterpreterData>, f: F) -> Result<R, ExecutionError>
    where
        F: for<'a> AsyncFnOnce(&'a Value) -> Result<R, ExecutionError>,
    {
        self.kind.with_inner(self.span, data.clone(), f).await
    }

    pub async fn modify<F, R>(
        &mut self,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: for<'a> AsyncFnOnce(&'a mut Value) -> Result<R, ExecutionError>,
    {
        self.kind.modify(self.span, data.clone(), f).await
    }

    pub async fn into_value(self, data: Arc<InterpreterData>) -> Value {
        self.kind.into_value(self.span, data).await
    }

    pub async fn take_value(&mut self, data: Arc<InterpreterData>) -> Value {
        let kind = std::mem::take(&mut self.kind);
        kind.into_value(self.span, data).await
    }

    pub fn replace_value(&mut self, value: Value) {
        self.kind = ArgumentKind::Value(value);
    }
}

impl Argument {
    pub async fn type_of(&self, data: Arc<InterpreterData>) -> FslType {
        self.kind
            .with(self.span, data, async |value| Ok(value.as_type()))
            .await
            .unwrap()
    }

    pub async fn type_of_inner(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<FslType, ExecutionError> {
        self.with(data.clone(), async |value| {
            value
                .as_literal_type(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span, data))
        })
        .await
    }

    pub async fn is_type(&self, fsl_type: FslType, data: Arc<InterpreterData>) -> bool {
        self.kind
            .with(self.span, data, async |value| Ok(value.is_type(fsl_type)))
            .await
            .unwrap()
    }

    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self.kind {
            ArgumentKind::Value(value) => value.mem_size().await,
            ArgumentKind::Path(_) => Ok(0),
        }
    }

    pub async fn equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        // TODO: Might deadlock if comparing two of the same map values, test later
        self.kind
            .with(self.span, data.clone(), async |l_value| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value| {
                        l_value
                            .equal(r_value, data.clone())
                            .map_err(|e| e.to_exec(self.span, data))
                    })
                    .await
            })
            .await
    }

    pub async fn soft_equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        self.kind
            .with(self.span, data.clone(), async |l_value| {
                other
                    .kind
                    .with(other.span, data.clone(), async |r_value| {
                        l_value
                            .soft_equal(r_value, data.clone())
                            .map_err(|e| e.to_exec(self.span, data))
                    })
                    .await
            })
            .await
    }

    pub async fn as_int(self, data: Arc<InterpreterData>) -> Result<i64, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_int(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_usize(self, data: Arc<InterpreterData>) -> Result<usize, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_usize(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_float(self, data: Arc<InterpreterData>) -> Result<f64, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_float(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_bool(self, data: Arc<InterpreterData>) -> Result<bool, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_bool(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_var_label(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_var_label(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_text(self, data: Arc<InterpreterData>) -> Result<SourceStr, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_text(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_list(self, data: Arc<InterpreterData>) -> Result<List, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_list(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_map(self, data: Arc<InterpreterData>) -> Result<Map, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_map(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_number(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let number = self
            .kind
            .into_value(self.span, data.clone())
            .await
            .as_number(data.clone())
            .await;
        let number = number.map(|v| Self::new(v, self.span));

        number.map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_raw_checked(
        self,
        data: Arc<InterpreterData>,
        valid_types: &'static [FslType],
    ) -> Result<Argument, ExecutionError> {
        let raw = self
            .kind
            .into_value(self.span, data.clone())
            .await
            .as_raw_checked(valid_types, data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let raw = self
            .kind
            .into_value(self.span, data.clone())
            .await
            .as_raw(data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_list_key(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<Vec<usize>, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_list_key(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_map_key(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<Vec<SourceStr>, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_map_key(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_command(self, data: Arc<InterpreterData>) -> Result<Command, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await
            .as_command(data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn get_var_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, ExecutionError> {
        self.kind
            .with(self.span, data.clone(), async |value| {
                value
                    .get_var_label(data.clone())
                    .map_err(|e| e.to_exec(self.span, data))
            })
            .await
    }

    pub async fn as_command_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, ExecutionError> {
        self.kind
            .with(self.span, data.clone(), async |value| {
                value
                    .as_command_label()
                    .map_err(|e| e.to_exec(self.span, data))
            })
            .await
    }
}
