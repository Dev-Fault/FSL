use std::{ops::Range, sync::Arc};

use async_recursion::async_recursion;

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
    pub head: Box<Argument>,
    pub body: Vec<Argument>,
}

enum PathKind {
    List {
        head: Argument,
        body: Vec<usize>,
    },
    Text {
        head: Argument,
        body: Vec<usize>,
    },
    Map {
        head: Argument,
        body: Vec<SourceStr>,
    },
}

impl PathArgument {
    pub fn new(head: Argument, body: Vec<Argument>) -> Self {
        Self {
            head: Box::new(head),
            body,
        }
    }

    #[async_recursion]
    async fn to_kind(
        self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<PathKind, ExecutionError> {
        match self.head.type_of_inner(data.clone()).await? {
            FslType::Text => {
                let mut body = Vec::with_capacity(self.body.len());
                for arg in self.body {
                    let arg_span = arg.span;
                    let value = arg.into_value(data.clone()).await?;
                    let key = value
                        .as_usize(data.clone())
                        .await
                        .map_err(|e| e.to_exec(arg_span, data.clone()))?;
                    body.push(key);
                }
                Ok(PathKind::Text {
                    head: *self.head,
                    body: body,
                })
            }
            FslType::List => {
                let mut body = Vec::with_capacity(self.body.len());
                for arg in self.body {
                    let arg_span = arg.span;
                    let value = arg.into_value(data.clone()).await?;
                    let key = value
                        .as_usize(data.clone())
                        .await
                        .map_err(|e| e.to_exec(arg_span, data.clone()))?;
                    body.push(key);
                }
                Ok(PathKind::List {
                    head: *self.head,
                    body: body,
                })
            }
            FslType::Map => {
                let mut body = Vec::with_capacity(self.body.len());
                for arg in self.body {
                    let arg_span = arg.span;
                    let value = arg.into_value(data.clone()).await?;
                    let key = match value.as_type() {
                        FslType::Var => value
                            .as_var_label(data.clone())
                            .await
                            .map_err(|e| e.to_exec(arg_span, data.clone()))?,
                        _ => value
                            .as_text(data.clone())
                            .await
                            .map_err(|e| e.to_exec(arg_span, data.clone()))?,
                    };
                    body.push(key);
                }
                Ok(PathKind::Map {
                    head: *self.head,
                    body: body,
                })
            }
            FslType::Command => {
                let value = self
                    .head
                    .as_command(data.clone())
                    .await?
                    .execute(data.clone())
                    .await?;
                let arg = Argument::new(value, span);
                let path = PathArgument::new(arg, self.body);
                path.to_kind(span, data).await
            }
            _ => Err(RuntimeError::NotIndexable.to_exec(span, data)),
        }
    }
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
    #[async_recursion]
    pub async fn into_value(
        self,
        span: Span,
        data: Arc<InterpreterData>,
    ) -> Result<Value, ExecutionError> {
        match self {
            ArgumentKind::Value(value) => Ok(value),
            ArgumentKind::Path(path) => match path.to_kind(span, data.clone()).await? {
                PathKind::Text { head, body } => {
                    let head = head.as_text(data.clone()).await?;
                    match &head.chars().nth(*body.get(0).unwrap()) {
                        Some(char) => Ok(Value::from(*char)),
                        None => Err(RuntimeError::IndexOutOfBounds.to_exec(span, data)),
                    }
                }
                PathKind::List { head, body } => {
                    let head = head.as_list(data.clone()).await?;
                    Ok(head.get_nested_clone(&body, data, span)?)
                }
                PathKind::Map { head, body } => {
                    let head = head.as_map(data.clone()).await?;
                    Ok(head.get_nested_clone(&body, data.clone(), span)?)
                }
            },
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
            ArgumentKind::Path(path) => match path.clone().to_kind(span, data.clone()).await? {
                PathKind::Text { head, body } => {
                    let i = *body.get(0).unwrap();
                    match head.get_var_label(data.clone()).await {
                        Ok(var) => {
                            let data_clone = data.clone();
                            let vars = data_clone.vars.read().await;
                            vars.modify(&var, span, data.clone(), async |value| match value {
                                Value::Text(text) => {
                                    let mut string = text.to_string();
                                    match string.chars().nth(i) {
                                        Some(char) => {
                                            let mut ch = Value::from(char);
                                            let result = f(&mut ch).await?;
                                            let replacement = ch
                                                .clone()
                                                .as_text(data.clone())
                                                .await
                                                .map_err(|e| e.to_exec(span, data.clone()))?;
                                            string.replace_range(
                                                i..i + char.len_utf8(),
                                                &replacement,
                                            );
                                            *value = Value::Text(SourceStr::Owned(string));
                                            Ok(result)
                                        }
                                        None => {
                                            Err(RuntimeError::IndexOutOfBounds.to_exec(span, data))
                                        }
                                    }
                                }
                                _ => Err(value
                                    .conversion_err_to_types(&[FslType::Text])
                                    .to_exec(span, data)),
                            })
                            .await
                        }
                        Err(_) => {
                            todo!()
                        }
                    }
                }
                PathKind::List { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::List(list) => {
                                let value = list.get_nested_mut(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::List])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let mut head = head.as_list(data.clone()).await?;
                        let value = head.get_nested_mut(&body, data, span)?;
                        f(value).await
                    }
                },
                PathKind::Map { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::Map(map) => {
                                let value = map.get_nested_mut(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Map])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let mut head = head.as_map(data.clone()).await?;
                        let value = head.get_nested_mut(&body, data, span)?;
                        f(value).await
                    }
                },
            },
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
            ArgumentKind::Path(path) => match path.clone().to_kind(span, data.clone()).await? {
                PathKind::Text { head, body } => {
                    let head = head.as_text(data.clone()).await?;
                    match &head.chars().nth(*body.get(0).unwrap()) {
                        Some(char) => {
                            let value = Value::from(*char);
                            f(&value).await
                        }
                        None => Err(RuntimeError::IndexOutOfBounds.to_exec(span, data)),
                    }
                }
                PathKind::List { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::List(list) => {
                                let value = list.get_nested(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::List])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let head = head.as_list(data.clone()).await?;
                        let value = head.get_nested(&body, data, span)?;
                        f(value).await
                    }
                },
                PathKind::Map { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::Map(map) => {
                                let value = map.get_nested(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Map])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let head = head.as_map(data.clone()).await?;
                        let value = head.get_nested(&body, data, span)?;
                        f(value).await
                    }
                },
            },
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
            ArgumentKind::Path(path) => match path.clone().to_kind(span, data.clone()).await? {
                PathKind::Text { head, body } => {
                    let head = head.as_text(data.clone()).await?;
                    match &head.chars().nth(*body.get(0).unwrap()) {
                        Some(char) => {
                            let value = Value::from(*char);
                            f(&value).await
                        }
                        None => Err(RuntimeError::IndexOutOfBounds.to_exec(span, data)),
                    }
                }
                PathKind::List { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::List(list) => {
                                let value = list.get_nested(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::List])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let head = head.as_list(data.clone()).await?;
                        let value = head.get_nested(&body, data, span)?;
                        f(value).await
                    }
                },
                PathKind::Map { head, body } => match head.get_var_label(data.clone()).await {
                    Ok(var) => {
                        let data_clone = data.clone();
                        let vars = data_clone.vars.read().await;
                        vars.modify(&var, span, data.clone(), async |value| match value {
                            Value::Map(map) => {
                                let value = map.get_nested(&body, data, span)?;
                                f(value).await
                            }
                            _ => Err(value
                                .conversion_err_to_types(&[FslType::Map])
                                .to_exec(span, data)),
                        })
                        .await
                    }
                    Err(_) => {
                        let head = head.as_map(data.clone()).await?;
                        let value = head.get_nested(&body, data, span)?;
                        f(value).await
                    }
                },
            },
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

    pub fn new_path(path: PathArgument, span: Span) -> Self {
        Self {
            kind: ArgumentKind::Path(path),
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

    pub async fn into_value(self, data: Arc<InterpreterData>) -> Result<Value, ExecutionError> {
        self.kind.into_value(self.span, data).await
    }

    pub async fn take_value(
        &mut self,
        data: Arc<InterpreterData>,
    ) -> Result<Value, ExecutionError> {
        let kind = std::mem::take(&mut self.kind);
        kind.into_value(self.span, data).await
    }

    pub fn replace_value(&mut self, value: Value) {
        self.kind = ArgumentKind::Value(value);
    }
}

impl Argument {
    pub async fn type_of(&self, data: Arc<InterpreterData>) -> Result<FslType, ExecutionError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.as_type()))
            .await
    }

    pub async fn type_of_inner(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<FslType, ExecutionError> {
        self.with(data.clone(), async |value| {
            value
                .type_of_inner(data.clone())
                .await
                .map_err(|e| e.to_exec(self.span, data))
        })
        .await
    }

    pub async fn is_type(
        &self,
        fsl_type: FslType,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.is_type(fsl_type)))
            .await
    }

    pub async fn is_var(&self, data: Arc<InterpreterData>) -> Result<bool, ExecutionError> {
        self.kind
            .with(self.span, data, async |value| Ok(value.is_var()))
            .await
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
            .await?
            .as_int(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_usize(self, data: Arc<InterpreterData>) -> Result<usize, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_usize(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_float(self, data: Arc<InterpreterData>) -> Result<f64, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_float(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_bool(self, data: Arc<InterpreterData>) -> Result<bool, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
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
            .await?
            .as_var_label(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_text(self, data: Arc<InterpreterData>) -> Result<SourceStr, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_text(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_list(self, data: Arc<InterpreterData>) -> Result<List, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_list(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_map(self, data: Arc<InterpreterData>) -> Result<Map, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_map(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_number(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let number = self
            .kind
            .into_value(self.span, data.clone())
            .await?
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
            .await?
            .as_raw_checked(valid_types, data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let raw = self
            .kind
            .into_value(self.span, data.clone())
            .await?
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
            .await?
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
            .await?
            .as_map_key(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_command(self, data: Arc<InterpreterData>) -> Result<Command, ExecutionError> {
        self.kind
            .into_value(self.span, data.clone())
            .await?
            .as_command(data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    #[async_recursion]
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
