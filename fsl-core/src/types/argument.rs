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
pub enum ArgumentKind {
    Value(Value),
    Path(Vec<SourceStr>),
}

impl Default for ArgumentKind {
    fn default() -> Self {
        Self::Value(Value::None)
    }
}

impl ArgumentKind {
    pub async fn into_value(self, data: Arc<InterpreterData>) -> Value {
        match self {
            ArgumentKind::Value(value) => value,
            ArgumentKind::Path(source_strs) => todo!(),
        }
    }

    pub async fn as_value(&self, data: Arc<InterpreterData>) -> &Value {
        match self {
            ArgumentKind::Value(value) => value,
            ArgumentKind::Path(source_strs) => todo!(),
        }
    }

    pub async fn as_value_mut(&mut self, data: Arc<InterpreterData>) -> &mut Value {
        match self {
            ArgumentKind::Value(value) => value,
            ArgumentKind::Path(source_strs) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    value: ArgumentKind,
    pub span: Span,
}

impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.span == other.span
    }
}

impl Argument {
    pub fn new(value: Value, span: Span) -> Self {
        Self {
            value: ArgumentKind::Value(value),
            span,
        }
    }

    pub async fn modify<F, R>(
        &mut self,
        data: Arc<InterpreterData>,
        f: F,
    ) -> Result<R, ExecutionError>
    where
        F: FnOnce(&mut Value) -> Result<R, ExecutionError>,
    {
        if let Value::Var(label) = self.value.as_value(data.clone()).await {
            let vars = data.vars.read().await;
            vars.with_mut(label, f)
                .await
                .map_err(|e| e.to_exec(self.span, data.clone()))?
        } else {
            f(self.value.as_value_mut(data.clone()).await)
        }
    }

    pub async fn value(&self, data: Arc<InterpreterData>) -> &Value {
        self.value.as_value(data).await
    }

    pub async fn value_mut(&mut self, data: Arc<InterpreterData>) -> &mut Value {
        self.value.as_value_mut(data).await
    }

    pub async fn into_value(self, data: Arc<InterpreterData>) -> Value {
        self.value.into_value(data).await
    }
}

impl Argument {
    pub async fn as_type(&self, data: Arc<InterpreterData>) -> FslType {
        self.value.as_value(data.clone()).await.as_type()
    }

    pub async fn as_literal_type(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<FslType, RuntimeError> {
        self.value
            .as_value(data.clone())
            .await
            .as_literal_type(data)
            .await
    }

    pub async fn is_type(&self, fsl_type: FslType, data: Arc<InterpreterData>) -> bool {
        self.value.as_value(data.clone()).await.is_type(fsl_type)
    }

    pub async fn mem_size(&self) -> Result<usize, RuntimeError> {
        match &self.value {
            ArgumentKind::Value(value) => value.mem_size().await,
            ArgumentKind::Path(_) => Ok(0),
        }
    }

    pub async fn equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        self.value
            .as_value(data.clone())
            .await
            .equal(other.value.as_value(data.clone()).await, data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn soft_equal(
        &self,
        other: &Argument,
        data: Arc<InterpreterData>,
    ) -> Result<bool, ExecutionError> {
        self.value
            .as_value(data.clone())
            .await
            .soft_equal(other.value.as_value(data.clone()).await, data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_int(self, data: Arc<InterpreterData>) -> Result<i64, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_int(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_usize(self, data: Arc<InterpreterData>) -> Result<usize, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_usize(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_float(self, data: Arc<InterpreterData>) -> Result<f64, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_float(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_bool(self, data: Arc<InterpreterData>) -> Result<bool, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_bool(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_var_label(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_var_label(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_text(self, data: Arc<InterpreterData>) -> Result<SourceStr, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_text(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_list(self, data: Arc<InterpreterData>) -> Result<List, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_list(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_map(self, data: Arc<InterpreterData>) -> Result<Map, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_map(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_number(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let number = self
            .value
            .into_value(data.clone())
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
            .value
            .into_value(data.clone())
            .await
            .as_raw_checked(valid_types, data.clone())
            .await;
        let raw = raw.map(|v| Self::new(v, self.span));

        raw.map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_raw(self, data: Arc<InterpreterData>) -> Result<Argument, ExecutionError> {
        let raw = self
            .value
            .into_value(data.clone())
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
        self.value
            .into_value(data.clone())
            .await
            .as_list_key(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_map_key(
        self,
        data: Arc<InterpreterData>,
    ) -> Result<Vec<SourceStr>, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_map_key(data.clone())
            .await
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_command(self, data: Arc<InterpreterData>) -> Result<Command, ExecutionError> {
        self.value
            .into_value(data.clone())
            .await
            .as_command(data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn get_var_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<SourceStr, ExecutionError> {
        self.value
            .as_value(data.clone())
            .await
            .get_var_label(data.clone())
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }

    pub async fn as_command_label(
        &self,
        data: Arc<InterpreterData>,
    ) -> Result<&str, ExecutionError> {
        self.value
            .as_value(data.clone())
            .await
            .as_command_label()
            .map_err(|e| e.to_exec(self.span, data.clone()))
    }
}
