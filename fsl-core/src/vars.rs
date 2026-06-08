use std::{collections::HashMap, ops::Deref, sync::Arc};

use parking_lot::RwLock;

use crate::{
    data::Limiter,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    source_str::SourceStr,
    span::Span,
    types::{ValueType, value::Value},
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

#[derive(Debug, Clone)]
pub enum Var {
    Const(Value),
    Mut(Value),
}

impl Var {
    pub fn take(self) -> Value {
        match self {
            Var::Const(_) => panic!("cannot take const var"),
            Var::Mut(value) => value,
        }
    }

    pub fn inner_mut(&mut self, label: &str) -> Result<&mut Value, RuntimeError> {
        match self {
            Var::Const(_) => Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            }),
            Var::Mut(value) => Ok(value),
        }
    }

    pub fn inner(&self) -> &Value {
        match self {
            Var::Const(value) => value,
            Var::Mut(value) => value,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Var::Const(_) => true,
            Var::Mut(_) => false,
        }
    }

    pub fn type_of(&self) -> ValueType {
        match self {
            Var::Const(value) => value.to_type(),
            Var::Mut(value) => value.to_type(),
        }
    }
}

impl Deref for Var {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Var::Const(value) => value,
            Var::Mut(value) => value,
        }
    }
}

impl Default for Var {
    fn default() -> Self {
        Self::Mut(Value::None)
    }
}

type VarMap = HashMap<SourceStr, Var>;

#[derive(Debug, Clone)]
pub struct VarStore {
    data: Vec<Arc<RwLock<VarMap>>>,
    limiter: Arc<Limiter>,
}

impl Default for VarStore {
    fn default() -> Self {
        Self {
            data: vec![Arc::new(RwLock::new(VarMap::new()))],
            limiter: Default::default(),
        }
    }
}

impl VarStore {
    pub fn new(limiter: Arc<Limiter>) -> Self {
        Self {
            limiter,
            ..Default::default()
        }
    }

    pub fn push(&mut self) {
        self.data.push(Arc::new(RwLock::new(VarMap::new())));
    }

    pub fn pop(&mut self) {
        if matches!(*self.limiter, Limiter::Limit(_)) {
            for var in self.data.last().unwrap().read().values() {
                self.limiter.deallocate(var);
            }
        }
        self.data.pop();
    }

    pub fn with_mut<F, R>(&self, label: &SourceStr, span: Span, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a mut Value, Span) -> Result<R, SpannedError>,
    {
        for map in self.data.iter().rev() {
            let mut map = map.write();
            if let Some(var) = map.get_mut(label) {
                return f(var.inner_mut(label).span(span)?, span);
            }
        }

        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        }
        .span(span))
    }

    pub fn with<F, R>(&self, label: &SourceStr, span: Span, f: F) -> Result<R, SpannedError>
    where
        F: for<'a> FnOnce(&'a Value, Span) -> Result<R, SpannedError>,
    {
        for map in self.data.iter().rev() {
            let map = map.read();
            if let Some(var) = map.get(label) {
                return f(var.inner(), span);
            }
        }

        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        }
        .span(span))
    }

    pub fn get_clone(&self, label: &SourceStr) -> Result<Value, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().get(label) {
                return Ok(var.deref().clone());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get_type(&self, label: &SourceStr) -> Result<ValueType, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().get(label) {
                return Ok(var.type_of());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn store(&mut self, label: &SourceStr, new_var: Var) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            if let Some(var) = map.write().get_mut(label) {
                if var.is_const() {
                    return Err(RuntimeError::AttemptToOverwriteConst {
                        label: label.to_string(),
                    });
                } else {
                    self.limiter.deallocate(var);
                    self.limiter.allocate(&new_var)?;
                    *var = new_var;
                    return Ok(());
                }
            }
        }
        self.limiter.allocate(&new_var)?;
        self.data
            .last_mut()
            .unwrap()
            .write()
            .insert(label.clone(), new_var);
        Ok(())
    }

    pub fn insert(&mut self, label: &SourceStr, var: Var) -> Result<(), RuntimeError> {
        let mut map = self.data.last_mut().unwrap().write();
        if let Some(Var::Const(_)) = map.get(label) {
            Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            })
        } else {
            if let Some(old_var) = map.get(label) {
                self.limiter.deallocate(old_var);
            }
            self.limiter.allocate(&var)?;
            map.insert(label.clone(), var);
            Ok(())
        }
    }

    pub fn replace(&mut self, label: &SourceStr, replacement: Value) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write();
            if let Some(var) = map.get_mut(label) {
                match var {
                    Var::Const(_) => {
                        return Err(RuntimeError::AttemptToOverwriteConst {
                            label: label.to_string(),
                        });
                    }
                    Var::Mut(value) => {
                        self.limiter.deallocate(value);
                        self.limiter.allocate(&replacement)?;
                        *value = replacement;
                        return Ok(());
                    }
                }
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn remove(&mut self, label: &SourceStr) -> Result<Option<Value>, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write();
            if let Some(Var::Const(_)) = map.get(label) {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            } else if let Some(var) = map.remove(label) {
                self.limiter.deallocate(&var);
                return Ok(Some(var.take()));
            }
        }
        Ok(None)
    }

    pub fn take(&mut self, label: &SourceStr) -> Result<Value, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write();
            if let Some(Var::Const(_)) = map.get(label) {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            } else {
                if let Some(var) = map.get_mut(label) {
                    self.limiter.deallocate(var);
                    return Ok(std::mem::take(var.inner_mut(label)?));
                }
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }
}
