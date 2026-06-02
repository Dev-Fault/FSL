use std::{collections::HashMap, ops::Deref, sync::Arc};

use tokio::sync::RwLock;

use crate::{
    data::Limiter,
    error::RuntimeError,
    source_str::SourceStr,
    types::{FslType, value::Value},
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

    pub fn type_of(&self) -> FslType {
        match self {
            Var::Const(value) => value.as_type(),
            Var::Mut(value) => value.as_type(),
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

    pub async fn pop(&mut self) {
        if matches!(*self.limiter, Limiter::Limit(_)) {
            for var in self.data.last().unwrap().read().await.values() {
                self.limiter.deallocate(var).await;
            }
        }
        self.data.pop();
    }

    pub async fn modify<F, R>(&self, label: &SourceStr, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&mut Value) -> R,
    {
        for map in self.data.iter().rev() {
            let mut map = map.write().await;
            if let Some(var) = map.get_mut(label) {
                return Ok(f(var.inner_mut(label)?));
            }
        }

        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn with<F, R>(&self, label: &SourceStr, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&Value) -> R,
    {
        for map in self.data.iter().rev() {
            let mut map = map.write().await;
            if let Some(var) = map.get_mut(label) {
                return Ok(f(var.inner()));
            }
        }

        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn get_clone(&self, label: &SourceStr) -> Result<Value, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().await.get(label) {
                return Ok(var.deref().clone());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn get_type(&self, label: &SourceStr) -> Result<FslType, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().await.get(label) {
                return Ok(var.type_of());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn store(&mut self, label: &SourceStr, new_var: Var) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            if let Some(var) = map.write().await.get_mut(label) {
                if var.is_const() {
                    return Err(RuntimeError::AttemptToOverwriteConst {
                        label: label.to_string(),
                    });
                } else {
                    self.limiter.deallocate(var).await;
                    self.limiter.allocate(&new_var).await?;
                    *var = new_var;
                    return Ok(());
                }
            }
        }
        self.limiter.allocate(&new_var).await?;
        self.data
            .last_mut()
            .unwrap()
            .write()
            .await
            .insert(label.clone(), new_var);
        Ok(())
    }

    pub async fn insert(&mut self, label: &SourceStr, var: Var) -> Result<(), RuntimeError> {
        let mut map = self.data.last_mut().unwrap().write().await;
        if let Some(Var::Const(_)) = map.get(label) {
            Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            })
        } else {
            if let Some(old_var) = map.get(label) {
                self.limiter.deallocate(old_var).await;
            }
            self.limiter.allocate(&var).await?;
            map.insert(label.clone(), var);
            Ok(())
        }
    }

    pub async fn replace(
        &mut self,
        label: &SourceStr,
        replacement: Value,
    ) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write().await;
            if let Some(var) = map.get_mut(label) {
                match var {
                    Var::Const(_) => {
                        return Err(RuntimeError::AttemptToOverwriteConst {
                            label: label.to_string(),
                        });
                    }
                    Var::Mut(value) => {
                        self.limiter.deallocate(value).await;
                        self.limiter.allocate(&replacement).await?;
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

    pub async fn remove(&mut self, label: &SourceStr) -> Result<Option<Value>, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write().await;
            if let Some(Var::Const(_)) = map.get(label) {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            } else if let Some(var) = map.remove(label) {
                self.limiter.deallocate(&var).await;
                return Ok(Some(var.take()));
            }
        }
        Ok(None)
    }

    pub async fn take(&mut self, label: &SourceStr) -> Result<Value, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write().await;
            if let Some(Var::Const(_)) = map.get(label) {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            } else {
                if let Some(var) = map.get_mut(label) {
                    self.limiter.deallocate(var).await;
                    return Ok(std::mem::take(var.inner_mut(label)?));
                }
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }
}
