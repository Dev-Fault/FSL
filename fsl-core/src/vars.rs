use std::{borrow::Cow, collections::HashMap, ops::Deref, sync::Arc};

use tokio::sync::RwLock;

use crate::{
    data::Limiter,
    error::RuntimeError,
    types::{
        FslType,
        value::{FslValue, Value},
    },
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

#[derive(Debug, Clone)]
pub enum Var<'c> {
    Const(Value<'c>),
    Mut(Value<'c>),
}

impl<'c> Var<'c> {
    pub fn take(self) -> Value<'c> {
        match self {
            Var::Const(_) => panic!("cannot take const var"),
            Var::Mut(value) => value,
        }
    }

    pub fn inner_mut(&mut self, label: &str) -> Result<&mut Value<'c>, RuntimeError> {
        match self {
            Var::Const(_) => Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            }),
            Var::Mut(value) => Ok(value),
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

impl<'c> Deref for Var<'c> {
    type Target = Value<'c>;

    fn deref(&self) -> &Self::Target {
        match self {
            Var::Const(value) => value,
            Var::Mut(value) => value,
        }
    }
}

impl<'c> Default for Var<'c> {
    fn default() -> Self {
        Self::Mut(Value::None)
    }
}

type VarMap<'c> = HashMap<Cow<'c, str>, Var<'c>>;

#[derive(Debug, Clone)]
pub struct VarStore<'c> {
    data: Vec<Arc<RwLock<VarMap<'c>>>>,
    limiter: Arc<Limiter>,
}

impl<'c> Default for VarStore<'c> {
    fn default() -> Self {
        Self {
            data: vec![Arc::new(RwLock::new(VarMap::new()))],
            limiter: Default::default(),
        }
    }
}

impl<'c> VarStore<'c> {
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
                self.limiter.deallocate(var);
            }
        }
        self.data.pop();
    }

    pub async fn with_mut<F, R>(&self, label: &str, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&mut Value<'c>) -> R,
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

    pub async fn get_clone(&self, label: &str) -> Result<Value<'c>, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().await.get(label) {
                return Ok(var.deref().clone());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn get_type(&self, label: &str) -> Result<FslType, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.read().await.get(label) {
                return Ok(var.type_of());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub async fn store(
        &mut self,
        label: &Cow<'c, str>,
        new_var: Var<'c>,
    ) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            if let Some(var) = map.write().await.get_mut(label) {
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
            .await
            .insert(label.clone(), new_var);
        Ok(())
    }

    pub async fn insert(&mut self, label: &Cow<'c, str>, var: Var<'c>) -> Result<(), RuntimeError> {
        let mut map = self.data.last_mut().unwrap().write().await;
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

    pub async fn replace(
        &mut self,
        label: &Cow<'c, str>,
        replacement: Value<'c>,
    ) -> Result<(), RuntimeError> {
        let mut map = self.data.last_mut().unwrap().write().await;
        match map.get_mut(label) {
            Some(var) => match var {
                Var::Const(_) => Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                }),
                Var::Mut(value) => {
                    self.limiter.deallocate(value);
                    self.limiter.allocate(&replacement)?;
                    *value = replacement;
                    Ok(())
                }
            },
            None => Err(RuntimeError::NonExistantVar {
                label: label.to_string(),
            }),
        }
    }

    pub async fn remove(
        &mut self,
        label: &Cow<'c, str>,
    ) -> Result<Option<Value<'c>>, RuntimeError> {
        let mut map = self.data.last_mut().unwrap().write().await;
        if let Some(Var::Const(_)) = map.get(label) {
            Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            })
        } else {
            match map.remove(label) {
                Some(var) => {
                    self.limiter.deallocate(&var);
                    Ok(Some(var.take()))
                }
                None => Ok(None),
            }
        }
    }

    pub async fn take(&mut self, label: &Cow<'c, str>) -> Result<Value<'c>, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            let mut map = map.write().await;
            if let Some(Var::Const(_)) = map.get(label) {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            } else {
                match map.get_mut(label) {
                    Some(var) => {
                        self.limiter.deallocate(&var);
                        return Ok(std::mem::take(var.inner_mut(label)?));
                    }
                    None => {}
                }
            }
        }
        panic!("cannot call take on non existant var")
    }
}
