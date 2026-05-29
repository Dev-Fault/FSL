use std::{
    borrow::Cow,
    collections::HashMap,
    mem,
    sync::{Arc, Mutex},
};

use crate::{
    data::Limiter,
    error::RuntimeError,
    types::{
        FslType,
        value::{FslValue, Value, ValueError},
    },
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

#[derive(Debug, Clone)]
pub enum Var<'c> {
    Const(Value<'c>),
    Literal(Value<'c>),
}

impl<'c> Var<'c> {
    pub fn take(self) -> Value<'c> {
        match self {
            Var::Const(_) => panic!("cannot take const var"),
            Var::Literal(value) => value,
        }
    }

    pub fn inner(&self) -> &Value<'c> {
        match self {
            Var::Const(value) => value,
            Var::Literal(value) => value,
        }
    }

    pub fn inner_mut(&mut self, label: &str) -> Result<&mut Value<'c>, RuntimeError> {
        match self {
            Var::Const(_) => Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            }),
            Var::Literal(value) => Ok(value),
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Var::Const(_) => true,
            Var::Literal(_) => false,
        }
    }

    pub fn type_of(&self) -> FslType {
        match self {
            Var::Const(value) => value.as_type(),
            Var::Literal(value) => value.as_type(),
        }
    }
}

impl<'c> Default for Var<'c> {
    fn default() -> Self {
        Self::Literal(Value::None)
    }
}

pub struct VarStore<'c> {
    data: Vec<HashMap<Cow<'c, str>, Var<'c>>>,
    limiter: Arc<Limiter>,
}

impl<'c> VarStore<'c> {
    pub fn push(&mut self) {
        self.data.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.data.pop();
    }

    pub fn get_clone(&self, label: &str) -> Result<Value<'c>, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.get(label) {
                return Ok(var.inner().clone());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get(&self, label: &str) -> Result<&Value<'c>, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.get(label) {
                return Ok(var.inner());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get_type(&self, label: &str) -> Result<FslType, RuntimeError> {
        for map in self.data.iter().rev() {
            if let Some(var) = map.get(label) {
                return Ok(var.type_of());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get_mut(&mut self, label: &str) -> Result<&mut Value<'c>, RuntimeError> {
        for map in self.data.iter_mut().rev() {
            if let Some(var) = map.get_mut(label) {
                return var.inner_mut(label);
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn store(&mut self, label: &Cow<'c, str>, new_var: Var<'c>) -> Result<(), RuntimeError> {
        for map in self.data.iter_mut().rev() {
            if let Some(var) = map.get_mut(label) {
                if var.is_const() {
                    return Err(RuntimeError::AttemptToOverwriteConst {
                        label: label.to_string(),
                    });
                } else {
                    *var = new_var;
                    return Ok(());
                }
            }
        }
        self.data.last_mut().unwrap().insert(label.clone(), new_var);
        Ok(())
    }

    pub fn insert(&mut self, label: &Cow<'c, str>, var: Var<'c>) -> Result<(), RuntimeError> {
        let map = self.data.last_mut().unwrap();
        if let Some(Var::Const(_)) = map.get(label) {
            Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            })
        } else {
            map.insert(label.clone(), var);
            Ok(())
        }
    }

    pub fn replace(
        &mut self,
        label: &Cow<'c, str>,
        replacement: Value<'c>,
    ) -> Result<(), RuntimeError> {
        let map = self.data.last_mut().unwrap();
        match map.get_mut(label) {
            Some(var) => match var {
                Var::Const(_) => Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                }),
                Var::Literal(value) => {
                    *value = replacement;
                    Ok(())
                }
            },
            None => Err(RuntimeError::NonExistantVar {
                label: label.to_string(),
            }),
        }
    }

    pub fn remove(&mut self, label: &Cow<'c, str>) -> Result<Option<Value<'c>>, RuntimeError> {
        let map = self.data.last_mut().unwrap();
        if let Some(Var::Const(_)) = map.get(label) {
            Err(RuntimeError::AttemptToOverwriteConst {
                label: label.to_string(),
            })
        } else {
            let removed = map.remove(label);
            Ok(removed.map(|var| var.take()))
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct VarEntry<'c> {
    pub value: Value<'c>,
    pub constant: bool,
}

impl<'c> VarEntry<'c> {
    fn new(value: Value) -> VarEntry {
        VarEntry {
            value: value,
            constant: false,
        }
    }

    fn new_const(value: Value) -> VarEntry {
        VarEntry {
            value: value,
            constant: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarMap<'c> {
    map: Arc<Mutex<HashMap<Cow<'c, str>, VarEntry<'c>>>>,
}

impl<'c> VarMap<'c> {
    pub fn new() -> Self {
        Self {
            map: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    fn insert_with<F>(
        &self,
        label: Cow<'c, str>,
        value: Value<'c>,
        make_entry: F,
    ) -> Result<(), RuntimeError>
    where
        F: FnOnce(Value<'c>) -> VarEntry<'c>,
    {
        match value {
            Value::Var(_) => Err(RuntimeError::InvalidVarValue {
                invalid_value: FslType::Var,
            }),
            Value::Command(_) => Err(RuntimeError::InvalidVarValue {
                invalid_value: FslType::Command,
            }),
            _ => {
                let lock = self.map.lock();
                let mut map = lock.unwrap();
                if map.get(&label).is_some_and(|entry| entry.constant) {
                    return Err(RuntimeError::AttemptToOverwriteConst {
                        label: label.to_string(),
                    });
                }
                map.insert(label, make_entry(value));
                Ok(())
            }
        }
    }

    pub fn insert(&self, label: Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        self.insert_with(label, value, VarEntry::new)
    }

    pub fn insert_const(&self, label: Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        self.insert_with(label, value, VarEntry::new_const)
    }

    pub fn remove_value(&self, label: &str) -> Result<Option<Value<'c>>, RuntimeError> {
        let lock = self.map.lock();
        let mut map = lock.unwrap();

        if map.get(label).is_some_and(|entry| entry.constant) {
            return Err(RuntimeError::AttemptToTakeConst {
                label: label.to_string(),
            });
        }
        Ok(map.remove(label).map(|entry| entry.value))
    }

    pub fn get_cloned(&self, label: &str) -> Result<Value<'c>, ValueError<'c>> {
        let entry = self.map.lock().unwrap().get(label).cloned();

        match entry {
            Some(entry) => {
                if entry.value.is_type(FslType::Var) {
                    return self.get_cloned(&entry.value.get_var_label()?);
                } else {
                    Ok(entry.value)
                }
            }
            None => Err(RuntimeError::NonExistantVar {
                label: label.to_string(),
            }
            .into()),
        }
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, RuntimeError> {
        let mut vars = self.map.lock().unwrap();
        let entry = vars.get_mut(label);

        if let Some(entry) = entry {
            if entry.constant {
                return Err(RuntimeError::AttemptToOverwriteConst {
                    label: label.to_string(),
                });
            }
            let var_entry = mem::take(entry);
            Ok(var_entry)
        } else {
            Err(RuntimeError::NonExistantVar {
                label: label.to_string(),
            })
        }
    }

    pub fn insert_entry(&self, label: Cow<'c, str>, var_entry: VarEntry<'c>) {
        let mut vars = self.map.lock().unwrap();
        vars.insert(label, var_entry);
    }

    pub fn contains(&self, label: &str) -> bool {
        let map = self.map.lock().unwrap();
        map.contains_key(label)
    }

    pub fn deallocate_var(&self, label: &Cow<'c, str>, limiter: Arc<Limiter>) {
        let map = self.map.lock().unwrap();
        if let Some(var) = map.get(label) {
            limiter.deallocate(&var.value)
        }
    }

    pub fn total_mem_size(&self) -> usize {
        let map = self.map.lock().unwrap();
        map.values()
            .map(|entry| entry.value.mem_size().unwrap_or(0))
            .sum()
    }

    pub fn get_type(&self, label: &str) -> FslType {
        let lock = self.map.lock();
        let map = lock.unwrap();
        let entry = map.get(label);

        match entry {
            Some(entry) => {
                let value = &entry.value;
                if value.is_type(FslType::Var) {
                    match value.get_var_label() {
                        Ok(label) => {
                            return self.get_type(&label);
                        }
                        Err(_) => FslType::None,
                    }
                } else {
                    value.as_type()
                }
            }
            None => FslType::None,
        }
    }

    pub fn mem_size(&self) -> usize {
        let map = self.map.lock().unwrap();
        let mut size = 0;
        for var in map.values() {
            size += var.value.mem_size().unwrap_or(0);
        }
        size
    }

    pub fn deallocate(&self, limiter: Arc<Limiter>) {
        let map = self.map.lock().unwrap();
        for var in map.values() {
            limiter.deallocate(&var.value);
        }
    }
}

#[derive(Debug)]
pub struct VarStack<'c> {
    stack: Mutex<Vec<VarMap<'c>>>,
    limiter: Arc<Limiter>,
}

impl<'c> Clone for VarStack<'c> {
    fn clone(&self) -> Self {
        Self {
            stack: Mutex::new(self.stack.lock().unwrap().clone()),
            limiter: self.limiter.clone(),
        }
    }
}

impl<'c> Default for VarStack<'c> {
    fn default() -> Self {
        Self {
            stack: Mutex::new(vec![VarMap::new()]),
            limiter: Arc::new(Limiter::NoLimit),
        }
    }
}

impl<'c> VarStack<'c> {
    pub fn new(limit: Arc<Limiter>) -> VarStack<'c> {
        VarStack {
            stack: Mutex::new(vec![(VarMap::new())]),
            limiter: limit.clone(),
        }
    }

    pub fn len(&self) -> usize {
        self.stack.lock().unwrap().len()
    }

    pub fn push(&self) {
        let mut stack = self.stack.lock().unwrap();
        stack.push(VarMap::new());
    }

    pub fn pop(&self) {
        let mut stack = self.stack.lock().unwrap();
        if let Some(map) = stack.pop() {
            map.deallocate(self.limiter.clone());
        }
    }

    fn find_stack_with_var(&self, label: &Cow<'c, str>) -> Result<VarMap<'c>, RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(&label) {
                return Ok(map.clone());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get_last_stack(&self) -> VarMap<'c> {
        let stack = self.stack.lock().unwrap();
        stack.last().expect("global stack must exist").clone()
    }

    /// Inserts var local first
    pub fn insert(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let stack = self.get_last_stack();
        stack.deallocate_var(label, self.limiter.clone());
        self.limiter.allocate(&value)?;
        stack.insert(label.clone(), value)?;
        Ok(())
    }

    /// Updates var local first, throws error if var doesn't exist
    pub fn update_var(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let stack = self.find_stack_with_var(label)?;
        stack.deallocate_var(label, self.limiter.clone());
        self.limiter.allocate(&value)?;
        stack.insert(label.clone(), value)?;
        Ok(())
    }

    /// Inserts new var or updates var if it already exists local first
    pub fn create_or_update(
        &self,
        label: &Cow<'c, str>,
        value: Value<'c>,
    ) -> Result<(), RuntimeError> {
        match self.find_stack_with_var(label) {
            Ok(stack) => {
                stack.deallocate_var(label, self.limiter.clone());
                self.limiter.allocate(&value)?;
                stack.insert(label.clone(), value)?;
            }
            Err(_) => {
                let stack = self.get_last_stack();
                stack.deallocate_var(label, self.limiter.clone());
                self.limiter.allocate(&value)?;
                stack.insert(label.clone(), value)?;
            }
        }
        Ok(())
    }

    /// Inserts const var local first
    pub fn insert_const(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let locals = self.get_last_stack();
        self.limiter.allocate(&value)?;
        locals.insert_const(label.clone(), value)
    }

    /// Removes var local first and returns removed value
    pub fn remove(&self, label: &Cow<'c, str>) -> Result<Option<Value<'c>>, RuntimeError> {
        let stack = self.find_stack_with_var(label)?;
        stack.deallocate_var(label, self.limiter.clone());
        let return_value = stack.remove_value(&label)?;
        Ok(return_value)
    }

    pub fn get(&self, label: &str) -> Result<Value<'c>, ValueError<'c>> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(label) {
                return map.get_cloned(label);
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        }
        .into())
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(label) {
                return map.take_entry(label);
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn insert_entry(
        &self,
        label: Cow<'c, str>,
        entry: VarEntry<'c>,
    ) -> Result<(), RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(&label) {
                map.insert_entry(label, entry);
                return Ok(());
            }
        }
        Err(RuntimeError::NonExistantVar {
            label: label.to_string(),
        })
    }

    pub fn get_type(&self, label: &str) -> FslType {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(label) {
                return map.get_type(label);
            }
        }
        FslType::None
    }
}
