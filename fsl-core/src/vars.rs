use std::{
    borrow::Cow,
    collections::HashMap,
    mem,
    sync::{Arc, Mutex},
};

use crate::{
    data::MemoryLimit,
    error::RuntimeError,
    types::{
        FslType,
        value::{FslValue, Value},
    },
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

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
            Value::Var(_) => {
                return Err(RuntimeError::InvalidVarValue(
                    "cannot store var in var".into(),
                ));
            }
            Value::Command(_) => {
                return Err(RuntimeError::InvalidVarValue(
                    "cannot store command in var".into(),
                ));
            }
            _ => {
                let lock = self.map.lock();
                let mut map = lock.unwrap();
                if map.get(&label).is_some_and(|entry| entry.constant) {
                    return Err(RuntimeError::AttemptToOverwriteConstant(format!(
                        "cannot overwrite constant var {}",
                        label
                    )));
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
            return Err(RuntimeError::AttemptToFreeConstant(format!(
                "cannot remove constant var {}",
                label
            )));
        }
        Ok(map.remove(label).map(|entry| entry.value))
    }

    pub fn get_cloned(&self, label: &str) -> Result<Value<'c>, RuntimeError> {
        let entry = self.map.lock().unwrap().get(label).cloned();

        if let Some(entry) = entry {
            if entry.value.is_type(FslType::Var) {
                return self.get_cloned(&entry.value.get_var_label()?);
            } else {
                Ok(entry.value)
            }
        } else {
            Err(RuntimeError::NonExistantVar(format!(
                "cannot not get value of non existant var {}",
                label
            )))
        }
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, RuntimeError> {
        let mut vars = self.map.lock().unwrap();
        let entry = vars.get_mut(label);

        if let Some(entry) = entry {
            if entry.constant {
                return Err(RuntimeError::AttemptToOverwriteConstant(format!(
                    "cannot overwrite constant var {}",
                    label
                )));
            }
            let var_entry = mem::take(entry);
            Ok(var_entry)
        } else {
            Err(RuntimeError::NonExistantVar(format!(
                "cannot not get value of non existant var {}",
                label
            )))
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

    pub fn get_size(&self, label: &Cow<'c, str>) -> usize {
        let map = self.map.lock().unwrap();
        map.get(label)
            .and_then(|entry| entry.value.mem_size())
            .unwrap_or(0)
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
}

#[derive(Debug)]
pub struct VarStack<'c> {
    stack: Mutex<Vec<VarMap<'c>>>,
    bound: Arc<MemoryLimit>,
}

impl<'c> Clone for VarStack<'c> {
    fn clone(&self) -> Self {
        Self {
            stack: Mutex::new(self.stack.lock().unwrap().clone()),
            bound: self.bound.clone(),
        }
    }
}

impl<'c> Default for VarStack<'c> {
    fn default() -> Self {
        Self {
            stack: Mutex::new(vec![VarMap::new()]),
            bound: Default::default(),
        }
    }
}

impl<'c> VarStack<'c> {
    pub fn new(limit: Arc<MemoryLimit>) -> VarStack<'c> {
        VarStack {
            stack: Mutex::new(vec![(VarMap::new())]),
            bound: limit.clone(),
        }
    }

    pub fn is_limited(&self) -> bool {
        self.bound.limit.is_some()
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
            self.bound.deallocate(map.total_mem_size());
        }
    }

    fn find_stack_with_var(&self, label: &Cow<'c, str>) -> Result<VarMap<'c>, RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(&label) {
                return Ok(map.clone());
            }
        }
        Err(RuntimeError::NonExistantVar(format!(
            "var {} does not exist",
            label
        )))
    }

    pub fn get_last_stack(&self) -> VarMap<'c> {
        let stack = self.stack.lock().unwrap();
        stack.last().expect("global stack must exist").clone()
    }

    /// Inserts var local first
    pub fn insert(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let size = value.mem_size();
        let stack = self.get_last_stack();
        self.bound.deallocate(stack.get_size(&label));
        stack.insert(label.clone(), value)?;
        self.bound.allocate(size)?;
        Ok(())
    }

    /// Updates var local first, throws error if var doesn't exist
    pub fn update_var(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let size = value.mem_size();
        let stack = self.find_stack_with_var(label)?;
        self.bound.deallocate(stack.get_size(&label));
        stack.insert(label.clone(), value)?;
        self.bound.allocate(size)?;
        Ok(())
    }

    /// Inserts new var or updates var if it already exists local first
    pub fn create_or_update(
        &self,
        label: &Cow<'c, str>,
        value: Value<'c>,
    ) -> Result<(), RuntimeError> {
        let size = value.mem_size();
        match self.find_stack_with_var(label) {
            Ok(stack) => {
                self.bound.deallocate(stack.get_size(&label));
                stack.insert(label.clone(), value)?;
                self.bound.allocate(size)?;
            }
            Err(_) => {
                let stack = self.get_last_stack();
                self.bound.deallocate(stack.get_size(&label));
                stack.insert(label.clone(), value)?;
                self.bound.allocate(size)?;
            }
        }
        Ok(())
    }

    /// Inserts const var local first
    pub fn insert_const(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), RuntimeError> {
        let size = value.mem_size();
        let locals = self.get_last_stack();
        self.bound.allocate(size)?;
        locals.insert_const(label.clone(), value)
    }

    /// Removes var local first and returns removed value
    pub fn remove(&self, label: &Cow<'c, str>) -> Result<Option<Value<'c>>, RuntimeError> {
        let stack = self.find_stack_with_var(label)?;
        let size = stack.get_size(&label);
        let return_value = stack.remove_value(&label)?;
        self.bound.deallocate(size);
        Ok(return_value)
    }

    pub fn get(&self, label: &str) -> Result<Value<'c>, RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(label) {
                return map.get_cloned(label);
            }
        }
        Err(RuntimeError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, RuntimeError> {
        let stack = self.stack.lock().unwrap();
        for map in stack.iter().rev() {
            if map.contains(label) {
                return map.take_entry(label);
            }
        }
        Err(RuntimeError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
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
        Err(RuntimeError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
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
