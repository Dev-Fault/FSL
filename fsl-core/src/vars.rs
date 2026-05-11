use std::{
    borrow::Cow,
    collections::HashMap,
    mem,
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

use crate::{
    error::ValueError,
    types::{FslType, value::Value},
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

#[derive(Debug, Clone)]
pub struct VarEntry<'c> {
    pub value: Value<'c>,
    pub constant: bool,
}

impl<'c> Default for VarEntry<'c> {
    fn default() -> Self {
        Self {
            value: Default::default(),
            constant: Default::default(),
        }
    }
}

impl<'c> VarEntry<'c> {
    fn new_mut(value: Value) -> VarEntry {
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

    pub fn insert_mut_value(
        &self,
        label: Cow<'c, str>,
        value: Value<'c>,
    ) -> Result<(), ValueError> {
        match value {
            Value::Var(_) => {
                return Err(ValueError::InvalidVarValue(
                    "cannot store var in var".into(),
                ));
            }
            Value::Command(_) => {
                return Err(ValueError::InvalidVarValue(
                    "cannot store command in var".into(),
                ));
            }
            _ => {
                let lock = self.map.lock();
                let mut map = lock.unwrap();
                if let Some(prev_entry) = map.get(&label) {
                    if prev_entry.constant {
                        return Err(ValueError::AttemptToOverwriteConstant(format!(
                            "cannot overwrite constant var {}",
                            label
                        )));
                    }
                }
                map.insert(label, VarEntry::new_mut(value));
                Ok(())
            }
        }
    }

    pub fn insert_const_value(
        &self,
        label: Cow<'c, str>,
        value: Value<'c>,
    ) -> Result<(), ValueError> {
        match value {
            Value::Var(_) => {
                return Err(ValueError::InvalidVarValue(
                    "cannot store var in var".into(),
                ));
            }
            Value::Command(_) => {
                return Err(ValueError::InvalidVarValue(
                    "cannot store command in var".into(),
                ));
            }
            _ => {
                let lock = self.map.lock();
                let mut map = lock.unwrap();
                if let Some(prev_entry) = map.get(&label) {
                    if prev_entry.constant {
                        return Err(ValueError::AttemptToOverwriteConstant(format!(
                            "cannot overwrite constant var {}",
                            label
                        )));
                    }
                }
                map.insert(label, VarEntry::new_const(value));
                Ok(())
            }
        }
    }

    pub fn remove_value(&self, label: &str) -> Result<Option<Value<'c>>, ValueError> {
        let lock = self.map.lock();
        let mut map = lock.unwrap();

        if let Some(prev_entry) = map.get(label) {
            if prev_entry.constant {
                return Err(ValueError::AttemptToFreeConstant(format!(
                    "cannot free constant var {}",
                    label
                )));
            }
        }
        Ok(map.remove(label).map(|entry| entry.value))
    }

    pub fn clone_value(&self, label: &str) -> Result<Value<'c>, ValueError> {
        let var_entry = self.map.lock().unwrap().get(label).cloned();

        match var_entry {
            Some(var_entry) => {
                if var_entry.value.is_type(FslType::Var) {
                    return self.clone_value(&var_entry.value.get_var_label()?);
                } else {
                    Ok(var_entry.value)
                }
            }
            None => Err(ValueError::NonExistantVar(format!(
                "cannot get value of a non existant var: \"{}\"",
                label
            ))),
        }
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, ValueError> {
        let mut vars = self.map.lock().unwrap();
        let var_entry = vars.get_mut(label);

        match var_entry {
            Some(var_entry) => {
                if var_entry.constant {
                    return Err(ValueError::AttemptToOverwriteConstant(format!(
                        "cannot overwrite constant var {}",
                        label
                    )));
                }
                let var_entry = mem::take(var_entry);
                Ok(var_entry)
            }
            None => Err(ValueError::NonExistantVar(format!(
                "cannot get value of a non existant var: \"{}\"",
                label
            ))),
        }
    }

    pub fn insert_entry(&self, label: Cow<'c, str>, var_entry: VarEntry<'c>) {
        let mut vars = self.map.lock().unwrap();
        vars.insert(label, var_entry);
    }

    pub fn has_entry(&self, label: &str) -> bool {
        let var_map = self.map.lock().unwrap();
        var_map.contains_key(label)
    }

    pub fn get_var_size(&self, label: &Cow<'c, str>) -> usize {
        let var_map = self.map.lock().unwrap();
        var_map
            .get(label)
            .and_then(|entry| entry.value.mem_size())
            .unwrap_or(0)
    }

    pub fn total_mem_size(&self) -> usize {
        let var_map = self.map.lock().unwrap();
        var_map
            .values()
            .map(|entry| entry.value.mem_size().unwrap_or(0))
            .sum()
    }

    pub fn get_type(&self, label: &str) -> FslType {
        let lock = self.map.lock();
        let map = lock.unwrap();
        let var_entry = map.get(label);

        match var_entry {
            Some(var_entry) => {
                let value = &var_entry.value;
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
    mem_limit: Option<usize>,
    pub(crate) allocated_mem: AtomicUsize,
}

impl<'c> VarStack<'c> {
    pub fn new_unbounded() -> VarStack<'c> {
        VarStack {
            stack: Mutex::new(vec![(VarMap::new())]),
            mem_limit: None,
            allocated_mem: AtomicUsize::new(0),
        }
    }

    pub fn new_bounded(byte_limit: usize) -> VarStack<'c> {
        VarStack {
            stack: Mutex::new(vec![(VarMap::new())]),
            mem_limit: Some(byte_limit),
            allocated_mem: AtomicUsize::new(0),
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
        if let Some(var_map) = stack.pop() {
            self.deallocate_mem(var_map.total_mem_size());
        }
    }

    pub fn allocate_mem(&self, size: Option<usize>) -> Result<(), ValueError> {
        let mem = self.allocated_mem.load(Ordering::Relaxed);
        match size {
            Some(size) => match mem.checked_add(size) {
                Some(new_mem) => {
                    if let Some(mem_limit) = self.mem_limit
                        && new_mem > mem_limit
                    {
                        return Err(ValueError::VarMemoryLimitReached);
                    }
                    self.allocated_mem.store(new_mem, Ordering::Relaxed);
                }
                None => return Err(ValueError::VarMemoryLimitReached),
            },
            None => {
                return Err(ValueError::VarMemoryLimitReached);
            }
        };
        Ok(())
    }

    pub fn deallocate_mem(&self, size: usize) {
        let mem = self.allocated_mem.load(Ordering::Relaxed);
        self.allocated_mem
            .store(mem.saturating_sub(size), Ordering::Relaxed);
    }

    fn find_stack_with_var(&self, label: &Cow<'c, str>) -> Result<VarMap<'c>, ValueError> {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(&label) {
                return Ok(var_map.clone());
            }
        }
        Err(ValueError::NonExistantVar(format!(
            "var \"{}\" does not exist",
            label
        )))
    }

    fn get_last_stack(&self) -> VarMap<'c> {
        let stack = self.stack.lock().unwrap();
        stack.last().expect("global stack must exist").clone()
    }

    /// Inserts var local first
    pub fn insert(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), ValueError> {
        let value_size = value.mem_size();
        let stack = self.get_last_stack();
        self.deallocate_mem(stack.get_var_size(&label));
        stack.insert_mut_value(label.clone(), value)?;
        self.allocate_mem(value_size)?;
        Ok(())
    }

    /// Updates var local first, throws error if var doesn't exist
    pub fn update_var(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), ValueError> {
        let value_size = value.mem_size();
        let stack = self.find_stack_with_var(label)?;
        self.deallocate_mem(stack.get_var_size(&label));
        stack.insert_mut_value(label.clone(), value)?;
        self.allocate_mem(value_size)?;
        Ok(())
    }

    /// Inserts new var or updates var if it already exists local first
    pub fn create_or_update(
        &self,
        label: &Cow<'c, str>,
        value: Value<'c>,
    ) -> Result<(), ValueError> {
        let value_size = value.mem_size();
        match self.find_stack_with_var(label) {
            Ok(stack) => {
                self.deallocate_mem(stack.get_var_size(&label));
                stack.insert_mut_value(label.clone(), value)?;
                self.allocate_mem(value_size)?;
            }
            Err(_) => {
                let stack = self.get_last_stack();
                self.deallocate_mem(stack.get_var_size(&label));
                stack.insert_mut_value(label.clone(), value)?;
                self.allocate_mem(value_size)?;
            }
        }
        Ok(())
    }

    /// Inserts const var local first
    pub fn insert_const(&self, label: &Cow<'c, str>, value: Value<'c>) -> Result<(), ValueError> {
        let value_size = value.mem_size();
        let locals = self.get_last_stack();
        self.allocate_mem(value_size)?;
        locals.insert_const_value(label.clone(), value)
    }

    /// Removes var local first and returns removed value
    pub fn remove(&self, label: &Cow<'c, str>) -> Result<Option<Value<'c>>, ValueError> {
        let stack = self.find_stack_with_var(label)?;
        let value_size = stack.get_var_size(&label);
        let return_value = stack.remove_value(&label)?;
        self.deallocate_mem(value_size);
        Ok(return_value)
    }

    pub fn get(&self, label: &str) -> Result<Value<'c>, ValueError> {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(label) {
                return var_map.clone_value(label);
            }
        }
        Err(ValueError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
    }

    pub fn take_entry(&self, label: &str) -> Result<VarEntry<'c>, ValueError> {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(label) {
                return var_map.take_entry(label);
            }
        }
        Err(ValueError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
    }

    pub fn insert_entry(
        &self,
        label: Cow<'c, str>,
        var_entry: VarEntry<'c>,
    ) -> Result<(), ValueError> {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(&label) {
                var_map.insert_entry(label, var_entry);
                return Ok(());
            }
        }
        Err(ValueError::NonExistantVar(format!(
            "cannot get value of non existant var {}",
            label
        )))
    }

    pub fn get_type(&self, label: &str) -> FslType {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(label) {
                return var_map.get_type(label);
            }
        }
        FslType::None
    }
}
