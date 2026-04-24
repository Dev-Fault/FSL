use std::{
    collections::HashMap,
    mem,
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

use crate::types::{
    FslType,
    value::{Value, ValueError},
};

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 5 * MEGABYTE;

#[derive(Debug, Clone)]
pub struct VarEntry {
    pub value: Value,
    pub constant: bool,
}

impl Default for VarEntry {
    fn default() -> Self {
        Self {
            value: Default::default(),
            constant: Default::default(),
        }
    }
}

impl VarEntry {
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

#[derive(Debug)]
pub struct VarMap {
    map: Mutex<HashMap<String, VarEntry>>,
}

impl VarMap {
    pub fn new() -> Self {
        Self {
            map: Mutex::new(HashMap::new()),
        }
    }

    pub fn insert_mut_value(&self, label: &str, value: Value) -> Result<(), ValueError> {
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
                if let Some(prev_entry) = map.get(label) {
                    if prev_entry.constant {
                        return Err(ValueError::AttemptToOverwriteConstant(format!(
                            "cannot overwrite constant var {}",
                            label
                        )));
                    }
                }
                map.insert(label.to_string(), VarEntry::new_mut(value.clone()));
                Ok(())
            }
        }
    }

    pub fn insert_const_value(&self, label: &str, value: Value) -> Result<(), ValueError> {
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
                if let Some(prev_entry) = map.get(label) {
                    if prev_entry.constant {
                        return Err(ValueError::AttemptToOverwriteConstant(format!(
                            "cannot overwrite constant var {}",
                            label
                        )));
                    }
                }
                map.insert(label.to_string(), VarEntry::new_const(value));
                Ok(())
            }
        }
    }

    pub fn remove_value(&self, label: &str) -> Result<Option<Value>, ValueError> {
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

    pub fn clone_value(&self, label: &str) -> Result<Value, ValueError> {
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

    pub fn take_entry(&self, label: &str) -> Result<VarEntry, ValueError> {
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

    pub fn insert_entry(&self, label: String, var_entry: VarEntry) {
        let mut vars = self.map.lock().unwrap();
        vars.insert(label, var_entry);
    }

    pub fn has_entry(&self, label: &str) -> bool {
        let var_map = self.map.lock().unwrap();
        var_map.contains_key(label)
    }

    pub fn get_var_size(&self, label: &str) -> usize {
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

const VAR_STACK_EXPECT: &str = "Var stack should always at least have global scope var map";

#[derive(Debug)]
pub struct VarStack {
    stack: Mutex<Vec<VarMap>>,
    mem_limit: Option<usize>,
    pub(crate) allocated_mem: AtomicUsize,
}

impl VarStack {
    pub fn new_unbounded() -> VarStack {
        VarStack {
            stack: Mutex::new(vec![(VarMap::new())]),
            mem_limit: None,
            allocated_mem: AtomicUsize::new(0),
        }
    }

    pub fn new_bounded(byte_limit: usize) -> VarStack {
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

    /// Inserts a variable in the current scope, overwrites it if it already exists
    pub fn insert_mut_var(&self, label: &str, value: Value) -> Result<(), ValueError> {
        let stack = self.stack.lock().unwrap();
        let var_map = stack
            .last()
            .expect("Var stack should always at least have global scope var map");
        self.deallocate_mem(var_map.get_var_size(label));
        let value_size = value.mem_size();
        var_map.insert_mut_value(label, value)?;
        self.allocate_mem(value_size)?;
        Ok(())
    }

    /// Updates var in current scope will throw error if no scopes contain var
    pub fn update_var(&self, label: &str, value: Value) -> Result<(), ValueError> {
        let stack = self.stack.lock().unwrap();
        let value_size = value.mem_size();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(label) {
                self.deallocate_mem(var_map.get_var_size(label));
                var_map.insert_mut_value(label, value)?;
                self.allocate_mem(value_size)?;
                return Ok(());
            }
        }
        Err(ValueError::NonExistantVar(format!(
            "cannot update value of non existant var {}",
            label
        )))
    }

    /// Updates or creates a var preffering outer scope
    pub fn update_or_create_mut_var(&self, label: &str, value: Value) -> Result<(), ValueError> {
        let stack = self.stack.lock().unwrap();
        let value_size = value.mem_size();
        for var_map in stack.iter() {
            if var_map.has_entry(label) {
                self.deallocate_mem(var_map.get_var_size(label));
                var_map.insert_mut_value(label, value)?;
                self.allocate_mem(value_size)?;
                return Ok(());
            }
        }
        let var_map = stack.last().expect(VAR_STACK_EXPECT);
        self.deallocate_mem(var_map.get_var_size(label));
        var_map.insert_mut_value(label, value)?;
        self.allocate_mem(value_size)?;
        Ok(())
    }

    /// Inserts a constant var, will error if constant already exists
    pub fn insert_const_var(&self, label: &str, value: Value) -> Result<(), ValueError> {
        let stack = self.stack.lock().unwrap();
        let value_size = value.mem_size();
        let var_map = stack.last().expect(VAR_STACK_EXPECT);
        self.allocate_mem(value_size)?;
        var_map.insert_const_value(label, value)
    }

    /// Removes a var from the most local scope, throws error if var is constant
    pub fn remove_var(&self, label: &str) -> Result<Option<Value>, ValueError> {
        let stack = self.stack.lock().unwrap();
        let var_map = stack.last().expect(VAR_STACK_EXPECT);
        let value_size = var_map.get_var_size(label);
        let return_value = var_map.remove_value(label)?;
        self.deallocate_mem(value_size);
        Ok(return_value)
    }

    /// Gets value of var in most local scope, throws error if it doesn't exist
    pub fn get_var_value(&self, label: &str) -> Result<Value, ValueError> {
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

    /// Does mem::take on var entry in the current local scope
    pub fn take_entry(&self, label: &str) -> Result<VarEntry, ValueError> {
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

    /// Inserts var entry into most local var map (intended to be used with take_entry)
    pub fn insert_entry(&self, label: String, var_entry: VarEntry) -> Result<(), ValueError> {
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

    /// Gets value of var in most local scope, throws error if it doesn't exist
    pub fn get_mut_var_value(&self, label: &str) -> Result<Value, ValueError> {
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

    /// Gets type of var in most local scope, returns None if doesn't exist
    pub fn get_var_type(&self, label: &str) -> FslType {
        let stack = self.stack.lock().unwrap();
        for var_map in stack.iter().rev() {
            if var_map.has_entry(label) {
                return var_map.get_type(label);
            }
        }
        FslType::None
    }
}
