use std::{
    borrow::Cow,
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;
pub const DEFAULT_LOOP_LIMIT: usize = u16::MAX as usize;

use tokio::sync::Mutex;

use crate::{
    error::{CommandError, ValueError},
    types::{command::UserDef, value::Value},
    vars::{DEFAULT_MEMORY_LIMIT, VarStack},
};

pub type UserDefinitions<'c> = HashMap<Cow<'c, str>, Arc<UserDef<'c>>>;

#[derive(Debug, Default)]
pub struct MemoryLimit {
    pub limit: Option<usize>,
    pub allocated: AtomicUsize,
}

impl MemoryLimit {
    pub fn new(limit: Option<usize>) -> MemoryLimit {
        MemoryLimit {
            limit,
            allocated: AtomicUsize::new(0),
        }
    }

    pub fn allocate(&self, size: Option<usize>) -> Result<(), ValueError> {
        let Some(limit) = self.limit else {
            return Ok(());
        };

        let mem = self.allocated.load(Ordering::Relaxed);
        match size {
            Some(size) => {
                match mem.checked_add(size) {
                    Some(new_mem) => {
                        if new_mem > limit {
                            return Err(ValueError::VarMemoryLimitReached);
                        }
                        self.allocated.store(new_mem, Ordering::Relaxed);
                    }
                    None => return Err(ValueError::VarMemoryLimitReached),
                };
            }
            None => return Err(ValueError::VarMemoryLimitReached),
        }
        Ok(())
    }

    pub fn deallocate(&self, size: usize) {
        if self.limit.is_some() {
            let mem = self.allocated.load(Ordering::Relaxed);
            self.allocated
                .store(mem.saturating_sub(size), Ordering::Relaxed);
        }
    }
}

#[derive(Debug)]
pub struct InterpreterFlags {
    pub break_flag: AtomicBool,
    pub continue_flag: AtomicBool,
    pub return_flag: AtomicBool,
}

impl Default for InterpreterFlags {
    fn default() -> Self {
        Self {
            break_flag: Default::default(),
            continue_flag: Default::default(),
            return_flag: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct InterpreterLimits {
    pub max_output_len: Option<usize>,
    pub max_loops: Option<usize>,
    pub memory_limit: Arc<MemoryLimit>,
}

impl InterpreterLimits {
    pub fn with_output_limit(mut self, limit: usize) -> Self {
        self.max_output_len = Some(limit);
        self
    }

    pub fn with_loop_limit(mut self, limit: usize) -> Self {
        self.max_loops = Some(limit);
        self
    }

    pub fn with_memory_limit(mut self, limit: usize) -> Self {
        self.memory_limit = Arc::new(MemoryLimit::new(Some(limit)));
        self
    }

    pub fn bounded() -> Self {
        InterpreterLimits {
            max_output_len: Some(DEFAULT_OUTPUT_LIMIT),
            max_loops: Some(DEFAULT_LOOP_LIMIT),
            memory_limit: Arc::new(MemoryLimit::new(Some(DEFAULT_MEMORY_LIMIT))),
        }
    }
}

#[derive(Debug, Default)]
pub struct InterpreterData<'c> {
    pub call_stack: Mutex<Vec<&'c str>>,
    pub user_call_stack: Mutex<Vec<Cow<'c, str>>>,

    pub args: Arc<Mutex<Vec<Value<'static>>>>,
    pub output: Mutex<String>,

    pub vars: VarStack<'c>,
    pub user_defs: Arc<Mutex<UserDefinitions<'c>>>,

    pub loop_depth: AtomicUsize,
    pub total_loops: AtomicUsize,

    pub flags: InterpreterFlags,

    pub limits: InterpreterLimits,
}

impl<'c> InterpreterData<'c> {
    pub fn with_args(mut self, args: Vec<Value<'static>>) -> Self {
        self.args = Arc::new(Mutex::new(args));
        self
    }

    pub fn with_limits(mut self, limits: InterpreterLimits) -> Self {
        self.vars = VarStack::new(limits.memory_limit.clone());
        self.limits = limits;
        self
    }

    pub fn from<'new>(data: &InterpreterData<'c>) -> InterpreterData<'new> {
        InterpreterData {
            args: data.args.clone(),
            vars: VarStack::new(data.limits.memory_limit.clone()),
            limits: data.limits.clone(),
            ..Default::default()
        }
    }

    pub async fn increment_loops(&self) -> Result<(), CommandError> {
        match self.limits.max_loops {
            Some(limit) => {
                let prev_loops = self.total_loops.fetch_add(1, Ordering::Relaxed);
                if prev_loops + 1 >= limit {
                    Err(CommandError::LoopLimitReached)
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }

    pub async fn increment_loops_by(&self, loops: &AtomicUsize) -> Result<(), CommandError> {
        match self.limits.max_loops {
            Some(limit) => {
                let loops = loops.load(Ordering::Relaxed);
                let prev_loops = self.total_loops.fetch_add(loops, Ordering::Relaxed);
                if prev_loops + loops > limit {
                    Err(CommandError::LoopLimitReached)
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }

    pub async fn push_call(&self, command_label: &'c str) {
        let mut call_stack = self.call_stack.lock().await;
        call_stack.push(command_label);
    }

    pub async fn pop_call(&self) {
        let mut call_stack = self.call_stack.lock().await;
        call_stack.pop();
    }

    pub async fn call_stack_to_string(&self) -> String {
        let call_stack = self.call_stack.lock().await;
        let mut output = String::new();
        for (i, call) in call_stack.iter().enumerate() {
            let call = if call.is_empty() { "scope" } else { call };

            if i < call_stack.len() - 1 {
                output.push_str(&format!("{} > ", call));
            } else {
                output.push_str(&format!("{}", call));
            }
        }
        output
    }

    pub async fn find_user_def(&self, label: &str) -> Option<Arc<UserDef<'c>>> {
        let root = self.user_defs.clone();
        let call_stack = self.user_call_stack.lock().await;

        let mut levels = vec![root.clone()];
        let mut current = root.clone();

        for call in call_stack.iter() {
            let defs;
            match current.lock().await.get(call) {
                Some(def) => {
                    defs = Some(def.local_defs.clone());
                }
                None => break,
            }
            if let Some(defs) = defs {
                current = defs.clone();
                levels.push(defs);
            }
        }

        for level in levels.iter().rev() {
            if let Some(def) = level.lock().await.get(label).cloned() {
                return Some(def);
            }
        }
        None
    }
}
