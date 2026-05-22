use std::{
    borrow::Cow,
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;
pub const DEFAULT_LOOP_LIMIT: usize = u16::MAX as usize;

use tokio::sync::Mutex;

use crate::{
    error::RuntimeError,
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

    pub fn allocate(&self, size: Option<usize>) -> Result<(), RuntimeError> {
        let Some(limit) = self.limit else {
            return Ok(());
        };

        let mem = self.allocated.load(Ordering::Relaxed);
        match size {
            Some(size) => {
                match mem.checked_add(size) {
                    Some(new_mem) => {
                        if new_mem > limit {
                            return Err(RuntimeError::VarMemoryLimitReached);
                        }
                        self.allocated.store(new_mem, Ordering::Relaxed);
                    }
                    None => return Err(RuntimeError::VarMemoryLimitReached),
                };
            }
            None => return Err(RuntimeError::VarMemoryLimitReached),
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

#[derive(Debug, Copy, Clone)]
pub struct InterpreterFlags {
    pub break_flag: bool,
    pub continue_flag: bool,
    pub return_flag: bool,
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

#[derive(Debug, Default, Clone)]
pub struct ExecutionContext<'c> {
    pub call_stack: Vec<&'c str>,
    pub def_stack: Vec<Cow<'c, str>>,
    pub flags: InterpreterFlags,
    pub loop_depth: usize,
}

#[derive(Debug, Default)]
pub struct InterpreterData<'c> {
    pub args: Arc<Mutex<Vec<Value<'static>>>>,
    pub user_defs: Arc<Mutex<UserDefinitions<'c>>>,
    pub output: Arc<Mutex<String>>,
    pub total_loops: Arc<AtomicUsize>,
    pub limits: Arc<InterpreterLimits>,

    pub vars: VarStack<'c>,

    pub ctx: Mutex<ExecutionContext<'c>>,
}

impl<'c> InterpreterData<'c> {
    pub fn with_args(mut self, args: Vec<Value<'static>>) -> Self {
        self.args = Arc::new(Mutex::new(args));
        self
    }

    pub fn with_limits(mut self, limits: InterpreterLimits) -> Self {
        self.vars = VarStack::new(limits.memory_limit.clone());
        self.limits = Arc::new(limits);
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

    pub async fn fork(&self) -> Arc<InterpreterData<'c>> {
        InterpreterData {
            args: self.args.clone(),
            limits: self.limits.clone(),
            user_defs: self.user_defs.clone(),
            total_loops: self.total_loops.clone(),
            output: self.output.clone(),
            vars: self.vars.clone(),
            ctx: Mutex::new(self.ctx.lock().await.clone()),
        }
        .into()
    }

    pub fn inc_total_loops(&self) -> Result<(), RuntimeError> {
        match self.limits.max_loops {
            Some(limit) => {
                let prev_loops = self.total_loops.fetch_add(1, Ordering::Relaxed);
                if prev_loops + 1 >= limit {
                    Err(RuntimeError::LoopLimitReached)
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }

    pub fn inc_loops_by(&self, loops: &AtomicUsize) -> Result<(), RuntimeError> {
        match self.limits.max_loops {
            Some(limit) => {
                let loops = loops.load(Ordering::Relaxed);
                let prev_loops = self.total_loops.fetch_add(loops, Ordering::Relaxed);
                if prev_loops + loops > limit {
                    Err(RuntimeError::LoopLimitReached)
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }

    pub async fn push_call(&self, command_label: &'c str) {
        let mut ctx = self.ctx.lock().await;
        ctx.call_stack.push(command_label);
    }

    pub async fn pop_call(&self) {
        let mut ctx = self.ctx.lock().await;
        ctx.call_stack.pop();
    }

    pub async fn push_def(&self, def: Cow<'c, str>) {
        let mut ctx = self.ctx.lock().await;
        ctx.def_stack.push(def);
    }

    pub async fn pop_def(&self) {
        let mut ctx = self.ctx.lock().await;
        ctx.def_stack.pop();
    }

    pub async fn call_stack_to_string(&self) -> String {
        let ctx = self.ctx.lock().await;
        let mut output = String::new();
        for (i, call) in ctx.call_stack.iter().enumerate() {
            let call = if call.is_empty() { "scope" } else { call };

            if i < ctx.call_stack.len() - 1 {
                output.push_str(&format!("{} > ", call));
            } else {
                output.push_str(&format!("{}", call));
            }
        }
        output
    }

    pub async fn inc_loop_depth(&self) {
        let mut ctx = self.ctx.lock().await;
        ctx.loop_depth += 1;
    }

    pub async fn dec_loop_depth(&self) {
        let mut ctx = self.ctx.lock().await;
        ctx.loop_depth -= 1;
    }

    pub async fn loop_depth(&self) -> usize {
        let ctx = self.ctx.lock().await;
        ctx.loop_depth
    }

    pub async fn get_return_flag(&self) -> bool {
        let ctx = self.ctx.lock().await;
        ctx.flags.return_flag
    }

    pub async fn set_return_flag(&self, value: bool) {
        let mut ctx = self.ctx.lock().await;
        ctx.flags.return_flag = value;
    }

    pub async fn get_continue_flag(&self) -> bool {
        let ctx = self.ctx.lock().await;
        ctx.flags.continue_flag
    }

    pub async fn set_continue_flag(&self, value: bool) {
        let mut ctx = self.ctx.lock().await;
        ctx.flags.continue_flag = value;
    }

    pub async fn get_break_flag(&self) -> bool {
        let ctx = self.ctx.lock().await;
        ctx.flags.break_flag
    }

    pub async fn set_break_flag(&self, value: bool) {
        let mut ctx = self.ctx.lock().await;
        ctx.flags.break_flag = value;
    }

    pub async fn should_execute(&self) -> bool {
        let ctx = self.ctx.lock().await;
        ctx.flags.return_flag || ctx.flags.continue_flag || ctx.flags.break_flag
    }

    pub async fn find_user_def(&self, label: &str) -> Option<Arc<UserDef<'c>>> {
        let ctx = self.ctx.lock().await;
        let root = self.user_defs.clone();

        let mut levels = vec![root.clone()];
        let mut current = root.clone();

        for call in ctx.def_stack.iter() {
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
