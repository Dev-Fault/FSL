use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;
pub const DEFAULT_LOOP_LIMIT: usize = u16::MAX as usize;

use bytes::Bytes;
use tokio::sync::{Mutex, RwLock};

use crate::{
    error::RuntimeError,
    source_str::SourceStr,
    types::{
        command::UserDef,
        value::{FslValue, Value},
    },
    vars::{DEFAULT_MEMORY_LIMIT, VarStore},
};

pub type UserDefinitions = HashMap<SourceStr, Arc<UserDef>>;

#[derive(Debug, Default)]
pub struct MemoryLimit {
    pub limit: usize,
    pub allocated: AtomicUsize,
}

#[derive(Debug)]
pub enum Limiter {
    NoLimit,
    Limit(MemoryLimit),
}

impl Default for Limiter {
    fn default() -> Self {
        Self::NoLimit
    }
}

impl Limiter {
    pub fn allocate(&self, value: &Value) -> Result<(), RuntimeError> {
        match self {
            Limiter::NoLimit => Ok(()),
            Limiter::Limit(memory_limit) => {
                let mem = memory_limit.allocated.load(Ordering::Relaxed);
                let size = value.mem_size()?;
                if let Some(new_mem) = mem.checked_add(size)
                    && new_mem < memory_limit.limit
                {
                    memory_limit.allocated.store(new_mem, Ordering::Relaxed);
                } else {
                    return Err(RuntimeError::VarMemoryLimitReached);
                };
                Ok(())
            }
        }
    }

    pub fn deallocate(&self, value: &Value) {
        match self {
            Limiter::NoLimit => {}
            Limiter::Limit(memory_limit) => {
                let mem = memory_limit.allocated.load(Ordering::Relaxed);
                memory_limit.allocated.store(
                    mem.saturating_sub(value.mem_size().unwrap_or(0)),
                    Ordering::Relaxed,
                );
            }
        }
    }
}

impl MemoryLimit {
    pub fn new(limit: usize) -> MemoryLimit {
        MemoryLimit {
            limit,
            allocated: AtomicUsize::new(0),
        }
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct InterpreterFlags {
    pub break_flag: bool,
    pub continue_flag: bool,
    pub return_flag: bool,
}

#[derive(Clone, Debug, Default)]
pub struct InterpreterLimits {
    pub max_output_len: Option<usize>,
    pub max_loops: Option<usize>,
    pub limiter: Arc<Limiter>,
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
        self.limiter = Arc::new(Limiter::Limit(MemoryLimit::new(limit)));
        self
    }

    pub fn default_limits() -> Self {
        InterpreterLimits {
            max_output_len: Some(DEFAULT_OUTPUT_LIMIT),
            max_loops: Some(DEFAULT_LOOP_LIMIT),
            limiter: Arc::new(Limiter::Limit(MemoryLimit::new(DEFAULT_MEMORY_LIMIT))),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExecutionContext {
    pub call_stack: Vec<SourceStr>,
    pub def_stack: Vec<SourceStr>,
    pub flags: InterpreterFlags,
    pub loop_depth: usize,
}

#[derive(Debug, Default)]
pub struct InterpreterData {
    pub source: Bytes,
    pub args: Arc<Mutex<Vec<Value>>>,
    pub user_defs: Arc<Mutex<UserDefinitions>>,
    pub output: Arc<Mutex<String>>,
    pub total_loops: Arc<AtomicUsize>,
    pub limits: Arc<InterpreterLimits>,

    pub vars: Arc<RwLock<VarStore>>,

    pub ctx: RwLock<ExecutionContext>,
}

impl InterpreterData {
    pub(crate) fn set_source(&mut self, source: String) {
        self.source = Bytes::from_owner(source)
    }

    pub fn source_str(&self) -> SourceStr {
        SourceStr::Borrowed(self.source.clone())
    }

    pub fn with_args(mut self, args: Vec<Value>) -> Self {
        self.args = Arc::new(Mutex::new(args));
        self
    }

    pub fn with_limits(mut self, limits: InterpreterLimits) -> Self {
        self.vars = Arc::new(RwLock::new(VarStore::new(limits.limiter.clone())));
        self.limits = Arc::new(limits);
        self
    }

    pub fn from<'new>(data: &InterpreterData) -> InterpreterData {
        InterpreterData {
            args: data.args.clone(),
            vars: Arc::new(RwLock::new(VarStore::new(data.limits.limiter.clone()))),
            limits: data.limits.clone(),
            ..Default::default()
        }
    }

    pub async fn fork(&self) -> Arc<InterpreterData> {
        InterpreterData {
            source: self.source.clone(),
            args: self.args.clone(),
            limits: self.limits.clone(),
            user_defs: self.user_defs.clone(),
            total_loops: self.total_loops.clone(),
            output: self.output.clone(),
            vars: Arc::new(RwLock::new(self.vars.read().await.clone())),
            ctx: RwLock::new(self.ctx.read().await.clone()),
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

    pub async fn push_call(&self, command_label: SourceStr) {
        let mut ctx = self.ctx.write().await;
        ctx.call_stack.push(command_label);
    }

    pub async fn pop_call(&self) {
        let mut ctx = self.ctx.write().await;
        ctx.call_stack.pop();
    }

    pub async fn push_def(&self, def: SourceStr) {
        let mut ctx = self.ctx.write().await;
        ctx.def_stack.push(def);
    }

    pub async fn pop_def(&self) {
        let mut ctx = self.ctx.write().await;
        ctx.def_stack.pop();
    }

    pub async fn call_stack_to_string(&self) -> String {
        let ctx = self.ctx.read().await;
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
        let mut ctx = self.ctx.write().await;
        ctx.loop_depth += 1;
    }

    pub async fn dec_loop_depth(&self) {
        let mut ctx = self.ctx.write().await;
        ctx.loop_depth -= 1;
    }

    pub async fn loop_depth(&self) -> usize {
        let ctx = self.ctx.read().await;
        ctx.loop_depth
    }

    pub async fn get_return_flag(&self) -> bool {
        let ctx = self.ctx.read().await;
        ctx.flags.return_flag
    }

    pub async fn set_return_flag(&self, value: bool) {
        let mut ctx = self.ctx.write().await;
        ctx.flags.return_flag = value;
    }

    pub async fn get_continue_flag(&self) -> bool {
        let ctx = self.ctx.read().await;
        ctx.flags.continue_flag
    }

    pub async fn set_continue_flag(&self, value: bool) {
        let mut ctx = self.ctx.write().await;
        ctx.flags.continue_flag = value;
    }

    pub async fn get_break_flag(&self) -> bool {
        let ctx = self.ctx.read().await;
        ctx.flags.break_flag
    }

    pub async fn set_break_flag(&self, value: bool) {
        let mut ctx = self.ctx.write().await;
        ctx.flags.break_flag = value;
    }

    pub async fn should_execute(&self) -> bool {
        let ctx = self.ctx.read().await;
        ctx.flags.return_flag || ctx.flags.continue_flag || ctx.flags.break_flag
    }

    pub async fn find_user_def(&self, label: &SourceStr) -> Option<Arc<UserDef>> {
        let ctx = self.ctx.read().await;
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
