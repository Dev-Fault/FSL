use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;
pub const DEFAULT_LOOP_LIMIT: usize = u16::MAX as usize;

use bytes::Bytes;
use parking_lot::RwLock;
use tokio::sync::Mutex;

use crate::{
    error::RuntimeError,
    source_str::SourceStr,
    span::Span,
    types::{command::Command, value::Value},
    vars::{DEFAULT_MEMORY_LIMIT, VarStore},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DefinitionKey {
    pub parent: SourceStr,
    pub label: SourceStr,
}

impl DefinitionKey {
    pub fn new(parent: SourceStr, label: SourceStr) -> Self {
        Self { parent, label }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserDeclaration {
    pub label: SourceStr,
    pub definition: Option<UserDefintion>,
}

impl UserDeclaration {
    pub fn new(label: SourceStr) -> Self {
        Self {
            label,
            definition: None,
        }
    }

    pub fn define(&mut self, definition: UserDefintion) {
        self.definition = Some(definition);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserDefintion {
    pub parameters: VecDeque<SourceStr>,
    pub commands: Vec<Command>,
}

impl UserDefintion {
    pub fn new(parameters: VecDeque<SourceStr>, commands: Vec<Command>) -> Self {
        Self {
            parameters,
            commands,
        }
    }
}

pub type UserDefinitionStore = HashMap<DefinitionKey, UserDeclaration>;

pub trait DefinitionFinder {
    fn find_def(&self, label: &SourceStr, call_stack: &Vec<SourceStr>) -> Option<UserDeclaration>;
    fn with_mut_def(
        &mut self,
        label: &SourceStr,
        call_stack: &Vec<SourceStr>,
        f: impl FnOnce(&mut UserDeclaration),
    ) -> Result<(), RuntimeError>;
}

impl DefinitionFinder for UserDefinitionStore {
    fn with_mut_def(
        &mut self,
        label: &SourceStr,
        call_stack: &Vec<SourceStr>,
        f: impl FnOnce(&mut UserDeclaration),
    ) -> Result<(), RuntimeError> {
        let key = DefinitionKey::new(SourceStr::Static(""), label.clone());

        if let Some(decl) = self.get_mut(&key) {
            f(decl);
            return Ok(());
        }

        for call in call_stack.iter().rev() {
            let key = DefinitionKey::new(call.clone(), label.clone());
            if let Some(decl) = self.get_mut(&key) {
                f(decl);
                return Ok(());
            }
        }
        return Err(RuntimeError::ValueDef);
    }

    fn find_def(&self, label: &SourceStr, call_stack: &Vec<SourceStr>) -> Option<UserDeclaration> {
        let key = DefinitionKey::new(SourceStr::Static(""), label.clone());

        if let Some(decl) = self.get(&key) {
            return Some(decl.clone());
        }

        for call in call_stack.iter().rev() {
            let key = DefinitionKey::new(call.clone(), label.clone());
            if let Some(decl) = self.get(&key) {
                return Some(decl.clone());
            }
        }
        None
    }
}

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

#[derive(Debug, Default)]
pub struct InterpreterFlags {
    pub break_flag: AtomicBool,
    pub continue_flag: AtomicBool,
    pub return_flag: AtomicBool,
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

#[derive(Debug, Default)]
pub struct ExecutionContext {
    pub def_stack: std::sync::Mutex<Vec<SourceStr>>,
    pub flags: InterpreterFlags,
    pub loop_depth: AtomicUsize,
}

#[derive(Debug, Default, Clone)]
pub struct InterpreterData {
    pub(crate) source: Bytes,

    pub args: Arc<Mutex<Vec<Value>>>,
    pub output: Arc<Mutex<String>>,
    pub total_loops: Arc<AtomicUsize>,
    pub limits: InterpreterLimits,

    pub vars: Arc<RwLock<VarStore>>,
    pub def_store: Arc<std::sync::Mutex<UserDefinitionStore>>,

    pub ctx: Arc<ExecutionContext>,
}

impl InterpreterData {
    pub(crate) fn set_source(&mut self, source: String) {
        self.source = Bytes::from_owner(source)
    }

    pub fn source_str(&self) -> SourceStr {
        SourceStr::Borrowed(self.source.clone())
    }

    pub fn source_span(&self, span: Span) -> SourceStr {
        SourceStr::from_span(span, self)
    }

    pub fn with_args(mut self, args: Vec<Value>) -> Self {
        self.args = Arc::new(Mutex::new(args));
        self
    }

    pub fn with_limits(mut self, limits: InterpreterLimits) -> Self {
        self.vars = Arc::new(RwLock::new(VarStore::new(limits.limiter.clone())));
        self.limits = limits;
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
            total_loops: self.total_loops.clone(),
            output: self.output.clone(),
            vars: Arc::new(RwLock::new(self.vars.read().clone())),
            def_store: self.def_store.clone(),
            ctx: Arc::new(ExecutionContext::default()),
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

    pub fn push_def(&self, def: SourceStr) {
        self.ctx.def_stack.lock().unwrap().push(def);
    }

    pub fn pop_def(&self) {
        self.ctx.def_stack.lock().unwrap().pop();
    }

    pub fn inc_loop_depth(&self) {
        self.ctx.loop_depth.fetch_add(1, Ordering::Relaxed);
    }

    pub fn dec_loop_depth(&self) {
        self.ctx.loop_depth.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn loop_depth(&self) -> usize {
        self.ctx.loop_depth.load(Ordering::Relaxed)
    }

    pub fn get_return_flag(&self) -> bool {
        self.ctx.flags.return_flag.load(Ordering::Relaxed)
    }

    pub fn set_return_flag(&self, value: bool) {
        self.ctx.flags.return_flag.store(value, Ordering::Relaxed);
    }

    pub fn get_continue_flag(&self) -> bool {
        self.ctx.flags.continue_flag.load(Ordering::Relaxed)
    }

    pub fn set_continue_flag(&self, value: bool) {
        self.ctx.flags.continue_flag.store(value, Ordering::Relaxed);
    }

    pub fn get_break_flag(&self) -> bool {
        self.ctx.flags.break_flag.load(Ordering::Relaxed)
    }

    pub fn set_break_flag(&self, value: bool) {
        self.ctx.flags.break_flag.store(value, Ordering::Relaxed);
    }

    pub fn should_execute(&self) -> bool {
        self.ctx.flags.return_flag.load(Ordering::Relaxed)
            || self.ctx.flags.continue_flag.load(Ordering::Relaxed)
            || self.ctx.flags.break_flag.load(Ordering::Relaxed)
    }

    pub fn find_def(&self, label: &SourceStr) -> Option<UserDeclaration> {
        let def_store = self.def_store.lock().unwrap();
        if let Some(decl) = def_store.get(&DefinitionKey::new(SourceStr::Static(""), label.clone()))
        {
            return Some(decl.clone());
        }
        let call_stack = self.ctx.def_stack.lock().unwrap();
        def_store.find_def(label, &call_stack)
    }
}
