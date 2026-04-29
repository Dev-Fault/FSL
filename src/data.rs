use std::{
    collections::HashMap,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;

use crate::{
    error::CommandError,
    types::command::UserCommand,
    vars::{DEFAULT_MEMORY_LIMIT, VarStack},
};

pub type UserCommands = HashMap<String, UserCommand>;

#[derive(Debug)]
pub struct InterpreterFlags {
    pub break_flag: AtomicBool,
    pub continue_flag: AtomicBool,
    pub return_flag: AtomicBool,
    pub if_flag: AtomicBool,
    pub switch_flag: AtomicBool,
}

impl Default for InterpreterFlags {
    fn default() -> Self {
        Self {
            break_flag: Default::default(),
            continue_flag: Default::default(),
            return_flag: Default::default(),
            if_flag: Default::default(),
            switch_flag: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct InterpreterData {
    pub(crate) call_stack: tokio::sync::Mutex<Vec<String>>,

    pub output_limit: Option<usize>,
    pub output: tokio::sync::Mutex<String>,

    pub vars: VarStack,

    pub user_commands: tokio::sync::Mutex<UserCommands>,

    pub loop_limit: Option<usize>,
    pub total_loops: AtomicUsize,
    pub loop_depth: AtomicUsize,

    pub flags: InterpreterFlags,
}

impl InterpreterData {
    pub fn new() -> Self {
        InterpreterData {
            output: tokio::sync::Mutex::new(String::new()),
            vars: VarStack::new_bounded(DEFAULT_MEMORY_LIMIT),
            user_commands: tokio::sync::Mutex::new(UserCommands::new()),
            call_stack: tokio::sync::Mutex::new(Vec::new()),
            loop_limit: Some(u16::MAX as usize),
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            flags: InterpreterFlags::default(),
            output_limit: Some(DEFAULT_OUTPUT_LIMIT),
        }
    }

    pub fn new_unbounded() -> Self {
        InterpreterData {
            output: tokio::sync::Mutex::new(String::new()),
            vars: VarStack::new_unbounded(),
            user_commands: tokio::sync::Mutex::new(UserCommands::new()),
            call_stack: tokio::sync::Mutex::new(Vec::new()),
            loop_limit: None,
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            flags: InterpreterFlags::default(),
            output_limit: None,
        }
    }

    pub async fn increment_loops(&self) -> Result<(), CommandError> {
        match self.loop_limit {
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
}
