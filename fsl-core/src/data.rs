use std::{
    borrow::Cow,
    collections::HashMap,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

pub const DEFAULT_OUTPUT_LIMIT: usize = u16::MAX as usize;

use tokio::sync::Mutex;

use crate::{
    error::CommandError,
    types::{command::UserCommand, value::Value},
    vars::{DEFAULT_MEMORY_LIMIT, VarStack},
};

pub type UserCommands<'c> = HashMap<Cow<'c, str>, UserCommand<'c>>;

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

#[derive(Debug)]
pub struct InterpreterData<'c> {
    pub(crate) call_stack: Mutex<Vec<&'c str>>,

    pub args: Mutex<Vec<Value<'static>>>,
    pub output_limit: Option<usize>,
    pub output: Mutex<String>,

    pub vars: VarStack<'c>,
    pub user_commands: Mutex<UserCommands<'c>>,

    pub loop_limit: Option<usize>,
    pub total_loops: AtomicUsize,
    pub loop_depth: AtomicUsize,

    pub flags: InterpreterFlags,
}

impl<'c> InterpreterData<'c> {
    pub fn new_bounded(args: &Vec<String>) -> Self {
        let args = args.iter().map(|s| Value::from(s.to_owned())).collect();
        InterpreterData {
            args: Mutex::new(args),
            output: Mutex::new(String::new()),
            vars: VarStack::new_bounded(DEFAULT_MEMORY_LIMIT),
            user_commands: Mutex::new(UserCommands::new()),
            call_stack: Mutex::new(Vec::new()),
            loop_limit: Some(u16::MAX as usize),
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            flags: InterpreterFlags::default(),
            output_limit: Some(DEFAULT_OUTPUT_LIMIT),
        }
    }

    pub fn new_unbounded(args: &Vec<String>) -> Self {
        let args = args.iter().map(|s| Value::from(s.to_owned())).collect();
        InterpreterData {
            args: Mutex::new(args),
            output: Mutex::new(String::new()),
            vars: VarStack::new_unbounded(),
            user_commands: Mutex::new(UserCommands::new()),
            call_stack: Mutex::new(Vec::new()),
            loop_limit: None,
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            flags: InterpreterFlags::default(),
            output_limit: None,
        }
    }

    pub fn from_bounded(
        input: Vec<Value<'static>>,
        loop_limit: usize,
        total_loops: usize,
        output_limit: usize,
        current_out_len: usize,
    ) -> Self {
        InterpreterData {
            args: Mutex::new(input),
            output: Mutex::new(String::new()),
            vars: VarStack::new_unbounded(),
            user_commands: Mutex::new(UserCommands::new()),
            call_stack: Mutex::new(Vec::new()),
            loop_limit: Some(loop_limit),
            total_loops: AtomicUsize::new(total_loops),
            loop_depth: AtomicUsize::new(0),
            flags: InterpreterFlags::default(),
            output_limit: Some(output_limit - current_out_len),
        }
    }

    pub fn from_unbounded(input: Vec<Value<'static>>) -> Self {
        InterpreterData {
            args: Mutex::new(input),
            output: Mutex::new(String::new()),
            vars: VarStack::new_unbounded(),
            user_commands: Mutex::new(UserCommands::new()),
            call_stack: Mutex::new(Vec::new()),
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
}
