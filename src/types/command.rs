use core::fmt;
use std::{collections::VecDeque, ops::Range, pin::Pin, sync::Arc};

use crate::{
    ErrorContext, FslError, InterpreterData,
    commands::EXIT,
    types::{FslType, value::Value},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ArgPos {
    Index(usize),
    OptionalIndex(usize),
    Range(Range<usize>),
    AnyAfter(usize),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgRule {
    pub position: ArgPos,
    pub valid_types: &'static [FslType],
}

impl ArgRule {
    pub const fn new(position: ArgPos, valid_types: &'static [FslType]) -> Self {
        Self {
            position,
            valid_types,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CommandSpec {
    label: &'static str,
    arg_rules: &'static [ArgRule],
}

impl CommandSpec {
    pub fn new(label: &'static str, arg_rules: &'static [ArgRule]) -> Self {
        Self { label, arg_rules }
    }
}

pub trait CommandFn: Send + Sync {
    fn execute(
        &self,
        command: Command,
        interpreter: Arc<InterpreterData>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, FslError>> + Send + '_>>;
}

impl<F, Fut> CommandFn for F
where
    F: Fn(Command, Arc<InterpreterData>) -> Fut + Send + Sync,
    Fut: Future<Output = Result<Value, FslError>> + Send + 'static,
{
    fn execute(
        &self,
        command: Command,
        data: Arc<InterpreterData>,
    ) -> Pin<Box<dyn Future<Output = Result<Value, FslError>> + Send + '_>> {
        Box::pin(self(command, data))
    }
}

pub type Executor = Option<Arc<dyn CommandFn>>;

pub struct Command {
    command_spec: CommandSpec,
    args: VecDeque<Value>,
    executor: Executor,
}

impl Command {
    pub fn new(command_spec: CommandSpec, executor: Executor) -> Self {
        Self {
            command_spec,
            args: VecDeque::new(),
            executor,
        }
    }

    pub fn get_label(&self) -> &'static str {
        self.command_spec.label
    }

    pub fn set_args(&mut self, args: VecDeque<Value>) {
        self.args = args;
    }

    pub fn take_args(self) -> VecDeque<Value> {
        self.args
    }

    pub fn get_args(&self) -> &VecDeque<Value> {
        &self.args
    }

    fn validate_arg_range(&self, arg_rule: &ArgRule, range: &Range<usize>) -> Result<(), FslError> {
        for i in range.start..range.end {
            let arg = &self.args[i];
            let fsl_type = arg.as_type();
            if !arg_rule.valid_types.contains(&fsl_type) {
                return Err(FslError::IncorrectArgs(ErrorContext::new(
                    self.get_label().to_string(),
                    format!(
                        "Arg {} of command {} cannot be of type {}\nValid types are {:?}",
                        i,
                        self.get_label().to_string(),
                        fsl_type.as_str(),
                        arg_rule.valid_types
                    ),
                )));
            }
        }
        Ok(())
    }

    fn validate_args(&self) -> Result<(), FslError> {
        let mut max_args = 0;
        for arg_rule in self.command_spec.arg_rules {
            match &arg_rule.position {
                ArgPos::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.validate_arg_range(arg_rule, &range)?;
                        }
                        None => {
                            return Err(FslError::IncorrectArgs(ErrorContext::new(
                                self.get_label().to_string(),
                                format!(
                                    "Arg {} of command {} must be present and be of type {:?}",
                                    i,
                                    self.get_label(),
                                    arg_rule.valid_types
                                ),
                            )));
                        }
                    }
                }
                ArgPos::Range(range) => {
                    max_args = if max_args < range.end {
                        range.end
                    } else {
                        max_args
                    };
                    if self.args.len() < range.start {
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!(
                                "Command {} must have at least {} arguments and only {} were given",
                                self.get_label(),
                                range.start,
                                self.args.len(),
                            ),
                        )));
                    } else if self.args.len() > range.end {
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!(
                                "Command {} must have no more than {} arguments and {} were given",
                                self.get_label(),
                                range.end,
                                self.args.len(),
                            ),
                        )));
                    } else {
                        self.validate_arg_range(arg_rule, range)?;
                    }
                }
                ArgPos::None => {
                    if self.args.len() > 0 {
                        return Err(FslError::IncorrectArgs(ErrorContext::new(
                            self.get_label().to_string(),
                            format!("Command {} does not take any arguments", self.get_label()),
                        )));
                    }
                }
                ArgPos::AnyAfter(i) => {
                    max_args = usize::MAX;
                    let range = Range::from(*i..self.args.len());
                    self.validate_arg_range(arg_rule, &range)?;
                }
                ArgPos::OptionalIndex(i) => {
                    let range = *i..*i;
                    if let Some(_) = self.args.get(*i) {
                        self.validate_arg_range(arg_rule, &range)?;
                    }
                }
            }
        }

        if self.args.len() > max_args {
            return Err(FslError::IncorrectArgs(ErrorContext::new(
                self.get_label().to_string(),
                format!(
                    "Command {} expected {} args but got {}",
                    self.get_label(),
                    max_args,
                    self.args.len()
                ),
            )));
        }
        Ok(())
    }
    const EXECUTE_EXPECT: &str = "Command should always have executor before being consumed";
    pub async fn execute(mut self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if self.get_label() == EXIT {
            return Err(FslError::ProgramExited());
        }

        self.validate_args()?;

        let executor = self.executor.take().expect(Self::EXECUTE_EXPECT);
        Ok(executor.execute(self, data).await?)
    }

    pub async fn execute_clone(&self, data: Arc<InterpreterData>) -> Result<Value, FslError> {
        if self.get_label() == EXIT {
            return Err(FslError::ProgramExited());
        }

        self.validate_args()?;

        Ok(self
            .executor
            .clone()
            .expect(Self::EXECUTE_EXPECT)
            .execute(self.clone(), data)
            .await?)
    }
}

impl PartialEq for Command {
    fn eq(&self, other: &Self) -> bool {
        self.get_label() == other.get_label()
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.get_label())
            .finish()
    }
}

impl Clone for Command {
    fn clone(&self) -> Self {
        Self {
            command_spec: self.command_spec,
            args: self.args.clone(),
            executor: self.executor.clone(),
        }
    }
}
