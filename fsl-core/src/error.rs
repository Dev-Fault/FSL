use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterErrorType {
    Lex(String),
    Parse(String),
    Command(CommandError),
    Value(ValueError),
    Import(String),
    UnmatchedCurlyBraces,
}

impl std::fmt::Display for InterpreterErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            InterpreterErrorType::Lex(output) => output,
            InterpreterErrorType::Parse(output) => output,
            InterpreterErrorType::Command(command_error) => &command_error.to_string(),
            InterpreterErrorType::Value(value_error) => &value_error.to_string(),
            InterpreterErrorType::UnmatchedCurlyBraces => "unmatched curly braces",
            InterpreterErrorType::Import(output) => output,
        };
        write!(f, "{}", output)
    }
}

#[derive(Debug, Clone)]
pub struct InterpreterError {
    pub error_type: InterpreterErrorType,
    pub stack_trace: Option<String>,
}

impl InterpreterError {
    pub fn new(error_type: InterpreterErrorType, stack_trace: Option<String>) -> Self {
        Self {
            error_type,
            stack_trace,
        }
    }
}

impl From<InterpreterErrorType> for InterpreterError {
    fn from(value: InterpreterErrorType) -> Self {
        Self {
            error_type: value,
            stack_trace: None,
        }
    }
}

impl From<CommandError> for InterpreterError {
    fn from(value: CommandError) -> Self {
        Self {
            error_type: InterpreterErrorType::Command(value),
            stack_trace: None,
        }
    }
}

impl From<ValueError> for InterpreterError {
    fn from(value: ValueError) -> Self {
        Self {
            error_type: InterpreterErrorType::Value(value),
            stack_trace: None,
        }
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.stack_trace {
            Some(trace) => write!(f, "{}\nError inside command:\n{}", self.error_type, trace),
            None => write!(f, "{}", self.error_type),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum CommandError {
    WrongArgType(String),
    WrongArgCount(String),
    WrongArgOrder(String),
    ValueError(ValueError),
    DivisionByZero,
    LoopLimitReached,
    OutputLimitExceeded,
    IndexOutOfBounds,
    KeyNotPresentInMap(String),
    InvalidRange,
    NonFiniteValue,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    NonExistantCommand(String),
    ProgramExited,
    Custom(String),
    SwitchMustHaveSingleFallbackCommand,
    ConditionFalse,
    MultipleThenCommandsInIf,
    MultipleElseCommandsInIf,
    InvalidCommandInIf,
    IfMustContainThen,
    ElseIfMustBePairedWithElse,
    InvalidArgument(String),
    Overflow,
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output: &str = match self {
            CommandError::WrongArgType(error_text) => error_text,
            CommandError::WrongArgCount(error_text) => error_text,
            CommandError::WrongArgOrder(error_text) => error_text,
            CommandError::ValueError(value_error) => &value_error.to_string(),
            CommandError::DivisionByZero => "cannot divide by zero",
            CommandError::LoopLimitReached => "maximum loop limit reached",
            CommandError::IndexOutOfBounds => "index out of bounds",
            CommandError::KeyNotPresentInMap(key) => &format!("key \"{}\" not present in map", key),
            CommandError::InvalidRange => "invalid range (min should be less than max)",
            CommandError::NonFiniteValue => "cannot use non finite value",
            CommandError::BreakOutsideLoop => "break can only be called inside loop",
            CommandError::ContinueOutsideLoop => "continue can only be called inside loop",
            CommandError::NonExistantCommand(error_text) => error_text,
            CommandError::ProgramExited => "",
            CommandError::Custom(error_text) => error_text,
            CommandError::SwitchMustHaveSingleFallbackCommand => {
                "switch statement must have single fallback command"
            }
            CommandError::ConditionFalse => "",
            CommandError::MultipleThenCommandsInIf => "if can only contain one then command",
            CommandError::MultipleElseCommandsInIf => "if can only contain one else command",
            CommandError::InvalidCommandInIf => {
                "if must only contain then, else_if, and else commands"
            }
            CommandError::IfMustContainThen => "if must contain a then command",
            CommandError::ElseIfMustBePairedWithElse => {
                "else if command(s) must be paired with else command"
            }
            CommandError::InvalidArgument(error_text) => error_text,
            CommandError::OutputLimitExceeded => "memory limit for print exceeded",
            CommandError::Overflow => "calculation resulted in overflow",
        };

        write!(f, "{}", output)
    }
}

impl From<ExecutionError> for CommandError {
    fn from(value: ExecutionError) -> Self {
        match value {
            ExecutionError::ValueError(value_error) => Self::ValueError(value_error),
            ExecutionError::CommandError(command_error) => command_error,
        }
    }
}

impl CommandError {
    pub fn exited_program(&self) -> bool {
        match self {
            CommandError::ProgramExited => true,
            _ => false,
        }
    }
}

impl From<ValueError> for CommandError {
    fn from(value: ValueError) -> Self {
        CommandError::ValueError(value)
    }
}

impl std::error::Error for CommandError {}

#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionError {
    ValueError(ValueError),
    CommandError(CommandError),
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            ExecutionError::ValueError(value_error) => value_error.to_string(),
            ExecutionError::CommandError(command_error) => command_error.to_string(),
        };

        write!(f, "{}", output)
    }
}

impl std::error::Error for ExecutionError {}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueError {
    InvalidComparison(String),
    InvalidConversion(String),
    FailedParse(String),
    NonExistantVar(String),
    InvalidVarName(String),
    InvalidVarValue(String),
    NegativeIndex(String),
    VarMemoryLimitReached,
    AttemptToOverwriteConstant(String),
    AttemptToFreeConstant(String),
    EmptyMapPath(String),
    NotAMap(String),
    NotAList(String),
    NonExistantKey(String),
    IndexOutOfBounds(String),
}

impl From<ValueError> for ExecutionError {
    fn from(value: ValueError) -> Self {
        Self::ValueError(value)
    }
}

impl From<CommandError> for ExecutionError {
    fn from(value: CommandError) -> Self {
        Self::CommandError(value)
    }
}

impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_text = match self {
            ValueError::InvalidComparison(error_text) => error_text,
            ValueError::InvalidConversion(error_text) => error_text,
            ValueError::FailedParse(error_text) => error_text,
            ValueError::NonExistantVar(error_text) => error_text,
            ValueError::InvalidVarName(error_text) => error_text,
            ValueError::NegativeIndex(error_text) => error_text,
            ValueError::InvalidVarValue(error_text) => error_text,
            ValueError::VarMemoryLimitReached => {
                &"interpreter var memory limit reached".to_string()
            }
            ValueError::AttemptToOverwriteConstant(error_text) => error_text,
            ValueError::AttemptToFreeConstant(error_text) => error_text,
            ValueError::EmptyMapPath(error_text) => error_text,
            ValueError::NotAMap(error_text) => error_text,
            ValueError::NotAList(error_text) => error_text,
            ValueError::NonExistantKey(error_text) => error_text,
            ValueError::IndexOutOfBounds(error_text) => error_text,
        };

        write!(f, "{}", error_text)
    }
}

impl std::error::Error for ValueError {}
