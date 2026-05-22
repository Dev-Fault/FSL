use std::fmt::Debug;

use crate::parser::{ParseError, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorContext<T> {
    pub kind: T,
    pub display: String,
}

impl<T> ErrorContext<T> {
    pub fn new(kind: T, display: String) -> Self {
        Self { kind, display }
    }
}

impl<'c> From<ExecutionError<'c>> for ErrorContext<RuntimeError> {
    fn from(value: ExecutionError<'c>) -> Self {
        let display = value.to_string();
        let kind = value.command_error;
        Self { kind, display }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    Lex(String),
    Parse(String),
    Execution(ErrorContext<RuntimeError>),
    Runtime(RuntimeError),
    Import(String),
    UnmatchedCurlyBraces,
    Exit,
}

impl From<RuntimeError> for InterpreterError {
    fn from(value: RuntimeError) -> Self {
        InterpreterError::Runtime(value)
    }
}

impl<'c> From<ParseError<'c>> for InterpreterError {
    fn from(value: ParseError<'c>) -> Self {
        InterpreterError::Parse(value.to_string())
    }
}

impl<'c> From<ExecutionError<'c>> for InterpreterError {
    fn from(value: ExecutionError<'c>) -> Self {
        InterpreterError::Execution(ErrorContext::from(value))
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            InterpreterError::Lex(output) => output,
            InterpreterError::Parse(output) => output,
            InterpreterError::Execution(exeuction_error) => &exeuction_error.display,
            InterpreterError::Runtime(command_error) => &command_error.to_string(),
            InterpreterError::UnmatchedCurlyBraces => "unmatched curly braces",
            InterpreterError::Import(output) => output,
            InterpreterError::Exit => "",
        };
        write!(f, "Error: {}", output)
    }
}
impl std::error::Error for InterpreterError {}

#[derive(Debug, Clone, PartialEq)]
pub struct ExecutionError<'c> {
    pub command_error: RuntimeError,
    pub span: Span<'c>,
}

impl<'c> ExecutionError<'c> {
    pub fn new(command_error: RuntimeError, span: Span<'c>) -> Self {
        Self {
            command_error,
            span,
        }
    }
    pub fn exited_program(&self) -> bool {
        self.command_error.exited_program()
    }
}

impl<'c> std::fmt::Display for ExecutionError<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dbg!(self);
        let line_header = format!("{}: ", self.span.start.line_number());

        let prefix = &self.span.start.line()[..self.span.start.line_location()];
        let padding = " ".repeat(line_header.len())
            + &prefix
                .chars()
                .map(|c| if c == '\t' { '\t' } else { ' ' })
                .collect::<String>();
        write!(
            f,
            "{} on line {}\n{}{}\n{}^",
            self.command_error.to_string(),
            self.span.start.line_number(),
            line_header,
            self.span.start.line(),
            padding,
        )
    }
}

impl<'c> std::error::Error for ExecutionError<'c> {}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    WrongArgType(String),
    WrongArgCount(String),
    WrongArgOrder(String),
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
    SwitchMustHaveSingleFallbackCommand,
    ConditionFalse,
    MultipleThenCommandsInIf,
    MultipleElseCommandsInIf,
    InvalidCommandInIf,
    IfMustContainThen,
    ElseIfMustBePairedWithElse,
    InvalidArgument(String),
    Overflow,
    InvalidComparison(String),
    InvalidConversion(String),
    FailedParse(String),
    NonExistantVar(String),
    NotAVar(String),
    InvalidVarValue(String),
    NegativeIndex,
    VarMemoryLimitReached,
    AttemptToOverwriteConstant(String),
    AttemptToFreeConstant(String),
    EmptyMapPath(String),
    NotAMap(String),
    NotIndexable,
    MissingIndex,
    MissingKey,
    NonExistantKey(String),
    Custom(String),
}

impl<'c> From<ExecutionError<'c>> for RuntimeError {
    fn from(value: ExecutionError<'c>) -> Self {
        value.command_error
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output: &str = match self {
            RuntimeError::WrongArgType(error_text) => error_text,
            RuntimeError::WrongArgCount(error_text) => error_text,
            RuntimeError::WrongArgOrder(error_text) => error_text,
            RuntimeError::DivisionByZero => "cannot divide by zero",
            RuntimeError::LoopLimitReached => "maximum loop limit reached",
            RuntimeError::IndexOutOfBounds => "index out of bounds",
            RuntimeError::KeyNotPresentInMap(key) => &format!("key \"{}\" not present in map", key),
            RuntimeError::InvalidRange => "invalid range (min should be less than max)",
            RuntimeError::NonFiniteValue => "cannot use non finite value",
            RuntimeError::BreakOutsideLoop => "break can only be called inside loop",
            RuntimeError::ContinueOutsideLoop => "continue can only be called inside loop",
            RuntimeError::NonExistantCommand(error_text) => error_text,
            RuntimeError::ProgramExited => "",
            RuntimeError::Custom(error_text) => error_text,
            RuntimeError::SwitchMustHaveSingleFallbackCommand => {
                "switch statement must have single fallback command"
            }
            RuntimeError::ConditionFalse => "",
            RuntimeError::MultipleThenCommandsInIf => "if can only contain one then command",
            RuntimeError::MultipleElseCommandsInIf => "if can only contain one else command",
            RuntimeError::InvalidCommandInIf => {
                "if must only contain then, else_if, and else commands"
            }
            RuntimeError::IfMustContainThen => "if must contain a then command",
            RuntimeError::ElseIfMustBePairedWithElse => {
                "else if command(s) must be paired with else command"
            }
            RuntimeError::InvalidArgument(error_text) => error_text,
            RuntimeError::OutputLimitExceeded => "memory limit for print exceeded",
            RuntimeError::Overflow => "calculation resulted in overflow",
            RuntimeError::InvalidComparison(error_text) => error_text,
            RuntimeError::InvalidConversion(error_text) => error_text,
            RuntimeError::FailedParse(error_text) => error_text,
            RuntimeError::NonExistantVar(error_text) => error_text,
            RuntimeError::NotAVar(error_text) => error_text,
            RuntimeError::NegativeIndex => "index cannot be a negative value",
            RuntimeError::InvalidVarValue(error_text) => error_text,
            RuntimeError::VarMemoryLimitReached => "interpreter var memory limit reached",
            RuntimeError::AttemptToOverwriteConstant(error_text) => error_text,
            RuntimeError::AttemptToFreeConstant(error_text) => error_text,
            RuntimeError::EmptyMapPath(error_text) => error_text,
            RuntimeError::NotAMap(error_text) => error_text,
            RuntimeError::NotIndexable => "used index on value that cannot be indexed",
            RuntimeError::NonExistantKey(error_text) => error_text,
            RuntimeError::MissingIndex => "indexing requires at least one value",
            RuntimeError::MissingKey => "key is required to access map value",
        };

        write!(f, "{}", output)
    }
}

impl RuntimeError {
    pub fn exited_program(&self) -> bool {
        match self {
            RuntimeError::ProgramExited => true,
            _ => false,
        }
    }

    pub fn to_exec<'c>(self, span: Span<'c>) -> ExecutionError<'c> {
        ExecutionError {
            command_error: self,
            span,
        }
    }
}

impl From<tokio::task::JoinError> for RuntimeError {
    fn from(e: tokio::task::JoinError) -> Self {
        RuntimeError::Custom(format!("join error: {}", e))
    }
}

impl std::error::Error for RuntimeError {}
