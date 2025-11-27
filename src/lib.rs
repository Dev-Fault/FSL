use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

use async_recursion::async_recursion;
use std::sync::Mutex;

use crate::{
    commands::*,
    lexer::LexerError,
    parser::{Expression, Parser, ParserError},
    types::{
        FslType,
        command::{ArgRule, Command, CommandError, CommandSpec, Executor, UserCommand},
        value::{Value, ValueError},
    },
};

pub mod commands;
mod lexer;
mod parser;
pub mod types;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FslError {
    error_type: InterpreterError,
    error_text: String,
}

impl FslError {
    pub fn new(error_type: InterpreterError, error_text: String) -> Self {
        Self {
            error_type,
            error_text,
        }
    }

    pub fn to_string(self) -> String {
        self.error_text
    }
}

#[derive(Debug, Clone)]
pub enum InterpreterError {
    LexerError(String),
    ParserError(String),
    CommandError(CommandError),
    UnmatchedCurlyBraces,
}

impl InterpreterError {
    pub fn to_string(self) -> String {
        match self {
            InterpreterError::LexerError(error_text) => error_text,
            InterpreterError::ParserError(error_text) => error_text,
            InterpreterError::CommandError(command_error) => command_error.to_string(),
            InterpreterError::UnmatchedCurlyBraces => "unmatched curly braces".into(),
        }
    }
}

impl From<CommandError> for InterpreterError {
    fn from(value: CommandError) -> Self {
        InterpreterError::CommandError(value)
    }
}

impl<'a> From<LexerError<'a>> for InterpreterError {
    fn from(value: LexerError) -> Self {
        Self::LexerError(value.to_string())
    }
}

impl<'a> From<ParserError<'a>> for InterpreterError {
    fn from(value: ParserError<'a>) -> Self {
        Self::ParserError(value.to_string())
    }
}

pub type CommandMap = HashMap<&'static str, Command>;
pub type UserCommands = HashMap<String, UserCommand>;

const MEGABYTE: usize = 1024 * 1024;
pub const DEFAULT_MEMORY_LIMIT: usize = 100 * MEGABYTE;

#[derive(Debug)]
pub struct VarMap {
    map: Mutex<HashMap<String, Value>>,
    mem_limit: Option<usize>,
    allocated_mem: AtomicUsize,
}

impl VarMap {
    pub fn new(mem_limit: Option<usize>) -> Self {
        Self {
            map: Mutex::new(HashMap::new()),
            mem_limit: mem_limit,
            allocated_mem: AtomicUsize::new(0),
        }
    }

    /// Adds size of value to currently allocated memory size, does nothing if mem_limit not set
    fn add_allocated_mem(&self, value_size: Option<usize>) -> Result<(), ValueError> {
        if self.mem_limit.is_none() {
            return Ok(());
        }

        if let Some(size) = value_size {
            if let Some(allocated_mem) =
                self.allocated_mem.load(Ordering::Relaxed).checked_add(size)
            {
                self.allocated_mem.store(allocated_mem, Ordering::Relaxed);
                if self.allocated_mem.load(Ordering::Relaxed) > self.mem_limit.unwrap() {
                    return Err(ValueError::VarMemoryLimitReached);
                }
            } else {
                return Err(ValueError::VarMemoryLimitReached);
            }
        } else {
            return Err(ValueError::VarMemoryLimitReached);
        }

        Ok(())
    }

    fn sub_allocated_mem(&self, value_size: Option<usize>) {
        if let Some(size) = value_size {
            let allocated_mem = self
                .allocated_mem
                .load(Ordering::Relaxed)
                .saturating_sub(size);

            self.allocated_mem.store(allocated_mem, Ordering::Relaxed);
        }
    }

    pub fn insert_value(&self, label: &str, value: Value) -> Result<(), ValueError> {
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
                if self.mem_limit.is_some() {
                    self.add_allocated_mem(value.mem_size())?;
                }
                let lock = self.map.lock();
                let mut map = lock.unwrap();
                map.insert(label.to_string(), value.clone());
                Ok(())
            }
        }
    }

    pub fn remove_value(&self, label: &str) -> Option<Value> {
        let lock = self.map.lock();
        let mut map = lock.unwrap();
        let value = map.get(label)?;

        if self.mem_limit.is_some() {
            self.sub_allocated_mem(value.mem_size());
        }
        map.remove(label)
    }

    pub fn clone_value(&self, label: &str) -> Result<Value, ValueError> {
        let value = self.map.lock().unwrap().get(label).cloned();

        match value {
            Some(value) => {
                if value.is_type(FslType::Var) {
                    return self.clone_value(&value.get_var_label()?);
                } else {
                    Ok(value)
                }
            }
            None => Err(ValueError::NonExistantVar(format!(
                "cannot get value of a non existant var"
            ))),
        }
    }

    pub fn get_type(&self, label: &str) -> Result<FslType, ValueError> {
        let lock = self.map.lock();
        let map = lock.unwrap();
        let value = map.get(label);

        match value {
            Some(value) => {
                if value.is_type(FslType::Var) {
                    return self.get_type(&value.get_var_label()?);
                } else {
                    Ok(value.as_type())
                }
            }
            None => Err(ValueError::NonExistantVar(format!(
                "cannot get type of non existant var"
            ))),
        }
    }
}

pub struct InterpreterData {
    pub output: tokio::sync::Mutex<String>,
    pub vars: VarMap,
    pub user_commands: tokio::sync::Mutex<UserCommands>,
    call_stack: tokio::sync::Mutex<Vec<String>>,
    pub total_loop_limit: Option<usize>,
    pub total_loops: AtomicUsize,
    pub inside_loop: AtomicBool,
    pub break_flag: AtomicBool,
    pub continue_flag: AtomicBool,
}

impl InterpreterData {
    pub fn new() -> Self {
        InterpreterData {
            output: tokio::sync::Mutex::new(String::new()),
            vars: VarMap::new(Some(DEFAULT_MEMORY_LIMIT)),
            user_commands: tokio::sync::Mutex::new(UserCommands::new()),
            call_stack: tokio::sync::Mutex::new(Vec::new()),
            total_loop_limit: Some(u16::MAX as usize),
            total_loops: AtomicUsize::new(0),
            inside_loop: AtomicBool::new(false),
            break_flag: AtomicBool::new(false),
            continue_flag: AtomicBool::new(false),
        }
    }

    pub async fn increment_loops(&self) -> Result<(), CommandError> {
        match self.total_loop_limit {
            Some(limit) => {
                let mut loops = self.total_loops.load(Ordering::Relaxed);
                loops += 1;
                self.total_loops.store(loops, Ordering::Relaxed);
                if loops >= limit {
                    Err(CommandError::LoopLimitReached)
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }
}

pub struct FslInterpreter {
    pub commands: CommandMap,
    pub data: Arc<InterpreterData>,
}

macro_rules! register_commands {
    ($self:expr, [
        $( ($label:expr, $rules:expr, $executor:path) ),* $(,)?
    ]) => {
        $(
            $self.add_command($label, $rules, Self::construct_executor($executor));
        )*
    };
}

impl FslInterpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            commands: CommandMap::new(),
            data: Arc::new(InterpreterData::new()),
        };
        interpreter.add_standard_commands();
        interpreter
    }

    pub fn reset_data(&mut self) {
        self.data = Arc::new(InterpreterData::new());
    }

    pub fn add_command(
        &mut self,
        label: &'static str,
        rules: &'static [ArgRule],
        executor: Executor,
    ) {
        self.commands.insert(
            label,
            Command::new(CommandSpec::new(label, rules), executor),
        );
    }

    pub fn construct_executor<F, Fut>(closure: F) -> Executor
    where
        F: Fn(Command, Arc<InterpreterData>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Value, CommandError>> + Send + 'static,
    {
        Some(Arc::new(move |command: Command, vars| {
            Box::pin(closure(command, vars))
        }))
    }

    pub async fn interpret<'a>(&self, code: &'a str) -> Result<String, FslError> {
        self.evaluate_expressions(code).await
    }

    pub async fn interpret_embedded_code(&mut self, input: &str) -> Result<String, FslError> {
        let mut output = String::with_capacity(input.len());
        let mut code_stack: Vec<String> = Vec::new();

        let mut code_depth: i16 = 0;

        for c in input.chars() {
            if c == '{' {
                code_stack.push(String::new());
                code_depth += 1;
            } else if c == '}' {
                code_depth -= 1;
                if code_depth < 0 {
                    return Err(FslError::new(
                        InterpreterError::UnmatchedCurlyBraces,
                        InterpreterError::UnmatchedCurlyBraces.to_string(),
                    ));
                } else {
                    match code_stack.pop() {
                        Some(code) => {
                            self.reset_data();
                            match self.interpret(&code).await {
                                Ok(eval) => match code_stack.last_mut() {
                                    Some(code) => code.push_str(&eval),
                                    None => output.push_str(&eval),
                                },
                                Err(e) => return Err(e),
                            };
                        }
                        None => {}
                    }
                }
            } else if code_depth == 0 {
                output.push(c);
            } else {
                match code_stack.last_mut() {
                    Some(s) => s.push(c),
                    None => {}
                }
            }
        }

        if code_depth != 0 {
            return Err(FslError::new(
                InterpreterError::UnmatchedCurlyBraces,
                InterpreterError::UnmatchedCurlyBraces.to_string(),
            ));
        }

        Ok(output)
    }

    async fn evaluate_expressions<'a>(&self, code: &'a str) -> Result<String, FslError> {
        let expressions = Parser::new().parse(code);
        match expressions {
            Ok(expressions) => {
                for expression in expressions {
                    let command = match self.parse_expression(expression.clone()).await {
                        Ok(value) => match value {
                            Value::Command(command) => command,
                            _ => unreachable!("parse expression should always return a command"),
                        },
                        Err(e) => {
                            return Err(FslError::new(
                                InterpreterError::CommandError(e.clone()),
                                format!(
                                    "{}\nError inside command: {}",
                                    e.to_string(),
                                    self.call_stack_to_string().await
                                ),
                            ));
                        }
                    };
                    if let Err(e) = command.execute(self.data.clone()).await {
                        match e {
                            CommandError::ProgramExited => break,
                            _ => {
                                return Err(FslError::new(
                                    InterpreterError::CommandError(e.clone()),
                                    format!(
                                        "{}\nError inside command: {}",
                                        e.to_string(),
                                        self.call_stack_to_string().await
                                    ),
                                ));
                            }
                        }
                    }
                }
            }
            Err(e) => {
                return Err(FslError::new(
                    InterpreterError::ParserError(e.to_string()),
                    e.to_string(),
                ));
            }
        }
        Ok(self.data.output.lock().await.clone())
    }

    async fn call_stack_to_string(&self) -> String {
        let call_stack = self.data.call_stack.lock().await;
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

    async fn parse_expression(&self, expression: Expression) -> Result<Value, CommandError> {
        if let Some(command) = self.commands.get(expression.name.as_str()) {
            let mut command = command.clone();

            let mut args: VecDeque<Value> = VecDeque::with_capacity(expression.args.len());

            for arg in expression.args {
                args.push_back(self.parse_arg(arg).await?);
            }

            command.set_args(args);

            Ok(Value::Command(command))
        } else if let Some(user_command) = self
            .data
            .user_commands
            .lock()
            .await
            .get(expression.name.as_str())
        {
            let mut command = Command::new(
                CommandSpec::new(&expression.name, RUN_RULES),
                Self::construct_executor(commands::run),
            );

            let mut args = VecDeque::new();
            args.push_back(Value::Var(user_command.label.clone()));

            for arg in expression.args {
                args.push_back(self.parse_arg(arg).await?);
            }

            command.set_args(args);

            Ok(Value::Command(command))
        } else {
            return Err(CommandError::NonExistantCommand(format!(
                "command with name {} does not exist",
                expression.name
            )));
        }
    }

    #[async_recursion]
    async fn parse_arg(&self, arg: parser::Arg) -> Result<Value, ValueError> {
        match arg {
            parser::Arg::Number(number) => {
                if number.contains('.') {
                    match number.parse::<f64>() {
                        Ok(value) => Ok(Value::Float(value)),
                        Err(_) => Err(ValueError::FailedParse(
                            "failed to convert to a number".into(),
                        )),
                    }
                } else {
                    if let Ok(value) = number.parse::<i64>() {
                        Ok(Value::Int(value))
                    } else {
                        if let Ok(value) = number.parse::<f64>() {
                            Ok(Value::Float(value))
                        } else {
                            Err(ValueError::FailedParse(
                                "failed to convert to a number".into(),
                            ))
                        }
                    }
                }
            }
            parser::Arg::String(text) => Ok(Value::Text(text)),
            parser::Arg::Keyword(keyword) => match keyword {
                lexer::Keyword::True => Ok(Value::Bool(true)),
                lexer::Keyword::False => Ok(Value::Bool(false)),
            },
            parser::Arg::Var(var) => Ok(Value::Var(var)),
            parser::Arg::List(args) => {
                let mut list: Vec<Value> = vec![];
                for arg in args {
                    let parsed_arg = self.parse_arg(arg).await?;
                    list.push(parsed_arg);
                }
                Ok(Value::List(list))
            }
            parser::Arg::Expression(expression) => Ok(self.parse_expression(expression).await?),
        }
    }

    fn add_standard_commands(&mut self) {
        register_commands!(
            self,
            [
                (ADD, MATH_RULES, commands::add),
                (SUB, MATH_RULES, commands::sub),
                (MUL, MATH_RULES, commands::mul),
                (DIV, MATH_RULES, commands::div),
                (MODULUS, MATH_RULES, commands::modulus),
                (PRECISION, PRECISION_RULES, commands::precision),
                (STORE, STORE_RULES, commands::store),
                (CLONE, CLONE_RULES, commands::clone),
                (FREE, FREE_RULES, commands::free),
                (PRINT, PRINT_RULES, commands::print),
                (SCOPE, SCOPE_RULES, commands::scope),
                (EQ, EQ_RULES, commands::eq),
                (GT, GT_RULES, commands::gt),
                (LT, LT_RULES, commands::lt),
                (NOT, NOT_RULES, commands::not),
                (AND, AND_RULES, commands::and),
                (OR, OR_RULES, commands::or),
                (IF_THEN, IF_THEN_RULES, commands::if_then),
                (IF_THEN_ELSE, IF_THEN_ELSE_RULES, commands::if_then_else),
                (WHILE_LOOP, WHILE_RULES, commands::while_command),
                (REPEAT, REPEAT_RULES, commands::repeat),
                (INDEX, INDEX_RULES, commands::index),
                (LENGTH, LENGTH_RULES, commands::length),
                (SWAP, SWAP_RULES, commands::swap),
                (INSERT, INSERT_RULES, commands::insert),
                (REMOVE, REMOVE_RULES, commands::remove),
                (PUSH, PUSH_RULES, commands::push),
                (POP, POP_RULES, commands::pop),
                (REPLACE, REPLACE_RULES, commands::replace),
                (SLICE_REPLACE, SLICE_REPLACE_RULES, commands::slice_replace),
                (
                    SEARCH_REPLACE,
                    SEARCH_REPLACE_RULES,
                    commands::search_replace
                ),
                (REVERSE, REVERSE_RULES, commands::reverse),
                (INC, INC_RULES, commands::inc),
                (DEC, DEC_RULES, commands::dec),
                (CONTAINS, CONTAINS_RULES, commands::contains),
                (STARTS_WITH, STARTS_WITH_RULES, commands::starts_with),
                (ENDS_WITH, ENDS_WITH_RULES, commands::ends_with),
                (CONCAT, CONCAT_RULES, commands::concat),
                (CAPITALIZE, CAPITALIZE_RULES, commands::capitalize),
                (UPPERCASE, UPPERCASE_RULES, commands::uppercase),
                (LOWERCASE, LOWERCASE_RULES, commands::lowercase),
                (IS_NUMBER, IS_NUMBER_RULES, commands::is_number),
                (IS_NONE, IS_NONE_RULES, commands::is_none),
                (IS_ALPHA, IS_ALPHA_RULES, commands::is_alpha),
                (IS_ALPHA_EN, IS_ALPHA_EN_RULES, commands::is_alpha_en),
                (IS_WHITESPACE, IS_WHITESPACE_RULES, commands::is_whitespace),
                (
                    REMOVE_WHITESPACE,
                    REMOVE_WHITESPACE_RULES,
                    commands::remove_whitespace
                ),
                (SPLIT, SPLIT_RULES, commands::split),
                (RANDOM_RANGE, RANDOM_RANGE_RULES, commands::random_range),
                (SLEEP, SLEEP_RULES, commands::sleep),
                (RANDOM_ENTRY, RANDOM_ENTRY_RULES, commands::random_entry),
                (DEF, DEF_RULES, commands::def),
                (EXIT, NO_ARGS, commands::exit),
                (BREAK, NO_ARGS, commands::break_command),
                (CONTINUE, NO_ARGS, commands::continue_command),
            ]
        );
    }
}

impl Default for FslInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod interpreter {
    use crate::commands::tests::{
        test_interpreter, test_interpreter_embedded, test_interpreter_err_type,
    };
    use crate::types::value::Value;
    use crate::{FslInterpreter, InterpreterError};

    async fn test_interpreter_err(code: &str) {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        assert!(result.is_err());
    }

    async fn test_interpreter_not_err(code: &str) {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn handle_print_overflow() {
        test_interpreter(
            "print(1000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000)",
            "inf",
        )
        .await;
    }

    #[tokio::test]
    async fn handle_int_overflow() {
        test_interpreter_not_err(
            "print(random_range(
            100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
            100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000000000000000))",
        )
        .await;
    }

    #[tokio::test]
    async fn mem_size_of_int() {
        let interpreter = FslInterpreter::new();
        interpreter.interpret("n.store(1)").await.unwrap();
        dbg!(
            interpreter
                .data
                .vars
                .allocated_mem
                .load(std::sync::atomic::Ordering::Relaxed)
        );
        assert!(
            interpreter
                .data
                .vars
                .allocated_mem
                .load(std::sync::atomic::Ordering::Relaxed)
                == size_of::<Value>()
        );
    }

    #[tokio::test]
    async fn mem_size_of_list() {
        let interpreter = FslInterpreter::new();
        interpreter.interpret(r#"n.store([1,2,3])"#).await.unwrap();
        dbg!(
            interpreter
                .data
                .vars
                .allocated_mem
                .load(std::sync::atomic::Ordering::Relaxed)
        );
        assert!(
            interpreter
                .data
                .vars
                .allocated_mem
                .load(std::sync::atomic::Ordering::Relaxed)
                == size_of::<Value>() * 4
        );
    }

    #[tokio::test]
    async fn catch_memory_overflow() {
        let err = test_interpreter_err_type(
            r#"
            big.store("123456789")
            repeat(1000, big.store(concat(big, big)))
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterError::CommandError(_)))
    }

    #[tokio::test]
    async fn hello_world() {
        test_interpreter("print(\"Hello, world!\")", "Hello, world!").await;
    }

    #[tokio::test]
    async fn single_line_comment() {
        test_interpreter(
            "
            # This will print \"Hello, world!\"
            print(\"Hello, world!\") # comments are completely ignored
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn multi_line_comment() {
        test_interpreter(
            "
            *
            This will print \"Hello, world!\"
            These comments are completely ignored by the interpreter
            they are only here for utility
            *
            print(\"Hello, world!\") * comments are completely ignored *
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_place_comment() {
        test_interpreter_err(
            "
            print(# comment right here \"Hello, world!\") 
            ",
        )
        .await;
    }

    #[tokio::test]
    async fn mixed_comments() {
        test_interpreter(
            "
            *
            This will print \"Hello, world!\"
            These comments are completely ignored by the interpreter
            # single lines inside multis should be ignored
            they are only here for utility
            *
            # multis inside single * lines should be ignored
            print(\"Hello, world!\") * comments are completely ignored *
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn print_list() {
        test_interpreter(r#"print([1, 2, 3])"#, r#"[1, 2, 3]"#).await;
    }

    #[tokio::test]
    async fn print_var_list() {
        test_interpreter(
            r#"a.store(0), b.store(0), c.store(0), print([a, b, c])"#,
            r#"[0, 0, 0]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_matrix() {
        test_interpreter(
            r#"print([[1, 2, 3], [4, 5, 6], [7, 8, 9]])"#,
            r#"[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_empty_list() {
        test_interpreter(r#"print([])"#, r#"[]"#).await;
    }

    #[tokio::test]
    async fn print_list_with_one_item() {
        test_interpreter(r#"print([1])"#, r#"[1]"#).await;
    }

    #[tokio::test]
    async fn interpret_embedded() {
        test_interpreter_embedded(
            r#"{print("hello", "{print(",", "{print(" world")}")}")}"#,
            "hello, world",
        )
        .await;
    }

    #[tokio::test]
    async fn split_command() {
        test_interpreter(
            r#"print(split("some text to split by whitespace", " "))"#,
            r#"[some, text, to, split, by, whitespace]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_command() {
        test_interpreter(
            r#"
            a.store("test")
            b.store([1, 2, 3, a])
            print(a.contains("p"), " ")
            print(a.contains("t"), " ")
            print(b.contains(a), " ")
            print(b.contains(1), " ")
            print(b.contains(5), " ")
            print(b.contains("test"))
            "#,
            r#"false true true true false true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_sub_array() {
        test_interpreter(
            r#"
            a.store([[1, 2], [3, 4], [5, 6]])
            print(a.contains([1, 3]))
            print(" ")
            print(a.contains([1, 2]))
            "#,
            r#"false true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_deep_array() {
        test_interpreter(
            r#"
            a.store([[1, 2], [[1, 3], 4], [5, 6]])
            print(a.contains([1, 3]))
            "#,
            r#"false"#,
        )
        .await;
    }

    #[tokio::test]
    async fn clone_command() {
        test_interpreter(
            r#"
            a.store(0)
            b.store(1)
            list.store([a.clone(), b.clone()])
            print(list.index(0), " ", list.index(1))
            "#,
            r#"0 1"#,
        )
        .await;
    }

    #[tokio::test]
    async fn inc_command() {
        test_interpreter(
            r#"
                i.store(0)
                i.inc()
                print(i)
                i.inc()
                print(i)
            "#,
            r#"12"#,
        )
        .await;
    }

    #[tokio::test]
    async fn dec_command() {
        test_interpreter(
            r#"
                i.store(0)
                i.dec()
                print(i)
                i.dec()
                print(i)
            "#,
            r#"-1-2"#,
        )
        .await;
    }

    #[tokio::test]
    async fn var_insert() {
        test_interpreter(
            r#"
                i.store([1, 2])
                i.push(3)
                print(i.index(2))
            "#,
            r#"3"#,
        )
        .await;
    }

    #[tokio::test]
    async fn push_pop() {
        test_interpreter(
            r#"
                text.store("hello")
                text.push("o")
                print(text)
                print("\n")
                text.pop()
                print(text)
            "#,
            "helloo\nhello",
        )
        .await;
    }

    #[tokio::test]
    async fn matrix_manipulation() {
        test_interpreter(
            "
                matrix.store([[1, 2, 3], [\"#\", \"#\", \"#\"], [7, 8, 9]])
                matrix.replace(1, matrix.index(1).replace(1, \"X\"))
                print(matrix.index(1).index(1))
            ",
            r#"X"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_iterated_list() {
        test_interpreter(
            r#"

            names.store(["John", "James", "Joseph", "Alexander"])
            i.store(0)
            repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))

            "#,
            "John\nJames\nJoseph\nAlexander\n",
        )
        .await;
    }

    #[tokio::test]
    async fn float_vars() {
        test_interpreter(
            r#"

            x.store(2)
            y.store(0.5)
            print(add(x, y))

            "#,
            "2.5",
        )
        .await;
    }

    #[tokio::test]
    async fn float_commands() {
        test_interpreter(
            r#"

            print(add(add(1, 1.5), add(1, 2)))

            "#,
            "5.5",
        )
        .await;
    }

    #[tokio::test]
    async fn invalid_command_arg() {
        test_interpreter_err(
            r#"

            print(add(print(2), print(2)))

            "#,
        )
        .await;
    }

    #[tokio::test]
    async fn nested_list_operations() {
        test_interpreter(
            r#"
        matrix.store([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        row.store(matrix.index(1))
        print(row.index(2))
        "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn deep_matrix() {
        test_interpreter(
            r#"
        matrix.store([[1, [2, 3, 4], 5], [4, 5, 6], [7, 8, 9]])
        print(matrix.index(0).index(1).index(1))
        "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn conditional_logic_chain() {
        test_interpreter(
            r#"
            x.store(10)
            y.store(20)
            if_then_else(
                gt(x, y),
                print("x is greater"),
                if_then_else(
                    lt(x, y),
                    print("y is greater"),
                    print("they are equal")
                )
            )
        "#,
            "y is greater",
        )
        .await;
    }

    #[tokio::test]
    async fn fibonacci_sequence() {
        test_interpreter(
            r#"
        a.store(0)
        b.store(1)
        i.store(0)
        repeat(10,
            print(a, " "),
            temp.store(a),
            a.store(b),
            b.store(add(temp, b))
        )
        "#,
            "0 1 1 2 3 5 8 13 21 34 ",
        )
        .await;
    }

    #[tokio::test]
    async fn bubble_sort() {
        test_interpreter(
            r#"
        numbers.store([5, 2, 8, 1, 9])
        n.store(numbers.length())
        i.store(0)
        repeat(n,
            j.store(0),
            repeat(sub(n, i, 1),
                if_then(
                    gt(numbers.index(j), numbers.index(add(j, 1))),
                    numbers.store(numbers.swap(j, add(j, 1)))
                ),
                j.store(add(j, 1))
            ),
            i.store(add(i, 1))
        )
        k.store(0)
        repeat(numbers.length(),
            print(numbers.index(k), " "),
            k.store(add(k, 1))
        )
        "#,
            "1 2 5 8 9 ",
        )
        .await;
    }

    #[tokio::test]
    async fn int_equality() {
        test_interpreter(
            r#"

            print(eq(1, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn float_equality() {
        test_interpreter(
            r#"

            print(eq(1.2, 1.2))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1.0))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn int_float_equality() {
        test_interpreter(
            r#"

            print(eq(1.0, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn string_equality() {
        test_interpreter(
            r#"

            print(eq("h", "h"))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq("e", "h"))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn text_search_and_count() {
        test_interpreter(
            r#"
        text.store("hello world hello universe hello")
        count.store(0)
        i.store(0)
        repeat(text.length(),
            if_then(
                and(
                    text.index(i).eq("h"),
                    if_then(i.add(1).lt(text.length()),
                        text.index(add(i, 1)).eq("e"),
                    )
                ),
                count.store(count.add(1))
            ),
            i.store(i.add(1)),
        )
        print("Found 'he' ", count, " times")
        "#,
            "Found 'he' 3 times",
        )
        .await;
    }

    #[tokio::test]
    async fn list_filter_evens() {
        test_interpreter(
            r#"
        numbers.store([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        evens.store([])
        i.store(0)
        repeat(numbers.length(),
            num.store(numbers.index(i)),
            if_then(
                eq(mod(num, 2), 0),
                evens.store(evens.insert(evens.length(), num))
            ),
            i.store(add(i, 1))
        )
        j.store(0)
        repeat(evens.length(),
            print(evens.index(j), " "),
            j.store(add(j, 1))
        )
        "#,
            "2 4 6 8 10 ",
        )
        .await;
    }

    #[tokio::test]
    async fn factorial_calculator() {
        test_interpreter(
            r#"
        n.store(5)
        result.store(1)
        counter.store(n)
        while(gt(counter, 0),
            result.store(mul(result, counter)),
            counter.store(sub(counter, 1))
        )
        print(n, "! = ", result)
        "#,
            "5! = 120",
        )
        .await;
    }

    #[tokio::test]
    async fn chained_method_calls() {
        test_interpreter(
            r#"
        text.store("  hello world  ")
        result.store(
            text
                .remove_whitespace()
                .uppercase()
                .concat("!!!")
        )
        print(result)
        "#,
            "HELLOWORLD!!!",
        )
        .await;
    }

    #[tokio::test]
    async fn list_contains_search() {
        test_interpreter(
            r#"
        fruits.store(["apple", "banana", "cherry", "date"])
        search.store("cherry")
        found.store(false)
        i.store(0)
        while(and(lt(i, fruits.length()), not(found)),
            if_then(
                eq(fruits.index(i), search),
                found.store(true),
            )
                i.store(add(i, 1))
        )
        i.store(i.sub(1))
        if_then_else(
            found,
            print("Found ", search, " at index ", i),
            print(search, " not found")
        )
        "#,
            "Found cherry at index 2",
        )
        .await;
    }

    #[tokio::test]
    async fn sum_of_list() {
        test_interpreter(
            r#"
        numbers.store([10, 20, 30, 40, 50])
        sum.store(0)
        i.store(0)
        repeat(numbers.length(),
            sum.store(add(sum, numbers.index(i))),
            i.store(add(i, 1))
        )
        print("Sum: ", sum)
        "#,
            "Sum: 150",
        )
        .await;
    }

    #[tokio::test]
    async fn find_max_in_list() {
        test_interpreter(
            r#"
        numbers.store([3, 7, 2, 9, 1, 5])
        max.store(numbers.index(0))
        i.store(1)
        repeat(sub(numbers.length(), 1),
            if_then(
                gt(numbers.index(i), max),
                max.store(numbers.index(i))
            ),
            i.store(add(i, 1))
        )
        print("Max: ", max)
        "#,
            "Max: 9",
        )
        .await;
    }

    #[tokio::test]
    async fn error_division_by_zero() {
        test_interpreter_err("print(div(10, 0))").await;
    }

    #[tokio::test]
    async fn error_index_out_of_bounds() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        print(list.index(10))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn error_swap_out_of_bounds() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        list.swap(0, 10)
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn bool_keyword_logic() {
        test_interpreter(
            r#"
        a.store(true)
        b.store(false)
        print(and(a, not(b)))
        "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_conditional_with_bools() {
        test_interpreter(
            r#"
        is_valid.store(true)
        is_ready.store(false)
        if_then_else(
            and(is_valid, is_ready),
            print("Both true"),
            if_then_else(
                or(is_valid, is_ready),
                print("At least one true"),
                print("Both false")
            )
        )
        "#,
            "At least one true",
        )
        .await;
    }

    #[tokio::test]
    async fn mixed_type_concatenation() {
        test_interpreter(
            r#"
        age.store(25)
        name.store("Alice")
        print(concat(name, " is ", age, " years old"))
        "#,
            "Alice is 25 years old",
        )
        .await;
    }

    #[tokio::test]
    async fn list_equality() {
        test_interpreter(
            r#"
        a.store([1, 2, 3])
        b.store([1, 2, 3])
        c.store([1, 2, 4])
        print(eq(a, b), " ", eq(a, c))
        "#,
            "true false",
        )
        .await;
    }

    #[tokio::test]
    async fn empty_list_operations() {
        test_interpreter(
            r#"
        empty.store([])
        print("Length: ", empty.length())
        empty.store(empty.insert(0, 42))
        print(" After insert: ", empty.index(0))
        "#,
            "Length: 0 After insert: 42",
        )
        .await;
    }

    #[tokio::test]
    async fn string_indexing() {
        test_interpreter(
            r#"
        word.store("hello")
        i.store(0)
        repeat(word.length(),
            print(word.index(i)),
            i.store(add(i, 1))
        )
        "#,
            "hello",
        )
        .await;
    }

    #[tokio::test]
    async fn string_starts_ends_with() {
        test_interpreter(
            r#"
        text.store("hello world")
        print(
            starts_with(text, "hello"), 
            " ", 
            ends_with(text, "world"), 
            " ", 
            starts_with(text, "world")
        )
        "#,
            "true true false",
        )
        .await;
    }

    #[tokio::test]
    async fn capitalize_edge_cases() {
        test_interpreter(
            r#"
        print(
            capitalize("hello"), 
            " ", 
            capitalize("a"), 
            " ", 
            capitalize("")
        )
        "#,
            "Hello A ",
        )
        .await;
    }

    #[tokio::test]
    async fn case_conversion_chain() {
        test_interpreter(
            r#"
        text.store("HeLLo WoRLd")
        print(
            text.lowercase(), 
            " | ", 
            text.uppercase()
        )
        "#,
            "hello world | HELLO WORLD",
        )
        .await;
    }

    #[tokio::test]
    async fn modulus_operations() {
        test_interpreter(
            r#"
        print(mod(10, 3), " ", mod(15, 4), " ", mod(7, 7))
        "#,
            "1 3 0",
        )
        .await;
    }

    #[tokio::test]
    async fn negative_numbers() {
        test_interpreter(
            r#"
        x.store(-5)
        y.store(10)
        print(add(x, y), " ", sub(x, y), " ", mul(x, y))
        "#,
            "5 -15 -50",
        )
        .await;
    }

    #[tokio::test]
    async fn float_division_precision() {
        test_interpreter(
            r#"
        print(div(10, 4), " ", div(10.0, 4))
        "#,
            "2 2.5",
        )
        .await;
    }

    #[tokio::test]
    async fn chained_comparisons() {
        test_interpreter(
            r#"
        x.store(5)
        result.store(
            and(
                gt(x, 0),
                lt(x, 10)
            )
        )
        print(result)
        "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn list_remove_and_length() {
        test_interpreter(
            r#"
        items.store([1, 2, 3, 4, 5])
        items.store(items.remove(2))
        print(items.length(), " ", items.index(2))
        "#,
            "4 4",
        )
        .await;
    }

    #[tokio::test]
    async fn list_replace() {
        test_interpreter(
            r#"
        items.store(["a", "b", "c"])
        items.store(items.replace(1, "X"))
        print(items.index(0), items.index(1), items.index(2))
        "#,
            "aXc",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_while_loops() {
        test_interpreter(
            r#"
        i.store(0)
        total.store(0)
        while(lt(i, 3),
            j.store(0),
            while(lt(j, 2),
                total.store(add(total, 1)),
                j.store(add(j, 1))
            ),
            i.store(add(i, 1))
        )
        print(total)
        "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn command_return_values() {
        test_interpreter(
            r#"
        x.store(5)
        result.store(if_then_else(gt(x, 3), add(x, 10), mul(x, 2)))
        print(result)
        "#,
            "15",
        )
        .await;
    }

    #[tokio::test]
    async fn repeat_with_multiple_commands() {
        test_interpreter(
            r#"
        sum.store(0)
        product.store(1)
        i.store(1)
        repeat(4,
            sum.store(add(sum, i)),
            product.store(mul(product, i)),
            i.store(add(i, 1))
        )
        print("Sum: ", sum, " Product: ", product)
        "#,
            "Sum: 10 Product: 24",
        )
        .await;
    }

    #[tokio::test]
    async fn zero_repetitions() {
        test_interpreter(
            r#"
        counter.store(0)
        repeat(0,
            counter.store(add(counter, 1))
        )
        print(counter)
        "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn free_variable() {
        test_interpreter(
            r#"
        x.store(42)
        print(x, " ")
        freed.store(free(x))
        print(freed)
        "#,
            "42 42",
        )
        .await;
    }

    #[tokio::test]
    async fn error_free_nonexistent() {
        test_interpreter_err(
            r#"
        result.store(free(nonexistent))
        print(result)
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn complex_expression_chaining() {
        test_interpreter(
            r#"
        numbers.store([1, 2, 3, 4, 5])
        result.store(
            numbers
                .index(2)
                .add(10)
                .mul(2)
                .sub(6)
        )
        print(result)
        "#,
            "20",
        )
        .await;
    }

    #[tokio::test]
    async fn string_insert() {
        test_interpreter(
            r#"
        text.store("helo")
        text.store(text.insert(3, "l"))
        print(text)
        "#,
            "hello",
        )
        .await;
    }

    #[tokio::test]
    async fn string_remove() {
        test_interpreter(
            r#"
        text.store("hello")
        text.store(text.remove(2))
        print(text)
        "#,
            "helo",
        )
        .await;
    }

    #[tokio::test]
    async fn string_replace() {
        test_interpreter(
            r#"
        text.store("hello")
        text.store(text.replace(1, "a"))
        print(text)
        "#,
            "hallo",
        )
        .await;
    }

    #[tokio::test]
    async fn error_modulus_by_zero() {
        test_interpreter_err("print(mod(10, 0))").await;
    }

    #[tokio::test]
    async fn error_negative_index() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        print(list.index(-1))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn error_insert_beyond_length() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        list.store(list.insert(99, 10))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn edit_matrix() {
        test_interpreter(
            r#"
        matrix.store([
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]
        ])
        row.store(1)
        column.store(1)
        matrix.replace(0, [3, 2, 1])
        matrix.replace(row,
            matrix.index(row).replace(column, 100)
        )
        matrix.print()
        "#,
            "[[3, 2, 1], [4, 100, 6], [7, 8, 9]]",
        )
        .await;
    }
}
