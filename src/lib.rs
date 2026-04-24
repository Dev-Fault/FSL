use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
};

use async_recursion::async_recursion;

use crate::{
    commands::*,
    lexer::LexerError,
    parser::{Expression, Parser, ParserError},
    types::{
        command::{ArgRule, Command, CommandError, CommandSpec, Executor, UserCommand},
        value::{Value, ValueError},
    },
    vars::{DEFAULT_MEMORY_LIMIT, VarStack},
};

pub mod commands;
mod lexer;
mod parser;
pub mod types;
mod vars;

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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug)]
pub struct InterpreterData {
    pub output: tokio::sync::Mutex<String>,
    pub vars: VarStack,
    pub user_commands: tokio::sync::Mutex<UserCommands>,
    call_stack: tokio::sync::Mutex<Vec<String>>,
    pub total_loop_limit: Option<usize>,
    pub total_loops: AtomicUsize,
    pub loop_depth: AtomicUsize,
    pub break_flag: AtomicBool,
    pub continue_flag: AtomicBool,
    pub return_flag: AtomicBool,
    pub if_flag: AtomicBool,
    pub switch_flag: AtomicBool,
}

impl InterpreterData {
    pub fn new() -> Self {
        InterpreterData {
            output: tokio::sync::Mutex::new(String::new()),
            vars: VarStack::new_bounded(DEFAULT_MEMORY_LIMIT),
            user_commands: tokio::sync::Mutex::new(UserCommands::new()),
            call_stack: tokio::sync::Mutex::new(Vec::new()),
            total_loop_limit: Some(u16::MAX as usize),
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            break_flag: AtomicBool::new(false),
            continue_flag: AtomicBool::new(false),
            return_flag: AtomicBool::new(false),
            if_flag: AtomicBool::new(false),
            switch_flag: AtomicBool::new(false),
        }
    }

    pub fn new_unbounded() -> Self {
        InterpreterData {
            output: tokio::sync::Mutex::new(String::new()),
            vars: VarStack::new_unbounded(),
            user_commands: tokio::sync::Mutex::new(UserCommands::new()),
            call_stack: tokio::sync::Mutex::new(Vec::new()),
            total_loop_limit: None,
            total_loops: AtomicUsize::new(0),
            loop_depth: AtomicUsize::new(0),
            break_flag: AtomicBool::new(false),
            continue_flag: AtomicBool::new(false),
            return_flag: AtomicBool::new(false),
            if_flag: AtomicBool::new(false),
            switch_flag: AtomicBool::new(false),
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

#[derive(Debug)]
pub struct FslInterpreter {
    pub commands: CommandMap,
    pub data: Arc<InterpreterData>,
    bounded: bool,
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
            bounded: true,
        };
        interpreter.add_standard_commands();
        interpreter
    }

    pub fn new_unbounded() -> Self {
        let mut interpreter = Self {
            commands: CommandMap::new(),
            data: Arc::new(InterpreterData::new_unbounded()),
            bounded: false,
        };
        interpreter.add_standard_commands();
        interpreter
    }

    pub fn reset_data(&mut self) {
        self.data = Arc::new(if self.bounded {
            InterpreterData::new()
        } else {
            InterpreterData::new_unbounded()
        });
    }

    /// Adds a command to the interpreters command map, over-writing the command if it was already in the map
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
                        if e.exited_program() {
                            break;
                        } else {
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

            Ok(Value::Command(Box::new(command)))
        } else {
            let user_command_label = {
                let user_commands = self.data.user_commands.lock().await;
                user_commands
                    .get(expression.name.as_str())
                    .map(|uc| uc.label.clone())
            };

            if let Some(label) = user_command_label {
                let mut command = Command::new(
                    CommandSpec::new(&expression.name, RUN_RULES),
                    Self::construct_executor(commands::run),
                );

                let mut args = VecDeque::new();
                args.push_back(Value::Var(label));

                for arg in expression.args {
                    args.push_back(self.parse_arg(arg).await?);
                }

                command.set_args(args);

                Ok(Value::Command(Box::new(command)))
            } else {
                return Err(CommandError::NonExistantCommand(format!(
                    "command with name {} does not exist",
                    expression.name
                )));
            }
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
            parser::Arg::KeyValue(_, _) => unreachable!(),
            parser::Arg::Map(map) => {
                let mut value_map = HashMap::new();

                for (key, value) in map {
                    value_map.insert(key, self.parse_arg(value).await?);
                }

                Ok(Value::Map(value_map))
            }
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
                (CLAMP, CLAMP_RULES, commands::clamp),
                (CLAMP_MIN, CLAMP_MIN_RULES, commands::clamp_min),
                (CLAMP_MAX, CLAMP_MAX_RULES, commands::clamp_max),
                (PRECISION, PRECISION_RULES, commands::precision),
                (STORE, STORE_RULES, commands::store),
                (CONST, CONST_RULES, commands::r#const),
                (LOCAL, LOCAL_RULES, commands::local),
                (UPDATE, UPDATE_RULES, commands::update),
                (CLONE, CLONE_RULES, commands::clone),
                (FREE, FREE_RULES, commands::free),
                (PRINT, PRINT_RULES, commands::print),
                (DEBUG, DEBUG_RULES, commands::debug),
                (SCOPE, SCOPE_RULES, commands::scope),
                (EQ, EQ_RULES, commands::eq),
                (GT, GT_RULES, commands::gt),
                (GTOE, GTOE_RULES, commands::gtoe),
                (LT, LT_RULES, commands::lt),
                (LTOE, LTOE_RULES, commands::ltoe),
                (NOT, NOT_RULES, commands::not),
                (AND, AND_RULES, commands::and),
                (OR, OR_RULES, commands::or),
                (IF_THEN, IF_THEN_RULES, commands::if_then),
                (IF_THEN_ELSE, IF_THEN_ELSE_RULES, commands::if_then_else),
                (IF, IF_RULES, commands::r#if),
                (THEN, THEN_RULES, commands::then),
                (ELSE_IF, ELSE_IF_RULES, commands::else_if),
                (ELSE, ELSE_RULES, commands::r#else),
                (SWITCH, SWITCH_RULES, commands::switch),
                (CASE, CASE_RULES, commands::case),
                (FALLBACK, FALLBACK_RULES, commands::fallback),
                (WHILE_LOOP, WHILE_RULES, commands::while_command),
                (REPEAT, REPEAT_RULES, commands::repeat),
                (INDEX, INDEX_RULES, commands::index),
                (GET, GET_RULES, commands::get),
                (SET, SET_RULES, commands::set),
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
                (TRIM, TRIM_RULES, commands::trim),
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
                (SHUFFLE, SHUFFLE_RULES, commands::shuffle),
                (DEF, DEF_RULES, commands::def),
                (EXIT, NO_ARGS, commands::exit),
                (BREAK, NO_ARGS, commands::break_command),
                (CONTINUE, NO_ARGS, commands::continue_command),
                (RETURN, RETURN_RULES, commands::r#return),
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
    use crate::types::command::CommandError;
    use crate::types::value::{Value, ValueError};
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
            "Size of value: {} \nSize of Vec<Value>: {}",
            size_of::<Value>(),
            size_of::<Vec<Value>>()
        );
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
                == (4 * size_of::<Value>()) + size_of::<Vec<Value>>()
        );
    }

    #[tokio::test]
    async fn deallocation_of_var_stacks() {
        let interpreter = FslInterpreter::new();
        interpreter
            .interpret(
                r#"
                    allocate_more.def(
                        string_three.local("hello world")
                        string_four.local("hello world")
                    )
                    allocate.def(
                        string_one.local("hello world")
                        string_two.local("hello world")
                        allocate_more()
                    )
                    allocate()
                "#,
            )
            .await
            .unwrap();
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
                == 0
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
                matrix.replace([1, 1], \"X\")
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
                if(gt(numbers.index(j), numbers.index(add(j, 1))),
                    then(numbers.swap(j, add(j, 1)))
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
                evens.insert(evens.length(), num)
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
        empty.insert(0, 42)
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
        items.remove(2)
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
        items.replace(1, "X")
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
        text.insert(3, "l")
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
            "l",
        )
        .await;
    }

    #[tokio::test]
    async fn string_replace() {
        test_interpreter(
            r#"
        text.store("hello")
        text.replace(1, "a")
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
        matrix.replace([1, 1], 10)
        matrix.print()
        "#,
            "[[1, 2, 3], [4, 10, 6], [7, 8, 9]]",
        )
        .await;
    }

    #[tokio::test]
    async fn parameter_var_in_function_in_list() {
        test_interpreter(
            r#"
            C_MODS.store(0)
            P_MOD.store(0)
            P_DUR.store(1)
            attacker_mods.store([])
            attacker.store([attacker_mods])
            potion.store([1, 1])
            mods.store([])
            add_potion_modifier.def(potion,
			    mods.store(attacker.index(C_MODS).clone())
			    mods.push([potion.index(P_MOD).clone(), potion.index(P_DUR).clone()])
			    attacker.index(C_MODS).store(mods.clone())
            )
            add_potion_modifier([50, 3])
			print("potion: [1, 1]\n", concat("mods: ", mods, "\nattacker_mods: ", attacker.index(C_MODS)))
            "#,
            "potion: [1, 1]\nmods: [[50, 3]]\nattacker_mods: [[50, 3]]",
        )
        .await;
    }

    #[tokio::test]
    async fn index_global_in_local_scope() {
        test_interpreter(
            r#"
	            MIN.store(0)
	            MAX.store(1)
	            HEALTH_POTION_RANGES.store([
		            [-100, 25],
		            [-25, 50],
		            [10, 100]
	            ])

	            get_potion_mod.def(
	                repeat(2,
	                    if_then(true,
			                potion_modifier.store(
			                    HEALTH_POTION_RANGES.index(0).index(MIN)
			                )
	                    )
	                )
			        potion_modifier.free()
	            )

	            get_potion_mod().print()
            "#,
            "-100",
        )
        .await;
    }

    #[tokio::test]
    async fn multiple_maps() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0
	            ])
	            player.set("name", "player")
	            player.set("health", 5)

	            opponent.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0
	            ])
	            opponent.set("name", "opponent")
	            opponent.set("health", 25)

	            characters.store([player, opponent])

	            print(player.get("health"), "\n")
	            print(opponent.get("health"))
            "#,
            "5\n25",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_map_access() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0,
	            	weapon: [
	            	    type: "sword",
	            	    damage: 100
	            	],
	            ])

	            player.get(["weapon", "type"]).print("\n")
	            player.set(["weapon", "type"], "knife")
	            player.get(["weapon", "type"]).print()
            "#,
            "sword\nknife",
        )
        .await;
    }

    #[tokio::test]
    async fn map_dot_notation() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0,
	            	weapon: [
	            	    type: "sword",
	            	    damage: 100
	            	],
	            ])

	            player.weapon.type.get().print("\n")
	            player.weapon.type.set("knife")
	            player.weapon.type.get().print()
            "#,
            "sword\nknife",
        )
        .await;
    }

    #[tokio::test]
    async fn custom_command_calls_custom_command() {
        test_interpreter(
            r#"
            add_two.def(value,
                value.add(2).return()
            )
            add_three.def(value,
                value.add(3).return()
            )

            number.store(0)
            number.store(add_two(add_three(number)))
            number.print()
            "#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn const_map() {
        let err = test_interpreter_err_type(
            r#"
	            player.const([
	            	name: "blah",
	            	health: 100,
	            ])

	            player.name.set("player")
            "#,
        )
        .await;

        match err {
            InterpreterError::CommandError(command_error) => match command_error {
                CommandError::ValueError(value_error) => match value_error {
                    ValueError::AttemptToOverwriteConstant(_) => {}
                    _ => panic!("should be overwrite constant err"),
                },
                _ => panic!("should be overwrite constant err"),
            },
            _ => panic!("should be overwrite constant err"),
        }
    }

    #[tokio::test]
    async fn indexing_maps_in_lists() {
        test_interpreter(
            r#"
            action.store([
                id: "id1"
            ])
            action2.store([
                id: "id2"
            ])
            actions.store([[action.clone(), action2.clone()]])
            actions.index(0).index(0).id.get().print()
            "#,
            "id1",
        )
        .await;
    }

    #[tokio::test]
    async fn indexing_map_with_index_command() {
        test_interpreter_err(
            r#"
            map.store([
                tags: [["rust", "cool"]]
            ])
            map.index(0).print()
            "#,
        )
        .await;
    }

    #[tokio::test]
    async fn indexing_deeply_nested_maps_and_lists() {
        test_interpreter(
            r#"
            inner.store([
                tags: [["rust", "cool"]]
            ])
            outer.store([
                data: [[inner.clone()]]
            ])
            outer.data.get().index(0).index(0).tags.get().index(0).index(0).print()
            "#,
            "rust",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_map_set() {
        test_interpreter(
            r#"
            player.store([
                weapons: [
                    sword: [
                        name: "Diamond Sword"
                    ]
                ]
            ])
            player.weapons.sword.name.set("Gold Sword")
            player.weapons.sword.name.get().print()
            "#,
            "Gold Sword",
        )
        .await;
    }
}
