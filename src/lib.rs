use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use tokio::sync::Mutex;

use crate::{
    commands::*,
    types::{ArgRule, Command, Error, Executor, Value, VarMap},
};

mod commands;
mod lexer;
mod parser;
mod types;

#[async_recursion]
async fn contains_float(
    values: &Vec<Value>,
    interpreter: Arc<FslInterpreter>,
) -> Result<bool, Error> {
    for value in values {
        match value {
            Value::Float(_) => return Ok(true),
            Value::Text(text) => {
                if text.contains('.') {
                    match text.parse::<f64>() {
                        Ok(_) => return Ok(true),
                        Err(_) => continue,
                    }
                }
            }
            Value::Var(_) => {
                return contains_float(
                    &vec![value.get_var_value(interpreter.clone())?],
                    interpreter,
                )
                .await;
            }
            Value::Command(command) => match command.execute(interpreter.clone()).await {
                Ok(value) => return contains_float(&vec![value], interpreter.clone()).await,
                Err(e) => return Err(e),
            },
            _ => {
                continue;
            }
        }
    }
    Ok(false)
}

pub type CommandMap = HashMap<String, Command>;

pub struct FslInterpreter {
    output: Arc<Mutex<String>>,
    commands: CommandMap,
    vars: VarMap,
    loops: Arc<Mutex<usize>>,
    loop_limit: Option<usize>,
}

impl FslInterpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            output: Arc::new(Mutex::new(String::new())),
            commands: CommandMap::new(),
            vars: VarMap::new(),
            loops: Arc::new(Mutex::new(0)),
            loop_limit: Some(u16::MAX as usize),
        };
        interpreter.add_standard_commands();
        interpreter
    }

    pub async fn increment_loops(&self) -> Result<(), Error> {
        match self.loop_limit {
            Some(limit) => {
                let mut loops = self.loops.lock().await;
                *loops += 1;
                if *loops >= limit {
                    *loops = limit;
                    Err(format!("Max loop limit of {} exceeded", limit))
                } else {
                    Ok(())
                }
            }
            None => Ok(()),
        }
    }

    pub fn add_command(&mut self, label: &str, rules: &'static [ArgRule], executor: Executor) {
        self.commands
            .insert(label.to_string(), Command::new(label, rules, executor));
    }

    pub fn construct_executor<F, Fut>(command: F) -> Executor
    where
        F: Fn(Arc<Vec<Value>>, Arc<FslInterpreter>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Value, Error>> + Send + 'static,
    {
        Arc::new(move |values, vars| Box::pin(command(values, vars)))
    }

    fn add_standard_commands(&mut self) {
        self.add_command("add", MATH_RULES, Self::construct_executor(commands::add));
        self.add_command("sub", MATH_RULES, Self::construct_executor(commands::sub));
        self.add_command("mul", MATH_RULES, Self::construct_executor(commands::mul));
        self.add_command("div", MATH_RULES, Self::construct_executor(commands::div));
        self.add_command(
            "mod",
            MATH_RULES,
            Self::construct_executor(commands::modulus),
        );
        self.add_command(
            "store",
            &STORE_RULES,
            Self::construct_executor(commands::store),
        );
        self.add_command(
            "free",
            &FREE_RULES,
            Self::construct_executor(commands::free),
        );
        self.add_command(
            "print",
            PRINT_RULES,
            Self::construct_executor(commands::print),
        );
        self.add_command("eq", &EQ_RULES, Self::construct_executor(commands::eq));
        self.add_command("gt", &GT_RULES, Self::construct_executor(commands::gt));
        self.add_command("lt", LT_RULES, Self::construct_executor(commands::lt));
        self.add_command("not", &NOT_RULES, Self::construct_executor(commands::not));
        self.add_command("and", AND_RULES, Self::construct_executor(commands::and));
        self.add_command("or", OR_RULES, Self::construct_executor(commands::or));
        self.add_command(
            "if_then",
            IF_THEN_RULES,
            Self::construct_executor(commands::if_then),
        );
        self.add_command(
            "if_then_else",
            IF_THEN_ELSE_RULES,
            Self::construct_executor(commands::if_then_else),
        );
        self.add_command(
            "while",
            WHILE_RULES,
            Self::construct_executor(commands::while_loop),
        );
        self.add_command(
            "repeat",
            REPEAT_RULES,
            Self::construct_executor(commands::repeat),
        );
        self.add_command(
            "index",
            INDEX_RULES,
            Self::construct_executor(commands::index),
        );
        self.add_command(
            "length",
            &LENGTH_RULES,
            Self::construct_executor(commands::length),
        );
        self.add_command(
            "swap",
            &SWAP_RULES,
            Self::construct_executor(commands::swap),
        );
        self.add_command(
            "insert",
            &INSERT_RULES,
            Self::construct_executor(commands::insert),
        );
        self.add_command(
            "remove",
            &REMOVE_RULES,
            Self::construct_executor(commands::remove),
        );
        self.add_command(
            "replace",
            &REPLACE_RULES,
            Self::construct_executor(commands::replace),
        );
        self.add_command(
            "starts_with",
            &STARTS_WITH_RULES,
            Self::construct_executor(commands::starts_with),
        );
        self.add_command(
            "ends_with",
            ENDS_WITH_RULES,
            Self::construct_executor(commands::ends_with),
        );
        self.add_command(
            "concat",
            &CONCAT_RULES,
            Self::construct_executor(commands::concat),
        );
        self.add_command(
            "capitalize",
            &CAPITALIZE_RULES,
            Self::construct_executor(commands::capitalize),
        );
        self.add_command(
            "uppercase",
            &UPPERCASE_RULES,
            Self::construct_executor(commands::uppercase),
        );
        self.add_command(
            "lowercase",
            &LOWERCASE_RULES,
            Self::construct_executor(commands::lowercase),
        );
        self.add_command(
            "remove_whitespace",
            &REMOVE_WHITESPACE_RULES,
            Self::construct_executor(commands::remove_whitespace),
        );
        self.add_command(
            "random_range",
            &RANDOM_RANGE_RULES,
            Self::construct_executor(commands::random_range),
        );
        self.add_command(
            "random_entry",
            &RANDOM_ENTRY_RULES,
            Self::construct_executor(commands::random_entry),
        );
    }
}
