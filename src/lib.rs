use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use tokio::sync::Mutex;

use crate::{
    commands::*,
    types::{
        ALL_VALUES, ArgPos, ArgRule, Command, Error, Executor, FslType, LOGIC_TYPES, MATH_RULES,
        NON_NONE_VALUES, NUMERIC_TYPES, Value, VarMap,
    },
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
        Self {
            output: Arc::new(Mutex::new(String::new())),
            commands: Self::construct_std_commands(),
            vars: VarMap::new(),
            loops: Arc::new(Mutex::new(0)),
            loop_limit: Some(u16::MAX as usize),
        }
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

    fn add_command(
        label: &str,
        rules: &'static [ArgRule],
        executor: Executor,
        command_map: &mut CommandMap,
    ) {
        command_map.insert(label.to_string(), Command::new(label, rules, executor));
    }

    fn construct_std_commands() -> CommandMap {
        let mut commands = HashMap::new();

        Self::add_command(
            "add",
            MATH_RULES,
            Arc::new(|values, vars| Box::pin(add(values, vars))),
            &mut commands,
        );
        Self::add_command(
            "sub",
            MATH_RULES,
            Arc::new(|values, vars| Box::pin(sub(values, vars))),
            &mut commands,
        );
        Self::add_command(
            "mul",
            MATH_RULES,
            Arc::new(|values, vars| Box::pin(mul(values, vars))),
            &mut commands,
        );
        Self::add_command(
            "div",
            MATH_RULES,
            Arc::new(|values, vars| Box::pin(div(values, vars))),
            &mut commands,
        );
        Self::add_command(
            "mod",
            MATH_RULES,
            Arc::new(|values, vars| Box::pin(modulus(values, vars))),
            &mut commands,
        );
        const STORE_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
            ArgRule::new(ArgPos::Index(1), &[FslType::Var]),
        ];
        Self::add_command(
            "store",
            &STORE_RULES,
            Arc::new(|values, vars| Box::pin(store(values, vars))),
            &mut commands,
        );
        const FREE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
        Self::add_command(
            "free",
            &FREE_RULES,
            Arc::new(|values, vars| Box::pin(commands::free(values, vars))),
            &mut commands,
        );
        const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::Any, NON_NONE_VALUES)];
        Self::add_command(
            "print",
            PRINT_RULES,
            Arc::new(|values, vars| Box::pin(print(values, vars))),
            &mut commands,
        );
        const EQ_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
            ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
        ];
        Self::add_command(
            "eq",
            &EQ_RULES,
            Arc::new(|values, vars| Box::pin(eq(values, vars))),
            &mut commands,
        );
        const GT_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
        ];
        Self::add_command(
            "gt",
            &GT_RULES,
            Arc::new(|values, vars| Box::pin(gt(values, vars))),
            &mut commands,
        );
        const LT_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
        ];
        Self::add_command(
            "lt",
            LT_RULES,
            Arc::new(|values, vars| Box::pin(lt(values, vars))),
            &mut commands,
        );
        const NOT_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), LOGIC_TYPES)];
        Self::add_command(
            "not",
            &NOT_RULES,
            Arc::new(|values, vars| Box::pin(not(values, vars))),
            &mut commands,
        );
        const AND_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
            ArgRule::new(ArgPos::Index(1), LOGIC_TYPES),
        ];
        Self::add_command(
            "and",
            AND_RULES,
            Arc::new(|values, vars| Box::pin(and(values, vars))),
            &mut commands,
        );
        const OR_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
            ArgRule::new(ArgPos::Index(1), LOGIC_TYPES),
        ];
        Self::add_command(
            "or",
            OR_RULES,
            Arc::new(|values, vars| Box::pin(or(values, vars))),
            &mut commands,
        );
        const IF_THEN_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
            ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
        ];
        Self::add_command(
            "if_then",
            IF_THEN_RULES,
            Arc::new(|values, vars| Box::pin(if_then(values, vars))),
            &mut commands,
        );
        const IF_THEN_ELSE_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
            ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
            ArgRule::new(ArgPos::Index(2), &[FslType::Command]),
        ];
        Self::add_command(
            "if_then_else",
            IF_THEN_ELSE_RULES,
            Arc::new(|values, vars| Box::pin(if_then_else(values, vars))),
            &mut commands,
        );
        const WHILE_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
            ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
        ];
        Self::add_command(
            "while",
            WHILE_RULES,
            Arc::new(|values, vars| Box::pin(while_loop(values, vars))),
            &mut commands,
        );
        const REPEAT_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
            ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
        ];
        Self::add_command(
            "repeat",
            REPEAT_RULES,
            Arc::new(|values, vars| Box::pin(repeat(values, vars))),
            &mut commands,
        );
        const INDEX_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
        ];
        Self::add_command(
            "index",
            INDEX_RULES,
            Arc::new(|values, vars| Box::pin(index(values, vars))),
            &mut commands,
        );
        const LENGTH_RULES: [ArgRule; 1] = [ArgRule::new(
            ArgPos::Index(0),
            &[FslType::List, FslType::Text],
        )];
        Self::add_command(
            "length",
            &LENGTH_RULES,
            Arc::new(|values, vars| Box::pin(length(values, vars))),
            &mut commands,
        );
        const SWAP_RULES: [ArgRule; 3] = [
            ArgRule::new(ArgPos::Index(0), &[FslType::List]),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
            ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
        ];
        Self::add_command(
            "swap",
            &SWAP_RULES,
            Arc::new(|values, vars| Box::pin(swap(values, vars))),
            &mut commands,
        );
        const INSERT_RULES: [ArgRule; 3] = [
            ArgRule::new(ArgPos::Index(0), &[FslType::List]),
            ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
            ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
        ];
        Self::add_command(
            "insert",
            &INSERT_RULES,
            Arc::new(|values, vars| Box::pin(insert(values, vars))),
            &mut commands,
        );
        const REMOVE_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), &[FslType::List]),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
        ];
        Self::add_command(
            "remove",
            &REMOVE_RULES,
            Arc::new(|values, vars| Box::pin(remove(values, vars))),
            &mut commands,
        );
        const REPLACE_RULES: [ArgRule; 3] = [
            ArgRule::new(ArgPos::Index(0), &[FslType::List]),
            ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
            ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
        ];
        Self::add_command(
            "replace",
            &REPLACE_RULES,
            Arc::new(|values, vars| Box::pin(replace(values, vars))),
            &mut commands,
        );
        const STARTS_WITH_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
            ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
        ];
        Self::add_command(
            "starts_with",
            &STARTS_WITH_RULES,
            Arc::new(|values, vars| Box::pin(starts_with(values, vars))),
            &mut commands,
        );
        const ENDS_WITH_RULES: &'static [ArgRule] = &[
            ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
            ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
        ];
        Self::add_command(
            "ends_with",
            ENDS_WITH_RULES,
            Arc::new(|values, vars| Box::pin(ends_with(values, vars))),
            &mut commands,
        );
        const CONCAT_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Any, NON_NONE_VALUES)];
        Self::add_command(
            "concat",
            &CONCAT_RULES,
            Arc::new(|values, vars| Box::pin(concat(values, vars))),
            &mut commands,
        );
        const CAPITALIZE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
        Self::add_command(
            "capitalize",
            &CAPITALIZE_RULES,
            Arc::new(|values, vars| Box::pin(capitalize(values, vars))),
            &mut commands,
        );
        const UPPERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
        Self::add_command(
            "uppercase",
            &UPPERCASE_RULES,
            Arc::new(|values, vars| Box::pin(uppercase(values, vars))),
            &mut commands,
        );
        const LOWERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
        Self::add_command(
            "lowercase",
            &LOWERCASE_RULES,
            Arc::new(|values, vars| Box::pin(lowercase(values, vars))),
            &mut commands,
        );
        const REMOVE_WHITESPACE_RULES: [ArgRule; 1] =
            [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
        Self::add_command(
            "remove_whitespace",
            &REMOVE_WHITESPACE_RULES,
            Arc::new(|values, vars| Box::pin(remove_whitespace(values, vars))),
            &mut commands,
        );
        const RANDOM_RANGE_RULES: [ArgRule; 2] = [
            ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
            ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
        ];
        Self::add_command(
            "random_range",
            &RANDOM_RANGE_RULES,
            Arc::new(|values, vars| Box::pin(commands::random_range(values, vars))),
            &mut commands,
        );
        const RANDOM_ENTRY_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::List])];
        Self::add_command(
            "random_entry",
            &RANDOM_ENTRY_RULES,
            Arc::new(|values, vars| Box::pin(random_entry(values, vars))),
            &mut commands,
        );

        commands
    }
}
