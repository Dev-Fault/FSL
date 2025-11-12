use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use tokio::sync::Mutex;

use crate::{
    commands::*,
    types::{
        ALL_VALUES, ArgPos, ArgRule, Command, Error, Executor, FslType, LOGIC_TYPES,
        NON_NONE_VALUES, NUMERIC_TYPES, Value, VarMap,
    },
};

mod commands;
mod lexer;
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
    std_out: Arc<Mutex<String>>,
    std_commands: CommandMap,
    custom_commands: CommandMap,
    vars: VarMap,
    loops: Arc<Mutex<usize>>,
    loop_limit: Option<usize>,
}

impl FslInterpreter {
    pub fn new() -> Self {
        Self {
            std_out: Arc::new(Mutex::new(String::new())),
            std_commands: Self::construct_std_commands(),
            custom_commands: CommandMap::new(),
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

    fn add_std_command(
        label: &str,
        rules: Vec<ArgRule>,
        executor: Executor,
        command_map: &mut CommandMap,
    ) {
        command_map.insert(
            label.to_string(),
            Command::new(label, rules.clone(), executor),
        );
    }

    fn construct_std_commands() -> CommandMap {
        let mut commands = HashMap::new();

        Self::add_std_command(
            "add",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(add(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "sub",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(sub(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "mul",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(mul(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "div",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(div(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "mod",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(modulus(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "store",
            vec![
                ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Var]),
            ],
            Arc::new(|values, vars| Box::pin(store(values, vars))),
            &mut commands,
        );
        /*
        Self::add_std_command(
            "list",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(list(values, vars))),
            &mut commands,
        );
        */
        Self::add_std_command(
            "free",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Var])],
            Arc::new(|values, vars| Box::pin(commands::free(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "print",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(print(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "eq",
            vec![
                ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES.into()),
            ],
            Arc::new(|values, vars| Box::pin(eq(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "gt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(gt(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "lt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(lt(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "not",
            vec![ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into())],
            Arc::new(|values, vars| Box::pin(not(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "and",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(and(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "or",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(or(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "if_then",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(if_then(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "if_then_else",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
                ArgRule::new(ArgPos::Index(2), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(if_then_else(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "while",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(while_loop(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "repeat",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(repeat(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "index",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List, FslType::Text]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(index(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "length",
            vec![ArgRule::new(
                ArgPos::Index(0),
                vec![FslType::List, FslType::Text],
            )],
            Arc::new(|values, vars| Box::pin(length(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "swap_indices",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(swap_indices(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "insert_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(insert_at(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "remove_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(remove_at(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "replace_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(replace_at(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "starts_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(starts_with(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "ends_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(ends_with(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "concat",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(concat(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "capitalize",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(capitalize(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "upper",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(upper(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "lower",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(lower(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "remove_whitespace",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(remove_whitespace(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "nl",
            vec![ArgRule::no_args_rule()],
            Arc::new(|values, vars| Box::pin(nl(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "random_range",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(commands::random_range(values, vars))),
            &mut commands,
        );
        Self::add_std_command(
            "random_entry",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::List])],
            Arc::new(|values, vars| Box::pin(random_entry(values, vars))),
            &mut commands,
        );

        commands
    }
}
