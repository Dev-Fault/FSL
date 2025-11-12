use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use rand::random_range;

use crate::{
    commands::*,
    types::{
        ALL_VALUES, ArgPos, ArgRule, Command, Error, Executor, FslType, LOGIC_TYPES,
        NON_NONE_VALUES, NUMERIC_TYPES, Value, VarMap,
    },
};

mod commands;
mod types;

#[async_recursion]
async fn contains_float(values: &Vec<Value>, vars: Arc<VarMap>) -> Result<bool, Error> {
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
                return contains_float(&vec![value.get_var_value(vars.clone())?], vars).await;
            }
            Value::Command(command) => match command.execute(vars.clone()).await {
                Ok(value) => return contains_float(&vec![value], vars.clone()).await,
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
    std_out: String,
    std_commands: CommandMap,
    custom_commands: CommandMap,
    var_map: VarMap,
}

impl FslInterpreter {
    pub fn new() -> Self {
        Self {
            std_out: String::new(),
            std_commands: Self::construct_std_commands(),
            custom_commands: CommandMap::new(),
            var_map: VarMap::new(),
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
        let mut lib = HashMap::new();

        Self::add_std_command(
            "add",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(add(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "sub",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(sub(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "mul",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(mul(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "div",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(div(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "mod",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(modulus(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "store",
            vec![
                ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Var]),
            ],
            Arc::new(|values, vars| Box::pin(store(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "list",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(list(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "clone",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Var])],
            Arc::new(|values, vars| Box::pin(commands::clone(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "drop",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Var])],
            Arc::new(|values, vars| Box::pin(commands::drop(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "print",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(print(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "eq",
            vec![ArgRule::new(ArgPos::Any, ALL_VALUES.into())],
            Arc::new(|values, vars| Box::pin(eq(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "gt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(gt(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "lt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(lt(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "not",
            vec![ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into())],
            Arc::new(|values, vars| Box::pin(not(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "and",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(and(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "or",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(or(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "if_then",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(if_then(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "if_then_else",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
                ArgRule::new(ArgPos::Index(2), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(if_then_else(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "while",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(while_loop(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "repeat",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(repeat(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "index_of",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(index_of(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "length_of",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::List])],
            Arc::new(|values, vars| Box::pin(length_of(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "swap_indexes",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(swap_indexes(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "insert_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES.into()),
            ],
            Arc::new(|values, vars| Box::pin(insert_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "remove_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(remove_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "replace_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES.into()),
            ],
            Arc::new(|values, vars| Box::pin(replace_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "starts_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(starts_with(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "ends_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(ends_with(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "concat",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(concat(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "capitalize",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(capitalize(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "upper",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(upper(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "lower",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(lower(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "remove_whitespace",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(remove_whitespace(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "nl",
            vec![ArgRule::no_args_rule()],
            Arc::new(|values, vars| Box::pin(nl(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "random_range",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(commands::random_range(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "random_entry",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::List])],
            Arc::new(|values, vars| Box::pin(random_entry(values, vars))),
            &mut lib,
        );

        lib
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use tokio;

    fn get_command(label: &str) -> Command {
        FslInterpreter::construct_std_commands()
            .get(label)
            .cloned()
            .unwrap()
    }

    async fn test_math(
        args: &Vec<Value>,
        command: &mut Command,
        expected_value: Value,
        vars: Option<Arc<VarMap>>,
    ) {
        let args = args.clone();
        let mut no_vars = VarMap::new();
        let mut vars = match vars {
            Some(vars) => vars,
            None => Arc::new(no_vars),
        };
        match expected_value {
            Value::None => {
                // Should be error
                command.set_args(args);
                let output = command.execute(vars).await;
                dbg!(&output);
                match output {
                    Ok(_) => panic!(),
                    Err(e) => {
                        dbg!(e);
                    }
                }
            }
            _ => {
                command.set_args(args);
                let output = command.execute(vars).await.unwrap();
                dbg!(&output);
                assert!(output == expected_value);
            }
        }
    }

    #[tokio::test]
    async fn math_two_ints() {
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_command("add"),
            Value::Int(9),
            None,
        )
        .await;
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_command("sub"),
            Value::Int(1),
            None,
        )
        .await;
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_command("mul"),
            Value::Int(20),
            None,
        )
        .await;
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_command("div"),
            Value::Int(1),
            None,
        )
        .await;
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_command("mod"),
            Value::Int(1),
            None,
        )
        .await;
    }

    #[tokio::test]
    async fn math_two_floats() {
        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_command("add"),
            Value::Float(9.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_command("sub"),
            Value::Float(1.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_command("mul"),
            Value::Float(20.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_command("div"),
            Value::Float(1.25),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_command("mod"),
            Value::Int(1),
            None,
        )
        .await;
    }

    #[tokio::test]
    async fn math_int_and_float() {
        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_command("add"),
            Value::Float(9.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_command("sub"),
            Value::Float(1.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_command("mul"),
            Value::Float(20.0),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_command("div"),
            Value::Float(1.25),
            None,
        )
        .await;

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_command("mod"),
            Value::Int(1),
            None,
        )
        .await;
    }

    #[tokio::test]
    async fn math_no_args() {
        test_math(&vec![], &mut get_command("add"), Value::None, None).await;
        test_math(&vec![], &mut get_command("sub"), Value::None, None).await;
        test_math(&vec![], &mut get_command("mul"), Value::None, None).await;
        test_math(&vec![], &mut get_command("div"), Value::None, None).await;
        test_math(&vec![], &mut get_command("mod"), Value::None, None).await;
    }

    #[tokio::test]
    async fn math_one_arg() {
        test_math(&vec![1.into()], &mut get_command("add"), Value::None, None).await;
        test_math(&vec![1.into()], &mut get_command("sub"), Value::None, None).await;
        test_math(&vec![1.into()], &mut get_command("mul"), Value::None, None).await;
        test_math(&vec![1.into()], &mut get_command("div"), Value::None, None).await;
        test_math(&vec![1.into()], &mut get_command("mod"), Value::None, None).await;
    }

    #[tokio::test]
    async fn math_wrong_type() {
        test_math(
            &vec![true.into(), Value::Int(1)],
            &mut get_command("add"),
            Value::None,
            None,
        )
        .await;

        test_math(
            &vec![true.into(), Value::Int(1)],
            &mut get_command("sub"),
            Value::None,
            None,
        )
        .await;

        test_math(
            &vec![true.into(), Value::Int(1)],
            &mut get_command("mul"),
            Value::None,
            None,
        )
        .await;

        test_math(
            &vec![true.into(), Value::Int(1)],
            &mut get_command("div"),
            Value::None,
            None,
        )
        .await;

        test_math(
            &vec![true.into(), Value::Int(1)],
            &mut get_command("mod"),
            Value::None,
            None,
        )
        .await;
    }

    #[tokio::test]
    async fn math_many_args() {
        let args = vec![5.0.into(), 3.into(), 3.into(), 100.into(), 57.into()];
        test_math(&args, &mut get_command("add"), Value::Float(168.0), None).await;
        test_math(&args, &mut get_command("sub"), Value::Float(-158.0), None).await;
        test_math(&args, &mut get_command("mul"), Value::Float(256500.0), None).await;
        test_math(
            &args,
            &mut get_command("div"),
            Value::Float(9.746588693957115e-5),
            None,
        )
        .await;
        test_math(&args, &mut get_command("mod"), Value::Int(2), None).await;
    }

    #[tokio::test]
    async fn math_vars() {
        let mut vars = Arc::new(VarMap::new());
        vars.insert_value("one", &1.into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::Int(2),
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("sub"),
            Value::Int(0),
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mul"),
            Value::Int(1),
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("div"),
            Value::Int(1),
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mod"),
            Value::Int(0),
            Some(vars.clone()),
        )
        .await;
    }

    #[tokio::test]
    async fn math_wrong_var() {
        let vars = Arc::new(VarMap::new());
        vars.insert_value("one", &"one".into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::None,
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("sub"),
            Value::None,
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mul"),
            Value::None,
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("div"),
            Value::None,
            Some(vars.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mod"),
            Value::None,
            Some(vars.clone()),
        )
        .await;
    }

    #[tokio::test]
    async fn math_commands() {
        let interpreter = FslInterpreter::new();
        let args = vec![5.into(), 4.into()];
        let mut command = interpreter.std_commands.get("add").cloned().unwrap();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![command.clone().into(), command.clone().into()];

        test_math(&args, &mut get_command("add"), Value::Int(18), None).await;
        test_math(&args, &mut get_command("sub"), Value::Int(0), None).await;
        test_math(&args, &mut get_command("mul"), Value::Int(81), None).await;
        test_math(&args, &mut get_command("div"), Value::Int(1), None).await;
        test_math(&args, &mut get_command("mod"), Value::Int(0), None).await;
    }

    #[tokio::test]
    async fn math_text() {
        let args = vec!["5".into(), "4".into()];
        test_math(&args, &mut get_command("add"), Value::Int(9), None).await;
        test_math(&args, &mut get_command("sub"), Value::Int(1), None).await;
        test_math(&args, &mut get_command("mul"), Value::Int(20), None).await;
        test_math(&args, &mut get_command("div"), Value::Int(1), None).await;
        test_math(&args, &mut get_command("mod"), Value::Int(1), None).await;
    }

    #[tokio::test]
    async fn detects_nested_float_in_command() {
        let interpreter = FslInterpreter::new();
        let args = vec![5.0.into(), 4.into()];
        let mut command = interpreter.std_commands.get("add").cloned().unwrap();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![command.clone().into(), command.clone().into()];

        test_math(&args, &mut get_command("add"), Value::Float(18.0), None).await;
    }

    #[tokio::test]
    async fn detects_nested_float_in_var() {
        let vars = Arc::new(VarMap::new());
        vars.insert_value("one", &1.0.into());
        vars.insert_value("one_nested", &Value::Var("one".to_string()));
        let var = Value::Var("one_nested".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::Float(2.0),
            Some(vars),
        )
        .await;
    }

    #[tokio::test]
    async fn division_by_zero() {
        let args = vec![1.into(), 0.into()];
        test_math(&args, &mut get_command("div"), Value::None, None).await;
        test_math(&args, &mut get_command("mod"), Value::None, None).await;
    }

    #[tokio::test]
    async fn repeat_add() {
        let vars = Arc::new(VarMap::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command)];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(vars).await.unwrap();
        dbg!(&output);
        assert!(output == Value::Int(9));
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_wrong_repititions() {
        let mut vars = Arc::new(VarMap::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec!["five".into(), Value::Command(command)];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(vars).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_wrong_thing_to_repeat() {
        let mut vars = Arc::new(VarMap::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), 5.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(vars).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_too_many_args() {
        let mut vars = Arc::new(VarMap::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command), 5.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(vars).await;
        dbg!(&output);
        output.unwrap();
    }
}
