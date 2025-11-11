use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use rand::random_range;

use crate::types::{
    ALL_VALUES, ArgPos, ArgRule, Command, Error, Executor, FslType, LOGIC_TYPES, NON_NONE_VALUES,
    NUMERIC_TYPES, Value, VarMap,
};

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
            Arc::new(|values, vars| Box::pin(Self::add(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "sub",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(Self::sub(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "mul",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(Self::mul(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "div",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(Self::div(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "mod",
            ArgRule::math_rules(),
            Arc::new(|values, vars| Box::pin(Self::modulus(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "store",
            vec![
                ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Var]),
            ],
            Arc::new(|values, vars| Box::pin(Self::store(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "list",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(Self::list(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "clone",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Var])],
            Arc::new(|values, vars| Box::pin(Self::clone(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "drop",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Var])],
            Arc::new(|values, vars| Box::pin(Self::drop(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "print",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(Self::print(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "eq",
            vec![ArgRule::new(ArgPos::Any, ALL_VALUES.into())],
            Arc::new(|values, vars| Box::pin(Self::eq(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "gt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::gt(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "lt",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::lt(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "not",
            vec![ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into())],
            Arc::new(|values, vars| Box::pin(Self::not(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "and",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::and(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "or",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), LOGIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::or(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "if_then",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(Self::if_then(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "if_then_else",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
                ArgRule::new(ArgPos::Index(2), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(Self::if_then_else(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "while",
            vec![
                ArgRule::new(ArgPos::Index(0), LOGIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(Self::while_loop(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "repeat",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Command]),
            ],
            Arc::new(|values, vars| Box::pin(Self::repeat(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "index_of",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::index_of(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "length_of",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::List])],
            Arc::new(|values, vars| Box::pin(Self::length_of(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "swap_indexes",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::swap_indexes(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "insert_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::insert_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "remove_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::remove_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "replace_at",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::List]),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::replace_at(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "starts_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(Self::starts_with(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "ends_with",
            vec![
                ArgRule::new(ArgPos::Index(0), vec![FslType::Text]),
                ArgRule::new(ArgPos::Index(1), vec![FslType::Text]),
            ],
            Arc::new(|values, vars| Box::pin(Self::ends_with(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "concat",
            vec![ArgRule::new(ArgPos::Any, NON_NONE_VALUES.into())],
            Arc::new(|values, vars| Box::pin(Self::concat(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "capitalize",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(Self::capitalize(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "upper",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(Self::upper(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "lower",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(Self::lower(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "remove_whitespace",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::Text])],
            Arc::new(|values, vars| Box::pin(Self::remove_whitespace(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "nl",
            vec![ArgRule::no_args_rule()],
            Arc::new(|values, vars| Box::pin(Self::nl(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "random_range",
            vec![
                ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES.into()),
                ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES.into()),
            ],
            Arc::new(|values, vars| Box::pin(Self::random_range(values, vars))),
            &mut lib,
        );
        Self::add_std_command(
            "random_entry",
            vec![ArgRule::new(ArgPos::Index(0), vec![FslType::List])],
            Arc::new(|values, vars| Box::pin(Self::random_entry(values, vars))),
            &mut lib,
        );

        lib
    }

    async fn add(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        if contains_float(&values, vars.clone()).await? {
            let mut sum: f64 = 0.0;
            for value in values.iter() {
                sum = sum + value.as_float(vars.clone()).await?;
            }
            Ok(Value::Float(sum))
        } else {
            let mut sum: i64 = 0;
            for value in values.iter() {
                sum = sum + value.as_int(vars.clone()).await?;
            }
            Ok(Value::Int(sum))
        }
    }

    async fn sub(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        if contains_float(&values, vars.clone()).await? {
            let mut diff = values[0].as_float(vars.clone()).await?;
            for value in &values[1..values.len()] {
                diff = diff - value.as_float(vars.clone()).await?;
            }
            Ok(Value::Float(diff))
        } else {
            let mut diff = values[0].as_int(vars.clone()).await?;
            for value in &values[1..values.len()] {
                diff = diff - value.as_int(vars.clone()).await?;
            }
            Ok(Value::Int(diff))
        }
    }

    async fn mul(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        if contains_float(&values, vars.clone()).await? {
            let mut product = values[0].as_float(vars.clone()).await?;
            for value in &values[1..values.len()] {
                product = product * value.as_float(vars.clone()).await?;
            }
            Ok(Value::Float(product))
        } else {
            let mut product = values[0].as_int(vars.clone()).await?;
            for value in &values[1..values.len()] {
                product = product * value.as_int(vars.clone()).await?;
            }
            Ok(Value::Int(product))
        }
    }

    async fn div(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        if contains_float(&values, vars.clone()).await? {
            let mut quotient = values[0].as_float(vars.clone()).await?;
            for value in &values[1..values.len()] {
                let value = value.as_float(vars.clone()).await?;
                if value == 0.0 {
                    return Err("division by zero".to_string());
                };
                quotient = quotient / value;
            }
            Ok(Value::Float(quotient))
        } else {
            let mut quotient = values[0].as_int(vars.clone()).await?;
            for value in &values[1..values.len()] {
                let value = value.as_int(vars.clone()).await?;
                if value == 0 {
                    return Err("division by zero".to_string());
                };
                quotient = quotient / value;
            }
            Ok(Value::Int(quotient))
        }
    }

    async fn modulus(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        let mut remainder = values[0].as_int(vars.clone()).await?;
        for value in &values[1..values.len()] {
            let value = value.as_int(vars.clone()).await?;
            if value == 0 {
                return Err("division by zero".to_string());
            };
            remainder = remainder % value;
        }
        Ok(Value::Int(remainder))
    }

    pub async fn store(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        vars.insert_value(&values[1].as_var()?, &values[0]);

        todo!();
    }

    async fn list(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!();
    }

    async fn clone(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn drop(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn print(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn eq(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn gt(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn lt(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn not(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok((!values[0].as_bool(vars).await?).into())
    }

    async fn and(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(
            (values[0].as_bool(vars.clone()).await? && values[1].as_bool(vars.clone()).await?)
                .into(),
        )
    }

    async fn or(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(
            (values[0].as_bool(vars.clone()).await? || values[1].as_bool(vars.clone()).await?)
                .into(),
        )
    }

    async fn if_then(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn if_then_else(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn while_loop(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn repeat(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        let repetitions = values[0].as_int(vars.clone()).await?;
        let command = values[1].as_command()?;
        let mut final_value = Value::None;
        for i in 0..repetitions {
            final_value = command.execute(vars.clone()).await?;
        }

        Ok(final_value)
    }

    async fn index_of(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn length_of(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn swap_indexes(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn insert_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn remove_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn replace_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        todo!()
    }

    async fn starts_with(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[0]
            .as_text(vars.clone())
            .await?
            .starts_with(&values[1].as_text(vars.clone()).await?)
            .into())
    }

    async fn ends_with(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[0]
            .as_text(vars.clone())
            .await?
            .ends_with(&values[1].as_text(vars.clone()).await?)
            .into())
    }

    async fn concat(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        let mut cat_string = String::new();

        for value in values.iter() {
            cat_string.push_str(&value.as_text(vars.clone()).await?);
        }

        Ok(cat_string.into())
    }

    async fn capitalize(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        let text = values[0].as_text(vars).await?;
        if text.len() < 1 {
            Ok("".into())
        } else {
            Ok(format!("{}{}", text[0..1].to_uppercase(), text[1..].to_uppercase()).into())
        }
    }

    async fn upper(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[0].as_text(vars).await?.to_uppercase().into())
    }

    async fn lower(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[0].as_text(vars).await?.to_lowercase().into())
    }

    async fn remove_whitespace(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[0]
            .as_text(vars)
            .await?
            .split_whitespace()
            .collect::<String>()
            .into())
    }

    async fn nl(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok("\n".into())
    }

    async fn random_range(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        if contains_float(&values, vars.clone()).await? {
            let min = values[0].as_float(vars.clone()).await?;
            let max = values[1].as_float(vars.clone()).await?;
            if min >= max {
                Err("min must be greater than max".to_string())
            } else {
                Ok(random_range(min..=max).into())
            }
        } else {
            let min = values[0].as_int(vars.clone()).await?;
            let max = values[1].as_int(vars.clone()).await?;
            if min >= max {
                Err("min must be greater than max".to_string())
            } else {
                Ok(random_range(min..=max).into())
            }
        }
    }

    async fn random_entry(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
        Ok(values[random_range(0..values.len())].clone())
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
