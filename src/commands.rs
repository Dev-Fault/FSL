use std::sync::Arc;

use crate::{
    FslInterpreter, contains_float,
    types::{Error, Value, VarMap},
};

pub async fn add(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        let mut sum: f64 = 0.0;
        for value in values.iter() {
            sum = sum + value.as_float(interpreter.clone()).await?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum: i64 = 0;
        for value in values.iter() {
            sum = sum + value.as_int(interpreter.clone()).await?;
        }
        Ok(Value::Int(sum))
    }
}

pub async fn sub(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        let mut diff = values[0].as_float(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_float(interpreter.clone()).await?;
        }
        Ok(Value::Float(diff))
    } else {
        let mut diff = values[0].as_int(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_int(interpreter.clone()).await?;
        }
        Ok(Value::Int(diff))
    }
}

pub async fn mul(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        let mut product = values[0].as_float(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            product = product * value.as_float(interpreter.clone()).await?;
        }
        Ok(Value::Float(product))
    } else {
        let mut product = values[0].as_int(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            product = product * value.as_int(interpreter.clone()).await?;
        }
        Ok(Value::Int(product))
    }
}

pub async fn div(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        let mut quotient = values[0].as_float(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            let value = value.as_float(interpreter.clone()).await?;
            if value == 0.0 {
                return Err("division by zero".to_string());
            };
            quotient = quotient / value;
        }
        Ok(Value::Float(quotient))
    } else {
        let mut quotient = values[0].as_int(interpreter.clone()).await?;
        for value in &values[1..values.len()] {
            let value = value.as_int(interpreter.clone()).await?;
            if value == 0 {
                return Err("division by zero".to_string());
            };
            quotient = quotient / value;
        }
        Ok(Value::Int(quotient))
    }
}

pub async fn modulus(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let mut remainder = values[0].as_int(interpreter.clone()).await?;
    for value in &values[1..values.len()] {
        let value = value.as_int(interpreter.clone()).await?;
        if value == 0 {
            return Err("division by zero".to_string());
        };
        remainder = remainder % value;
    }
    Ok(Value::Int(remainder))
}

pub async fn store(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let label = &values[0].get_var_label()?;
    interpreter.vars.insert_value(label, &values[1]);
    Ok(interpreter.vars.get_value(label)?)
}

pub async fn list(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let list = values[0..values.len() - 1]
        .iter()
        .cloned()
        .collect::<Vec<Value>>();
    let list = Value::List(list);
    interpreter
        .vars
        .insert_value(&values[1].get_var_label()?, &list);
    Ok(list)
}

pub async fn free(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let label = &values[0].get_var_label()?;
    match interpreter.vars.remove_value(label) {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub async fn print(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let mut std_out = interpreter.std_out.lock().await;
    for value in values.iter() {
        std_out.push_str(&value.as_text(interpreter.clone()).await?);
    }
    Ok(Value::None)
}

pub async fn eq(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    Ok(Value::Bool(values[0] == values[1]))
}

pub async fn gt(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        Ok(Value::Bool(
            values[0].as_float(interpreter.clone()).await?
                > values[1].as_float(interpreter).await?,
        ))
    } else {
        Ok(Value::Bool(
            values[0].as_int(interpreter.clone()).await? > values[1].as_int(interpreter).await?,
        ))
    }
}

pub async fn lt(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        Ok(Value::Bool(
            values[0].as_float(interpreter.clone()).await?
                < values[1].as_float(interpreter).await?,
        ))
    } else {
        Ok(Value::Bool(
            values[0].as_int(interpreter.clone()).await? < values[1].as_int(interpreter).await?,
        ))
    }
}

pub async fn not(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok((!values[0].as_bool(interpreter).await?).into())
}

pub async fn and(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok((values[0].as_bool(interpreter.clone()).await?
        && values[1].as_bool(interpreter.clone()).await?)
        .into())
}

pub async fn or(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    Ok((values[0].as_bool(interpreter.clone()).await?
        || values[1].as_bool(interpreter.clone()).await?)
        .into())
}

pub async fn if_then(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn if_then_else(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn while_loop(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn repeat(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let repetitions = values[0].as_int(interpreter.clone()).await?;
    let command = values[1].as_command()?;
    let mut final_value = Value::None;
    for i in 0..repetitions {
        final_value = command.execute(interpreter.clone()).await?;
    }

    Ok(final_value)
}

pub async fn index_of(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn length_of(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn swap_indexes(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn insert_at(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn remove_at(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn replace_at(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    todo!()
}

pub async fn starts_with(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(interpreter.clone())
        .await?
        .starts_with(&values[1].as_text(interpreter.clone()).await?)
        .into())
}

pub async fn ends_with(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(interpreter.clone())
        .await?
        .ends_with(&values[1].as_text(interpreter.clone()).await?)
        .into())
}

pub async fn concat(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let mut cat_string = String::new();

    for value in values.iter() {
        cat_string.push_str(&value.as_text(interpreter.clone()).await?);
    }

    Ok(cat_string.into())
}

pub async fn capitalize(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let text = values[0].as_text(interpreter).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        Ok(format!("{}{}", text[0..1].to_uppercase(), text[1..].to_uppercase()).into())
    }
}

pub async fn upper(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0].as_text(interpreter).await?.to_uppercase().into())
}

pub async fn lower(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0].as_text(interpreter).await?.to_lowercase().into())
}

pub async fn remove_whitespace(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(interpreter)
        .await?
        .split_whitespace()
        .collect::<String>()
        .into())
}

pub async fn nl(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    Ok("\n".into())
}

pub async fn random_range(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if contains_float(&values, interpreter.clone()).await? {
        let min = values[0].as_float(interpreter.clone()).await?;
        let max = values[1].as_float(interpreter.clone()).await?;
        if min >= max {
            Err("min must be greater than max".to_string())
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let min = values[0].as_int(interpreter.clone()).await?;
        let max = values[1].as_int(interpreter.clone()).await?;
        if min >= max {
            Err("min must be greater than max".to_string())
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub async fn random_entry(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[rand::random_range(0..values.len())].clone())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::types::Command;

    use super::*;

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
        vars: Option<Arc<FslInterpreter>>,
    ) {
        let args = args.clone();
        let default = FslInterpreter::new();
        let vars = match vars {
            Some(vars) => vars,
            None => Arc::new(default),
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
        let interpreter = Arc::new(FslInterpreter::new());
        interpreter.vars.insert_value("one", &1.into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::Int(2),
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("sub"),
            Value::Int(0),
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mul"),
            Value::Int(1),
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("div"),
            Value::Int(1),
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mod"),
            Value::Int(0),
            Some(interpreter.clone()),
        )
        .await;
    }

    #[tokio::test]
    async fn math_wrong_var() {
        let interpreter = Arc::new(FslInterpreter::new());
        interpreter.vars.insert_value("one", &"one".into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::None,
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("sub"),
            Value::None,
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mul"),
            Value::None,
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("div"),
            Value::None,
            Some(interpreter.clone()),
        )
        .await;
        test_math(
            &args,
            &mut get_command("mod"),
            Value::None,
            Some(interpreter.clone()),
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
        let interpreter = Arc::new(FslInterpreter::new());
        interpreter.vars.insert_value("one", &1.0.into());
        interpreter
            .vars
            .insert_value("one_nested", &Value::Var("one".to_string()));
        let var = Value::Var("one_nested".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(
            &args,
            &mut get_command("add"),
            Value::Float(2.0),
            Some(interpreter),
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
        let interpreter = Arc::new(FslInterpreter::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command)];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await.unwrap();
        dbg!(&output);
        assert!(output == Value::Int(9));
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_wrong_repititions() {
        let interpreter = Arc::new(FslInterpreter::new());
        let mut vars = Arc::new(VarMap::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec!["five".into(), Value::Command(command)];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_wrong_thing_to_repeat() {
        let interpreter = Arc::new(FslInterpreter::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), 5.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    #[should_panic]
    async fn repeat_too_many_args() {
        let interpreter = Arc::new(FslInterpreter::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command), 5.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    async fn create_var() {
        let interpreter = Arc::new(FslInterpreter::new());
        let result = store(
            Arc::new(vec![Value::Var("n".to_string()), Value::Int(1)]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        assert!(result == Value::Int(1));
        assert!(interpreter.vars.get_value("n").unwrap() == Value::Int(1));
    }

    #[tokio::test]
    async fn create_list() {
        let interpreter = Arc::new(FslInterpreter::new());
        let result = store(
            Arc::new(vec![
                Value::Var("list".to_string()),
                Value::List(vec!["1".into(), 2.into(), 3.0.into()]),
            ]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        let cmp_list = Value::List(vec!["1".into(), 2.into(), 3.0.into()]);
        assert!(result == cmp_list);
        assert!(interpreter.vars.get_value("list").unwrap() == cmp_list);
    }

    #[tokio::test]
    async fn free_var() {
        let interpreter = Arc::new(FslInterpreter::new());
        let vars = Arc::new(VarMap::new());
        store(
            Arc::new(vec![Value::Var("n".to_string()), Value::Int(1)]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        let result = free(
            Arc::new(vec![Value::Var("n".to_string())]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        assert!(result == Value::Int(1));
        assert!(vars.get_value("n").is_err());
    }

    #[tokio::test]
    async fn print_values() {
        let interpreter = Arc::new(FslInterpreter::new());
        print(
            Arc::new(vec!["One plus one equals ".into(), 2.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        assert!(interpreter.std_out.lock().await.eq("One plus one equals 2"));
    }

    #[tokio::test]
    async fn value_eq_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = eq(Arc::new(vec!["1".into(), "1".into()]), interpreter.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = eq(Arc::new(vec![1.into(), 1.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = eq(Arc::new(vec![1.into(), 1.3.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn value_gt_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = gt(Arc::new(vec!["1".into(), "2".into()]), interpreter.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);

        let result = gt(Arc::new(vec![2.into(), 1.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = gt(Arc::new(vec![1.into(), 1.3.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn value_lt_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = lt(Arc::new(vec!["1".into(), "2".into()]), interpreter.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = lt(Arc::new(vec![2.into(), 1.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);

        let result = lt(Arc::new(vec![1.into(), 1.3.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);
    }

    #[tokio::test]
    async fn bool_logic_not() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = not(Arc::new(vec![false.into()]), interpreter.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = not(Arc::new(vec![true.into()]), interpreter.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn bool_logic_and() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = and(
            Arc::new(vec![false.into(), true.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == false);

        let result = and(
            Arc::new(vec![true.into(), true.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);
    }

    #[tokio::test]
    async fn bool_logic_or() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = or(
            Arc::new(vec![false.into(), true.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);

        let result = or(
            Arc::new(vec![true.into(), true.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.clone()).await.unwrap() == true);
    }
}
