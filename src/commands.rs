use std::sync::Arc;

use crate::{
    FslInterpreter, contains_float,
    types::{ArgPos, ArgRule, Error, FslType, Value},
};

pub const ALL_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Var,
    FslType::Command,
    FslType::None,
];

pub const NON_NONE_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Var,
    FslType::Command,
];

pub const VAR_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Var,
];

pub const NUMERIC_TYPES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const LOGIC_TYPES: &[FslType] = &[FslType::Command, FslType::Var, FslType::Text];

pub const NO_RULES: &[ArgRule] = &[ArgRule {
    position: ArgPos::None,
    valid_types: &[],
}];

pub const MATH_RULES: &[ArgRule] = &[
    ArgRule {
        position: ArgPos::AnyAfter(0),
        valid_types: NUMERIC_TYPES,
    },
    ArgRule {
        position: ArgPos::Index(0),
        valid_types: NUMERIC_TYPES,
    },
    ArgRule {
        position: ArgPos::Index(1),
        valid_types: NUMERIC_TYPES,
    },
];

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

pub const STORE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Var]),
];

pub async fn store(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let label = &values[0].get_var_label()?;
    match &values[1] {
        Value::Var(var) => {
            let value = interpreter
                .vars
                .get_value(var)?
                .as_raw(interpreter.clone())
                .await?;
            interpreter.vars.insert_value(label, &value);
        }
        Value::Command(command) => {
            interpreter
                .clone()
                .vars
                .insert_value(label, &command.execute(interpreter.clone()).await?);
        }
        Value::None => todo!(),
        _ => {
            interpreter.vars.insert_value(label, &values[1]);
        }
    }
    Ok(interpreter.vars.get_value(label)?)
}

pub const FREE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
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

pub const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
pub async fn print(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let mut output = String::new();

    for value in values.iter() {
        output.push_str(&value.as_text(interpreter.clone()).await?);
    }

    let mut std_out = interpreter.output.lock().await;
    std_out.push_str(&output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
pub async fn scope(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    for value in values.iter() {
        value.as_raw(interpreter.clone()).await?;
    }

    Ok(Value::None)
}

pub const EQ_RULES: &'static [ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub async fn eq(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    let a = values[0].as_raw(interpreter.clone()).await?;
    let b = values[1].as_raw(interpreter.clone()).await?;

    Ok(Value::Bool(a == b))
}

pub const GT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
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

pub const LT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
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

pub const NOT_RULES: &[ArgRule; 1] = &[ArgRule::new(ArgPos::Index(0), LOGIC_TYPES)];
pub async fn not(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok((!values[0].as_bool(interpreter).await?).into())
}

pub const AND_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), LOGIC_TYPES),
];
pub async fn and(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok((values[0].as_bool(interpreter.clone()).await?
        && values[1].as_bool(interpreter.clone()).await?)
        .into())
}

pub const OR_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), LOGIC_TYPES),
];
pub async fn or(values: Arc<Vec<Value>>, interpreter: Arc<FslInterpreter>) -> Result<Value, Error> {
    Ok((values[0].as_bool(interpreter.clone()).await?
        || values[1].as_bool(interpreter.clone()).await?)
        .into())
}

pub const IF_THEN_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
];
pub async fn if_then(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if values[0].as_bool(interpreter.clone()).await? {
        let result = values[1].as_command()?.execute(interpreter).await?;
        Ok(result)
    } else {
        Ok(Value::None)
    }
}

pub const IF_THEN_ELSE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
    ArgRule::new(ArgPos::Index(2), &[FslType::Command]),
];
pub async fn if_then_else(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    if values[0].as_bool(interpreter.clone()).await? {
        let result = values[1].as_command()?.execute(interpreter).await?;
        Ok(result)
    } else {
        let result = values[2].as_command()?.execute(interpreter).await?;
        Ok(result)
    }
}

pub const WHILE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyAfter(1), &[FslType::Command]),
];
pub async fn while_loop(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let mut final_value = Value::None;

    while values[0].as_bool(interpreter.clone()).await? {
        for command in &values[1..] {
            let command = command.as_command()?;
            final_value = command.execute(interpreter.clone()).await?;
        }
        interpreter.increment_loops().await?;
    }

    Ok(final_value)
}

pub const REPEAT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::AnyAfter(1), &[FslType::Command]),
];
pub async fn repeat(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let repetitions = values[0].as_int(interpreter.clone()).await?;
    let mut final_value = Value::None;

    for _ in 0..repetitions {
        for command in &values[1..] {
            let command = command.as_command()?;
            final_value = command.execute(interpreter.clone()).await?;
        }
        interpreter.increment_loops().await?;
    }

    Ok(final_value)
}

pub const INDEX_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn index(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let list = array.as_list(interpreter.clone()).await?;
        let index = values[1].as_int(interpreter).await?;
        match list.get(index as usize) {
            Some(value) => Ok(value.clone()),
            None => Err(format!("index {} out of bounds in list {:?}", index, list)),
        }
    } else {
        let text = array.as_text(interpreter.clone()).await?;
        let index = values[1].as_int(interpreter).await?;
        match text.chars().nth(index as usize) {
            Some(char) => Ok(char.into()),
            None => Err(format!("index {} out of bounds in text {}", index, text)),
        }
    }
}

pub const LENGTH_RULES: [ArgRule; 1] = [ArgRule::new(
    ArgPos::Index(0),
    &[FslType::List, FslType::Text],
)];
pub async fn length(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let list = array.as_list(interpreter).await?;
        Ok((list.len() as i64).into())
    } else {
        let text = array.as_text(interpreter).await?;
        Ok((text.len() as i64).into())
    }
}

pub const SWAP_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub async fn swap(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    let a = values[1].as_int(interpreter.clone()).await? as usize;
    let b = values[2].as_int(interpreter.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(interpreter).await?;
        if a > list.len() || b > list.len() {
            Err("cannot swap indexes that are not in bounds".to_string())
        } else {
            let tmp = list[a].clone();
            list[a] = list[b].clone();
            list[b] = tmp;
            Ok(list.into())
        }
    } else {
        let text = array.as_text(interpreter).await?;

        if a > text.len() || b > text.len() {
            Err("cannot swap indexes that are not in bounds".to_string())
        } else {
            let mut text = text.chars().collect::<Vec<char>>();
            let tmp = text[a].clone();
            text[a] = text[b].clone();
            text[b] = tmp;
            Ok(text.iter().collect::<String>().into())
        }
    }
}

pub const INSERT_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub async fn insert(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    let i = values[2].as_int(interpreter.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(interpreter.clone()).await?;
        let to_insert = values[1].as_raw(interpreter).await?;

        if i > list.len() {
            Err("cannot insert value at index greater than size of list".to_string())
        } else {
            list.insert(i, to_insert);
            Ok(list.into())
        }
    } else {
        let mut text = array.as_text(interpreter.clone()).await?;
        let to_insert = values[1].as_text(interpreter).await?;

        if i > text.len() {
            Err("cannot insert value at index greater than size of text".to_string())
        } else {
            text.insert_str(i, &to_insert);
            Ok(text.into())
        }
    }
}

pub const REMOVE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn remove(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(interpreter.clone()).await?;
        let i = values[1].as_int(interpreter).await? as usize;
        match list.get(i) {
            Some(_) => {
                list.remove(i);
                Ok(list.into())
            }
            None => Err(format!("index {} out of bounds in list {:?}", i, list)),
        }
    } else {
        let mut text = array.as_text(interpreter.clone()).await?;
        let i = values[1].as_int(interpreter).await? as usize;
        match text.chars().nth(i) {
            Some(_) => {
                text.remove(i);
                Ok(text.into())
            }
            None => Err(format!("index {} out of bounds in text {}", i, text)),
        }
    }
}

pub const REPLACE_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub async fn replace(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let array = values[0].as_raw(interpreter.clone()).await?;

    let i = values[2].as_int(interpreter.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(interpreter).await?;
        let replacement = values[1].clone();
        if i > list.len() {
            Err("cannot replace index that is out of bounds".to_string())
        } else {
            list[i] = replacement;
            Ok(list.into())
        }
    } else {
        let mut text = array.as_text(interpreter.clone()).await?;
        let replacement = values[1].as_text(interpreter).await?;

        if i > text.len() {
            Err("cannot replace index that is out of bounds".to_string())
        } else {
            text.replace_range(i..i + 1, &replacement);
            Ok(text.into())
        }
    }
}

pub const STARTS_WITH_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
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

pub const ENDS_WITH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
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

pub const CONCAT_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
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

pub const CAPITALIZE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn capitalize(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    let text = values[0].as_text(interpreter).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        Ok(format!("{}{}", text[0..1].to_uppercase(), &text[1..]).into())
    }
}

pub const UPPERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn uppercase(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0].as_text(interpreter).await?.to_uppercase().into())
}

pub const LOWERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn lowercase(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[0].as_text(interpreter).await?.to_lowercase().into())
}

pub const REMOVE_WHITESPACE_RULES: [ArgRule; 1] =
    [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
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

pub const RANDOM_RANGE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
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

pub const RANDOM_ENTRY_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::List])];
pub async fn random_entry(
    values: Arc<Vec<Value>>,
    interpreter: Arc<FslInterpreter>,
) -> Result<Value, Error> {
    Ok(values[rand::random_range(0..values.len())].clone())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::VarMap;
    use crate::types::Command;

    use super::*;

    fn get_command(label: &str) -> Command {
        let interpreter = FslInterpreter::new();
        interpreter.commands.get(label).cloned().unwrap()
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
        let mut command = interpreter.commands.get("add").cloned().unwrap();
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
        let mut command = interpreter.commands.get("add").cloned().unwrap();
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
    async fn repeat_limit() {
        let interpreter = Arc::new(FslInterpreter::new());
        let args = vec![5.into(), 4.into()];
        let mut command = get_command("add");
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![
            ((interpreter.loop_limit.unwrap() + 1) as i64).into(),
            Value::Command(command),
        ];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await;
        dbg!(&output);
        assert!(output.is_err());
    }

    #[tokio::test]
    async fn repeat_limit_nested() {
        let interpreter = Arc::new(FslInterpreter::new());
        let mut add_command = get_command("add");
        add_command.set_args(vec![5.into(), 4.into()]);
        let add_command = Arc::new(add_command);

        let loops = Value::Int((interpreter.loop_limit.unwrap() - 10) as i64);
        let mut nested_repeat = get_command("repeat");
        nested_repeat.set_args(vec![loops.clone(), Value::Command(add_command)]);
        let nested_repeat = Arc::new(nested_repeat);

        let args = vec![2.into(), nested_repeat.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter).await;
        dbg!(&output);
        assert!(output.is_err());
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

    /*
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
    */

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

        assert!(interpreter.output.lock().await.eq("One plus one equals 2"));
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

    #[tokio::test]
    async fn logic_if_then() {
        let interpreter = Arc::new(FslInterpreter::new());

        let mut print_command = get_command("print");
        print_command.set_args(vec!["hello".into()]);
        let print_command = Arc::new(print_command);
        let result = if_then(
            Arc::new(vec![true.into(), print_command.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.output.lock().await);
        assert!(*interpreter.output.lock().await == "hello");

        let mut print_command = get_command("print");
        print_command.set_args(vec![", world".into()]);
        let print_command = Arc::new(print_command);
        let result = if_then(
            Arc::new(vec![true.into(), print_command.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.output.lock().await);
        assert!(*interpreter.output.lock().await == "hello, world");
    }

    #[tokio::test]
    async fn logic_if_then_else() {
        let interpreter = Arc::new(FslInterpreter::new());

        let mut print_command = get_command("print");
        print_command.set_args(vec!["hello".into()]);
        let print_command = Arc::new(print_command);

        let mut print_command2 = get_command("print");
        print_command2.set_args(vec!["goodbye".into()]);
        let print_command2 = Arc::new(print_command2);

        let result = if_then_else(
            Arc::new(vec![
                true.into(),
                print_command.clone().into(),
                print_command2.clone().into(),
            ]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.output.lock().await);
        assert!(*interpreter.output.lock().await == "hello");

        let result = if_then_else(
            Arc::new(vec![
                false.into(),
                print_command.into(),
                print_command2.into(),
            ]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.output.lock().await);
        assert!(*interpreter.output.lock().await == "hellogoodbye");
    }

    #[tokio::test]
    async fn logic_while() {
        let interpreter = Arc::new(FslInterpreter::new());

        let mut add_command = get_command("add");
        add_command.set_args(vec![5.into(), 4.into()]);
        let add_command = Arc::new(add_command);
        let result = while_loop(Arc::new(vec![true.into(), add_command.into()]), interpreter).await;
        dbg!(result.clone());
        assert!(result.is_err_and(|e| e.contains("loop")));
    }

    #[tokio::test]
    async fn get_index() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = index(Arc::new(vec![list, 1.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result == Value::Int(2));

        let result = index(Arc::new(vec![text, 1.into()]), interpreter)
            .await
            .unwrap();
        assert!(result == Value::Text("x".into()));
    }

    #[tokio::test]
    async fn get_length() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = length(Arc::new(vec![list]), interpreter.clone())
            .await
            .unwrap();
        assert!(result == Value::Int(3));

        let result = length(Arc::new(vec![text]), interpreter).await.unwrap();
        assert!(result == Value::Int(7));
    }

    #[tokio::test]
    async fn array_swap_indicies() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = swap(
            Arc::new(vec![list, 1.into(), 2.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 3.into(), 2.into()]));

        let result = swap(Arc::new(vec![text, 1.into(), 2.into()]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("eaxmple".into()));
    }

    #[tokio::test]
    async fn array_insert() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = insert(
            Arc::new(vec![list, 10.into(), 1.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 10.into(), 2.into(), 3.into()]));

        let result = insert(Arc::new(vec![text, 10.into(), 2.into()]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("ex10ample".into()));
    }

    #[tokio::test]
    async fn remove_index() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = remove(Arc::new(vec![list, 1.into()]), interpreter.clone())
            .await
            .unwrap();
        assert!(result == Value::List(vec![1.into(), 3.into()]));

        let result = remove(Arc::new(vec![text, 1.into()]), interpreter)
            .await
            .unwrap();
        assert!(result == Value::Text("eample".into()));
    }

    #[tokio::test]
    async fn array_replace_at() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = replace(
            Arc::new(vec![list, 10.into(), 1.into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 10.into(), 3.into()]));

        let result = replace(Arc::new(vec![text, 10.into(), 2.into()]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("ex10mple".into()));
    }

    #[tokio::test]
    async fn text_starts_with() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());
        let result = starts_with(
            Arc::new(vec![text.clone(), "ex".into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(true));
        let result = starts_with(Arc::new(vec![text, "x".into()]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(false));
    }

    #[tokio::test]
    async fn text_ends_with() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());
        let result = ends_with(
            Arc::new(vec![text.clone(), "le".into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(true));
        let result = starts_with(Arc::new(vec![text, "x".into()]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(false));
    }

    #[tokio::test]
    async fn concat_values() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());
        let number = Value::Int(10);
        let bool = Value::Bool(false);

        let result = concat(Arc::new(vec![text, number, bool]), interpreter)
            .await
            .unwrap();
        assert!(result == Value::Text("example10false".into()));
    }

    #[tokio::test]
    async fn text_capitalize() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());

        let result = capitalize(Arc::new(vec![text]), interpreter).await.unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("Example".into()));
    }

    #[tokio::test]
    async fn text_uppercase() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());

        let result = uppercase(Arc::new(vec![text]), interpreter).await.unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("EXAMPLE".into()));
    }

    #[tokio::test]
    async fn text_lowercase() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("EXAMPLE".into());

        let result = lowercase(Arc::new(vec![text]), interpreter).await.unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("example".into()));
    }

    #[tokio::test]
    async fn text_remove_whitespace() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("e\nx\t a m p l e".into());

        let result = remove_whitespace(Arc::new(vec![text]), interpreter)
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("example".into()));
    }

    #[tokio::test]
    async fn get_random_range() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = random_range(Arc::new(vec![1.into(), 6.into()]), interpreter.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result.is_type(crate::types::FslType::Int));
        let rnd = result.as_int(interpreter).await.unwrap();
        assert!(rnd <= 6 && rnd >= 1)
    }

    #[tokio::test]
    async fn get_random_entry() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = random_entry(
            Arc::new(vec!["one".into(), "two".into(), "three".into()]),
            interpreter.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result.is_type(crate::types::FslType::Text));
        let rnd = result.as_text(interpreter).await.unwrap();
        assert!(rnd == "one" || rnd == "two" || rnd == "three");
    }
}
