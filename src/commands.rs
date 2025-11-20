use std::sync::{Arc, atomic::Ordering};

use async_recursion::async_recursion;

use crate::{
    DIV, ErrorContext, FslError, FslInterpreter, INDEX, InterpreterData, MODULUS, RANDOM_RANGE,
    REPEAT, SWAP, WHILE_LOOP,
    types::{ArgPos, ArgRule, FslType, Value},
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

#[async_recursion]
async fn contains_float(values: &Vec<Value>, data: Arc<InterpreterData>) -> Result<bool, FslError> {
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
            Value::Var(var) => {
                if data
                    .clone()
                    .vars
                    .get_value(var)?
                    .as_raw(data.clone())
                    .await?
                    .is_type(FslType::Float)
                {
                    return Ok(true);
                }
            }
            Value::Command(command) => {
                return Ok(contains_float(command.get_args(), data).await?);
            }
            _ => {
                continue;
            }
        }
    }
    Ok(false)
}

pub async fn add(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        let mut sum: f64 = 0.0;
        for value in values.iter() {
            sum = sum + value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum: i64 = 0;
        for value in values.iter() {
            sum = sum + value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(sum))
    }
}

pub async fn sub(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        let mut diff = values[0].as_float(data.clone()).await?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(diff))
    } else {
        let mut diff = values[0].as_int(data.clone()).await?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(diff))
    }
}

pub async fn mul(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        let mut product = values[0].as_float(data.clone()).await?;
        for value in &values[1..values.len()] {
            product = product * value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(product))
    } else {
        let mut product = values[0].as_int(data.clone()).await?;
        for value in &values[1..values.len()] {
            product = product * value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(product))
    }
}

pub async fn div(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        let mut quotient = values[0].as_float(data.clone()).await?;
        for value in &values[1..values.len()] {
            let value = value.as_float(data.clone()).await?;
            if value == 0.0 {
                return Err(FslError::DivisionByZero(ErrorContext::new(
                    DIV.into(),
                    "".into(),
                )));
            };
            quotient = quotient / value;
        }
        Ok(Value::Float(quotient))
    } else {
        let mut quotient = values[0].as_int(data.clone()).await?;
        for value in &values[1..values.len()] {
            let value = value.as_int(data.clone()).await?;
            if value == 0 {
                return Err(FslError::DivisionByZero(ErrorContext::new(
                    DIV.into(),
                    "".into(),
                )));
            };
            quotient = quotient / value;
        }
        Ok(Value::Int(quotient))
    }
}

pub async fn modulus(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut remainder = values[0].as_int(data.clone()).await?;
    for value in &values[1..values.len()] {
        let value = value.as_int(data.clone()).await?;
        if value == 0 {
            return Err(FslError::DivisionByZero(ErrorContext::new(
                MODULUS.into(),
                "".into(),
            )));
        };
        remainder = remainder % value;
    }
    Ok(Value::Int(remainder))
}

pub const STORE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Var]),
];

pub async fn store(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let label = &values[0].get_var_label()?;
    match &values[1] {
        Value::Var(var) => {
            let value = data.vars.get_value(var)?.as_raw(data.clone()).await?;
            data.vars.insert_value(label, &value)?;
        }
        Value::Command(command) => {
            data.clone()
                .vars
                .insert_value(label, &command.execute(data.clone()).await?)?;
        }
        Value::None => {
            return Err(FslError::CannotStoreValueInVar(format!(
                "Cannot store {} in a var",
                FslType::None.as_str()
            )));
        }
        _ => {
            data.vars.insert_value(label, &values[1])?;
        }
    }
    Ok(data.vars.get_value(label)?)
}

pub const CLONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn clone(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let value = &values[0].get_var_value(data)?;
    Ok(value.clone())
}

pub const FREE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn free(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let label = &values[0].get_var_label()?;
    match data.vars.remove_value(label) {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
pub async fn print(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut output = String::new();

    for value in values.iter() {
        output.push_str(&value.as_text(data.clone()).await?);
    }

    let mut std_out = data.output.lock().await;
    std_out.push_str(&output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
pub async fn scope(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    for value in values.iter() {
        value.as_raw(data.clone()).await?;
    }

    Ok(Value::None)
}

pub const EQ_RULES: &'static [ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub async fn eq(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let a = values[0].as_raw(data.clone()).await?;
    let b = values[1].as_raw(data.clone()).await?;

    Ok(Value::Bool(a == b))
}

pub const GT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn gt(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        Ok(Value::Bool(
            values[0].as_float(data.clone()).await? > values[1].as_float(data).await?,
        ))
    } else {
        Ok(Value::Bool(
            values[0].as_int(data.clone()).await? > values[1].as_int(data).await?,
        ))
    }
}

pub const LT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn lt(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        Ok(Value::Bool(
            values[0].as_float(data.clone()).await? < values[1].as_float(data).await?,
        ))
    } else {
        Ok(Value::Bool(
            values[0].as_int(data.clone()).await? < values[1].as_int(data).await?,
        ))
    }
}

pub const NOT_RULES: &[ArgRule; 1] = &[ArgRule::new(ArgPos::Index(0), LOGIC_TYPES)];
pub async fn not(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    Ok((!values[0].as_bool(data).await?).into())
}

pub const AND_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), LOGIC_TYPES)];
pub async fn and(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut result = values[0].as_bool(data.clone()).await?;
    for value in &values[1..values.len()] {
        result = result && value.as_bool(data.clone()).await?;
    }
    Ok(result.into())
}

pub const OR_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyAfter(0), LOGIC_TYPES)];
pub async fn or(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut result = values[0].as_bool(data.clone()).await?;
    for value in &values[1..values.len()] {
        result = result || value.as_bool(data.clone()).await?;
    }
    Ok(result.into())
}

pub const IF_THEN_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
];
pub async fn if_then(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    if values[0].as_bool(data.clone()).await? {
        let result = values[1].as_command()?.execute(data).await?;
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
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    if values[0].as_bool(data.clone()).await? {
        let result = values[1].as_command()?.execute(data).await?;
        Ok(result)
    } else {
        let result = values[2].as_command()?.execute(data).await?;
        Ok(result)
    }
}

pub const WHILE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyAfter(1), &[FslType::Command]),
];
pub async fn while_command(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut final_value = Value::None;

    'outer: while values[0].as_bool(data.clone()).await? {
        for command in &values[1..] {
            let command = command.as_command()?;
            final_value = command.execute(data.clone()).await?;
            if data.continue_flag.load(Ordering::Relaxed) {
                data.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.break_flag.load(Ordering::Relaxed) {
                data.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops(WHILE_LOOP).await?;
    }

    Ok(final_value)
}

pub const REPEAT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::AnyAfter(1), &[FslType::Command]),
];
pub async fn repeat(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let repetitions = values[0].as_int(data.clone()).await?;
    let mut final_value = Value::None;

    'outer: for _ in 0..repetitions {
        for command in &values[1..] {
            let command = command.as_command()?;
            final_value = command.execute(data.clone()).await?;
            if data.continue_flag.load(Ordering::Relaxed) {
                data.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.break_flag.load(Ordering::Relaxed) {
                data.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops(REPEAT).await?;
    }

    Ok(final_value)
}

pub const INDEX_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn index(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let list = array.as_list(data.clone()).await?;
        let i = values[1].as_int(data.clone()).await?;
        match list.get(i as usize) {
            Some(value) => Ok(value.as_raw(data).await?),
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                INDEX.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            ))),
        }
    } else {
        let text = array.as_text(data.clone()).await?;
        let i = values[1].as_int(data).await?;
        match text.chars().nth(i as usize) {
            Some(char) => Ok(char.into()),
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                INDEX.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            ))),
        }
    }
}

pub const LENGTH_RULES: [ArgRule; 1] = [ArgRule::new(
    ArgPos::Index(0),
    &[FslType::List, FslType::Text],
)];
pub async fn length(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let list = array.as_list(data).await?;
        Ok((list.len() as i64).into())
    } else {
        let text = array.as_text(data).await?;
        Ok((text.len() as i64).into())
    }
}

pub const SWAP_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub async fn swap(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    let a = values[1].as_int(data.clone()).await? as usize;
    let b = values[2].as_int(data.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        if list.get(a).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", a, list),
            )))
        } else if list.get(b).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", b, list),
            )))
        } else {
            let tmp = list[a].clone();
            list[a] = list[b].clone();
            list[b] = tmp;
            let list = Value::List(list);
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &list)?;
            }
            Ok(list)
        }
    } else {
        let text = array.as_text(data.clone()).await?;

        if text.chars().nth(a).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", a, text),
            )))
        } else if text.chars().nth(b).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", b, text),
            )))
        } else {
            let mut text = text.chars().collect::<Vec<char>>();
            let tmp = text[a].clone();
            text[a] = text[b].clone();
            text[b] = tmp;
            let text = Value::Text(text.iter().collect::<String>().into());
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &text)?;
            }
            Ok(text)
        }
    }
}

pub const INSERT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub async fn insert(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    let i = values[2].as_int(data.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let to_insert = values[1].as_raw(data.clone()).await?;

        if i > list.len() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            )))
        } else {
            list.insert(i, to_insert);
            let list = Value::List(list);
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &list)?;
            }
            Ok(list)
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let to_insert = values[1].as_text(data.clone()).await?;

        if i > text.len() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            )))
        } else {
            text.insert_str(i, &to_insert);
            let text = Value::Text(text);
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &text)?;
            }
            Ok(text)
        }
    }
}

pub const PUSH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub async fn push(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    if let Ok(mut list) = values[0].as_list(data.clone()).await {
        let to_push = values[1].as_raw(data.clone()).await?;

        list.push(to_push);
        let list = Value::List(list);
        if values[0].is_type(FslType::Var) {
            data.vars.insert_value(&values[0].get_var_label()?, &list)?;
        }
        Ok(list)
    } else {
        let mut text = values[0].as_text(data.clone()).await?;
        let to_insert = values[1].as_text(data.clone()).await?;

        text.push_str(&to_insert);
        let text = Value::Text(text);
        if values[0].is_type(FslType::Var) {
            data.vars.insert_value(&values[0].get_var_label()?, &text)?;
        }
        Ok(text)
    }
}

pub const POP_RULES: &[ArgRule] = &[ArgRule::new(
    ArgPos::Index(0),
    &[FslType::List, FslType::Text],
)];
pub async fn pop(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(data.clone()).await?;

        let ret_value = list.pop();
        let list = Value::List(list);
        if values[0].is_type(FslType::Var) {
            data.vars.insert_value(&values[0].get_var_label()?, &list)?;
        }
        Ok(ret_value.unwrap_or(Value::None))
    } else {
        let mut text = array.as_text(data.clone()).await?;

        let ret_value = text.pop();
        let text = Value::Text(text);
        if values[0].is_type(FslType::Var) {
            data.vars.insert_value(&values[0].get_var_label()?, &text)?;
        }
        match ret_value {
            Some(ch) => Ok(Value::Text(ch.to_string())),
            None => Ok(Value::None),
        }
    }
}

pub const REMOVE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn remove(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let i = values[1].as_int(data.clone()).await? as usize;
        match list.get(i) {
            Some(_) => {
                list.remove(i);
                let list = Value::List(list);
                if values[0].is_type(FslType::Var) {
                    data.vars.insert_value(&values[0].get_var_label()?, &list)?;
                }
                Ok(list)
            }
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            ))),
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let i = values[1].as_int(data.clone()).await? as usize;
        match text.chars().nth(i) {
            Some(_) => {
                text.remove(i);
                let text = Value::Text(text);
                if values[0].is_type(FslType::Var) {
                    data.vars.insert_value(&values[0].get_var_label()?, &text)?;
                }
                Ok(text)
            }
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            ))),
        }
    }
}

pub const REPLACE_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Int]),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub async fn replace(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let array = values[0].as_raw(data.clone()).await?;

    let i = values[1].as_int(data.clone()).await? as usize;

    if array.is_type(crate::types::FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let replacement = values[2].as_raw(data.clone()).await?;
        if list.get(i).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            )))
        } else {
            list[i] = replacement;
            let list = Value::List(list);
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &list)?;
            }
            Ok(list)
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let replacement = values[2].as_text(data.clone()).await?;

        if text.chars().nth(i).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            )))
        } else {
            text.replace_range(i..i + 1, &replacement);
            let text = Value::Text(text);
            if values[0].is_type(FslType::Var) {
                data.vars.insert_value(&values[0].get_var_label()?, &text)?;
            }
            Ok(text)
        }
    }
}
pub const INC_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn inc(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let n = values[0].as_int(data.clone()).await?;
    let new_value = Value::Int(n + 1);
    data.vars
        .insert_value(&values[0].get_var_label()?, &new_value)?;
    Ok(new_value)
}

pub const DEC_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn dec(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let n = values[0].as_int(data.clone()).await?;
    let new_value = Value::Int(n - 1);
    data.vars
        .insert_value(&values[0].get_var_label()?, &new_value)?;
    Ok(new_value)
}

pub const CONTAINS_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub async fn contains(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let a = values[0].as_raw(data.clone()).await?;
    let b = values[1].as_raw(data.clone()).await?;
    if let Value::List(list) = a {
        for value in list {
            match value.as_raw(data.clone()).await?.cmp(&b) {
                Ok(is_eq) => {
                    if is_eq {
                        return Ok(Value::Bool(true));
                    }
                }
                Err(_) => {}
            }
        }
        return Ok(Value::Bool(false));
    } else {
        let text = a.as_text(data.clone()).await?;
        return Ok(Value::Bool(text.contains(&b.as_text(data).await?)));
    }
}

pub const STARTS_WITH_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub async fn starts_with(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    Ok(values[0]
        .as_text(data.clone())
        .await?
        .starts_with(&values[1].as_text(data.clone()).await?)
        .into())
}

pub const ENDS_WITH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub async fn ends_with(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    Ok(values[0]
        .as_text(data.clone())
        .await?
        .ends_with(&values[1].as_text(data.clone()).await?)
        .into())
}

pub const CONCAT_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::AnyAfter(0), NON_NONE_VALUES)];
pub async fn concat(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut cat_string = String::new();

    for value in values.iter() {
        cat_string.push_str(&value.as_text(data.clone()).await?);
    }

    Ok(cat_string.into())
}

pub const CAPITALIZE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn capitalize(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let text = values[0].as_text(data).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        Ok(format!("{}{}", text[0..1].to_uppercase(), &text[1..]).into())
    }
}

pub const UPPERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn uppercase(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    Ok(values[0].as_text(data).await?.to_uppercase().into())
}

pub const LOWERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn lowercase(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    Ok(values[0].as_text(data).await?.to_lowercase().into())
}

pub const REMOVE_WHITESPACE_RULES: [ArgRule; 1] =
    [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub async fn remove_whitespace(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    Ok(values[0]
        .as_text(data)
        .await?
        .split_whitespace()
        .collect::<String>()
        .into())
}

pub const SPLIT_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub async fn split(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let text_to_split = values[0].as_text(data.clone()).await?;
    let pattern = values[1].as_text(data.clone()).await?;
    let split = text_to_split
        .split(&pattern)
        .map(|s| Value::Text(s.to_string()))
        .collect::<Vec<Value>>();
    Ok(Value::List(split))
}

pub const RANDOM_RANGE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub async fn random_range(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    if contains_float(&values, data.clone()).await? {
        let min = values[0].as_float(data.clone()).await?;
        let max = values[1].as_float(data.clone()).await?;
        if min >= max {
            Err(FslError::InvalidRange(ErrorContext::new(
                RANDOM_RANGE.into(),
                "min must be greater than max".into(),
            )))
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let min = values[0].as_int(data.clone()).await?;
        let max = values[1].as_int(data.clone()).await?;
        if min >= max {
            Err(FslError::InvalidRange(ErrorContext::new(
                RANDOM_RANGE.into(),
                "min must be greater than max".into(),
            )))
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub const RANDOM_ENTRY_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::List])];
pub async fn random_entry(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let list = values[0].as_list(data).await?;
    Ok(list[rand::random_range(0..list.len())].clone())
}

pub async fn break_command(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    data.break_flag
        .store(true, std::sync::atomic::Ordering::Relaxed);
    Ok(Value::None)
}

pub async fn continue_command(
    values: Arc<Vec<Value>>,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    data.continue_flag
        .store(true, std::sync::atomic::Ordering::Relaxed);
    Ok(Value::None)
}

pub async fn exit(values: Arc<Vec<Value>>, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    Ok(Value::None)
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
        let interpreter = match vars {
            Some(vars) => vars,
            None => Arc::new(default),
        };
        match expected_value {
            Value::None => {
                // Should be error
                command.set_args(args);
                let output = command.execute(interpreter.data.clone()).await;
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
                let output = command.execute(interpreter.data.clone()).await.unwrap();
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
        interpreter.data.vars.insert_value("one", &1.into());
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
        interpreter.data.vars.insert_value("one", &"one".into());
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
    async fn detects_float_in_var() {
        let interpreter = Arc::new(FslInterpreter::new());
        interpreter
            .data
            .vars
            .insert_value("one", &1.0.into())
            .unwrap();
        let var = Value::Var("one".to_string());
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
        let output = command.execute(interpreter.data.clone()).await.unwrap();
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
            ((interpreter.data.clone().loop_limit.unwrap() + 1) as i64).into(),
            Value::Command(command),
        ];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter.data.clone()).await;
        dbg!(&output);
        assert!(output.is_err());
    }

    #[tokio::test]
    async fn repeat_limit_nested() {
        let interpreter = Arc::new(FslInterpreter::new());
        let mut add_command = get_command("add");
        add_command.set_args(vec![5.into(), 4.into()]);
        let add_command = Arc::new(add_command);

        let loops = Value::Int((interpreter.data.clone().loop_limit.unwrap() - 10) as i64);
        let mut nested_repeat = get_command("repeat");
        nested_repeat.set_args(vec![loops.clone(), Value::Command(add_command)]);
        let nested_repeat = Arc::new(nested_repeat);

        let args = vec![2.into(), nested_repeat.into()];
        let mut command = get_command("repeat");
        command.set_args(args);
        let output = command.execute(interpreter.data.clone()).await;
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
        let output = command.execute(interpreter.data.clone()).await;
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
        let output = command.execute(interpreter.data.clone()).await;
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
        let output = command.execute(interpreter.data.clone()).await;
        dbg!(&output);
        output.unwrap();
    }

    #[tokio::test]
    async fn create_var() {
        let interpreter = Arc::new(FslInterpreter::new());
        let result = store(
            Arc::new(vec![Value::Var("n".to_string()), Value::Int(1)]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        assert!(result == Value::Int(1));
        assert!(interpreter.data.vars.get_value("n").unwrap() == Value::Int(1));
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        let result = free(
            Arc::new(vec![Value::Var("n".to_string())]),
            interpreter.data.clone(),
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(
            interpreter
                .data
                .output
                .lock()
                .await
                .eq("One plus one equals 2")
        );
    }

    #[tokio::test]
    async fn value_eq_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = eq(
            Arc::new(vec!["1".into(), "1".into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);

        let result = eq(
            Arc::new(vec![1.into(), 1.into()]),
            interpreter.data.clone().clone(),
        )
        .await
        .unwrap();
        assert!(
            result
                .as_bool(interpreter.data.clone().clone())
                .await
                .unwrap()
                == true
        );

        let result = eq(
            Arc::new(vec![1.into(), 1.3.into()]),
            interpreter.data.clone().clone(),
        )
        .await
        .unwrap();
        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn value_gt_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = gt(
            Arc::new(vec!["1".into(), "2".into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);

        let result = gt(Arc::new(vec![2.into(), 1.into()]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);

        let result = gt(
            Arc::new(vec![1.into(), 1.3.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn value_lt_compare() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = lt(
            Arc::new(vec!["1".into(), "2".into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);

        let result = lt(Arc::new(vec![2.into(), 1.into()]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);

        let result = lt(
            Arc::new(vec![1.into(), 1.3.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);
    }

    #[tokio::test]
    async fn bool_logic_not() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = not(Arc::new(vec![false.into()]), interpreter.data.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);

        let result = not(Arc::new(vec![true.into()]), interpreter.data.clone())
            .await
            .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);
    }

    #[tokio::test]
    async fn bool_logic_and() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = and(
            Arc::new(vec![false.into(), true.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == false);

        let result = and(
            Arc::new(vec![true.into(), true.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);
    }

    #[tokio::test]
    async fn bool_logic_or() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = or(
            Arc::new(vec![false.into(), true.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);

        let result = or(
            Arc::new(vec![true.into(), true.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();

        assert!(result.as_bool(interpreter.data.clone()).await.unwrap() == true);
    }

    #[tokio::test]
    async fn logic_if_then() {
        let interpreter = Arc::new(FslInterpreter::new());

        let mut print_command = get_command("print");
        print_command.set_args(vec!["hello".into()]);
        let print_command = Arc::new(print_command);
        let result = if_then(
            Arc::new(vec![true.into(), print_command.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.data.output.lock().await);
        assert!(*interpreter.data.output.lock().await == "hello");

        let mut print_command = get_command("print");
        print_command.set_args(vec![", world".into()]);
        let print_command = Arc::new(print_command);
        let result = if_then(
            Arc::new(vec![true.into(), print_command.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.data.output.lock().await);
        assert!(*interpreter.data.output.lock().await == "hello, world");
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.data.output.lock().await);
        assert!(*interpreter.data.output.lock().await == "hello");

        let result = if_then_else(
            Arc::new(vec![
                false.into(),
                print_command.into(),
                print_command2.into(),
            ]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        println!("{}", *interpreter.data.output.lock().await);
        assert!(*interpreter.data.output.lock().await == "hellogoodbye");
    }

    #[tokio::test]
    async fn logic_while() {
        let interpreter = Arc::new(FslInterpreter::new());

        let mut add_command = get_command("add");
        add_command.set_args(vec![5.into(), 4.into()]);
        let add_command = Arc::new(add_command);
        let result = while_command(
            Arc::new(vec![true.into(), add_command.into()]),
            interpreter.data.clone(),
        )
        .await;
        dbg!(result.clone());
        assert!(result.is_err_and(|e| matches!(e, FslError::LoopLimitExceeded(_))));
    }

    #[tokio::test]
    async fn get_index() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = index(Arc::new(vec![list, 1.into()]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::Int(2));

        let result = index(Arc::new(vec![text, 1.into()]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::Text("x".into()));
    }

    #[tokio::test]
    async fn get_length() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = length(Arc::new(vec![list]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::Int(3));

        let result = length(Arc::new(vec![text]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::Int(7));
    }

    #[tokio::test]
    async fn array_swap_indicies() {
        let interpreter = Arc::new(FslInterpreter::new());
        let list = Value::List(vec![1.into(), 2.into(), 3.into()]);
        let text = Value::Text("example".into());
        let result = swap(
            Arc::new(vec![list, 1.into(), 2.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 3.into(), 2.into()]));

        let result = swap(
            Arc::new(vec![text, 1.into(), 2.into()]),
            interpreter.data.clone(),
        )
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 10.into(), 2.into(), 3.into()]));

        let result = insert(
            Arc::new(vec![text, 10.into(), 2.into()]),
            interpreter.data.clone(),
        )
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
        let result = remove(Arc::new(vec![list, 1.into()]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::List(vec![1.into(), 3.into()]));

        let result = remove(Arc::new(vec![text, 1.into()]), interpreter.data.clone())
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
            Arc::new(vec![list, 1.into(), 10.into()]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::List(vec![1.into(), 10.into(), 3.into()]));

        let result = replace(
            Arc::new(vec![text, 2.into(), 10.into()]),
            interpreter.data.clone(),
        )
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(true));
        let result = starts_with(Arc::new(vec![text, "x".into()]), interpreter.data.clone())
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
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Bool(true));
        let result = starts_with(Arc::new(vec![text, "x".into()]), interpreter.data.clone())
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

        let result = concat(Arc::new(vec![text, number, bool]), interpreter.data.clone())
            .await
            .unwrap();
        assert!(result == Value::Text("example10false".into()));
    }

    #[tokio::test]
    async fn text_capitalize() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());

        let result = capitalize(Arc::new(vec![text]), interpreter.data.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("Example".into()));
    }

    #[tokio::test]
    async fn text_uppercase() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("example".into());

        let result = uppercase(Arc::new(vec![text]), interpreter.data.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("EXAMPLE".into()));
    }

    #[tokio::test]
    async fn text_lowercase() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("EXAMPLE".into());

        let result = lowercase(Arc::new(vec![text]), interpreter.data.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("example".into()));
    }

    #[tokio::test]
    async fn text_remove_whitespace() {
        let interpreter = Arc::new(FslInterpreter::new());
        let text = Value::Text("e\nx\t a m p l e".into());

        let result = remove_whitespace(Arc::new(vec![text]), interpreter.data.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result == Value::Text("example".into()));
    }

    #[tokio::test]
    async fn get_random_range() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = random_range(Arc::new(vec![1.into(), 6.into()]), interpreter.data.clone())
            .await
            .unwrap();
        dbg!(result.clone());
        assert!(result.is_type(crate::types::FslType::Int));
        let rnd = result.as_int(interpreter.data.clone()).await.unwrap();
        assert!(rnd <= 6 && rnd >= 1)
    }

    #[tokio::test]
    async fn get_random_entry() {
        let interpreter = Arc::new(FslInterpreter::new());

        let result = random_entry(
            Arc::new(vec![Value::List(vec![
                "one".into(),
                "two".into(),
                "three".into(),
            ])]),
            interpreter.data.clone(),
        )
        .await
        .unwrap();
        dbg!(result.clone());
        assert!(result.is_type(crate::types::FslType::Text));
        let rnd = result.as_text(interpreter.data.clone()).await.unwrap();
        assert!(rnd == "one" || rnd == "two" || rnd == "three");
    }
}
