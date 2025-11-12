use std::sync::Arc;

use crate::{
    contains_float,
    types::{Error, Value, VarMap},
};

pub async fn add(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
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

pub async fn sub(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
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

pub async fn mul(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
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

pub async fn div(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
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

pub async fn modulus(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
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

pub async fn list(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!();
}

pub async fn clone(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn drop(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn print(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn eq(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn gt(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn lt(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn not(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok((!values[0].as_bool(vars).await?).into())
}

pub async fn and(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok((values[0].as_bool(vars.clone()).await? && values[1].as_bool(vars.clone()).await?).into())
}

pub async fn or(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok((values[0].as_bool(vars.clone()).await? || values[1].as_bool(vars.clone()).await?).into())
}

pub async fn if_then(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn if_then_else(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn while_loop(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn repeat(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    let repetitions = values[0].as_int(vars.clone()).await?;
    let command = values[1].as_command()?;
    let mut final_value = Value::None;
    for i in 0..repetitions {
        final_value = command.execute(vars.clone()).await?;
    }

    Ok(final_value)
}

pub async fn index_of(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn length_of(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn swap_indexes(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn insert_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn remove_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn replace_at(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    todo!()
}

pub async fn starts_with(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(vars.clone())
        .await?
        .starts_with(&values[1].as_text(vars.clone()).await?)
        .into())
}

pub async fn ends_with(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(vars.clone())
        .await?
        .ends_with(&values[1].as_text(vars.clone()).await?)
        .into())
}

pub async fn concat(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    let mut cat_string = String::new();

    for value in values.iter() {
        cat_string.push_str(&value.as_text(vars.clone()).await?);
    }

    Ok(cat_string.into())
}

pub async fn capitalize(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    let text = values[0].as_text(vars).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        Ok(format!("{}{}", text[0..1].to_uppercase(), text[1..].to_uppercase()).into())
    }
}

pub async fn upper(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[0].as_text(vars).await?.to_uppercase().into())
}

pub async fn lower(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[0].as_text(vars).await?.to_lowercase().into())
}

pub async fn remove_whitespace(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[0]
        .as_text(vars)
        .await?
        .split_whitespace()
        .collect::<String>()
        .into())
}

pub async fn nl(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok("\n".into())
}

pub async fn random_range(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    if contains_float(&values, vars.clone()).await? {
        let min = values[0].as_float(vars.clone()).await?;
        let max = values[1].as_float(vars.clone()).await?;
        if min >= max {
            Err("min must be greater than max".to_string())
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let min = values[0].as_int(vars.clone()).await?;
        let max = values[1].as_int(vars.clone()).await?;
        if min >= max {
            Err("min must be greater than max".to_string())
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub async fn random_entry(values: Arc<Vec<Value>>, vars: Arc<VarMap>) -> Result<Value, Error> {
    Ok(values[rand::random_range(0..values.len())].clone())
}
