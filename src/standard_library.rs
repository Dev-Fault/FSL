use crate::{ContainsFloat, Error, Value};

pub fn add(values: Vec<Value>) -> Result<Value, Error> {
    if values.contains_float() {
        let mut sum: f64 = 0.0;
        for value in values {
            sum = sum + value.as_float()?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum: i64 = 0;
        for value in values {
            sum = sum + value.as_int()?;
        }
        Ok(Value::Int(sum))
    }
}

pub fn sub(values: Vec<Value>) -> Result<Value, Error> {
    if values.contains_float() {
        let mut diff = values[0].as_float()?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_float()?;
        }
        Ok(Value::Float(diff))
    } else {
        // Should never panic as length and types have already been checked
        let mut diff = values[0].as_int()?;
        for value in &values[1..values.len()] {
            diff = diff - value.as_int()?;
        }
        Ok(Value::Int(diff))
    }
}

pub fn repeat(values: Vec<Value>) -> Result<Value, Error> {
    let repetitions = values[0].as_int()?;
    let command = values[1].as_command()?;
    let mut final_value = Value::None;
    for i in 0..repetitions {
        final_value = command.execute()?;
    }

    Ok(final_value)
}
