use core::fmt;
use std::{ops::Range, sync::Arc};

type Error = String;

#[derive(Debug, Clone, PartialEq, Eq)]
enum FslType {
    Int,
    Float,
    Text,
    Bool,
    List,
    Var,
    Command,
    None,
}

impl FslType {
    fn as_str(&self) -> &str {
        match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ArgCount {
    Finite(usize),
    Infinite,
}

enum ArgRange {
    Index(usize),
    Range(Range<usize>),
    Infinite,
}

struct ArgRule {
    range: ArgRange,
    fsl_types: Vec<FslType>,
}

impl ArgRule {
    fn new(range: ArgRange, fsl_types: Vec<FslType>) -> Self {
        Self { range, fsl_types }
    }
}

type Executor = Box<dyn Fn(Vec<Value>) -> Result<Value, Error> + Send + Sync>;
struct Command {
    label: String,
    arg_rules: Vec<ArgRule>,
    args: Vec<Value>,
    executor: Executor,
}

impl Command {
    fn new(label: &str, arg_rules: Vec<ArgRule>, executor: Executor) -> Self {
        Self {
            label: label.to_string(),
            arg_rules,
            args: vec![],
            executor,
        }
    }

    fn set_args(&mut self, args: Vec<Value>) {
        self.args = args;
    }

    fn validate_arg_range(&self, arg_rule: &ArgRule, range: &Range<usize>) -> Result<(), Error> {
        for (i, arg) in self.args[range.start..range.end].iter().enumerate() {
            let fsl_type = arg.as_type();
            if !arg_rule.fsl_types.contains(&fsl_type) {
                return Err(format!(
                    "Arg {} of command {} cannot be of type {}\nValid types are {:?}",
                    i,
                    self.label,
                    fsl_type.as_str(),
                    arg_rule.fsl_types
                ));
            }
        }
        Ok(())
    }

    fn execute(&self) -> Result<Value, Error> {
        let mut max_args = 0;
        for arg_rule in &self.arg_rules {
            match &arg_rule.range {
                ArgRange::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.validate_arg_range(arg_rule, &range)?;
                        }
                        None => {
                            return Err(format!(
                                "Arg {} of command {} must be present and be of type {:?}",
                                i, self.label, arg_rule.fsl_types
                            ));
                        }
                    }
                }
                ArgRange::Range(range) => {
                    max_args = if max_args < range.end {
                        range.end
                    } else {
                        max_args
                    };
                    if self.args.len() < range.start {
                        return Err(format!(
                            "Command {} must have at least {} arguments and only {} were given",
                            self.label,
                            range.start,
                            self.args.len(),
                        ));
                    } else if self.args.len() > range.end {
                        return Err(format!(
                            "Command {} must have no more than {} arguments and {} were given",
                            self.label,
                            range.end,
                            self.args.len(),
                        ));
                    } else {
                        self.validate_arg_range(arg_rule, range)?;
                    }
                }
                ArgRange::Infinite => {
                    max_args = usize::MAX;
                    let range = Range::from(0..self.args.len());
                    self.validate_arg_range(arg_rule, &range)?;
                }
            }
        }

        if self.args.len() > max_args {
            return Err(format!(
                "Command {} expected {} args but got {}",
                self.label,
                max_args,
                self.args.len()
            ));
        }

        Ok((self.executor)(self.args.clone())?)
    }
}

impl PartialEq for Command {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.label)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i64),
    Float(f64),
    Text(String),
    Bool(bool),
    List(Vec<Value>),
    Var(Box<Value>),
    Command(Arc<Command>),
    None,
}

impl Value {
    // Recursive
    /// Tries to get the inner value if self is Var; returns self if not Var.  
    pub fn get_value_if_var(&self) -> Result<&Value, Error> {
        match self {
            Value::Var(var) => Ok(var.get_value_if_var()?),
            _ => Ok(self),
        }
    }

    pub fn as_type(&self) -> FslType {
        match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Var(_) => FslType::Var,
            Value::Command(_) => FslType::Command,
            Value::None => FslType::None,
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            Value::Int(value) => Ok(value.clone()),
            Value::Float(value) => Ok(value.clone() as i64),
            Value::Var(var) => var.get_value_if_var()?.as_int(),
            Value::Command(command) => command.execute()?.as_int(),
            _ => Err("invalid Int conversion".into()),
        }
    }

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            Value::Int(value) => Ok(value.clone() as f64),
            Value::Float(value) => Ok(value.clone()),
            Value::Var(var) => var.get_value_if_var()?.as_float(),
            Value::Command(command) => command.execute()?.as_float(),
            _ => Err("invalid Int conversion".into()),
        }
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        if let Value::Text(value) = self {
            Ok(value)
        } else {
            Err("invalid Text conversion".into())
        }
    }

    pub fn as_bool(&self) -> Result<bool, Error> {
        if let Value::Bool(value) = self {
            Ok(value.clone())
        } else {
            Err("invalid Bool conversion".into())
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>, Error> {
        if let Value::List(value) = self {
            Ok(value)
        } else {
            Err("invalid Text conversion".into())
        }
    }

    pub fn as_command(&self) -> Result<Arc<Command>, Error> {
        if let Value::Command(command) = self {
            Ok(command.clone())
        } else {
            Err("invalid Command conversion".into())
        }
    }
}

trait ContainsFloat {
    fn contains_float(&self) -> bool;
}

impl ContainsFloat for Vec<Value> {
    fn contains_float(&self) -> bool {
        for value in self {
            match value {
                Value::Float(_) => {
                    return true;
                }
                _ => {
                    continue;
                }
            }
        }
        false
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Text(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Text(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<Arc<Command>> for Value {
    fn from(value: Arc<Command>) -> Self {
        Value::Command(value.clone())
    }
}

fn add(values: Vec<Value>) -> Result<Value, Error> {
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

fn sub(values: Vec<Value>) -> Result<Value, Error> {
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

fn repeat(values: Vec<Value>) -> Result<Value, Error> {
    let repetitions = values[0].as_int()?;
    let command = values[1].as_command()?;
    let mut final_value = Value::None;
    for i in 0..repetitions {
        final_value = command.execute()?;
    }

    Ok(final_value)
}

#[cfg(test)]
mod tests {

    use super::*;

    const NUMBER_TYPES: &[FslType] =
        &[FslType::Int, FslType::Float, FslType::Command, FslType::Var];

    fn get_add() -> Command {
        let arg_rules = vec![ArgRule::new(ArgRange::Infinite, NUMBER_TYPES.into())];
        Command::new("add", arg_rules, Box::new(add))
    }

    fn get_sub() -> Command {
        let arg_rules = vec![
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Infinite, NUMBER_TYPES.into()),
        ];
        Command::new("sub", arg_rules, Box::new(sub))
    }

    fn get_repeat() -> Command {
        let arg_rules = vec![
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(1), vec![FslType::Command]),
        ];
        Command::new("repeat", arg_rules, Box::new(repeat))
    }

    #[test]
    fn add_two_ints() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(9));
    }

    #[test]
    fn add_two_floats() {
        let args = vec![5.0.into(), 4.0.into()];
        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(9.0));
    }

    #[test]
    fn add_int_and_float() {
        let args = vec![5.0.into(), 4.into()];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(9.0));
    }

    #[test]
    fn add_nothing() {
        let args = vec![];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(0));
    }

    #[test]
    #[should_panic]
    fn add_wrong_type() {
        let args = vec![Value::Text("one".into()), Value::Int(1)];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    fn add_many() {
        let args = vec![5.0.into(), 3.into(), 3.into(), 100.into(), 57.into()];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(168.0));
    }

    #[test]
    fn add_vars() {
        let var = Value::Var(Box::new(Value::Int(1)));
        let args = vec![var.clone(), var.clone()];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(2));
    }

    #[test]
    #[should_panic]
    fn add_wrong_var() {
        let var = Value::Var(Box::new("1".into()));
        let args = vec![var.clone(), var.clone()];

        let mut command = get_add();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    fn add_commands() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![command.clone().into(), command.clone().into()];
        let mut command = get_add();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(18));
    }

    #[test]
    fn sub_two_ints() {
        let args = vec![5.into(), 4.into()];

        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(1));
    }

    #[test]
    fn sub_two_floats() {
        let args = vec![5.0.into(), 4.0.into()];

        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(1.0));
    }

    #[test]
    fn sub_int_and_float() {
        let args = vec![5.0.into(), 4.into()];
        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(1.0));
    }

    #[test]
    #[should_panic]
    fn sub_nothing() {
        let args = vec![];

        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    #[should_panic]
    fn sub_wrong_type() {
        let args = vec![Value::Text("one".into()), Value::Int(1)];

        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    fn sub_many() {
        let args = vec![5.0.into(), 3.into(), 3.into(), 100.into(), 57.into()];

        let mut command = get_sub();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Float(-158.0));
    }

    #[test]
    fn repeat_add() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command)];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute().unwrap();
        dbg!(&output);
        assert!(output == Value::Int(9));
    }

    #[test]
    #[should_panic]
    fn repeat_wrong_repititions() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec!["5".into(), Value::Command(command)];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    #[should_panic]
    fn repeat_wrong_thing_to_repeat() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), 5.into()];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    #[should_panic]
    fn repeat_too_many_args() {
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command), 5.into()];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute();
        dbg!(&output);
        output.unwrap();
    }
}
