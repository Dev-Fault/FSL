use std::{collections::HashMap, sync::Arc};

use async_recursion::async_recursion;
use std::sync::Mutex;

use crate::{
    commands::*,
    parser::{Expression, Parser},
    types::{ArgRule, Command, Error, Executor, FslType, Value},
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
            Value::Var(var) => {
                if interpreter.vars.get_value(var)?.is_type(FslType::Float) {
                    return Ok(true);
                }
            }
            Value::Command(command) => {
                return Ok(contains_float(command.get_args(), interpreter).await?);
            }
            _ => {
                continue;
            }
        }
    }
    Ok(false)
}

pub type CommandMap = HashMap<String, Command>;
#[derive(Debug)]
pub struct VarMap(Arc<Mutex<HashMap<String, Value>>>);

impl VarMap {
    pub fn new() -> Self {
        Self {
            0: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn insert_value(&self, label: &str, value: &Value) {
        match value {
            Value::Var(_) => panic!("Cannot store a var in a var"),
            Value::Command(_) => panic!("cannot store a command in a var"),
            Value::None => panic!("Cannot store none in a var"),
            _ => {
                self.0
                    .lock()
                    .unwrap()
                    .insert(label.to_string(), value.clone());
            }
        }
    }

    pub fn remove_value(&self, label: &str) -> Option<Value> {
        self.0.lock().unwrap().remove(label)
    }

    pub fn get_value(&self, label: &str) -> Result<Value, Error> {
        let value = self.0.lock().unwrap().get(label).cloned();

        match value {
            Some(value) => {
                if value.is_type(FslType::Var) {
                    return self.get_value(&value.get_var_label()?);
                } else {
                    Ok(value.clone())
                }
            }
            None => Err(format!("tried to get value of non existant var {}", label)),
        }
    }
}

pub struct FslInterpreter {
    pub output: Arc<tokio::sync::Mutex<String>>,
    pub commands: CommandMap,
    pub vars: VarMap,
    pub loops: Arc<tokio::sync::Mutex<usize>>,
    pub loop_limit: Option<usize>,
}

impl FslInterpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            output: Arc::new(tokio::sync::Mutex::new(String::new())),
            commands: CommandMap::new(),
            vars: VarMap::new(),
            loops: Arc::new(tokio::sync::Mutex::new(0)),
            loop_limit: Some(u16::MAX as usize),
        };
        interpreter.add_standard_commands();
        interpreter
    }

    async fn parse_expression(&self, expression: Expression) -> Result<Value, String> {
        match self.commands.get(&expression.name) {
            Some(command) => {
                let mut command = command.clone();

                let mut args: Vec<Value> = vec![];

                for arg in expression.args {
                    args.push(self.parse_arg(arg).await?);
                }

                command.set_args(args);

                Ok(Value::Command(Arc::new(command)))
            }
            None => {
                return Err(format!(
                    "Error: No command named {} exists",
                    &expression.name
                ));
            }
        }
    }

    #[async_recursion]
    async fn parse_arg(&self, arg: parser::Arg) -> Result<Value, String> {
        match arg {
            parser::Arg::Number(number) => {
                if number.contains('.') {
                    Ok(Value::Float(number.parse::<f64>().unwrap()))
                } else {
                    Ok(Value::Int(number.parse::<i64>().unwrap()))
                }
            }
            parser::Arg::String(text) => Ok(Value::Text(text)),
            parser::Arg::Keyword(keyword) => match keyword {
                lexer::Keyword::True => Ok(Value::Bool(true)),
                lexer::Keyword::False => Ok(Value::Bool(false)),
            },
            parser::Arg::Var(var) => Ok(Value::Var(var)),
            parser::Arg::List(args) => {
                let mut list: Vec<Value> = vec![];
                for arg in args {
                    list.push(self.parse_arg(arg).await?);
                }
                Ok(Value::List(list))
            }
            parser::Arg::Expression(expression) => Ok(self.parse_expression(expression).await?),
        }
    }

    pub async fn interpret<'a>(self, code: &'a str) -> Result<String, String> {
        let interpreter = Arc::new(self);
        let expressions = Parser::new().parse(code);
        match expressions {
            Ok(expressions) => {
                for expression in expressions {
                    let command = match interpreter.parse_expression(expression).await? {
                        Value::Command(command) => command,
                        _ => unreachable!("parse expression should always return a command"),
                    };
                    command.execute(interpreter.clone()).await?;
                }
            }
            Err(e) => {
                return Err(format!("{:?}", e));
            }
        }
        Ok(interpreter.output.lock().await.clone())
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

    pub fn add_command(&mut self, label: &str, rules: &'static [ArgRule], executor: Executor) {
        self.commands
            .insert(label.to_string(), Command::new(label, rules, executor));
    }

    pub fn construct_executor<F, Fut>(command: F) -> Executor
    where
        F: Fn(Arc<Vec<Value>>, Arc<FslInterpreter>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Value, Error>> + Send + 'static,
    {
        Arc::new(move |values, vars| Box::pin(command(values, vars)))
    }

    fn add_standard_commands(&mut self) {
        self.add_command("add", MATH_RULES, Self::construct_executor(commands::add));
        self.add_command("sub", MATH_RULES, Self::construct_executor(commands::sub));
        self.add_command("mul", MATH_RULES, Self::construct_executor(commands::mul));
        self.add_command("div", MATH_RULES, Self::construct_executor(commands::div));
        self.add_command(
            "mod",
            MATH_RULES,
            Self::construct_executor(commands::modulus),
        );
        self.add_command(
            "store",
            &STORE_RULES,
            Self::construct_executor(commands::store),
        );
        self.add_command(
            "free",
            &FREE_RULES,
            Self::construct_executor(commands::free),
        );
        self.add_command(
            "print",
            PRINT_RULES,
            Self::construct_executor(commands::print),
        );
        self.add_command("eq", &EQ_RULES, Self::construct_executor(commands::eq));
        self.add_command("gt", &GT_RULES, Self::construct_executor(commands::gt));
        self.add_command("lt", LT_RULES, Self::construct_executor(commands::lt));
        self.add_command("not", &NOT_RULES, Self::construct_executor(commands::not));
        self.add_command("and", AND_RULES, Self::construct_executor(commands::and));
        self.add_command("or", OR_RULES, Self::construct_executor(commands::or));
        self.add_command(
            "if_then",
            IF_THEN_RULES,
            Self::construct_executor(commands::if_then),
        );
        self.add_command(
            "if_then_else",
            IF_THEN_ELSE_RULES,
            Self::construct_executor(commands::if_then_else),
        );
        self.add_command(
            "while",
            WHILE_RULES,
            Self::construct_executor(commands::while_loop),
        );
        self.add_command(
            "repeat",
            REPEAT_RULES,
            Self::construct_executor(commands::repeat),
        );
        self.add_command(
            "index",
            INDEX_RULES,
            Self::construct_executor(commands::index),
        );
        self.add_command(
            "length",
            &LENGTH_RULES,
            Self::construct_executor(commands::length),
        );
        self.add_command(
            "swap",
            &SWAP_RULES,
            Self::construct_executor(commands::swap),
        );
        self.add_command(
            "insert",
            &INSERT_RULES,
            Self::construct_executor(commands::insert),
        );
        self.add_command(
            "remove",
            &REMOVE_RULES,
            Self::construct_executor(commands::remove),
        );
        self.add_command(
            "replace",
            &REPLACE_RULES,
            Self::construct_executor(commands::replace),
        );
        self.add_command(
            "starts_with",
            &STARTS_WITH_RULES,
            Self::construct_executor(commands::starts_with),
        );
        self.add_command(
            "ends_with",
            ENDS_WITH_RULES,
            Self::construct_executor(commands::ends_with),
        );
        self.add_command(
            "concat",
            &CONCAT_RULES,
            Self::construct_executor(commands::concat),
        );
        self.add_command(
            "capitalize",
            &CAPITALIZE_RULES,
            Self::construct_executor(commands::capitalize),
        );
        self.add_command(
            "uppercase",
            &UPPERCASE_RULES,
            Self::construct_executor(commands::uppercase),
        );
        self.add_command(
            "lowercase",
            &LOWERCASE_RULES,
            Self::construct_executor(commands::lowercase),
        );
        self.add_command(
            "remove_whitespace",
            &REMOVE_WHITESPACE_RULES,
            Self::construct_executor(commands::remove_whitespace),
        );
        self.add_command(
            "random_range",
            &RANDOM_RANGE_RULES,
            Self::construct_executor(commands::random_range),
        );
        self.add_command(
            "random_entry",
            &RANDOM_ENTRY_RULES,
            Self::construct_executor(commands::random_entry),
        );
    }
}

#[cfg(test)]
mod interpreter {
    use crate::FslInterpreter;

    async fn test_interpreter(code: &str, expected_output: &str) {
        let result = FslInterpreter::new().interpret(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    async fn test_interpreter_err(code: &str) {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn hello_world() {
        test_interpreter("print(\"Hello, world!\")", "Hello, world!").await;
    }

    #[tokio::test]
    async fn print_list() {
        test_interpreter(
            r#"

            names.store(["John", "James", "Joseph", "Alexander"])
            i.store(0)
            repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))

            "#,
            "John\nJames\nJoseph\nAlexander\n",
        )
        .await;
    }

    #[tokio::test]
    async fn float_vars() {
        test_interpreter(
            r#"

            x.store(2)
            y.store(0.5)
            print(add(x, y))

            "#,
            "2.5",
        )
        .await;
    }

    #[tokio::test]
    async fn float_commands() {
        test_interpreter(
            r#"

            print(add(add(1, 1.5), add(1, 2)))

            "#,
            "5.5",
        )
        .await;
    }

    #[tokio::test]
    async fn invalid_command_arg() {
        test_interpreter_err(
            r#"

            print(add(print(2), print(2)))

            "#,
        )
        .await;
    }

    #[tokio::test]
    async fn nested_list_operations() {
        test_interpreter(
            r#"
        matrix.store([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        row.store(matrix.index(1))
        print(row.index(2))
        "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn deep_matrix() {
        test_interpreter(
            r#"
        matrix.store([[1, [2, 3, 4], 5], [4, 5, 6], [7, 8, 9]])
        print(matrix.index(1).index(1))
        "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn conditional_logic_chain() {
        test_interpreter(
            r#"
            x.store(10)
            y.store(20)
            if_then_else(
                gt(x, y),
                print("x is greater"),
                if_then_else(
                    lt(x, y),
                    print("y is greater"),
                    print("they are equal")
                )
            )
        "#,
            "y is greater",
        )
        .await;
    }

    #[tokio::test]
    async fn fibonacci_sequence() {
        test_interpreter(
            r#"
        a.store(0)
        b.store(1)
        i.store(0)
        repeat(10,
            print(a, " "),
            temp.store(a),
            a.store(b),
            b.store(add(temp, b))
        )
        "#,
            "0 1 1 2 3 5 8 13 21 34 ",
        )
        .await;
    }

    #[tokio::test]
    async fn bubble_sort() {
        test_interpreter(
            r#"
        numbers.store([5, 2, 8, 1, 9])
        n.store(numbers.length())
        i.store(0)
        repeat(n,
            j.store(0),
            repeat(sub(n, i, 1),
                if_then(
                    gt(numbers.index(j), numbers.index(add(j, 1))),
                    numbers.store(numbers.swap(j, add(j, 1)))
                ),
                j.store(add(j, 1))
            ),
            i.store(add(i, 1))
        )
        k.store(0)
        repeat(numbers.length(),
            print(numbers.index(k), " "),
            k.store(add(k, 1))
        )
        "#,
            "1 2 5 8 9 ",
        )
        .await;
    }

    #[tokio::test]
    async fn int_equality() {
        test_interpreter(
            r#"

            print(eq(1, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn float_equality() {
        test_interpreter(
            r#"

            print(eq(1.2, 1.2))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1.0))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn int_float_equality() {
        test_interpreter(
            r#"

            print(eq(1.0, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn string_equality() {
        test_interpreter(
            r#"

            print(eq("h", "h"))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq("e", "h"))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn text_search_and_count() {
        test_interpreter(
            r#"
        text.store("hello world hello universe hello")
        count.store(0)
        i.store(0)
        repeat(text.length(),
            if_then(
                and(
                    text.index(i).eq("h"),
                    if_then_else(i.add(1).lt(text.length()),
                        text.index(add(i, 1)).eq("e"),
                        false
                    )
                ),
                count.store(count.add(1))
            ),
            i.store(i.add(1)),
        )
        print("Found 'he' ", count, " times")
        "#,
            "Found 'he' 3 times",
        )
        .await;
    }

    #[tokio::test]
    async fn list_filter_evens() {
        test_interpreter(
            r#"
        numbers.store([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        evens.store([])
        i.store(0)
        repeat(numbers.length(),
            num.store(numbers.index(i)),
            if_then(
                eq(mod(num, 2), 0),
                evens.store(evens.insert(num, evens.length()))
            ),
            i.store(add(i, 1))
        )
        j.store(0)
        repeat(evens.length(),
            print(evens.index(j), " "),
            j.store(add(j, 1))
        )
        "#,
            "2 4 6 8 10 ",
        )
        .await;
    }

    #[tokio::test]
    async fn factorial_calculator() {
        test_interpreter(
            r#"
        n.store(5)
        result.store(1)
        counter.store(n)
        while(gt(counter, 0),
            result.store(mul(result, counter)),
            counter.store(sub(counter, 1))
        )
        print(n, "! = ", result)
        "#,
            "5! = 120",
        )
        .await;
    }

    #[tokio::test]
    async fn chained_method_calls() {
        test_interpreter(
            r#"
        text.store("  hello world  ")
        result.store(
            text
                .remove_whitespace()
                .uppercase()
                .concat("!!!")
        )
        print(result)
        "#,
            "HELLOWORLD!!!",
        )
        .await;
    }
}
