use std::sync::Arc;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::{CommandError, ValueError},
    register_command,
    types::{
        FslType,
        command::{ArgPos, ArgRule, Command, Handler},
        value::Value,
    },
};

use futures::future::join_all;

use futures::FutureExt;

pub fn register_join(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, JOIN, JOIN_RULES, join);
}

pub const JOIN_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), &[FslType::Command])];
pub const JOIN: &str = "join";
pub async fn join<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let args = command.take_args();
    let args = args.into_iter();
    let args: Result<Vec<Command<'c>>, ValueError> = args
        .map(|arg| arg.as_command())
        .collect::<Result<Vec<Command<'c>>, ValueError>>();
    let args = args?;

    let commands = args
        .into_iter()
        .map(|command| command.execute(data.clone()));

    let results = join_all(commands).await;

    let mut list: Vec<Value> = Vec::new();

    for result in results {
        let value = result?;
        list.push(value);
    }

    Ok(Value::List(list))
}

#[cfg(test)]
mod tests {
    use crate::libraries::standard::tests::test_interpreter;

    #[tokio::test]
    async fn join() {
        test_interpreter(
            r#"
                join(
                    add(1, 0)
                    add(2, 0)
                    add(3, 0)
                ).print()
            "#,
            "[1, 2, 3]",
        )
        .await;
    }

    #[tokio::test]
    async fn join_on_custom_commands() {
        test_interpreter(
            r#"
                x.store(0)
                fast_op.def(
                    repeat(100,
                        sleep(random_range(0.001, 0.002))
                        x.inc()
                        say(x)
                    )
                )

                slow_op.def(
                    repeat(10,
                        sleep(random_range(0.01, 0.03))
                        say("hello")
                    )
                )

                join(
                    slow_op()
                    fast_op()
                )
            "#,
            "",
        )
        .await;
    }
}
