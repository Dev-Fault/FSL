use std::sync::Arc;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::CommandError,
    register_command,
    types::{
        FslType,
        command::{ArgPos, ArgRule, Command, Handler},
        value::{FslValue, Value},
    },
};

use futures::{FutureExt, StreamExt, future::join_all};

pub fn register_async(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, JOIN, JOIN_RULES, join);
    register_command!(interpreter, YIELD, YIELD_RULES, r#yield);
}

pub const JOIN_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), &[FslType::Command])];
pub const JOIN: &str = "join";
pub async fn join<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let args = command.take_args();
    let args = args.into_iter();
    let args: Result<Vec<Command<'c>>, CommandError> = args
        .map(|arg| arg.as_command())
        .collect::<Result<Vec<Command<'c>>, CommandError>>();
    let commands = args?;
    let mut executors = Vec::new();
    for command in commands {
        let future = command.execute(data.fork().await);
        executors.push(future);
    }

    let results = join_all(executors).await;

    let mut list: Vec<Value> = Vec::new();

    for result in results {
        let value = result?;
        list.push(value);
    }

    Ok(Value::List(list))
}

pub const YIELD_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Command])];
pub const YIELD: &str = "yield";
pub async fn r#yield<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut args = command.take_args();
    let command = args.pop_front().unwrap().value.as_command()?;
    tokio::task::yield_now().await;
    let result = command.execute(data).await?;

    Ok(result)
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
    async fn join_def_stack() {
        test_interpreter(
            r#"
                progress.def(
                    repeat(10,
                        say("working...")
                    )
                    return(true)
                )
                zero_out.def(x,
                    i.local(0)
                    debug("SLEEPING ", x)
                    repeat(random_range(1000,10000), div(1.0, 2.0).yield())
                    debug("WOKE ", x)
                    while(x.gt(0)
                        x.dec()
                        i.inc()
                    )
                    return([i, x])
                )

                debug("ABOUT TO JOIN")
                join(
                    zero_out(100)
                    zero_out(50)
                    zero_out(25)
                    progress()
                ).print()
            "#,
            "[[100, 0], [50, 0], [25, 0], true]",
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
                        x.inc()
                        say(x)
                    )
                    return(1)
                )

                slow_op.def(
                    repeat(10,
                        sleep(random_range(0.01, 0.03))
                        say("hello")
                    )
                    return(1)
                )

                join(
                    slow_op()
                    fast_op()
                ).print(" ", x)
            "#,
            "[1, 1] 100",
        )
        .await;
    }
}
