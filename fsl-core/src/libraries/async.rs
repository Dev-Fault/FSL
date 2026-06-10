use std::sync::Arc;

use futures::future::join_all;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    execute_command, potential_future,
    types::{
        command::{Command, CommandSignature, ExpectedArgs},
        value::Value,
    },
};

pub fn register_async(interpreter: &mut FslInterpreter) {
    crate::register_async!(interpreter, JOIN, JOIN_RULES, join);
    crate::register_async!(interpreter, YIELD, YIELD_RULES, r#yield);
}

pub const JOIN_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(1));
pub const JOIN: &str = "join";
pub async fn join(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut commands: Vec<_> = Vec::new();
    for arg in &mut command.args {
        commands.push(potential_future!(arg.to_command(data.clone())?));
    }

    let mut executors = Vec::new();
    for command in commands {
        let data = data.fork().await;
        let future = tokio::spawn(Box::pin(
            async move { execute_command!(command, data.clone()) },
        ));
        executors.push(future);
    }

    let results = join_all(executors).await;
    let mut list: Vec<Value> = Vec::new();
    for result in results {
        match result {
            Ok(Ok(value)) => list.push(value),
            Ok(Err(e)) => return Err(e),
            Err(e) => {
                return Err(
                    RuntimeError::Custom(format!("Failed to join threads:\n {}", e))
                        .span(command.span),
                );
            }
        }
    }

    Ok(Value::from(list))
}

pub const YIELD_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::Exactly(1));
pub const YIELD: &str = "yield";
pub async fn r#yield(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let command = potential_future!(command.args[0].to_command(data.clone())?);
    tokio::task::yield_now().await;
    let result = execute_command!(command, data.clone())?;

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
                progress.def(msg,
                    repeat(10,
                        say(msg)
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
                    progress("working...")
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
