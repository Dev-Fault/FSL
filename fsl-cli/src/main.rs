use std::io::{self, IsTerminal, Read};

use clap::Parser;
use fsl_core::{FslInterpreter, data::InterpreterData, fsl, types::value::Value};

#[derive(Parser, Debug, Clone)]
pub struct Args {
    #[arg(short, long)]
    file: bool,

    #[arg(short, long)]
    embeded: bool,

    script: String,
    input: Option<String>,
}

#[tokio::main]
async fn main() {
    flamegraph_test().await;
    return;
    let args = Args::parse();

    let stdin = if io::stdin().is_terminal() {
        None
    } else {
        let mut s = String::new();
        io::stdin().read_to_string(&mut s).unwrap();
        Some(s)
    };

    let input = args.input.or(stdin);

    let script = if args.file {
        match std::fs::read_to_string(args.script) {
            Ok(contents) => contents,
            Err(e) => {
                eprintln!("{e}");
                return;
            }
        }
    } else {
        args.script
    };

    let mut interpreter = FslInterpreter::new();

    interpreter.register_all_libraries();

    let result = if args.embeded {
        interpreter
            .interpret_embedded_code(
                script,
                InterpreterData::default()
                    .with_args(vec![Value::from(input.unwrap_or("".to_string()))]),
            )
            .await
    } else {
        interpreter
            .interpret(
                script,
                InterpreterData::default()
                    .with_args(vec![Value::from(input.unwrap_or("".to_string()))]),
            )
            .await
    };

    match result {
        Ok(output) => println!("{output}"),
        Err(e) => eprintln!("{e}"),
    }
}

async fn flamegraph_test() {
    let _ = fsl!(
        r#"
            increment.def(
                sum.inc()
            )

            sum.store(0)

            stopwatch(
                repeat(1000000,
                    increment()
                )
            ).elapsed.mul(1000).precision(4)
            .prepend("1,000,000 def(sum.inc())\n")
            .concat("ms\n").say()
            print("\ndone")
        "#
    );
}
