use std::io::{self, IsTerminal, Read};

use clap::Parser;
use fsl_core::{FslInterpreter, libraries::Library};

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
    let args = Args::parse();
    let mut interpreter = FslInterpreter::new_unbounded();
    if let Err(e) = interpreter.register_library(Library::Exec) {
        eprintln!("{e}");
        return;
    };
    if let Err(e) = interpreter.register_library(Library::Io) {
        eprintln!("{e}");
        return;
    };

    let stdin = if io::stdin().is_terminal() {
        None
    } else {
        let mut s = String::new();
        io::stdin().read_to_string(&mut s).unwrap();
        Some(s)
    };

    let input = args.input.or(stdin);

    if let Some(input) = input {
        interpreter.add_arg(input).await;
    }

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

    let result = if args.embeded {
        interpreter.interpret_embedded_code(&script).await
    } else {
        interpreter.interpret(&script).await
    };

    match result {
        Ok(output) => println!("{output}"),
        Err(e) => eprintln!("{e}"),
    }
}
