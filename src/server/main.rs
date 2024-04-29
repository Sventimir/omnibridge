extern crate bridge;
#[macro_use]
mod command;

use serde::Serialize;
use std::io;

use command::{Cmd, CommandResult, CommandError};


#[derive(Debug, Serialize)]
enum ServerError {
    CommandError(CommandError),
    SexprError(String)
}

fn interpret(expr: &str) -> Result<CommandResult, ServerError> {
    let cmd = serde_sexpr::from_str::<Cmd>(&expr)
        .map_err(|e| ServerError::SexprError(e.to_string()))?;
    cmd.execute().map_err(ServerError::CommandError)
}

fn main() -> Result<(), String> {
    println!("Hello, this is your Bridge Mentor server. Awaiting orders!");
    let mut cmd = String::new();
    let stdin = io::stdin();
    loop {
        cmd.clear();
        match stdin.read_line(&mut cmd) {
            Ok(0) => break,
            Ok(_) => {
                let resp = serde_sexpr::to_string(&interpret(&cmd)).expect("error serializing response");
                println!("{}", resp);
            }
            Err(e) => return Err(format!("Error reading from stdin: {}", e))
        }
    }
    Ok(())
}
