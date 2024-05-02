extern crate bridge;
#[macro_use]
mod command;

use hex::ToHex;
use ring::digest::{Digest, SHA256, digest};
use serde::Serialize;
use std::io;

use command::{Cmd, CommandResult, CommandError};


#[derive(Debug, Serialize)]
enum ServerError {
    CommandError(CommandError),
    SexprError(String)
}

#[derive(Debug)]
struct Response {
    result: Result<CommandResult, ServerError>,
    request_id: Digest
}

impl Serialize for Response {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("Response", 2)?;
        let req_id: String = ToHex::encode_hex(&self.request_id);
        state.serialize_field("request_id", &req_id)?;
        match &self.result {
            Ok(result) => state.serialize_field("ok", result)?,
            Err(err) => state.serialize_field("error", err)?,
        }
        state.end()
    }
}

fn interpret(expr: &str) -> Response {
    let request_id = digest(&SHA256, expr.as_bytes());     
    let result = 
        serde_sexpr::from_str::<Cmd>(&expr)
        .map_err(|e| ServerError::SexprError(e.to_string()))
        .and_then(|cmd| cmd.execute().map_err(ServerError::CommandError));
    Response { result, request_id }
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
