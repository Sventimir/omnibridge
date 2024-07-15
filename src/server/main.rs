extern crate bridge;
#[macro_use]
mod command;
mod protocol;
mod state;

use bridge::language::{self, ast::expect::ExpectError, pair, IntoSexp, Sexp};
use hex::ToHex;
use ring::digest::{digest, Digest, SHA256};
use state::State;
use std::{io, sync::Mutex};

use command::{Cmd, CommandError, CommandResult};

#[derive(Debug, Clone)]
enum ServerError {
    CommandError(CommandError),
    LexerError(language::parser::ParseError),
    SexprError(ExpectError),
}

impl IntoSexp for ServerError {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            ServerError::CommandError(err) => err.into_sexp(),
            ServerError::LexerError(err) => err.into_sexp(),
            ServerError::SexprError(err) => err.into_sexp(),
        }
    }
}

struct Response {
    result: Result<CommandResult, ServerError>,
    request_id: Digest,
}

impl IntoSexp for Response {
    fn into_sexp<S: Sexp>(self) -> S {
        let req_id: String = ToHex::encode_hex(&self.request_id);
        S::list(vec![
            pair(S::symbol("request_id".to_string()), S::symbol(req_id)),
            match &self.result {
                Ok(result) => pair(S::symbol("ok".to_string()), result.clone().into_sexp()),
                Err(err) => S::list(vec![
                    S::symbol("error".to_string()),
                    err.clone().into_sexp(),
                ]),
            },
        ])
    }
}

fn interpret(expr: &str, state: &mut Mutex<State>) -> Response {
    let request_id = digest(&SHA256, expr.as_bytes());
    let result = bridge::language::parser::parse(expr)
        .map_err(|e| ServerError::LexerError(e))
        .and_then(|sexp| match sexp.as_slice() {
            [s] => Cmd::try_from(s).map_err(ServerError::SexprError),
            _ => Err(ServerError::SexprError(ExpectError::WrongLength(
                0,
                sexp.clone(),
            ))),
        })
        .and_then(|cmd| cmd.execute(state).map_err(ServerError::CommandError));
    Response { result, request_id }
}

fn main() -> Result<(), String> {
    println!("Hello, this is your OmniBridge server. Awaiting orders!");
    let mut cmd = String::new();
    let stdin = io::stdin();
    let mut state = Mutex::new(state::State::new());
    loop {
        cmd.clear();
        match stdin.read_line(&mut cmd) {
            Ok(0) => break,
            Ok(_) => {
                let resp: String = interpret(&cmd, &mut state).into_sexp();
                println!("{}", resp);
            }
            Err(e) => return Err(format!("Error reading from stdin: {}", e)),
        }
    }
    Ok(())
}
