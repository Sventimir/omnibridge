extern crate bridge;
#[macro_use]
mod command;
mod protocol;
mod state;

use hex::ToHex;
use language::{self, pair, IntoSexp, Sexp};
use ring::digest::{digest, Digest, SHA256};
use state::State;
use std::{io, sync::Mutex};

type Env = language::env::Env<language::BuiltinType, language::BuiltinInstr>;

#[derive(Debug, Clone)]
enum ServerError {
    LexerError(language::parser::ParseError),
    CompilationError(language::CompilationError<language::Meta, language::BuiltinType>),
}

impl IntoSexp for ServerError {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            ServerError::LexerError(err) => err.into_sexp(),
            ServerError::CompilationError(err) => err.into_sexp(),
        }
    }
}

struct Response {
    result: Result<String, ServerError>,
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

fn eval_lisp(src: &str, mut env: Env) -> Result<String, ServerError> {
    let mut ast = language::parse(src).map_err(ServerError::LexerError)?;
    let prog = language::compile(&mut ast, &mut env)
        .map_err(|e| ServerError::CompilationError(e))?;
    Ok(prog.eval())
}

fn interpret(expr: &str, _state: &mut Mutex<State>) -> Response {
    let request_id = digest(&SHA256, expr.as_bytes());
    let mut env = Env::new();
    env.init();
    let result = eval_lisp(expr, env);
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
