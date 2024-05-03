extern crate bridge;
#[macro_use]
mod command;

use hex::ToHex;
use ring::digest::{digest, Digest, SHA256};
use sexp::Sexp;
use std::io;

use bridge::sexpr::*;

use command::{Cmd, CommandError, CommandResult};

enum ServerError {
    CommandError(CommandError),
    SexprError(SexpError),
}

impl ServerError {
    fn to_sexp(&self) -> Sexp {
        match self {
            ServerError::CommandError(err) => err.to_sexp(),
            ServerError::SexprError(err) => err.to_sexp(),
        }
    }
}

struct Response {
    result: Result<CommandResult, ServerError>,
    request_id: Digest,
}

impl Response {
    fn to_sexp(&self) -> Sexp {
        let req_id: String = ToHex::encode_hex(&self.request_id);
        sexp::list(&[
            (sexp::atom_s("request_id"), sexp::atom_s(&req_id)).to_sexp(),
            match &self.result {
                Ok(result) => (sexp::atom_s("ok"), result.clone()).to_sexp(),
                Err(err) => sexp::list(&[sexp::atom_s("error"), err.to_sexp()]),
            },
        ])
    }
}

fn interpret(expr: &str) -> Response {
    let request_id = digest(&SHA256, expr.as_bytes());
    let result = sexp::parse(&expr)
        .map_err(|e| ServerError::SexprError(SexpError::ParseError(*e)))
        .and_then(|cmd| Cmd::from_sexp(&cmd).map_err(ServerError::SexprError))
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
                let resp = interpret(&cmd).to_sexp();
                println!("{}", resp);
            }
            Err(e) => return Err(format!("Error reading from stdin: {}", e)),
        }
    }
    Ok(())
}
