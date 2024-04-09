extern crate bridge;
mod command;

use sexp;
use bridge::data::sexpable::{Sexpable, mk_assoc_pair};
use std::io;

use command::Cmd;


fn format_read_error(e: Box<sexp::Error>) -> sexp::Sexp {
    sexp::list(&[
        sexp::atom_s("error"),
        mk_assoc_pair("type", sexp::atom_s("parse-error")),
        mk_assoc_pair("message", sexp::atom_s(e.message)),
        mk_assoc_pair("line", sexp::atom_i(e.line as i64)),
        mk_assoc_pair("column", sexp::atom_i(e.column as i64)),
        mk_assoc_pair("index", sexp::atom_i(e.index as i64))
    ])
}


fn interpret(expr: &str) -> Result<sexp::Sexp, sexp::Sexp> {
    let s = sexp::parse(expr).map_err(format_read_error)?;
    let cmd = Cmd::from_sexp(&s).map_err(|e| e.to_sexp())?;
    let resp = cmd.execute()?;
    Ok(sexp::list(&[ sexp::atom_s("ok"), resp ]))
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
                let resp = match interpret(&cmd) {
                    Ok(resp) => resp,
                    Err(e) => e
                };
                println!("{}", resp.to_string());
            }
            Err(e) => return Err(format!("Error reading from stdin: {}", e))
        }
    }
    Ok(())
}
