extern crate bridge;
#[macro_use]
mod command;

use std::io;

use bridge::dealer::deal;
use command::Cmd;

// fn interpret(expr: &str) -> Result<sexp::Sexp, sexp::Sexp> {
//     let s = sexp::parse(expr).map_err(format_read_error)?;
//     let cmd = Cmd::from_sexp(&s).map_err(|e| e.to_sexp())?;
//     let resp = cmd.execute()?;
//     Ok(sexp::list(&[ sexp::atom_s("ok"), resp ]))
// }

fn main() -> Result<(), String> {
    println!("Hello, this is your Bridge Mentor server. Awaiting orders!");
    let mut cmd = String::new();
    let stdin = io::stdin();
    let board = deal(13);
    println!("{}", &serde_sexpr::to_string(&board).unwrap());
    // loop {
    //     cmd.clear();
    //     match stdin.read_line(&mut cmd) {
    //         Ok(0) => break,
    //         Ok(_) => {
    //             let resp = match interpret(&cmd) {
    //                 Ok(resp) => resp,
    //                 Err(e) => e
    //             };
    //             println!("{}", resp.to_string());
    //         }
    //         Err(e) => return Err(format!("Error reading from stdin: {}", e))
    //     }
    // }
    Ok(())
}
