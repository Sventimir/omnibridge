use bridge::dealer::deal;

pub enum Cmd {
    Deal(u8),
}

impl Cmd {
    pub fn execute(&self) -> Result<serde_sexpr::Value, serde_sexpr::Value> {
        match self {
            Cmd::Deal(board) => todo!("Ok(deal(*board).from())"),
        }
    }
}
