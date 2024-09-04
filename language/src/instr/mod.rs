pub mod bool;

pub trait Instr {
    fn exec(&mut self);
}
