use std::ops::Range;

use crate::{ast::AST, compile, compiler::Typed, parse, program::Program, src_location::WithLocation, typed::Type, var::Var};


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Meta {
    loc: Range<usize>,
    ty: Type,
}

impl Typed for Meta {
    fn typ(&self) -> Type {
        self.ty.clone()
    }

    fn assign_type(&mut self, ty: Type) {
        self.ty = ty;
    }
}

impl WithLocation for Meta {
    type Loc = Range<usize>;

    fn get_location(&self) -> Range<usize> {
        self.loc.clone()
    }

    fn annot(&mut self, loc: Range<usize>) {
        self.loc = loc;
    }
}

impl Default for Meta {
    fn default() -> Self {
        Meta {
            loc: 0..0,
            ty: Type::Nil,
        }
    }
}

pub fn exec(src: &str) -> Var {
    let mut ast: Vec<AST<Meta>> = parse(src).unwrap();
    let prog: Program = compile(&mut ast).unwrap();
    prog.exec();
    prog.result_var().unwrap()
}
