pub mod old {
    use std::ops::Range;

    use crate::{
        ast::AST,
        compile,
        compiler::Typed,
        parse,
        program::Program,
        src_location::WithLocation,
        typed::{Type, TypeConstr, TypePrimitive},
        var::Var,
    };

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
                ty: Type(TypeConstr::Prim(TypePrimitive::Nil)),
            }
        }
    }

    pub fn exec(src: &str) -> Var {
        println!("src: '{}'", src);
        let mut ast: Vec<AST<Meta>> = parse(src).unwrap();
        let prog: Program = compile(&mut ast).unwrap();
        println!("{:?}", prog);
        prog.exec();
        prog.result_var().unwrap()
    }
}

use std::ops::Range;

use crate::{builtin_type::BuiltinType, src_location::WithLocation, type_checker::Typed, type_var::TypeVar};

#[derive(Clone, Debug)]
pub struct Meta {
    loc: Range<usize>,
    ty: TypeVar<BuiltinType>,
}

impl Typed for Meta {
    type Type = BuiltinType;

    fn get_type(&self) -> TypeVar<Self::Type> {
        self.ty.clone()
    }
    
    fn assign_type(&mut self, ty: TypeVar<Self::Type>) {
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
            ty: TypeVar::unknown(),
        }
    }
}
