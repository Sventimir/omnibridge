use std::{any::Any, ops::Range, sync::Arc};

use crate::{
    ast::AST,
    builtin_instr::BuiltinInstr,
    builtin_type::BuiltinType,
    compile,
    env::Env,
    interpreter, parse,
    src_location::WithLocation,
    type_var::{TypeExpr, TypeVar, TypedMeta, VarLabeler},
    IntoSexp, Sexp,
};

#[derive(Clone, Debug)]
pub struct Meta {
    loc: Range<usize>,
    ty: TypeExpr<BuiltinType>,
}

impl TypedMeta for Meta {
    type Type = BuiltinType;

    fn get_type(&self) -> TypeExpr<Self::Type> {
        self.ty.clone()
    }

    fn assign_type(&mut self, ty: TypeExpr<Self::Type>) {
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
        let v = TypeVar::unknown(vec![]);
        Meta {
            loc: 0..0,
            ty: TypeExpr {
                body: v.make_ref(),
                vars: vec![v],
            },
        }
    }
}

impl IntoSexp for Meta {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![
            S::symbol("meta".to_string()),
            self.ty.into_sexp(),
            S::list(vec![self.loc.start.into_sexp(), self.loc.end.into_sexp()]),
        ])
    }
}

pub fn get_result<T: Clone + 'static>(stack: &[Arc<dyn Any>]) -> T {
    stack
        .last()
        .expect("stack is empty!")
        .downcast_ref::<T>()
        .expect("cast failed!")
        .clone()
}

pub fn exec<T: Any + Clone>(src: &str) -> T {
    println!("src: '{}'", src);
    let mut ast: Vec<AST<Meta>> = parse(src).unwrap();
    let mut env: Env<BuiltinType, BuiltinInstr> = Env::new();
    env.init();
    match compile(&mut ast, &mut env) {
        Ok(prog) => {
            println!("{:?}", prog.clone().into_sexp::<String>());
            let stack = interpreter::execute(prog.as_slice());
            get_result::<T>(stack.as_slice())
        }
        Err(e) => {
            let mut labeler = VarLabeler::new();
            ast[0].label_type_vars(&mut labeler);
            println!("{}", ast[0].clone().into_sexp_debug::<String>());
            println!("{}", e.into_sexp::<String>());
            panic!("Failed to compile the program!")
        }
    }
}
