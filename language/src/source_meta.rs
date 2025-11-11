use std::ops::Range;
use crate::{
    src_location::WithLocation, type_var::{TypeExpr, TypeVar, TypedMeta}, BuiltinType, IntoSexp, Sexp
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
