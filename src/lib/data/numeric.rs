pub trait Numeric<N> {
  fn to_int(&self) -> N;
  fn from_int(i: N) -> Self;
}

pub fn conv<A, B, I>(a : A) -> B
  where A : Numeric<I>, B : Numeric<I> { 
  B::from_int(a.to_int())
}

