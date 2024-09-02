use std::ops::Range;

pub trait SrcLocation {
    fn byte_range(&self) -> Range<usize>;
    fn find<'a>(&self, src: &'a str) -> &'a str {
        &src[self.byte_range()]
    }
}

pub trait WithLocation {
    type Loc: SrcLocation;

    fn annot(&mut self, loc: Range<usize>);
    fn get_location(&self) -> Self::Loc;

    fn find<'a>(&self, src: &'a str) -> &'a str {
        self.get_location().find(src)
    }
}

impl SrcLocation for Range<usize> {
    fn byte_range(&self) -> Range<usize> {
        self.clone()
    }
}

impl WithLocation for Range<usize> {
    type Loc = Range<usize>;

    fn annot(&mut self, loc: Range<usize>) {
        *self = loc;
    }

    fn get_location(&self) -> Range<usize> {
        self.clone()
    }
}
