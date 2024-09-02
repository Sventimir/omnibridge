pub enum StackError {
    OutOfBoundsAccess(usize),
}

pub trait Stack<T> {
    fn push(&mut self, item: T);
    fn pop(&mut self) -> Option<T>;
    fn peek(&self, depth: usize) -> Option<&T>;
    fn size(&self) -> usize;
    fn swap(&mut self);
    fn dig<'a>(&'a mut self, n: usize) -> Result<(), StackError>;
}

fn swap_vec<T>(v: &mut Vec<T>) -> Option<()> {
    let last = v.pop()?;
    let second_to_last = v.pop()?;
    v.push(last);
    v.push(second_to_last);
    Some(())
}

impl<T> Stack<T> for Vec<T> {
    fn push(&mut self, item: T) {
        self.push(item);
    }

    fn pop(&mut self) -> Option<T> {
        self.pop()
    }

    fn peek(&self, depth: usize) -> Option<&T> {
        if depth >= self.len() {
            return None;
        } else {
            self.get(self.len() - depth - 1)
        }
    }

    fn size(&self) -> usize {
        self.len()
    }

    fn swap(&mut self) {
        swap_vec(self).unwrap_or(());
    }

    fn dig(&mut self, depth: usize) -> Result<(), StackError> {
        if depth >= self.len() {
            return Err(StackError::OutOfBoundsAccess(depth));
        }

        let item = self.remove(self.len() - depth - 1);
        self.push(item);

        Ok(())
    }
}
