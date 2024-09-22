use crate::data::stack::Stack;

#[test]
fn empty_stacks_size_is_zero() {
    let s: Vec<()> = Vec::new();
    assert_eq!(s.size(), 0);
}

#[test]
fn peek_on_empty_stack_returns_none() {
    let s: Vec<i32> = Vec::new();
    assert_eq!(s.peek(0), None)
}

proptest! {
    fn pop_after_push_returns_inserted_item(stack: Vec<i32>, item: i32) {
        // Apparently there is a bug in quickcheck! which forbids declaring
        // the function argument as mutable.
        let mut stack = stack;
        stack.push(item);
        assert_eq!(stack.pop(), Some(item))
    }

    fn pop_removes_item_from_the_stack(stack: Vec<i32>, item: i32) {
        let top = if stack.is_empty() {
            None
        } else {
            Some(stack[stack.len() - 1].clone())
        };
        let mut s = stack;
        s.push(item);
        s.pop();
        assert_eq!(s.peek(0), top.as_ref())
    }

    fn push_increments_size(stack: Vec<i32>, item: i32) {
        let mut s = stack;
        let size = s.size();
        s.push(item);
        assert_eq!(s.size(), size + 1)
    }

    fn pop_decrements_size(stack: Vec<i32>, item: i32) {
        let mut s = stack;
        s.push(item);
        let size = s.size();
        s.pop();
        assert_eq!(s.size(), size - 1)
    }

    fn peek_returns_last_pushed_item(stack: Vec<i32>, item: i32) {
        let mut s = stack;
        s.push(item);
        assert_eq!(s.peek(0), Some(&item))
    }

    fn dig_moves_the_right_item_to_the_top(stack: Vec<i32>, index: usize) {
        let mut s = stack;
        let size = s.size();
        let item = s.peek(index).cloned();
        let res = s.dig(index);
        if index >= s.size() {
            assert!(res.is_err())
        } else {
            assert!(
                res.is_ok()
                    && s.peek(0) == item.as_ref()
                    && s.size() == size
            )
        }
    }
}
