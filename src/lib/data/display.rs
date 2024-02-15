
pub trait Display {
  fn show(&self) -> String;
  fn display(&self) -> String {
    self.show()
  }
}
