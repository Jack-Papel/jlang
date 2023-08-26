#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub len: u16
}

impl Span {
    pub fn new(start: usize, len: usize) -> Self {
        Span {
            start,
            len: len as u16
        }
    }

    pub fn end(&self) -> usize {
        self.start + self.len as usize
    }

    pub fn source<'a> (&self, file: &'a str) -> &'a str {
        &file[self.start..self.end()]
    }
}