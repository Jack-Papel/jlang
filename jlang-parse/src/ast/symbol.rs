use std::{
    fmt::Display,
    sync::{Arc, RwLock},
};

use super::span::Span;

#[derive(Clone, Debug)]
pub struct Symbol {
    pub text: Arc<str>,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

// TODO: arena allocate
static INTERNED_STRINGS: RwLock<Vec<Arc<str>>> = RwLock::new(Vec::new());

impl Symbol {
    pub fn new(str: &str) -> Self {
        Symbol {
            text: Symbol::intern(str),
        }
    }

    pub fn from_file(file: &str, start: usize, len: usize) -> Self {
        Symbol {
            text: Symbol::intern(Span::new(start, len).source(file)),
        }
    }

    fn intern(str: &str) -> Arc<str> {
        {
            if let Some(arc) = INTERNED_STRINGS
                .read()
                .unwrap()
                .iter()
                .find(|arc| arc.as_ref() == str)
            {
                return arc.clone();
            }
        }
        let arc: Arc<str> = Arc::from(str);
        INTERNED_STRINGS.write().unwrap().push(arc.clone());
        arc
    }
}
