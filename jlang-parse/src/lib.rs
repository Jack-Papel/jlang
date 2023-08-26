pub mod ast;
pub mod token;

pub mod prelude {
    use thiserror::Error;

    use crate::token::stream::TokenParseError;

    #[derive(Error, Debug)]
    pub enum ParseError {
        #[error("Failed to parse tokens: {0}")]
        TokenParseError(TokenParseError),
    }
}
