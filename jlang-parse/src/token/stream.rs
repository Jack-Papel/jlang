use std::iter::Peekable;
use std::str::CharIndices;

use crate::ast::{span::Span, symbol::Symbol};

use super::{Lit, LitKind, Token, TokenKind};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TokenParseError {
    #[error("Unterminated string: `{0}` at {0}")]
    UnterminatedString(Box<str>, Span),
    #[error("Invalid token: `{0}` at {0}")]
    InvalidToken(Box<str>, Span),
}

type Result<T> = std::result::Result<T, TokenParseError>;

pub struct TokenStream<'a> {
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        TokenStream {
            chars: input.char_indices().peekable(),
        }
    }
}

enum ParsingState {
    Unknown,
    Whitespace,
    Parsing(ParsingKind),
}

#[derive(Clone, Copy)]
enum ParsingKind {
    Symbol,
    Ident,
    Number,
    String,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = String::new();
        let mut state = ParsingState::Unknown;
        let mut start_index = 0;

        loop {
            match state {
                ParsingState::Whitespace => {
                    buf.clear();
                    state = ParsingState::Unknown;
                }
                ParsingState::Unknown => {
                    if let Some((idx, chr)) = self.chars.next() {
                        buf.push(chr);
                        state = determine_token_type(chr, self.chars.peek().map(|tup| tup.1));
                        start_index = idx;
                    } else if buf.is_empty() {
                        return None;
                    } else {
                        let len = buf.len();
                        return Some(Err(TokenParseError::InvalidToken(
                            buf.into(),
                            Span::new(start_index, len),
                        )));
                    }
                }
                ParsingState::Parsing(kind) => {
                    match kind.parse(start_index, &mut buf, &mut self.chars) {
                        TokenParseResult::Done(result) => {
                            return Some(result.map(|kind| Token {
                                kind,
                                span: Span::new(start_index, buf.len()),
                            }));
                        }
                        TokenParseResult::Continue => {}
                    }
                }
            }
        }
    }
}

#[rustfmt::skip]
fn determine_token_type(first_char: char, next: Option<char>) -> ParsingState {
    if first_char.is_ascii_whitespace() {
        ParsingState::Whitespace
    } else if first_char.is_alphabetic() {
        ParsingState::Parsing(ParsingKind::Ident)
    } else if first_char == '"' {
        ParsingState::Parsing(ParsingKind::String)
    } else if first_char.is_ascii_digit() | (first_char == '.' && next.as_ref().is_some_and(char::is_ascii_digit)) {
        ParsingState::Parsing(ParsingKind::Number)
    } else if matches!(first_char,
        | '[' | ']' | '=' | '<' | '!' | '>' | '&' | '|' | '.' | '+' | '-' | '*' | '/' 
        | '%' | ',' | ';' | ':' | '?' | '(' | ')' | '{' | '}'
    ) {
        ParsingState::Parsing(ParsingKind::Symbol)
    } else {
        ParsingState::Unknown
    }
}

enum TokenParseResult {
    Done(Result<TokenKind>),
    /// Indicates a need for more characters (the token isn't complete)
    Continue,
}

impl ParsingKind {
    /// This gets called for every character that is encountered after we have determined
    /// the parsing kind
    fn parse(
        &self,
        start_index: usize,
        buf: &mut String,
        chars: &mut Peekable<CharIndices>,
    ) -> TokenParseResult {
        match self {
            Self::Ident => parse_ident(buf, chars),
            Self::Number => parse_number(buf, chars),
            Self::Symbol => parse_symbol(start_index, buf, chars),
            Self::String => parse_string(start_index, buf, chars),
        }
    }
}

fn parse_ident(buf: &mut String, chars: &mut Peekable<CharIndices>) -> TokenParseResult {
    if let Some((_, chr)) = chars.peek() {
        if chr.is_ascii_alphanumeric() || *chr == '_' {
            buf.push(*chr);
            chars.next();
            return TokenParseResult::Continue;
        }
    }

    TokenParseResult::Done(Ok(TokenKind::Ident(Symbol::new(buf))))
}

fn parse_number(buf: &mut String, chars: &mut Peekable<CharIndices>) -> TokenParseResult {
    if let Some((_, chr)) = chars.peek() {
        if chr.is_ascii_digit() || *chr == '.' {
            buf.push(*chr);
            chars.next();
            return TokenParseResult::Continue;
        }
    }

    let kind = if buf.contains('.') {
        LitKind::Float
    } else {
        LitKind::Integer
    };

    TokenParseResult::Done(Ok(TokenKind::Literal(Lit {
        kind,
        symbol: Symbol::new(buf),
    })))
}

fn parse_string(
    start_index: usize,
    buf: &mut String,
    chars: &mut Peekable<CharIndices>,
) -> TokenParseResult {
    if buf.starts_with('"') {
        buf.remove(0);
    }

    if let Some((_, chr)) = chars.peek() {
        if *chr == '"' {
            chars.next();

            TokenParseResult::Done(Ok(TokenKind::Literal(Lit {
                kind: LitKind::Str,
                symbol: Symbol::new(buf),
            })))
        } else {
            buf.push(*chr);
            chars.next();
            TokenParseResult::Continue
        }
    } else {
        TokenParseResult::Done(Err(TokenParseError::UnterminatedString(
            buf.as_str().into(),
            Span::new(start_index, buf.len()),
        )))
    }
}

fn parse_symbol(
    start_index: usize,
    buf: &mut String,
    chars: &mut Peekable<CharIndices>,
) -> TokenParseResult {
    use super::BinOp::*;
    use super::Delimiter::*;
    use super::TokenKind::*;
    use super::UnOp::*;

    TokenParseResult::Done(Ok(match (buf.as_str(), chars.peek().map(|tup| tup.1)) {
        ("+", Some('+'))
        | ("-", Some('-'))
        | ("+" | "-" | "*" | "/" | "%" | "<" | "=" | "!" | ">", Some('='))
        | ("&", Some('&'))
        | ("|", Some('|'))
        | (".", Some('.'))
        | ("-", Some('>')) => {
            if let Some((_, chr)) = chars.next() {
                buf.push(chr);
            }
            return TokenParseResult::Continue;
        }

        ("=", _) => Eq,

        ("!", _) => UnOp(Not),
        ("++", _) => UnOp(PlusPlus),
        ("--", _) => UnOp(MinusMinus),

        ("+", _) => BinOp(Plus),
        ("-", _) => BinOp(Minus),
        ("*", _) => BinOp(Star),
        ("/", _) => BinOp(Slash),
        ("%", _) => BinOp(Percent),
        ("+=", _) => BinOpEq(Plus),
        ("-=", _) => BinOpEq(Minus),
        ("*=", _) => BinOpEq(Star),
        ("/=", _) => BinOpEq(Slash),
        ("%=", _) => BinOpEq(Percent),
        ("<", _) => Lt,
        ("<=", _) => Le,
        ("==", _) => EqEq,
        ("!=", _) => Ne,
        (">=", _) => Ge,
        (">", _) => Gt,
        ("&&", _) => AndAnd,
        ("||", _) => OrOr,
        ("..", _) => DotDot,

        (".", _) => Dot,
        (",", _) => Comma,
        (";", _) => Semicolon,
        (":", _) => Colon,
        ("->", _) => RArrow,
        ("?", _) => Question,
        ("(", _) => OpenDelim(Parenthesis),
        ("{", _) => OpenDelim(Brace),
        ("[", _) => OpenDelim(Bracket),
        (")", _) => CloseDelim(Parenthesis),
        ("}", _) => CloseDelim(Brace),
        ("]", _) => CloseDelim(Bracket),
        _ => {
            return TokenParseResult::Done(Err(TokenParseError::InvalidToken(
                buf.as_str().into(),
                Span::new(start_index, buf.len()),
            )));
        }
    }))
}
