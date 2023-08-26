use std::fmt::Display;

use crate::ast::{span::Span, symbol::Symbol};
pub mod stream;

#[derive(Clone, Debug)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Not,
    PlusPlus,
    MinusMinus,
}

#[derive(Clone, Debug)]
pub enum LitKind {
    Bool,
    Integer, // e.g. `1`
    Float,   // e.g. `1.`, `1.0`
    Str,
}

#[derive(Clone, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
}

#[derive(Clone)]
pub enum TokenKind {
    Eq,
    Lt,
    Le,
    EqEq,
    Ne,
    Ge,
    Gt,
    AndAnd,
    OrOr,
    DotDot,
    UnOp(UnOp),
    BinOp(BinOp),
    BinOpEq(BinOp),

    Dot,
    Comma,
    Semicolon,
    Colon,
    RArrow,
    Question,
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    Literal(Lit),
    Ident(Symbol),
}

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::EqEq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::AndAnd => write!(f, "&&"),
            Self::OrOr => write!(f, "||"),
            Self::DotDot => write!(f, ".."),
            Self::UnOp(op) => write!(f, "{op}"),
            Self::BinOp(op) => write!(f, "{op}"),
            Self::BinOpEq(op) => write!(f, "{op}="),
            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::RArrow => write!(f, "->"),
            Self::Question => write!(f, "?"),
            Self::OpenDelim(Delimiter::Parenthesis) => write!(f, "("),
            Self::OpenDelim(Delimiter::Brace) => write!(f, "{{"),
            Self::OpenDelim(Delimiter::Bracket) => write!(f, "["),
            Self::CloseDelim(Delimiter::Parenthesis) => write!(f, ")"),
            Self::CloseDelim(Delimiter::Brace) => write!(f, "}}"),
            Self::CloseDelim(Delimiter::Bracket) => write!(f, "]"),
            Self::Literal(Lit {
                kind: LitKind::Str,
                symbol,
            }) => {
                write!(f, "\"{}\"", symbol.text)
            }
            Self::Literal(lit) => write!(f, "{}", lit.symbol.text),
            Self::Ident(ident) => write!(f, "{}", ident.text),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
            Self::Percent => write!(f, "%"),
            Self::Plus => write!(f, "+"),
            Self::Slash => write!(f, "/"),
            Self::Star => write!(f, "*"),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MinusMinus => write!(f, "--"),
            Self::Not => write!(f, "!"),
            Self::PlusPlus => write!(f, "++"),
        }
    }
}
