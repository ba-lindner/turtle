use super::*;

macro_rules! lex_this {
    ($lex:ident, $code:expr) => {
        let mut ident = SymbolTable::new();
        let mut feat = FeatureConf::default();
        #[allow(unused_mut)]
        let mut $lex = Lexer::new(&mut ident, &mut feat, $code.chars());
    };
}

macro_rules! assert_lex {
        ($lex:ident, $($line:literal,$col:literal,$len:literal $token:expr),+ $(,)?) => {
            #[allow(unused_imports)]
            {
                use crate::tokens::{Keyword::*, PredefVar::*};
                use LexToken::*;
                $(assert_eq!($lex.next_token(), Some(Ok($token.with_span(((FilePos::new($line, $col), FilePos::new($line, $col + $len)))))));)+
                assert_eq!($lex.next_token(), None);
            }
        };
    }

#[test]
fn empty() {
    lex_this!(lex, "");
    assert_eq!(lex.lookahead(), None);
    assert_eq!(lex.next_char(), None);
    assert_eq!(lex.next_token(), None);
}

#[test]
fn leading_whitespace() {
    lex_this!(lex, "     \n\n \t\t\n   \t\n  *");
    lex.skip_comment();
    assert_eq!(lex.next_char(), Some('*'));
}

#[test]
fn symbols() {
    lex_this!(lex, "/");
    assert_eq!(*lex.next_token().unwrap().unwrap(), LexToken::Symbol('/'));
}

#[test]
fn skip_nothing() {
    lex_this!(lex1, "path");
    lex_this!(lex2, "path");
    lex2.skip_comment();
    assert_eq!(lex1, lex2);
}

#[test]
fn num_literal() {
    lex_this!(lex, ".123");
    assert_lex!(lex, 1,1,4 FloatLiteral(0.123));
}

fn keyword_after_num() {
    lex_this!(lex, "if@1=12then endif");
    assert_lex!(lex,
        1,1,2 Keyword(If),
        1,3,2 PredefVar(Arg(1)),
        1,5,1 Symbol('='),
        1,6,2 IntLiteral(12),
        1,8,4 Keyword(Then),
        1,13,5 Keyword(Endif),
    );
}
