use crate::{
    pos::{FilePos, Positionable as _},
    prog::parser::ParseError,
};

#[test]
fn unfinished_expression() {
    parse_this!(
        parser =>
        IntLiteral(3),
        Symbol('+'),
    );
    let res = parser.parse_expr().unwrap_err();
    assert_eq!(
        res,
        ParseError::UnexpectedEnd.attach_pos(FilePos::new(2, 1))
    );
}

#[test]
fn nested_expr() {
    //3+|2-(5*4)|^(1+2*3)
    parse_this!(
        parser =>
        IntLiteral(3),
        Symbol('+'),
        Symbol('|'),
        IntLiteral(2),
        Symbol('-'),
        Symbol('('),
        IntLiteral(5),
        Symbol('*'),
        IntLiteral(4),
        Symbol(')'),
        Symbol('|'),
        Symbol('^'),
        Symbol('('),
        IntLiteral(1),
        Symbol('+'),
        IntLiteral(2),
        Symbol('*'),
        IntLiteral(3),
        Symbol(')'),
    );
    assert_eq!(
        parser.parse_expr().unwrap(),
        expr!(
            1 19 BiOperation(
                1 1 Const(Number(3.0)),
                Add,
                3 19 BiOperation(
                    3 11 Absolute(
                        4 10 BiOperation(
                            4 4 Const(Number(2.0)),
                            Sub,
                            6 10 Bracket(
                                7 9 BiOperation(
                                    7 7 Const(Number(5.0)),
                                    Mul,
                                    9 9 Const(Number(4.0))
                                )
                            )
                        )
                    ),
                    Exp,
                    13 19 Bracket(
                        14 18 BiOperation(
                            14 14 Const(Number(1.0)),
                            Add,
                            16 18 BiOperation(
                                16 16 Const(Number(2.0)),
                                Mul,
                                18 18 Const(Number(3.0))
                            )
                        )
                    )
                )
            )
        )
    )
}

#[test]
fn conversions() {
    // num(string(true) + 'a' <> 'b') > 1 = bool(2)
    parse_this!(
        parser =>
        Keyword(Num),
        Symbol('('),
        Keyword(String),
        Symbol('('),
        Keyword(True),
        Symbol(')'),
        Symbol('+'),
        StringLiteral("a".to_string()),
        Symbol('<'),
        Symbol('>'),
        StringLiteral("b".to_string()),
        Symbol(')'),
        Symbol('>'),
        IntLiteral(1),
        Symbol('='),
        Keyword(Bool),
        Symbol('('),
        IntLiteral(2),
        Symbol(')'),
    );
    assert_eq!(
        parser.parse_expr().unwrap(),
        expr!(
            1 19 BiOperation(
                1 14 BiOperation(
                    1 12 Convert(
                        Number,
                        3 11 BiOperation(
                            3 8 BiOperation(
                                3 6 Convert(
                                    String,
                                    5 5 Const(Boolean(true))
                                ),
                                Add,
                                8 8 Const(String("a".to_string()))
                            ),
                            UnEqual,
                            11 11 Const(String("b".to_string()))
                        )
                    ),
                    Greater,
                    14 14 Const(Number(1.0))
                ),
                Equal,
                16 19 Convert(
                    Boolean,
                    18 18 Const(Number(2.0)),
                )
            )
        )
    );
}

#[test]
fn functions() {
    // calc(sin(1), cos(tan(2))) + sqrt(rand(0, 10))
    parse_this!(
        parser (Calc(2)) =>
        Identifier(0),
        Symbol('('),
        Keyword(Sin),
        Symbol('('),
        IntLiteral(1),
        Symbol(')'),
        Symbol(','),
        Keyword(Cos),
        Symbol('('),
        Keyword(Tan),
        Symbol('('),
        IntLiteral(2),
        Symbol(')'),
        Symbol(')'),
        Symbol(')'),
        Symbol('+'),
        Keyword(Sqrt),
        Symbol('('),
        Keyword(Rand),
        Symbol('('),
        IntLiteral(0),
        Symbol(','),
        IntLiteral(10),
        Symbol(')'),
        Symbol(')'),
    );
    assert_eq!(
        parser.parse_expr().unwrap(),
        expr!(
            1 25 BiOperation(
                1 15 CalcCall(
                    0 (
                        3 6 FuncCall(
                            Sin(5 5 Const(Number(1.0)))
                        ),
                        8 14 FuncCall(
                            Cos(
                                10 13 FuncCall(
                                    Tan(12 12 Const(Number(2.0)))
                                )
                            )
                        ),
                    )
                ),
                Add,
                17 25 FuncCall(
                    Sqrt(
                        19 24 FuncCall(
                            Rand(
                                21 21 Const(Number(0.0)),
                                23 23 Const(Number(10.0)),
                            )
                        )
                    )
                )
            )
        )
    );
}
