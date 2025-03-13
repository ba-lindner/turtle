#[test]
fn if_branch() {
    parse_this!(
        parser (LocalVar)
        Keyword(If),
        Identifier(0),
        Symbol('<'),
        IntLiteral(2),
        Keyword(Then),
        Keyword(Stop),
        Keyword(Endif),
    );
    assert_eq!(
        parser.parse_stm().unwrap(),
        stmt!(
            1 IfBranch(
                2 4 BiOperation(
                    2 2 Variable(2 Local(0)),
                    Less,
                    4 4 Const(Number(2.0))
                ),
                1 {
                    6 Stop ()
                }
            )
        )
    );
}

#[test]
fn if_else_branch() {
    parse_this!(
        parser (LocalVar)
        Keyword(If),
        Keyword(Not),
        Symbol('('),
        Identifier(0),
        Symbol('>'),
        Symbol('='),
        IntLiteral(2),
        Symbol(')'),
        Keyword(Then),
        Keyword(Stop),
        Keyword(Else),
        Keyword(Walk),
        IntLiteral(30),
        Keyword(Endif),
    );
    assert_eq!(
        parser.parse_stm().unwrap(),
        stmt!(
            1 IfElseBranch(
                2 8 UnOperation(
                    Not,
                    3 8 Bracket (
                        4 7 BiOperation(
                            4 4 Variable(4 Local(0)),
                            GreaterEqual,
                            7 7 Const(Number(2.0)),
                        )
                    )
                ),
                1 {
                    10 Stop ()
                },
                11 {
                    12 MoveDist(
                        13 13 Const(Number(30.0)),
                        true,
                        false,
                    )
                }
            )
        )
    );
}
