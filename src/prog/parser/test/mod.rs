use super::*;

macro_rules! parse_this {
        (
            $parser:ident
            ($($id:expr),* $(,)?)
            $($lex:expr),+ $(,)?
        ) => {
            let mut ident = SymbolTable::new();
            #[allow(unused)]
            {
                use Identified::*;
                let mut cnt = 0;
                $(
                    cnt += 1;
                    ident.insert(format!("ident_{cnt}"), $id);
                )*
            }
            let mut ltokens = Vec::new();
            #[allow(unused)]
            {
                use LexToken::*;
                use crate::tokens::Keyword::*;
                let mut cnt = 0;
                $(
                    cnt += 1;
                    ltokens.push($lex.attach_pos(FilePos::new(cnt, 1)));
                )+
            }
            let mut $parser = Parser::new(
                &mut ident,
                ltokens
            );
        };
    }

macro_rules! block {
        ($start:literal { $($spos:literal $skind:ident $sargs:tt)* }) => {
            Block {
                begin: FilePos::new($start, 1),
                statements: vec![
                    $(stmt!($spos $skind $sargs))*
                ],
            }
        };
    }

macro_rules! stmt {
        ($pos:literal $kind:ident $args:tt) => {
            stmt!(@args $kind $kind $args).attach_pos(FilePos::new($pos, 1))
        };
        (@args MoveDist $kind:ident (
            $s:literal $e:literal $k:ident $a:tt ,
            $draw:expr ,
            $back:expr $(,)?
        )) => {
            Statement::$kind {
                dist: expr!($s $e $k $a),
                draw: $draw,
                back: $back,
            }
        };
        (@args MoveHome $kind:ident ($draw:expr)) => {
            Statement::$kind($draw)
        };
        (@args Turn $kind:ident (
            $left:expr,
            $s:literal $e:literal $k:ident $a:tt $(,)?
        )) => {
            Statement::$kind { left: $left, by: expr!($s $e $k $a) }
        };
        (@args Direction $kind:ident ( $s:literal $e:literal $k:ident $a:tt )) => {
            Statement::$kind(expr!($s $e $k $a))
        };
        (@args Color $kind:ident (
            $rs:literal $re:literal $rk:ident $ra:tt ,
            $gs:literal $ge:literal $gk:ident $ga:tt ,
            $bs:literal $be:literal $bk:ident $ba:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($rs $re $rk $ra),
                expr!($gs $ge $gk $ga),
                expr!($bs $be $bk $ba),
            )
        };
        (@args PathCall $kind:ident ( $path:literal (
            $($s:literal $e:literal $k:ident $a:tt),* $(,)?
        ))) => {
            Statement::$kind(
                $path,
                vec![$(expr!($s $e $k $a)),*]
            )
        };
        (@args Store $kind:ident (
            $s:literal $e:literal $k:ident $a:tt,
            $vp:literal $vk:ident ($va:expr) $(,)?
        )) => {
            Statement::$kind(
                expr!($s $e $k $a),
                var!($vp $vk $va)
            )
        };
        (@args Calc $kind:ident (
            $vp:literal $vk:ident ($va:expr),
            $s:literal $e:literal $k:ident $a:tt,
            $op:ident $(,)?
        )) => {
            Statement::$kind {
                var: var!($vp $vk $va),
                val: expr!($s $e $k $a),
                op: BiOperator::$op,
            }
        };
        (@args MoveMark $kind:ident ($draw:expr)) => {
            Statement::$kind($draw)
        };
        (@args Print $kind:ident ( $s:literal $e:literal $k:ident $a:tt )) => {
            Statement::$kind(expr!($s $e $k $a))
        };
        (@args IfBranch $kind:ident (
            $start:literal $end:literal $ekind:ident $eargs:tt ,
            $bstart:literal $block:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($start $end $ekind $eargs),
                block!($bstart $block),
            )
        };
        (@args IfElseBranch $kind:ident (
            $start:literal $end:literal $ekind:ident $eargs:tt ,
            $bstart:literal $block:tt ,
            $estart:literal $eblock:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($start $end $ekind $eargs),
                block!($bstart $block),
                block!($estart $eblock),
            )
        };
        (@args DoLoop $kind:ident (
            $start:literal $end:literal $ekind:ident $eargs:tt ,
            $bstart:literal $block:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($start $end $ekind $eargs),
                block!($bstart $block),
            )
        };
        (@args CounterLoop $kind:ident (
            $cp:literal $ck:ident ($ca:expr),
            $fs:literal $fe:literal $fk:ident $fa:tt,
            $up:expr,
            $ts:literal $te:literal $tk:ident $ta:tt,
            $(step: $ss:literal $se:literal $sk:ident $sa:tt,)?
            $bs:literal $block:tt $(,)?
        )) => {
            Statement::$kind {
                counter: var!($cp $ck $ca),
                from: expr!($fs $fe $fk $fa),
                up: $up,
                to: expr!($ts $te $tk $ta),
                step: None $(.unwrap_or(expr!($ss $se $sk $sa)))?,
                body: block!($bs $block),
            }
        };
        (@args WhileLoop $kind:ident (
            $start:literal $end:literal $ekind:ident $eargs:tt ,
            $bstart:literal $block:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($start $end $ekind $eargs),
                block!($bstart $block),
            )
        };
        (@args RepeatLoop $kind:ident (
            $start:literal $end:literal $ekind:ident $eargs:tt ,
            $bstart:literal $block:tt $(,)?
        )) => {
            Statement::$kind(
                expr!($start $end $ekind $eargs),
                block!($bstart $block),
            )
        };
        (@args $_:ident $kind:ident ()) => {
            Statement::$kind
        };
    }

macro_rules! expr {
        ($start:literal $end:literal $kind:ident $args:tt) => {
            expr!(@args $kind $kind $args).at(FilePos::new($start, 1), FilePos::new($end, 1))
        };
        (@args Const $kind:ident ($val:expr)) => {
            {
                use Value::*;
                ExprKind::$kind($val)
            }
        };
        (@args Variable $kind:ident ($pos:literal $var:ident ($varg:expr))) => {
            ExprKind::$kind(var!($pos $var $varg))
        };
        (@args BiOperation $kind:ident (
            $ls:literal $le:literal $lk:ident $la:tt,
            $op:ident,
            $rs:literal $re:literal $rk:ident $ra:tt $(,)?
        )) => {
            ExprKind::$kind(
                Box::new(expr!($ls $le $lk $la)),
                BiOperator::$op,
                Box::new(expr!($rs $re $rk $ra))
            )
        };
        (@args UnOperation $kind:ident (
            $op:ident,
            $s:literal $e:literal $k:ident $a:tt $(,)?
        )) => {
            ExprKind::$kind(
                UnOperator::$op,
                Box::new(expr!($s $e $k $a))
            )
        };
        (@args Absolute $kind:ident ($s:literal $e:literal $k:ident $a:tt $(,)?)) => {
            ExprKind::$kind(Box::new(expr!($s $e $k $a)))
        };
        (@args Bracket $kind:ident ($s:literal $e:literal $k:ident $a:tt $(,)?)) => {
            ExprKind::$kind(Box::new(expr!($s $e $k $a)))
        };
        (@args Convert $kind:ident ($s:literal $e:literal $k:ident $a:tt, $vt:ident $(,)?)) => {
            ExprKind::$kind(Box::new(expr!($s $e $k $a)), ValType::$vt)
        };
        (@args FuncCall $kind:ident ( $func:ident (
            $($s:literal $e:literal $k:ident $a:tt),* $(,)?
        ))) => {
            ExprKind::$kind(
                PredefFunc::$func,
                vec![$(expr!($s $e $k $a)),*],
            )
        };
        (@args CalcCall $kind:ident ( $calc:literal (
            $($s:literal $e:literal $k:ident $a:tt),* $(,)?
        ))) => {
            ExprKind::$kind(
                $calc,
                vec![$(expr!($s $e $k $a)),*],
            )
        };
    }

macro_rules! var {
        ($pos:literal $kind:ident $arg:expr) => {
            var!(@args $kind $kind $arg).at(FilePos::new($pos, 1))
        };
        (@args GlobalPreDef $kind:ident $arg:expr) => {
            VariableKind::$kind($arg)
        };
        (@args $kind:ident $_:ident $arg:expr) => {
            VariableKind::$kind($arg, ValType::Any)
        };
    }

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
    let stmt = parser.parse_stm().unwrap();
    assert_eq!(
        stmt,
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

#[test]
fn unfinished_expression() {
    let fp = FilePos::new(0, 0);
    let mut ident = SymbolTable::new();
    let mut parse = Parser::new(
        &mut ident,
        vec![
            LexToken::IntLiteral(3).attach_pos(fp),
            LexToken::Symbol('+').attach_pos(fp),
        ],
    );
    let res = parse.parse_expr();
    assert_eq!(res, Err(ParseError::UnexpectedEnd.attach_pos(fp)));
}

#[test]
fn nested_expr() {
    //3+|2-(5*4)|^(1+2*3)
    parse_this!(
        parser ()
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
