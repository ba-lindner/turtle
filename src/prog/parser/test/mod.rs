macro_rules! parse_this {
    (
        $parser:ident
        ($($id:expr),* $(,)?)
        $($lex:expr),+ $(,)?
    ) => {
        let mut ident = crate::SymbolTable::new();
        #[allow(unused)]
        {
            use crate::Identified::*;
            let mut cnt = 0;
            $(
                cnt += 1;
                ident.insert(format!("ident_{cnt}"), $id);
            )*
        }
        let mut ltokens = Vec::new();
        #[allow(unused)]
        {
            use crate::{prog::lexer::LexToken::*, tokens::Keyword::*, Positionable as _};
            let mut cnt = 0;
            $(
                cnt += 1;
                ltokens.push($lex.attach_pos(crate::FilePos::new(cnt, 1)));
            )+
        }
        let mut $parser = crate::prog::Parser::new(
            &mut ident,
            ltokens
        );
    };
}

macro_rules! block {
    ($start:literal { $($spos:literal $skind:ident $sargs:tt)* }) => {
        crate::tokens::Block {
            begin: crate::FilePos::new($start, 1),
            statements: vec![
                $(stmt!($spos $skind $sargs)),*
            ],
        }
    };
}

macro_rules! stmt {
    ($pos:literal $kind:ident $args:tt) => {
        crate::Pos::new(stmt!(@args $kind $kind $args), crate::FilePos::new($pos, 1))
    };
    (@args MoveDist $kind:ident (
        $s:literal $e:literal $k:ident $a:tt ,
        $draw:expr ,
        $back:expr $(,)?
    )) => {
        crate::tokens::Statement::$kind {
            dist: expr!($s $e $k $a),
            draw: $draw,
            back: $back,
        }
    };
    (@args MoveHome $kind:ident ($draw:expr)) => {
        crate::tokens::Statement::$kind($draw)
    };
    (@args Turn $kind:ident (
        $left:expr,
        $s:literal $e:literal $k:ident $a:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind { left: $left, by: expr!($s $e $k $a) }
    };
    (@args Direction $kind:ident ( $s:literal $e:literal $k:ident $a:tt )) => {
        crate::tokens::Statement::$kind(expr!($s $e $k $a))
    };
    (@args Color $kind:ident (
        $rs:literal $re:literal $rk:ident $ra:tt ,
        $gs:literal $ge:literal $gk:ident $ga:tt ,
        $bs:literal $be:literal $bk:ident $ba:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind(
            expr!($rs $re $rk $ra),
            expr!($gs $ge $gk $ga),
            expr!($bs $be $bk $ba),
        )
    };
    (@args PathCall $kind:ident ( $path:literal (
        $($s:literal $e:literal $k:ident $a:tt),* $(,)?
    ))) => {
        crate::tokens::Statement::$kind(
            $path,
            vec![$(expr!($s $e $k $a)),*]
        )
    };
    (@args Store $kind:ident (
        $s:literal $e:literal $k:ident $a:tt,
        $vp:literal $vk:ident ($va:expr) $(,)?
    )) => {
        crate::tokens::Statement::$kind(
            expr!($s $e $k $a),
            var!($vp $vk $va)
        )
    };
    (@args Calc $kind:ident (
        $vp:literal $vk:ident ($va:expr),
        $s:literal $e:literal $k:ident $a:tt,
        $op:ident $(,)?
    )) => {
        crate::tokens::Statement::$kind {
            var: var!($vp $vk $va),
            val: expr!($s $e $k $a),
            op: crate::tokens::BiOperator::$op,
        }
    };
    (@args MoveMark $kind:ident ($draw:expr)) => {
        crate::tokens::Statement::$kind($draw)
    };
    (@args Print $kind:ident ( $s:literal $e:literal $k:ident $a:tt )) => {
        crate::tokens::Statement::$kind(expr!($s $e $k $a))
    };
    (@args IfBranch $kind:ident (
        $start:literal $end:literal $ekind:ident $eargs:tt ,
        $bstart:literal $block:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind(
            expr!($start $end $ekind $eargs),
            block!($bstart $block),
        )
    };
    (@args IfElseBranch $kind:ident (
        $start:literal $end:literal $ekind:ident $eargs:tt ,
        $bstart:literal $block:tt ,
        $estart:literal $eblock:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind(
            expr!($start $end $ekind $eargs),
            block!($bstart $block),
            block!($estart $eblock),
        )
    };
    (@args DoLoop $kind:ident (
        $start:literal $end:literal $ekind:ident $eargs:tt ,
        $bstart:literal $block:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind(
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
        crate::tokens::Statement::$kind {
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
        crate::tokens::Statement::$kind(
            expr!($start $end $ekind $eargs),
            block!($bstart $block),
        )
    };
    (@args RepeatLoop $kind:ident (
        $start:literal $end:literal $ekind:ident $eargs:tt ,
        $bstart:literal $block:tt $(,)?
    )) => {
        crate::tokens::Statement::$kind(
            expr!($start $end $ekind $eargs),
            block!($bstart $block),
        )
    };
    (@args $_:ident $kind:ident ()) => {
        crate::tokens::Statement::$kind
    };
}

macro_rules! expr {
    ($start:literal $end:literal $kind:ident $args:tt) => {
        expr!(@args $kind $kind $args).at(crate::FilePos::new($start, 1), crate::FilePos::new($end, 1))
    };
    (@args Const $kind:ident ($val:expr)) => {
        {
            use crate::tokens::Value::*;
            crate::tokens::ExprKind::$kind($val)
        }
    };
    (@args Variable $kind:ident ($pos:literal $var:ident ($varg:expr))) => {
        crate::tokens::ExprKind::$kind(var!($pos $var $varg))
    };
    (@args BiOperation $kind:ident (
        $ls:literal $le:literal $lk:ident $la:tt,
        $op:ident,
        $rs:literal $re:literal $rk:ident $ra:tt $(,)?
    )) => {
        crate::tokens::ExprKind::$kind(
            Box::new(expr!($ls $le $lk $la)),
            crate::tokens::BiOperator::$op,
            Box::new(expr!($rs $re $rk $ra))
        )
    };
    (@args UnOperation $kind:ident (
        $op:ident,
        $s:literal $e:literal $k:ident $a:tt $(,)?
    )) => {
        crate::tokens::ExprKind::$kind(
            crate::tokens::UnOperator::$op,
            Box::new(expr!($s $e $k $a))
        )
    };
    (@args Absolute $kind:ident ($s:literal $e:literal $k:ident $a:tt $(,)?)) => {
        crate::tokens::ExprKind::$kind(Box::new(expr!($s $e $k $a)))
    };
    (@args Bracket $kind:ident ($s:literal $e:literal $k:ident $a:tt $(,)?)) => {
        crate::tokens::ExprKind::$kind(Box::new(expr!($s $e $k $a)))
    };
    (@args Convert $kind:ident ($vt:ident, $s:literal $e:literal $k:ident $a:tt $(,)?)) => {
        crate::tokens::ExprKind::$kind(Box::new(expr!($s $e $k $a)), crate::tokens::ValType::$vt)
    };
    (@args FuncCall $kind:ident ( $func:ident (
        $($s:literal $e:literal $k:ident $a:tt),* $(,)?
    ))) => {
        crate::tokens::ExprKind::$kind(
            crate::tokens::PredefFunc::$func,
            vec![$(expr!($s $e $k $a)),*],
        )
    };
    (@args CalcCall $kind:ident ( $calc:literal (
        $($s:literal $e:literal $k:ident $a:tt),* $(,)?
    ))) => {
        crate::tokens::ExprKind::$kind(
            $calc,
            vec![$(expr!($s $e $k $a)),*],
        )
    };
}

macro_rules! var {
    ($pos:literal $kind:ident $arg:expr) => {
        var!(@args $kind $kind $arg).at(crate::FilePos::new($pos, 1))
    };
    (@args GlobalPreDef $kind:ident $arg:expr) => {
        crate::tokens::VariableKind::$kind($arg)
    };
    (@args $kind:ident $_:ident $arg:expr) => {
        crate::tokens::VariableKind::$kind($arg, crate::tokens::ValType::Any)
    };
}

mod exprs;
mod statements;
