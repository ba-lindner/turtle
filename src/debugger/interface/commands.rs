//! Common debugger commands
//!
//! This module provides an enum for common debugger commands, [`DbgCommand`],
//! aswell as a function for parsing it, [`parse_command`].

use std::{fmt::Display, num::ParseIntError};

use crate::{
    Disp,
    debugger::{
        Breakpoint, DbgEvent, DebugErr, Debugger, FrameInfo, FuncType, ProgEnd, TurtleInfo,
        VarDump, window::Window,
    },
    pos::{FilePos, FilePosParseErr},
    tokens::{StmtKind, Value},
};

macro_rules! dbg_commands {
    (
        $run:ident
        $(
            $(#[doc = $doc:expr])+
            $cc:ident $(($($at:ty),+))? $(|$($an:ident),+|)? {$($exec:tt)*}
            $(return $sc:ident ($($rn:ident $rt:ty),*))?
        )+
    ) => {
        /// Turtle debugger commands
        ///
        /// This enum lists all commands available for the
        /// [default turtle debugger](super::Terminal).
        /// It is very similar to the
        /// [available debug API](crate::debugger::Debugger).
        ///
        /// Please take note that all `Step`-variants will also stop
        /// on encountering a breakpoint, even if breakpoints are
        /// only mentioned for [`Run`](DbgCommand::Run).
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum DbgCommand {
            $($(#[doc = $doc])+ $cc $(($($at),+))?,)+
        }

        impl DbgCommand {
            #[allow(unused_parens)]
            fn call_debugger<'p, W: Window + 'p>(self, $run: &mut Debugger<'p, W>) -> Result<Option<CmdOutput>, StopCause> {
                match self {
                    $(Self::$cc $(($($an),+))? => {
                        $(let ($($rn),*) = )? {$($exec)*} $(; return Ok(Some(CmdOutput::$cc($($rn),*)));)?
                    })+
                }
                Ok(None)
            }
        }

        dbg_commands! {@filter $([($(@return ($($rn $rt),*))? $cc $($sc)?)])+}
    };
    (@filter $([$((@return $($t:tt)*))? $(($_:ident))?])*) => {
        dbg_commands! {@exec $($($($t)*)?)*}
    };
    (@exec $(($($p:ident $t:ty),+) $cc:ident $sc:ident)+) => {
        pub enum CmdOutput {
            $($cc($($t),+),)+
        }

        #[allow(unused_parens)]
        impl CmdOutput {
            $(pub fn $sc(self) -> Option<($($t),+)> {
                if let Self::$cc($($p),+) = self {
                    Some(($($p),+))
                } else { None }
            })+
        }
    };
}

dbg_commands! { run
    /// Run [`DebugController::step_single()`](crate::debugger::Debugger::step_single()) n times.
    StepSingle(usize) |n| { for _ in 0..n { if run.step_single()?.is_none() { break; }}}
    /// Run [`DebugController::step_over()`](crate::debugger::Debugger::step_over()) n times.
    StepOver(usize) |n| { for _ in 0..n { if run.step_over()? { break; }}}
    /// Run [`DebugController::step_out()`](crate::debugger::Debugger::step_out()).
    StepOut { run.step_out()?; }
    /// Run [`DebugController::step_kind()`](crate::debugger::Debugger::step_kind())
    /// with [`StmtKind::Turtle`](crate::tokens::StmtKind::Turtle).
    StepTurtle { run.step_kind(StmtKind::Turtle)?; }
    /// Run [`DebugController::step_kind()`](crate::debugger::Debugger::step_kind())
    /// with [`StmtKind::Draw`](crate::tokens::StmtKind::Draw).
    StepDraw { run.step_kind(StmtKind::Draw)?; }
    /// Run [`DebugController::step_sync()`](crate::debugger::Debugger::step_sync()).
    StepSync { run.step_sync()?; }
    /// Run [`DebugController::run_breakpoints()`](crate::debugger::Debugger::run_breakpoints()).
    Run { run.run_breakpoints()?; }
    /// Call [`DebugController::toggle_narrate()`](crate::debugger::Debugger::toggle_narrate()).
    ToggleNarrate { run.toggle_narrate() } return toggle_narrate (active bool)
    /// Print [`DebugController::vardump()`](crate::debugger::Debugger::vardump())
    /// with [`None`] (active frame).
    Vardump { run.vardump(None)? } return vardump (vars VarDump)
    /// Print [`DebugController::curr_pos()`](crate::debugger::Debugger::curr_pos()).
    CurrPos { run.curr_pos() } return curr_pos (active usize, ft FuncType, fp FilePos)
    /// Print [`DebugController::list_turtles()`](crate::debugger::Debugger::list_turtles()).
    ListTurtles { run.list_turtles() } return list_turtles (sync bool, list Vec<TurtleInfo>)
    /// Call [`DebugController::select_turtle()`](crate::debugger::Debugger::select_turtle()).
    SelectTurtle(usize) |id| { run.select_turtle(id)?; }
    /// Print [`DebugController::list_breakpoints()`](crate::debugger::Debugger::list_breakpoints()).
    ListBreakpoints { run.list_breakpoints() } return list_breakpoints (bp Vec<Breakpoint>)
    /// Call [`DebugController::add_breakpoint()`](crate::debugger::Debugger::add_breakpoint()).
    AddBreakpoint(FilePos) |pos| { run.add_breakpoint(pos) } return add_breakpoint (id usize)
    /// Call [`DebugController::delete_breakpoint()`](crate::debugger::Debugger::delete_breakpoint()).
    DeleteBreakpoint(usize) |id| { run.delete_breakpoint(id); }
    /// Call [`DebugController::enable_breakpoint()`](crate::debugger::Debugger::enable_breakpoint()).
    EnableBreakpoint(usize, bool) |id, active| { run.enable_breakpoint(id, active)?; }
    /// Print [`DebugController::stacktrace()`](crate::debugger::Debugger::stacktrace()).
    Stacktrace { run.stacktrace() } return stacktrace (frames Vec<FrameInfo>)
    /// Print [`DebugController::eval_expr()`](crate::debugger::Debugger::eval_expr()).
    Evaluate(String) |expr| { run.eval_expr(None, &expr)? } return evaluate (val Value)
    /// Call [`DebugController::set_var()`](crate::debugger::Debugger::set_var()).
    Set(String, String) |var, expr| { run.set_var(None, &var, &expr)?; }
    /// Run [`DebugController::exec_stmt()`](crate::debugger::Debugger::exec_stmt()).
    Execute(String) |stmt| { run.exec_stmt(&stmt)? } return execute (finished bool)
}

pub struct CmdResult {
    pub output: Option<CmdOutput>,
    pub error: Option<DebugErr>,
    pub stmt_count: usize,
    pub run_events: Vec<DbgEvent>,
}

enum StopCause {
    ProgEnd(ProgEnd),
    Err(DebugErr),
}

impl From<ProgEnd> for StopCause {
    fn from(value: ProgEnd) -> Self {
        Self::ProgEnd(value)
    }
}

impl From<DebugErr> for StopCause {
    fn from(value: DebugErr) -> Self {
        Self::Err(value)
    }
}

impl DbgCommand {
    pub fn exec<'p, W: Window + 'p>(self, run: &mut Debugger<'p, W>) -> Result<CmdResult, ProgEnd> {
        let (output, stop) = match self.call_debugger(run) {
            Ok(Some(out)) => (Some(out), None),
            Ok(None) => (None, None),
            Err(why) => (None, Some(why)),
        };
        let (stmt_count, run_events) = run.events();
        let error = match stop {
            None => None,
            Some(StopCause::Err(err)) => Some(err),
            Some(StopCause::ProgEnd(end)) => return Err(end),
        };
        Ok(CmdResult {
            output,
            error,
            stmt_count,
            run_events,
        })
    }
}

/// Help for all commands
const DEBUG_HELP: &str = "available commands:
  run                     - run until breakpoint is hit
  step ...                - execute single steps
  narrator                - toggle narrator
  variables               - dump variables
  position                - print current position
  breakpoint ...          - manage breakpoints
  turtle                  - list active turtles
  turtle <id>             - switch active turtle
  stacktrace              - view stacktrace
  evaluate <expr>         - get value of expression
  set <var> <expr>        - set variable to value of expression
  execute <stmt>          - let active turtle execute statement
  help [step|breakpoint]  - show this help / detailed help for subcommand
  quit                    - exit the debugger";

/// Help for step commands
const DEBUG_HELP_STEP: &str = "subcommands for step:
  step [<count>]          - execute <count> statements
                            <count> defaults to one
  step over [<count>]     - step over path / calc call
                            if no such call occurs, identical to step [<count>]
  step out                - step out of current function
  step turtle             - skip until next turtle action
  step draw               - skip until next drawn line
  step sync               - skip until turtles are synced
                            this is almost identical to step draw,
                            but allows you to select the active turtle";

/// Help for breakpoint commands
const DEBUG_HELP_BP: &str = "subcommands for breakpoint:
  breakpoint              - list breakpoints
  breakpoint add <pos>    - add breakpoint at <pos>
  breakpoint remove <id>  - remove breakpoint <id>
  breakpoint enable <id>  - enable breakpoint <id>
  breakpoint disable <id> - disable breakpoint <id>";

/// Extends a [`str`] to any of the given words if it is a prefix.
///
/// This can be used to easily allow users to only enter the prefix
/// of a command.
pub fn extend_str<'i>(inp: &'i str, words: &'i [&'_ str]) -> &'i str {
    for word in words {
        if word.starts_with(inp) {
            return word;
        }
    }
    inp
}

/// Reasons why no command was returned
///
/// This is mainly used to handle quasi-commands like `quit` and `help`.
pub enum NoCmdReason<'l> {
    /// User wants to quit
    Quit,
    /// Show given help
    Help(&'static str),
    /// Empty input
    Empty,
    /// Error parsing command
    Err(CmdError<'l>),
}

impl<'l> From<CmdError<'l>> for NoCmdReason<'l> {
    fn from(value: CmdError<'l>) -> Self {
        Self::Err(value)
    }
}

impl From<ParseIntError> for NoCmdReason<'_> {
    fn from(value: ParseIntError) -> Self {
        CmdError::ParseInt(value).into()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CmdError<'l> {
    #[error("unknown command {0}")]
    Unknown(&'l str),
    #[error("unknown subcommand {1} for {0}")]
    UnknownSub(&'l str, &'l str),
    #[error("missing argument")]
    MissingArg,
    #[error("invalid argument: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("invalid argument: {0}")]
    ParsePos(#[from] FilePosParseErr),
}

/// Parse a line of user input into a command
pub fn parse_command(inp: &str) -> Result<DbgCommand, NoCmdReason<'_>> {
    let mut words = inp.split_whitespace();
    // TODO: once stabilized, use `SplitWhitespace::remainder()` instead
    let rem = || {
        inp.split_once(char::is_whitespace)
            .map(|(_, rem)| rem)
            .unwrap_or_default()
            .to_string()
    };
    Ok(match_extended!(words.next() => {
        None => return Err(NoCmdReason::Empty),
        "step" => step_command(words)?,
        "run" => DbgCommand::Run,
        "narrator" => DbgCommand::ToggleNarrate,
        "variables" => DbgCommand::Vardump,
        "position" => DbgCommand::CurrPos,
        "breakpoint" => breakpoint_command(words)?,
        "turtle" => match words.next() {
            Some(id) => DbgCommand::SelectTurtle(id.parse()?),
            None => DbgCommand::ListTurtles,
        },
        "stacktrace" => DbgCommand::Stacktrace,
        "evaluate" => DbgCommand::Evaluate(rem()),
        "set" => DbgCommand::Set(
            words.next().ok_or(CmdError::MissingArg)?.to_string(),
            words.next().ok_or(CmdError::MissingArg)?.to_string()
        ),
        "execute" => DbgCommand::Execute(rem()),
        "help" => return Err(match_extended!(words.next() => {
            None => NoCmdReason::Help(DEBUG_HELP),
            "step" => NoCmdReason::Help(DEBUG_HELP_STEP),
            "breakpoint" => NoCmdReason::Help(DEBUG_HELP_BP),
            other => CmdError::UnknownSub("help", other).into(),
        })),
        "quit" => return Err(NoCmdReason::Quit),
        other => Err(CmdError::Unknown(other))?,
    }))
}

/// Parse the various `step`-commands
fn step_command<'w>(mut words: impl Iterator<Item = &'w str>) -> Result<DbgCommand, CmdError<'w>> {
    Ok(match_extended!(words.next() => {
        None => DbgCommand::StepSingle(1),
        "over" => DbgCommand::StepOver(words.next().unwrap_or("1").parse()?),
        "out" => DbgCommand::StepOut,
        "turtle" => DbgCommand::StepTurtle,
        "draw" => DbgCommand::StepDraw,
        "sync" => DbgCommand::StepSync,
        count => DbgCommand::StepSingle(count.parse()?),
    }))
}

/// Parse the various `breakpoint`-commands
fn breakpoint_command<'w>(
    mut words: impl Iterator<Item = &'w str>,
) -> Result<DbgCommand, CmdError<'w>> {
    Ok(match_extended!(words.next() => {
        None => DbgCommand::ListBreakpoints,
        "add" => DbgCommand::AddBreakpoint(words.next().ok_or(CmdError::MissingArg)?.parse()?),
        "remove" => DbgCommand::DeleteBreakpoint(words.next().ok_or(CmdError::MissingArg)?.parse()?),
        "enable" => DbgCommand::EnableBreakpoint(words.next().ok_or(CmdError::MissingArg)?.parse()?, true),
        "disable" => DbgCommand::EnableBreakpoint(words.next().ok_or(CmdError::MissingArg)?.parse()?, false),
        other => return Err(CmdError::UnknownSub("breakpoint", other)),
    }))
}

impl Disp for CmdOutput {
    fn disp(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbols: &crate::SymbolTable,
    ) -> std::fmt::Result {
        match self {
            CmdOutput::ToggleNarrate(narrator) => {
                write!(f, "narrator is {}", if *narrator { "ON" } else { "OFF" })
            }
            CmdOutput::Vardump(vars) => vars.fmt(f),
            CmdOutput::CurrPos(id, ft, pos) => write!(
                f,
                "turtle #{id} is currently in {} at {pos}",
                ft.with_symbols(symbols)
            ),
            CmdOutput::ListTurtles(sync, list) => {
                write!(f, "turtles are {}in sync", if *sync { "" } else { "NOT " })?;
                for ttl in list {
                    ttl.with_symbols(symbols).fmt(f)?;
                }
                Ok(())
            }
            CmdOutput::ListBreakpoints(list) => {
                for bp in list {
                    bp.fmt(f)?;
                }
                Ok(())
            }
            CmdOutput::AddBreakpoint(id) => write!(f, "added breakpoint #{id}"),
            CmdOutput::Stacktrace(frames) => {
                for frame in frames {
                    frame.with_symbols(symbols).fmt(f)?;
                }
                Ok(())
            }
            CmdOutput::Evaluate(val) => val.fmt(f),
            CmdOutput::Execute(end) => {
                if *end {
                    write!(f, "turtle finished execution")?;
                }
                Ok(())
            }
        }
    }
}
