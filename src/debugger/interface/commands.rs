//! Common debugger commands
//! 
//! This module provides an enum for common debugger commands, [`DbgCommand`],
//! aswell as a function for parsing it, [`parse_command`].

use std::num::ParseIntError;

use crate::pos::{FilePos, FilePosParseErr};

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
    /// Run [`DebugController::step_single()`](crate::debugger::Debugger::step_single()) n times.
    StepSingle(usize),
    /// Run [`DebugController::step_over()`](crate::debugger::Debugger::step_over()) n times.
    StepOver(usize),
    /// Run [`DebugController::step_out()`](crate::debugger::Debugger::step_out()).
    StepOut,
    /// Run [`DebugController::step_kind()`](crate::debugger::Debugger::step_kind())
    /// with [`StmtKind::Turtle`](crate::tokens::StmtKind::Turtle).
    StepTurtle,
    /// Run [`DebugController::step_kind()`](crate::debugger::Debugger::step_kind())
    /// with [`StmtKind::Draw`](crate::tokens::StmtKind::Draw).
    StepDraw,
    /// Run [`DebugController::step_sync()`](crate::debugger::Debugger::step_sync()).
    StepSync,
    /// Run [`DebugController::run_breakpoints()`](crate::debugger::Debugger::run_breakpoints()).
    Run,
    /// Call [`DebugController::toggle_narrate()`](crate::debugger::Debugger::toggle_narrate()).
    ToggleNarrate,
    /// Print [`DebugController::vardump()`](crate::debugger::Debugger::vardump())
    /// with [`None`] (active frame).
    Vardump,
    /// Print [`DebugController::curr_pos()`](crate::debugger::Debugger::curr_pos()).
    CurrPos,
    /// Print [`DebugController::list_turtles()`](crate::debugger::Debugger::list_turtles()).
    ListTurtles,
    /// Call [`DebugController::select_turtle()`](crate::debugger::Debugger::select_turtle()).
    SelectTurtle(usize),
    /// Print [`DebugController::list_turtles()`](crate::debugger::Debugger::list_turtles()).
    ListBreakpoints,
    /// Call [`DebugController::add_breakpoint()`](crate::debugger::Debugger::add_breakpoint()).
    AddBreakpoint(FilePos),
    /// Call [`DebugController::delete_breakpoint()`](crate::debugger::Debugger::delete_breakpoint()).
    DeleteBreakpoint(usize),
    /// Call [`DebugController::enable_breakpoint()`](crate::debugger::Debugger::enable_breakpoint()).
    EnableBreakpoint(usize, bool),
    /// Print [`DebugController::stacktrace()`](crate::debugger::Debugger::stacktrace()).
    Stacktrace,
    /// Print [`DebugController::eval_expr()`](crate::debugger::Debugger::eval_expr()).
    Evaluate(String),
    /// Run [`DebugController::exec_stmt()`](crate::debugger::Debugger::exec_stmt()).
    Execute(String),
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
    /// Error parsing comman
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
        "evaluate" => DbgCommand::Evaluate(words.fold(String::new(), |mut acc, e| {
            acc += e;
            acc += " ";
            acc
        })),
        "execute" => DbgCommand::Execute(words.fold(String::new(), |mut acc, e| {
            acc += e;
            acc += " ";
            acc
        })),
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
