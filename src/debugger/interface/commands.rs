use std::num::ParseIntError;

use crate::pos::{FilePos, FilePosParseErr};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DbgCommand {
    StepSingle(usize),
    StepOver(usize),
    StepOut,
    StepTurtle,
    StepDraw,
    StepSync,
    Run,
    ToggleNarrate,
    Vardump,
    CurrPos,
    ListTurtles,
    SelectTurtle(usize),
    ListBreakpoints,
    AddBreakpoint(FilePos),
    DeleteBreakpoint(usize),
    EnableBreakpoint(usize, bool),
    Stacktrace,
    Evaluate(String),
    Execute(String),
}

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

const DEBUG_HELP_BP: &str = "subcommands for breakpoint:
  breakpoint              - list breakpoints
  breakpoint add <pos>    - add breakpoint at <pos>
  breakpoint remove <id>  - remove breakpoint <id>
  breakpoint enable <id>  - enable breakpoint <id>
  breakpoint disable <id> - disable breakpoint <id>";

pub fn extend_str<'i>(inp: &'i str, words: &'i [&'_ str]) -> &'i str {
    for word in words {
        if word.starts_with(inp) {
            return word;
        }
    }
    inp
}

pub enum NoCmdReason<'l> {
    Quit,
    Help(&'static str),
    Empty,
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
