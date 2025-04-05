use std::{fmt::Display, str::FromStr};

use crate::pos::FilePos;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DbgCommand {
    StepSingle(usize),
    StepOver(usize),
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
  help [step|breakpoint]  - show this help / detailed help for subcommand
  quit                    - exit the debugger";

const DEBUG_HELP_STEP: &str = "subcommands for step:
  step [<count>]      - execute <count> statements
                        <count> defaults to one
  step over [<count>] - step over path / calc call
                        if no such call occurs, identical to step [<count>]
  step turtle         - skip until next turtle action
  step draw           - skip until next drawn line
  step sync           - skip until turtles are synced
                        this is almost identical to step draw,
                        but allows you to select the active turtle";

const DEBUG_HELP_BP: &str = "subcommands for breakpoint:
  breakpoint              - list breakpoints
  breakpoint add <pos>    - add breakpoint at <pos>
  breakpoint remove <id>  - remove breakpoint <id>
  breakpoint enable <id>  - enable breakpoint <id>
  breakpoint disable <id> - disable breakpoint <id>";

fn extend_str<'i>(inp: &'i str, words: &'i [&'_ str]) -> &'i str {
    for word in words {
        if word.starts_with(inp) {
            return word;
        }
    }
    inp
}

pub fn get_command() -> Option<DbgCommand> {
    loop {
        let line = std::io::stdin().lines().next()?.ok()?;
        if line.is_empty() {
            return None;
        }
        let mut words = line.split_whitespace();
        let cmd = extend_str(
            words.next().unwrap(),
            &[
                "step",
                "run",
                "narrator",
                "variables",
                "position",
                "breakpoint",
                "turtle",
                "help",
                "quit",
            ],
        );
        match cmd {
            "step" => {
                if let Some(cmd) = step_command(words) {
                    return Some(cmd);
                }
            }
            "run" => return Some(DbgCommand::Run),
            "narrator" => return Some(DbgCommand::ToggleNarrate),
            "variables" => return Some(DbgCommand::Vardump),
            "position" => return Some(DbgCommand::CurrPos),
            "breakpoint" => {
                if let Some(cmd) = breakpoint_command(words) {
                    return Some(cmd);
                }
            }
            "turtle" => {
                match words.next().map(str::parse) {
                    Some(Ok(count)) => return Some(DbgCommand::SelectTurtle(count)),
                    Some(Err(why)) => eprintln!("not a number: {why}"),
                    None => return Some(DbgCommand::ListTurtles),
                }
            }
            "help" => {
                if let Some(sub) = words.next() {
                    match extend_str(sub, &["step", "breakpoint"]) {
                        "step" => println!("{DEBUG_HELP_STEP}"),
                        "breakpoint" => println!("{DEBUG_HELP_BP}"),
                        inp => eprintln!("no help for command {inp}"),
                    }
                } else {
                    println!("{DEBUG_HELP}");
                }
            }
            "quit" => return None,
            _ => eprintln!("unknown command {cmd}"),
        }
    }
}

fn step_command<'w>(mut words: impl Iterator<Item = &'w str>) -> Option<DbgCommand> {
    let Some(inp) = words.next() else {
        return Some(DbgCommand::StepSingle(1));
    };
    let sub = extend_str(inp, &["over", "turtle", "draw", "sync"]);
    match sub {
        "over" => match words.next().map(str::parse) {
            Some(Ok(count)) => return Some(DbgCommand::StepOver(count)),
            Some(Err(why)) => eprintln!("not a number: {why}"),
            None => return Some(DbgCommand::StepOver(1)),
        },
        "turtle" => return Some(DbgCommand::StepTurtle),
        "draw" => return Some(DbgCommand::StepDraw),
        "sync" => return Some(DbgCommand::StepSync),
        count => match count.parse() {
            Ok(count) => return Some(DbgCommand::StepSingle(count)),
            Err(why) => eprintln!("not a number: {why}"),
        },
    }
    None
}

fn breakpoint_command<'w>(mut words: impl Iterator<Item = &'w str>) -> Option<DbgCommand> {
    let Some(inp) = words.next() else {
        return Some(DbgCommand::ListBreakpoints);
    };
    let sub = extend_str(inp, &["add", "remove", "enable", "disable"]);
    fn get_arg<T>(arg: Option<&str>, sc: &str, kind: &str) -> Option<T> 
    where T: FromStr,
        <T as FromStr>::Err: Display,
    {
        if let Some(arg) = arg {
            arg.parse().inspect_err(|why| eprintln!("not a {kind}: {why}")).ok()
        } else {
            eprintln!("missing argument for subcommand {sc}");
            None
        }
    }
    match sub {
        "add" => Some(DbgCommand::AddBreakpoint(get_arg(words.next(), "add", "position")?)),
        "remove" => Some(DbgCommand::DeleteBreakpoint(get_arg(words.next(), "remove", "number")?)),
        "enable" => Some(DbgCommand::EnableBreakpoint(get_arg(words.next(), "enable", "number")?, true)),
        "disable" => Some(DbgCommand::EnableBreakpoint(get_arg(words.next(), "disable", "number")?, false)),
        _ => {
            eprintln!("unknown subcommand {sub}");
            None
        }
    }
}
