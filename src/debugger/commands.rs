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
}

const DEBUG_HELP: &str = "available commands:
  run                 - run / stop debugger
  step [count]        - execute single statement
  step over [count]   - step over path / calc call
                        if no such call occurs, identical to step [count]
  step turtle         - skip until next turtle action
  step draw           - skip until next drawn line
  step sync           - skip until turtles are synced
                        this is almost identical to step draw,
                        but might eventually let you switch turtles
  narrator            - toggle narrator
  variables           - dump variables
  position            - print current position
  help                - show this help";

pub fn get_command() -> Option<DbgCommand> {
    loop {
        let line = std::io::stdin().lines().next()?.ok()?;
        if line.is_empty() {
            return None;
        }
        let mut words = line.split_whitespace();
        let cmd = words.next().unwrap();
        if "step".starts_with(cmd) {
            match words.next() {
                Some(over) if "over".starts_with(over) => match words.next().map(str::parse) {
                    Some(Ok(count)) => return Some(DbgCommand::StepOver(count)),
                    Some(Err(why)) => eprintln!("not a number: {why}"),
                    None => return Some(DbgCommand::StepOver(1)),
                },
                Some(turtle) if "turtle".starts_with(turtle) => {
                    return Some(DbgCommand::StepTurtle)
                }
                Some(draw) if "draw".starts_with(draw) => return Some(DbgCommand::StepDraw),
                Some(sync) if "sync".starts_with(sync) => return Some(DbgCommand::StepSync),
                Some(count) => match count.parse() {
                    Ok(count) => return Some(DbgCommand::StepSingle(count)),
                    Err(why) => eprintln!("not a number: {why}"),
                },
                None => return Some(DbgCommand::StepSingle(1)),
            }
        } else if "run".starts_with(cmd) {
            return Some(DbgCommand::Run);
        } else if "narrator".starts_with(cmd) {
            return Some(DbgCommand::ToggleNarrate);
        } else if "variables".starts_with(cmd) {
            return Some(DbgCommand::Vardump);
        } else if "position".starts_with(cmd) {
            return Some(DbgCommand::CurrPos);
        } else if "help".starts_with(cmd) {
            println!("{DEBUG_HELP}");
        } else if "quit".starts_with(cmd) {
            return None;
        } else {
            eprintln!("unknown command {cmd}");
        }
    }
}
