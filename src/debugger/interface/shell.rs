use std::io::Write as _;

use crate::{
    debugger::{window::Window, Debugger, ProgEnd},
    prog::parser::ParseError,
    tokens::PredefVar,
    TurtleError,
};

use super::DbgInterface;

pub struct Shell;

enum ShellCmd {
    Exec(String),
    Pos,
}

const SHELL_HELP: &str = "available commands:
  /pos    - current position
  /help   - this help
  /quit   - exit shell";

enum TryParseResult {
    Cmd(ShellCmd),
    Quit,
    Other,
    Unfinished,
}

impl Shell {
    fn try_parse<'p, W: Window + 'p>(
        &self,
        run: &mut Debugger<'p, W>,
        inp: &str,
    ) -> TryParseResult {
        if inp.is_empty() {
            return TryParseResult::Other;
        }
        if let Some(cmd) = inp.strip_prefix('/') {
            return match super::commands::extend_str(cmd, &["help", "pos", "quit"]) {
                "help" | "?" => {
                    println!("{SHELL_HELP}");
                    TryParseResult::Other
                }
                "pos" => TryParseResult::Cmd(ShellCmd::Pos),
                "quit" => TryParseResult::Quit,
                other => {
                    eprintln!("unknown command /{other}");
                    TryParseResult::Other
                }
            };
        }
        let res = run.prog.with_parser(inp, |p| match p.parse_stm() {
            Err(why) => match *why {
                ParseError::UnexpectedEnd => Ok(false),
                _ => Err(TurtleError::ParseError(why)),
            },
            Ok(_) => Ok(true),
        });
        match res {
            Ok(true) => TryParseResult::Cmd(ShellCmd::Exec(inp.to_string())),
            Ok(false) => TryParseResult::Unfinished,
            Err(why) => {
                eprintln!("{why}");
                TryParseResult::Other
            }
        }
    }

    fn get_command<'p, W: Window + 'p>(&self, run: &mut Debugger<'p, W>) -> Option<ShellCmd> {
        let mut cmd = String::new();
        loop {
            if cmd.is_empty() {
                print!("> ");
            } else {
                print!("  ");
            }
            std::io::stdout().flush().unwrap();
            let line = std::io::stdin().lines().next()?.ok()?;
            cmd += &line;
            match self.try_parse(run, &cmd) {
                TryParseResult::Cmd(cmd) => return Some(cmd),
                TryParseResult::Quit => return None,
                TryParseResult::Other => cmd.clear(),
                TryParseResult::Unfinished => cmd.push(' '),
            }
        }
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &self,
        run: &mut Debugger<'p, W>,
        cmd: ShellCmd,
    ) -> Result<(), ProgEnd> {
        match cmd {
            ShellCmd::Exec(stmt) => match run.exec_stmt(&stmt) {
                Ok(true) => return Err(ProgEnd::AllTurtlesFinished),
                Err(why) => eprintln!("{why}"),
                _ => {}
            },
            ShellCmd::Pos => {
                let vars = run.vardump(None).unwrap().predef;
                let (x, y, dir) = (
                    vars[&PredefVar::X].num(),
                    vars[&PredefVar::Y].num(),
                    vars[&PredefVar::Dir].num(),
                );
                println!("turtle is at {x:.3}, {y:.3} and looking in direction {dir:.1}");
            }
        }
        Ok(())
    }
}

impl DbgInterface for Shell {
    fn exec<'p, W: Window + 'p>(&mut self, run: &mut Debugger<'p, W>) -> ProgEnd {
        println!("turtle shell v{}", env!("CARGO_PKG_VERSION"));
        println!("enter '/help' to view available commands");
        while let Some(cmd) = self.get_command(run) {
            if let Err(end) = self.exec_cmd(run, cmd) {
                return end;
            }
        }
        ProgEnd::WindowExited
    }
}
