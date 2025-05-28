use std::io::Write as _;

use crate::{
    debugger::{window::Window, Debugger, FuncType, ProgEnd},
    prog::parser::{ParseError, Parser, TokenExpectation},
    tokens::{EventKind, PredefVar},
    TurtleError,
};

use super::DbgInterface;

/// A turtle shell.
///
/// This is a [`DbgInterface`] that by default executes
/// a statement or defines a function, requiring all
/// commands to be prefixed with `'/'`.
/// Additionally, code can span multiple lines.
///
/// In contrast to other interfaces, far less commands
/// are supported, as a shell is somewhat different from
/// a debugger. The available commands are represented
/// by [`ShellCmd`].
///
/// No multithreading or eventing is supported by the
/// shell as of now. While events can be defined and
/// the `split` statement can be executed, the turtles
/// spawned this way will never do anything (besides
/// taking up memory).
pub struct Shell;

/// Commands available in the turtle shell.
enum ShellCmd {
    /// Execute some statement.
    Exec(String),
    /// Define a function.
    ///
    /// This can be a path, a calculation or
    /// either event.
    Func(String),
    /// Undefine a function.
    ///
    /// If no function is specified,
    /// all functions are undefined.
    Undef(Option<FuncType>),
    /// Print the current position.
    ///
    /// Unlike [`DbgCommand::CurrPos`](super::commands::DbgCommand::CurrPos),
    /// this prints the current turtle position.
    Pos,
}

/// The help for shell commands
const SHELL_HELP: &str = "available commands:
  <code>          - execute statement or define function 
                    can span multiple lines
  !<stmt>         - execute statement
                    mainly used to disambiguate
                    a path definition from a path call
  /pos            - current position
  /undef [<func>] - undefine function. If no <func> is given,
                    all functions are removed.
                    possible variants for <func>:
                    * path <name>
                    * calc <name>
                    * event key
                    * event mouse
  /help           - this help
  /quit           - exit shell";

/// Possible results for trying to parse potentially unfinished user input.
///
/// Especially the [`Finished`](TryParseResult::Finished) and [`Unfinished`](TryParseResult::Unfinished)
/// variants are important to note. As shell inputs can span multiple lines,
/// the shell needs to keep track of when to append user input to the
/// buffer and when to clear the buffer.
enum TryParseResult {
    /// User input was successfully parsed into a command.
    ///
    /// This obviously clears the buffer.
    Cmd(ShellCmd),
    /// User exited the shell.
    ///
    /// As the buffer is soon to be deallocated,
    /// no further action is neccessary.
    Quit,
    /// Finished handling current input.
    ///
    /// This is the indication to clear the buffer.
    ///
    /// Possible causes:
    /// * The help was printed
    /// * An error occurred (and was already reported)
    Finished,
    /// User input is incomplete.
    ///
    /// This is the indication to append the next line
    /// to the buffer.
    ///
    /// It is returned if the parser returns
    /// `Err(ParseError::UnexpectedEnd)`.
    Unfinished,
}

impl Shell {
    /// Attempt to parse current user input.
    ///
    /// There are multiple things to consider:
    /// * If input starts with `'/'`, the remainder is parsed as a command.
    ///   This will never return [`TryParseResult::Unfinished`].
    /// * If input starts with `'!'`, the remainder is parsed as a statement.
    ///   This might return [`TryParseResult::Unfinished`].
    /// * Otherwise, the input is first parsed as a function. If the parser
    ///   reports the first token to be unbefitting of starting a function,
    ///   the input is then parsed as a statement. Other errors (with the
    ///   exception of [`ParseError::UnexpectedEnd`]) are reported to the
    ///   user, returning [`TryParseResult::Finished`].
    ///
    /// Return values are explained in [`TryParseResult`].
    fn try_parse<'p, W: Window + 'p>(&self, run: &Debugger<'p, W>, inp: &str) -> TryParseResult {
        if inp.is_empty() {
            return TryParseResult::Finished;
        }
        if let Some(cmd) = inp.strip_prefix('/') {
            let mut words = cmd.split_whitespace();
            match_extended!(words.next() => {
                None => eprintln!("empty command"),
                "help" => println!("{SHELL_HELP}"),
                "?" => println!("{SHELL_HELP}"),
                "pos" => return TryParseResult::Cmd(ShellCmd::Pos),
                "undef" => {
                    let sym = |name: &str| run.prog.extensions.symbols.borrow().get_index_of(name);
                    macro_rules! err {
                        ($($t:tt)*) => {{
                            eprintln!($($t)*);
                            return TryParseResult::Finished;
                        }};
                    }
                    let ft = match_extended!(words.next() => {
                        None => None,
                        "path" => {
                            let Some(name) = words.next() else { err!("missing path name"); };
                            let Some(id) = sym(name) else { err!("no path named {name}"); };
                            Some(FuncType::Path(id))
                        },
                        "calculation" => {
                            let Some(name) = words.next() else { err!("missing calculation name"); };
                            let Some(id) = sym(name) else { err!("no calculation named {name}"); };
                            Some(FuncType::Calc(id))
                        },
                        "event" => match_extended!(words.next() => {
                            None => err!("missing event kind"),
                            "key" => Some(FuncType::Event(EventKind::Key)),
                            "mouse" => Some(FuncType::Event(EventKind::Mouse)),
                            other => err!("unknown event type {other}"),
                        }),
                        other => err!("unknown function type {other}"),
                    });
                    return TryParseResult::Cmd(ShellCmd::Undef(ft));
                },
                "quit" => return TryParseResult::Quit,
                other => eprintln!("unknown command /{other}"),
            });
            return TryParseResult::Finished;
        }
        enum Found {
            Stmt,
            Func,
            None,
        }
        let code = inp.strip_prefix("!").unwrap_or(inp);
        let no_func = code != inp;
        let res = run.prog.with_parser(code, |p| {
            let p_stmt = |p: &mut Parser<'_, '_>| match p.parse_stm() {
                Err(why) => match *why {
                    ParseError::UnexpectedEnd => Ok(Found::None),
                    _ => Err(TurtleError::ParseError(why)),
                },
                Ok(_) => Ok(Found::Stmt),
            };
            if no_func {
                p_stmt(p)
            } else {
                match p.parse_next() {
                    Some(Err(why)) => match *why {
                        ParseError::UnexpectedToken(_, TokenExpectation::BlockStart) => {
                            p.reset();
                            p_stmt(p)
                        }
                        ParseError::UnexpectedEnd => Ok(Found::None),
                        _ => Err(TurtleError::ParseError(why)),
                    },
                    Some(Ok(_)) => Ok(Found::Func),
                    None => Ok(Found::None),
                }
            }
        });
        match res {
            Ok(Found::Stmt) => TryParseResult::Cmd(ShellCmd::Exec(code.to_string())),
            Ok(Found::Func) => TryParseResult::Cmd(ShellCmd::Func(code.to_string())),
            Ok(Found::None) => TryParseResult::Unfinished,
            Err(why) => {
                eprintln!("{why}");
                TryParseResult::Finished
            }
        }
    }

    /// Obtain the next command.
    ///
    /// Like [`CommonInterface::get_command`](super::CommonInterface::get_command),
    /// this returns [`None`] if the user exited. However, as parsing
    /// requires a reference to the [`Debugger`],
    /// [`CommonInterface`](super::CommonInterface) cannot be used.
    fn get_command<'p, W: Window + 'p>(&self, run: &Debugger<'p, W>) -> Option<ShellCmd> {
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
                TryParseResult::Finished => cmd.clear(),
                TryParseResult::Unfinished => cmd.push(' '),
            }
        }
    }

    /// Execute the command obtained by [`get_command`](Shell::get_command).
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
            ShellCmd::Func(func) => {
                if let Err(why) = run.add_func(&func) {
                    eprintln!("{why}");
                }
            }
            ShellCmd::Undef(func) => {
                if let Err(why) = run.undef_func(func) {
                    eprintln!("{why}");
                }
            }
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
