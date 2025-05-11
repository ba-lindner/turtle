use std::sync::mpsc::{self, Receiver, Sender};

use crate::{
    debugger::{window::Window, DbgEvent, Debugger, ProgEnd},
    tokens::StmtKind,
};

use super::{
    commands::{self, DbgCommand, NoCmdReason},
    CommonInterface,
};

/// The "output" type of the [`Strings`] debug interface.
type StringRes = Result<String, String>;

/// Generic [`DbgInterface`](super::DbgInterface) with default commands.
/// 
/// This type offers the same user interface as the default
/// [`Terminal`](super::Terminal) one, but uses an [`Iterator`] for input
/// and a [`mpsc::channel`] for output instead of stdin/stdout.
/// All parsing from strings and formatting into strings is also
/// handled by this interface.
/// 
/// Since [`Receiver`] also implements [`IntoIterator`], this
/// interface can also be used with a channel for both input
/// and output.
/// 
/// [`DbgInterface`](super::DbgInterface) is implemented via [`CommonInterface`].
pub struct Strings<I> {
    /// Iterator to draw user input from
    inputs: I,
    /// Sender to dump output into
    outputs: Sender<StringRes>,
}

impl Strings<std::iter::Empty<String>> {
    /// Construct a new [`Strings`] instance together with the output channel.
    pub fn construct<I: IntoIterator<Item = String>>(
        inp: I,
    ) -> (Strings<I::IntoIter>, Receiver<StringRes>) {
        let (tx, rx) = mpsc::channel();
        (Strings::new(inp.into_iter(), tx), rx)
    }
}

impl<I: Iterator<Item = String>> Strings<I> {
    /// Construct a new [`Strings`] instance from an already existing channel for output.
    pub fn new(inputs: I, outputs: Sender<StringRes>) -> Self {
        Self { inputs, outputs }
    }
}

impl<I: Iterator<Item = String>> CommonInterface for Strings<I> {
    type Command = DbgCommand;

    fn get_command(&mut self) -> Option<Self::Command> {
        loop {
            match commands::parse_command(&self.inputs.next()?) {
                Ok(cmd) => return Some(cmd),
                Err(NoCmdReason::Quit) => return None,
                Err(NoCmdReason::Empty) => {}
                Err(NoCmdReason::Help(help)) => _ = self.outputs.send(Ok(help.to_string())),
                Err(NoCmdReason::Err(why)) => _ = self.outputs.send(Err(why.to_string())),
            }
        }
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &mut self,
        run: &mut Debugger<'p, W>,
        cmd: Self::Command,
    ) -> Result<(), ProgEnd> {
        macro_rules! ok {
            ($fmt:literal $($t:tt)*) => { ok!(format!($fmt $($t)*)) };
            ($e:expr) => { _ = self.outputs.send(Ok($e)) };
        }
        macro_rules! err {
            ($e:expr) => { err!($e, _ => {}) };
            ($e:expr, $p:pat => $($t:tt)*) => {
                match $e {
                    Ok($p) => {$($t)*}
                    Err(why) => _ = self.outputs.send(Err(why.to_string())),
                }
            };
        }
        match cmd {
            DbgCommand::StepSingle(count) => {
                for _ in 0..count {
                    run.step_single()?;
                }
            }
            DbgCommand::StepOver(count) => {
                for _ in 0..count {
                    run.step_over()?;
                }
            }
            DbgCommand::StepOut => run.step_out()?,
            DbgCommand::StepTurtle => run.step_kind(StmtKind::Turtle)?,
            DbgCommand::StepDraw => run.step_kind(StmtKind::Draw)?,
            DbgCommand::StepSync => run.step_sync()?,
            DbgCommand::Run => run.run_breakpoints()?,
            DbgCommand::ToggleNarrate => {
                let n = if run.toggle_narrate() { "ON" } else { "OFF" };
                ok!("narrator is {n}");
            }
            DbgCommand::Vardump => err!(run.vardump(None), vars => {
                for (name, value) in vars.locals {
                    ok!("{name:<20} {value}");
                }
                for (name, value) in vars.globals {
                    ok!("@{name:<19} {value}");
                }
                for (pdv, value) in vars.predef {
                    ok!("@{:<19} {value}", pdv.get_str());
                }
            }),
            DbgCommand::CurrPos => {
                let id = run.active_id();
                let (func, pos) = run.curr_pos();
                let disp = func.disp(&run.prog.symbols);
                ok!("turtle #{id} is currently in {disp} at {pos}");
            }
            DbgCommand::ListTurtles => {
                let (sync, turtles) = run.list_turtles();
                ok!("turtles are {}in sync", if sync { "" } else { "NOT " });
                for ttl in turtles {
                    ok!(ttl.disp(&run.prog.symbols));
                }
            }
            DbgCommand::SelectTurtle(id) => err!(
                run.select_turtle(id),
                () => ok!("turtle #{id} selected");
            ),
            DbgCommand::ListBreakpoints => {
                for bp in run.list_breakpoints() {
                    ok!(bp.to_string());
                }
            }
            DbgCommand::AddBreakpoint(pos) => _ = run.add_breakpoint(pos),
            DbgCommand::DeleteBreakpoint(id) => run.delete_breakpoint(id),
            DbgCommand::EnableBreakpoint(id, active) => err!(run.enable_breakpoint(id, active)),
            DbgCommand::Stacktrace => {
                for frame in run.stacktrace() {
                    ok!(frame.disp(&run.prog.symbols));
                }
            }
            DbgCommand::Evaluate(expr) => err!(
                run.eval_expr(None, &expr),
                val => ok!(val.to_string());
            ),
            DbgCommand::Execute(stmt) => err!(run.exec_stmt(&stmt)),
        }
        let (stmt_count, events) = run.events();
        if stmt_count > 0 {
            ok!("executed {} statements", stmt_count);
        }
        for evt in events {
            match evt {
                DbgEvent::TurtleFinished(id) => ok!("turtle #{id} finished"),
                DbgEvent::BreakpointHit(id) => ok!("breakpoint #{id} hit"),
            };
        }
        Ok(())
    }
}
