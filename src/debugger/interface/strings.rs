use std::sync::mpsc::{self, Receiver, Sender};

use crate::{
    debugger::{window::Window, DbgEvent, Debugger, ProgEnd},
    tokens::StmtKind,
};

use super::{
    commands::{self, DbgCommand, NoCmdReason},
    CommonInterface,
};

type StringRes = Result<String, String>;

pub struct Strings<I> {
    inputs: I,
    outputs: Sender<StringRes>,
}

impl Strings<std::iter::Empty<String>> {
    pub fn construct<I: IntoIterator<Item = String>>(
        inp: I,
    ) -> (Strings<I::IntoIter>, Receiver<StringRes>) {
        let (tx, rx) = mpsc::channel();
        (Strings::new(inp.into_iter(), tx), rx)
    }
}

impl<I: Iterator<Item = String>> Strings<I> {
    pub fn new(inputs: I, outputs: Sender<StringRes>) -> Self {
        Self {
            inputs,
            outputs,
        }
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
                let narrator = if run.toggle_narrate() { "ON" } else { "OFF" };
                _ = self.outputs.send(Ok(format!("narrator is {narrator}")));
            }
            DbgCommand::Vardump => match run.vardump(None) {
                Ok(vars) => {
                    for (name, value) in vars.locals {
                        _ = self.outputs.send(Ok(format!("{name:<20} {value}")));
                    }
                    for (name, value) in vars.globals {
                        _ = self.outputs.send(Ok(format!("@{name:<19} {value}")));
                    }
                    for (pdv, value) in vars.predef {
                        _ = self.outputs.send(Ok(format!("@{:<19} {value}", pdv.get_str())));
                    }
                }
                Err(why) => _ = self.outputs.send(Err(why.to_string())),
            },
            DbgCommand::CurrPos => {
                let pos = run.curr_pos();
                let res = format!(
                    "turtle #{} is currently in {} at {}",
                    run.active_id(),
                    pos.0.disp(&run.prog.symbols),
                    pos.1
                );
                _ = self.outputs.send(Ok(res));
            }
            DbgCommand::ListTurtles => {
                let (sync, turtles) = run.list_turtles();
                let sync = format!("turtles are {}in sync", if sync { "" } else { "NOT " });
                _ = self.outputs.send(Ok(sync));
                for ttl in turtles {
                    _ = self.outputs.send(Ok(ttl.disp(&run.prog.symbols)));
                }
            }
            DbgCommand::SelectTurtle(id) => match run.select_turtle(id) {
                Ok(()) => _ = self.outputs.send(Ok(format!("turtle #{id} selected"))),
                Err(why) => _ = self.outputs.send(Err(why.to_string())),
            },
            DbgCommand::ListBreakpoints => {
                for bp in run.list_breakpoints() {
                    _ = self.outputs.send(Ok(bp.to_string()));
                }
            }
            DbgCommand::AddBreakpoint(pos) => _ = run.add_breakpoint(pos),
            DbgCommand::DeleteBreakpoint(id) => run.delete_breakpoint(id),
            DbgCommand::EnableBreakpoint(id, active) => {
                if let Err(why) = run.enable_breakpoint(id, active) {
                    _ = self.outputs.send(Err(why.to_string()));
                }
            }
            DbgCommand::Stacktrace => {
                for frame in run.stacktrace() {
                    _ = self.outputs.send(Ok(frame.disp(&run.prog.symbols)))
                }
            }
            DbgCommand::Evaluate(expr) => match run.eval_expr(None, &expr) {
                Ok(val) => _ = self.outputs.send(Ok(val.to_string())),
                Err(why) => _ = self.outputs.send(Err(why.to_string())),
            },
            DbgCommand::Execute(stmt) => {
                if let Err(why) = run.exec_stmt(&stmt) {
                    _ = self.outputs.send(Err(why.to_string()));
                }
            }
        }
        let (stmt_count, events) = run.events();
        if stmt_count > 0 {
            _ = self
                .outputs
                .send(Ok(format!("executed {} statements", stmt_count)));
        }
        for evt in events {
            let msg = match evt {
                DbgEvent::TurtleFinished(id) => format!("turtle #{id} finished"),
                DbgEvent::BreakpointHit(id) => format!("breakpoint #{id} hit"),
            };
            _ = self.outputs.send(Ok(msg));
        }
        Ok(())
    }
}
