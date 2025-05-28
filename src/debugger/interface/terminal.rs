use std::io::Write;

use crate::{
    debugger::{window::Window, DbgEvent, Debugger, ProgEnd},
    tokens::StmtKind,
};

use super::{
    commands::{DbgCommand, NoCmdReason},
    CommonInterface,
};

/// The default [`DbgInterface`](super::DbgInterface).
///
/// This uses the common command format defined in [`super::commands`].
/// Input is read from stdin and output printed to stdout / stderr.
///
/// [`DbgInterface`](super::DbgInterface) is implemented via [`CommonInterface`].
pub struct Terminal;

impl CommonInterface for Terminal {
    type Command = DbgCommand;

    fn greeting(&self) -> Option<&str> {
        Some(concat!(
            "turtle debugger v",
            env!("CARGO_PKG_VERSION"),
            "\nenter 'help' to view available commands"
        ))
    }

    fn get_command(&mut self) -> Option<Self::Command> {
        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();
            let line = std::io::stdin().lines().next()?.ok()?;
            match super::commands::parse_command(&line) {
                Ok(cmd) => return Some(cmd),
                Err(NoCmdReason::Quit) => return None,
                Err(NoCmdReason::Empty) => {}
                Err(NoCmdReason::Help(help)) => println!("{help}"),
                Err(NoCmdReason::Err(why)) => println!("{why}"),
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
                let narrator = run.toggle_narrate();
                println!("narrator is {}", if narrator { "ON" } else { "OFF" });
            }
            DbgCommand::Vardump => match run.vardump(None) {
                Ok(vars) => {
                    if !vars.locals.is_empty() {
                        for (name, value) in vars.locals {
                            println!("{name:<20} {value}");
                        }
                        println!()
                    }
                    if !vars.globals.is_empty() {
                        for (name, value) in vars.globals {
                            println!("@{name:<19} {value}");
                        }
                        println!()
                    }
                    for (pdv, value) in vars.predef {
                        println!("@{:<19} {value}", pdv.get_str());
                    }
                }
                Err(why) => eprintln!("{why}"),
            },
            DbgCommand::CurrPos => {
                let pos = run.curr_pos();
                println!(
                    "turtle #{} is currently in {} at {}",
                    run.active_id(),
                    pos.0.disp(&run.prog.symbols),
                    pos.1
                );
            }
            DbgCommand::ListTurtles => {
                let (sync, turtles) = run.list_turtles();
                println!("turtles are {}in sync", if sync { "" } else { "NOT " });
                for ttl in turtles {
                    println!("{}", ttl.disp(&run.prog.symbols));
                }
            }
            DbgCommand::SelectTurtle(id) => match run.select_turtle(id) {
                Ok(()) => println!("turtle #{id} selected"),
                Err(why) => eprintln!("{why}"),
            },
            DbgCommand::ListBreakpoints => {
                for bp in run.list_breakpoints() {
                    println!("{bp}");
                }
            }
            DbgCommand::AddBreakpoint(pos) => _ = run.add_breakpoint(pos),
            DbgCommand::DeleteBreakpoint(id) => run.delete_breakpoint(id),
            DbgCommand::EnableBreakpoint(id, active) => {
                if let Err(why) = run.enable_breakpoint(id, active) {
                    eprintln!("{why}");
                }
            }
            DbgCommand::Stacktrace => {
                for frame in run.stacktrace() {
                    println!("{}", frame.disp(&run.prog.symbols));
                }
            }
            DbgCommand::Evaluate(expr) => match run.eval_expr(None, &expr) {
                Ok(val) => println!("{val}"),
                Err(why) => eprintln!("{why}"),
            },
            DbgCommand::Execute(stmt) => {
                if let Err(why) = run.exec_stmt(&stmt) {
                    eprintln!("{why}");
                }
            }
        }
        let (stmt_count, events) = run.events();
        if stmt_count > 0 {
            println!("executed {} statements", stmt_count);
        }
        for evt in events {
            match evt {
                DbgEvent::TurtleFinished(id) => println!("turtle #{id} finished"),
                DbgEvent::BreakpointHit(id) => println!("breakpoint #{id} hit"),
            }
        }
        Ok(())
    }
}
