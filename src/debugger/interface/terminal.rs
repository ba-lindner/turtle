use crate::{
    debugger::{window::Window, DbgEvent, DebugRun, ProgEnd},
    tokens::StmtKind,
};

use super::{
    commands::{DbgCommand, NoCmdReason},
    DbgInterface,
};

pub struct Terminal;

impl Terminal {
    fn get_command(&self) -> Option<DbgCommand> {
        loop {
            let line = std::io::stdin().lines().next()?.ok()?;
            match super::commands::parse_command(&line) {
                Ok(cmd) => return Some(cmd),
                Err(NoCmdReason::Empty | NoCmdReason::Quit) => return None,
                Err(NoCmdReason::Help(help)) => println!("{help}"),
                Err(NoCmdReason::Err(why)) => println!("{why}"),
            }
        }
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &self,
        run: &mut DebugRun<'p, W>,
        cmd: DbgCommand,
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
                    let func = ttl.start_task.0.disp(&run.prog.symbols);
                    print!(
                        "#{:<3} {} {func}",
                        ttl.id,
                        if ttl.is_active { "x" } else { " " }
                    );
                    for val in &ttl.start_task.1 {
                        print!(" {val}");
                    }
                    println!()
                }
            }
            DbgCommand::SelectTurtle(id) => match run.select_turtle(id) {
                Ok(()) => println!("turtle #{id} selected"),
                Err(why) => eprintln!("{why}"),
            },
            DbgCommand::ListBreakpoints => {
                for bp in run.list_breakpoints() {
                    println!(
                        "#{:<3} {} {}",
                        bp.id,
                        if bp.enabled { "enabled " } else { "disabled" },
                        bp.pos
                    );
                }
            }
            DbgCommand::AddBreakpoint(pos) => {
                run.add_breakpoint(pos);
            }
            DbgCommand::DeleteBreakpoint(id) => {
                run.delete_breakpoint(id);
            }
            DbgCommand::EnableBreakpoint(id, active) => {
                if let Err(why) = run.enable_breakpoint(id, active) {
                    eprintln!("{why}");
                }
            }
            DbgCommand::Stacktrace => {
                for frame in run.stacktrace() {
                    println!(
                        "{:<3} {} {}",
                        frame.index,
                        frame.func.disp(&run.prog.symbols),
                        frame.pos
                    );
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

impl DbgInterface for Terminal {
    fn exec<'p, W: Window + 'p>(&mut self, mut run: DebugRun<'p, W>) {
        println!("turtle debugger v{}", env!("CARGO_PKG_VERSION"));
        println!("enter 'help' to view available commands");
        while let Some(cmd) = self.get_command() {
            match self.exec_cmd(&mut run, cmd) {
                Ok(()) => {}
                Err(ProgEnd::AllTurtlesFinished) => {
                    run.finished();
                    return;
                }
                Err(ProgEnd::WindowExited) => return,
            }
        }
    }
}
