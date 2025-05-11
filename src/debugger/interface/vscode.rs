use crate::{
    debugger::{turtle::FuncType, window::Window, DbgEvent, Debugger, ProgEnd},
    pos::FilePos,
    tokens::{EventKind, PredefVar},
};

use super::CommonInterface;

pub enum VSCodeCmd {
    StepOver,
    StepIn,
    StepOut,
    Run,
    Break(usize),
    RBreak(Option<usize>),
    Stacktrace,
    Variables(usize),
    Evaluate(usize, String),
}

pub struct VSCode;

impl CommonInterface for VSCode {
    type Command = VSCodeCmd;

    fn get_command(&mut self) -> Option<Self::Command> {
        for line in std::io::stdin().lines() {
            let line = line.ok()?;
            match line.as_str() {
                "step" | "step over" => return Some(VSCodeCmd::StepOver),
                "step in" => return Some(VSCodeCmd::StepIn),
                "step out" => return Some(VSCodeCmd::StepOut),
                "run" => return Some(VSCodeCmd::Run),
                "rbreak" => return Some(VSCodeCmd::RBreak(None)),
                "stacktrace" => return Some(VSCodeCmd::Stacktrace),
                "exit" => return None,
                _ => {}
            }
            if let Some(bp) = line.strip_prefix("break ") {
                return Some(VSCodeCmd::Break(bp.parse().ok()?));
            }
            if let Some(bp) = line.strip_prefix("rbreak ") {
                return Some(VSCodeCmd::RBreak(Some(bp.parse().ok()?)));
            }
            if let Some(frame) = line.strip_prefix("variables ") {
                return Some(VSCodeCmd::Variables(frame.parse().ok()?));
            }
            if let Some(args) = line.strip_prefix("evaluate ") {
                let (frame, expr) = args.split_once(" ")?;
                return Some(VSCodeCmd::Evaluate(frame.parse().ok()?, expr.to_string()));
            }
        }
        None
    }

    fn exec_cmd<'p, W: Window + 'p>(
        &mut self,
        run: &mut Debugger<'p, W>,
        cmd: Self::Command,
    ) -> Result<(), ProgEnd> {
        let moves = matches!(
            cmd,
            VSCodeCmd::StepOver | VSCodeCmd::StepIn | VSCodeCmd::StepOut | VSCodeCmd::Run
        );
        match cmd {
            VSCodeCmd::StepOver => run.step_over()?,
            VSCodeCmd::StepIn => _ = run.step_single()?,
            VSCodeCmd::StepOut => run.step_out()?,
            VSCodeCmd::Run => run.run_breakpoints()?,
            VSCodeCmd::Break(line) => _ = run.add_breakpoint(FilePos::new(line, 1)),
            VSCodeCmd::RBreak(line) => {
                for bp in run.list_breakpoints() {
                    if line.is_none() || line.is_some_and(|l| bp.pos.line == l) {
                        run.delete_breakpoint(bp.id);
                    }
                }
            }
            VSCodeCmd::Stacktrace => {
                for frame in run.stacktrace() {
                    let short = match frame.func {
                        FuncType::Main => "main",
                        FuncType::Calc(id) | FuncType::Path(id) => {
                            run.prog.symbols.get_index(id).unwrap().0
                        }
                        FuncType::Event(EventKind::Key) => "key",
                        FuncType::Event(EventKind::Mouse) => "mouse",
                    };
                    println!(
                        "{} {short} {}:{}",
                        frame.index, frame.pos.line, frame.pos.column
                    );
                }
            }
            VSCodeCmd::Variables(frame) => {
                if let Ok(vars) = run.vardump(Some(frame)) {
                    for (name, val) in vars.locals {
                        println!("{name} {val}");
                    }
                };
            }
            VSCodeCmd::Evaluate(frame, var) => {
                if let Ok(vars) = run.vardump(Some(frame)) {
                    if let Some(global) = var.strip_prefix('@') {
                        if let Some(val) = vars.globals.get(global) {
                            println!("{val}");
                        } else if let Ok(pdv) = global.parse::<PredefVar>() {
                            println!("{}", vars.predef[&pdv]);
                        }
                    } else if let Some(val) = vars.locals.get(&var) {
                        println!("{val}");
                    }
                };
            }
        }
        if moves {
            let curr_pos = run.curr_pos().1;
            if let Some(id) = run.events().1.into_iter().find_map(|e| match e {
                DbgEvent::BreakpointHit(bp) => Some(bp),
                _ => None,
            }) {
                let bp = run
                    .list_breakpoints()
                    .into_iter()
                    .find(|bp| bp.id == id)
                    .unwrap();
                println!(
                    "currently at {}:{} [breakpoint: {}]",
                    curr_pos.line, curr_pos.column, bp.pos.line
                );
            } else {
                println!("currently at {}:{}", curr_pos.line, curr_pos.column);
            }
        }
        Ok(())
    }
}
