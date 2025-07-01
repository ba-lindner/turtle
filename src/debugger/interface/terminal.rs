use std::io::Write;

use crate::{
    Disp,
    debugger::{Debugger, ProgEnd, window::Window},
};

use super::{
    CommonInterface,
    commands::{DbgCommand, NoCmdReason},
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
        let res = cmd.exec(run)?;
        if let Some(out) = res.output {
            println!("{}", out.with_symbols(&run.prog.symbols));
        }
        if let Some(why) = res.error {
            eprintln!("{why}");
        }
        if res.stmt_count > 0 {
            println!("executed {} statements", res.stmt_count);
        }
        for evt in res.run_events {
            println!("{evt}");
        }
        Ok(())
    }
}
