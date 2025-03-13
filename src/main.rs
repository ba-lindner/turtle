use clap::{Parser, Subcommand};
use std::process::Command;
use turtle::TProgram;

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: TCommand,
}

#[derive(Subcommand)]
enum TCommand {
    /// Start interpreter
    Run {
        /// the turtle source file to run
        file: String,
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Start debugger
    Debug {
        /// the turtle source file to debug
        file: String,
        /// set breakpoints in format line,column
        #[arg(short, long)]
        breakpoint: Vec<String>,
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Compile to C
    Compile {
        /// the turtle source file to compile
        file: String,
        /// enable debug builds
        #[arg(short, long)]
        debug: bool,
        /// rebuild turtle interface
        #[arg(short, long)]
        no_cache: bool,
    },
    /// Check syntax
    Check {
        /// the turtle source file to check
        file: String,
        #[arg(short = 's', long)]
        print_symbols: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        TCommand::Run { file, args } => get_prog(&file).interpret(&args),
        TCommand::Debug {
            file,
            breakpoint: bp,
            args,
        } => get_prog(&file).debug(&args, &bp),
        TCommand::Compile {
            file,
            debug,
            no_cache,
        } => compile(get_prog(&file), &file, no_cache, debug),
        TCommand::Check {
            file,
            print_symbols,
        } => match std::fs::read_to_string(file) {
            Ok(code) => TProgram::check_code(code, print_symbols),
            Err(why) => eprintln!("{why}"),
        },
    }
}

fn get_prog(file: &str) -> TProgram {
    match TProgram::from_file(file) {
        Ok(prog) => prog,
        Err(why) => {
            eprintln!("invalid turtle program: {why}");
            std::process::exit(1)
        }
    }
}

fn compile(prog: TProgram, filename: &str, no_cache: bool, debug: bool) {
    let mut cc = turtle::CComp::new(prog);
    let resfile = format!(
        "ccomp\\{}",
        filename.replace(".tg", ".c").split('\\').last().unwrap()
    );
    cc.set_filename(&resfile);
    cc.compile();
    Command::new("gcc")
        .args([
            "-Wall",
            "-g",
            "-I",
            "C:\\Users\\Bernhard\\Documents\\SDL2-2.26.2\\x86_64-w64-mingw32\\include",
            "-c",
            &resfile,
            "-o",
            "ccomp\\obj\\turtlegraphic.o",
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if no_cache {
        Command::new("gcc")
            .args([
                "-Wall",
                "-g",
                "-I",
                "C:\\Users\\Bernhard\\Documents\\SDL2-2.26.2\\x86_64-w64-mingw32\\include",
                "-c",
                "ccomp\\turtleinterf.c",
                "-o",
                "ccomp\\obj\\turtleinterf.o",
            ])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
        Command::new("gcc")
            .args([
                "-Wall",
                "-g",
                "-I",
                "C:\\Users\\Bernhard\\Documents\\SDL2-2.26.2\\x86_64-w64-mingw32\\include",
                "-c",
                "ccomp\\turtleinterf_debug.c",
                "-o",
                "ccomp\\obj\\turtleinterf_debug.o",
            ])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
    }
    let ti = if debug {
        "ccomp\\obj\\turtleinterf_debug.o"
    } else {
        "ccomp\\obj\\turtleinterf.o"
    };
    Command::new("gcc")
        .args([
            "-L",
            "C:\\Users\\Bernhard\\Documents\\SDL2-2.26.2\\x86_64-w64-mingw32\\lib",
            "-o",
            "turtle.exe",
            "ccomp\\obj\\sdlinterf.o",
            ti,
            "ccomp\\obj\\turtlegraphic.o",
            "-static",
            "-lmingw32",
            "-lSDL2main",
            "-lSDL2",
            "-Wl,--no-undefined",
            "-lm",
            "-ldinput8",
            "-ldxguid",
            "-ldxerr8",
            "-luser32",
            "-lgdi32",
            "-lwinmm",
            "-limm32",
            "-lole32",
            "-loleaut32",
            "-lshell32",
            "-lversion",
            "-luuid",
            "-static-libgcc",
            "-lhid",
            "-lsetupapi",
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}

#[cfg(test)]
mod test {
    use clap::CommandFactory;

    use super::*;

    #[test]
    fn cli() {
        Cli::command().debug_assert();
    }
}
