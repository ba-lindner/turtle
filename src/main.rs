use std::process::Command;
use clap::{Parser, Subcommand};
use turtle::{TProgram, StepVariant};

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: TCommand,
    /// the turtle source file to operate on
    file: String,
}

#[derive(Subcommand)]
enum TCommand {
    /// Start interpreter
    Interpret {
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Start debugger
    Debug {
        /// set step size
        #[arg(short, long, value_enum, default_value_t = StepVariant::Statement)]
        step: StepVariant,
        /// set breakpoints in format line,column
        #[arg(short, long)]
        breakpoint: Vec<String>,
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Compile to C
    Compile {
        /// enable debug builds
        #[arg(short, long)]
        debug: bool,
        /// rebuild turtle interface
        #[arg(long)]
        no_cache: bool,
    },
    /// Check syntax
    Check,
}

fn main() {
    let cli = Cli::parse();
    let prog = turtle::parse_file(&cli.file).unwrap();
    match cli.command {
        TCommand::Interpret { args } => prog.interpret(&args),
        TCommand::Debug { step, breakpoint: bp, args } => prog.debug(&args, step, &bp),
        TCommand::Compile { debug, no_cache } => compile(prog, &cli.file, no_cache, debug),
        TCommand::Check => {}
    }
}

fn compile(prog: TProgram, filename: &str, recomp: bool, debug: bool) {
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
    if recomp {
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
