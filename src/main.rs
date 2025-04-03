use clap::{Args, Parser, Subcommand};
use std::process::Command;
use turtle::{features::{Feature, FeatureConf, FeatureState}, pos::FilePos, TProgram};

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
        #[command(flatten)]
        source: Source,
        /// const-fold before execution
        #[arg(short, long)]
        optimized: bool,
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Start debugger
    Debug {
        #[command(flatten)]
        source: Source,
        /// set breakpoints in format line,column
        #[arg(short, long)]
        breakpoint: Vec<FilePos>,
        /// args passed to turtle
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Compile to C
    Compile {
        #[command(flatten)]
        source: Source,
        /// enable debug builds
        #[arg(short, long)]
        debug: bool,
        /// rebuild turtle interface
        #[arg(short, long)]
        no_cache: bool,
    },
    /// Check syntax
    Check {
        #[command(flatten)]
        source: Source,
        /// print symbols on error
        #[arg(short = 's', long)]
        print_symbols: bool,
    },
}

#[derive(Args)]
#[group()]
struct Source {
    /// turtle source file
    file: String,
    /// enabled features
    #[arg(short = 'f', long = "feature")]
    features: Vec<Feature>,
    /// disabled features
    #[arg(short = 'F', long = "disabled")]
    disabled_features: Vec<Feature>,
}

impl Source {
    fn feature_conf(&self) -> FeatureConf {
        let mut res = FeatureConf::default();
        for f in &self.features {
            res[*f] = FeatureState::Enabled;
        }
        for f in &self.disabled_features {
            res[*f] = FeatureState::Disabled;
        }
        res
    }

    fn get_prog(&self) -> TProgram {
        match TProgram::from_file(&self.file, false, self.feature_conf()) {
            Ok(prog) => prog,
            Err(why) => {
                eprintln!("invalid turtle program: {why}");
                std::process::exit(1)
            }
        }
    }

    fn check(&self, print_symbols: bool) {
        if let Err(why) = TProgram::from_file(&self.file, print_symbols, self.feature_conf()) {
            eprintln!("invalid turtle program: {why}");
            std::process::exit(1)
        }
    }
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        TCommand::Run { source, optimized, args } => {
            let mut prog = source.get_prog();
            if optimized {
                prog.optimize();
            }
            prog.interpret(&args);
        }
        TCommand::Debug {
            source,
            breakpoint: bp,
            args,
        } => source.get_prog().debug(&args, bp),
        TCommand::Compile {
            source,
            debug,
            no_cache,
        } => compile(source.get_prog(), &source.file, no_cache, debug),
        TCommand::Check {
            source,
            print_symbols,
        } => source.check(print_symbols),
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

#[allow(unused)]
mod presi {
    use turtle::tokens::BiOperator;

    enum Expr {
        Const(f64),
        BiOperation(Box<Expr>, BiOperator, Box<Expr>),
        Bracket(Box<Expr>),
        Absolute(Box<Expr>),
    }

    struct Interpreter;

    impl Interpreter {
fn eval_expr(&mut self, expr: &Expr) -> f64 {
    match expr {
        Expr::Const(val) => *val,
        Expr::BiOperation(lhs, op, rhs) => {
            let lhs = self.eval_expr(lhs);
            let rhs = self.eval_expr(rhs);
            op.calc(lhs, rhs)
        }
        Expr::Bracket(expr) => self.eval_expr(expr),
        Expr::Absolute(expr) => self.eval_expr(expr).abs(),
    }
}

async fn debug_expr(&mut self, expr: &Expr) -> f64 {
    Box::pin(async { match expr {
        Expr::Const(val) => *val,
        Expr::BiOperation(lhs, op, rhs) => {
            let lhs = self.debug_expr(lhs).await;
            let rhs = self.debug_expr(rhs).await;
            op.calc(lhs, rhs)
        }
        Expr::Bracket(expr) => self.debug_expr(expr).await,
        Expr::Absolute(expr) => self.debug_expr(expr).await.abs(),
    }}).await
}
    }
}