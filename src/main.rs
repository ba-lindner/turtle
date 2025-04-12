use clap::{Args, Parser, Subcommand, ValueEnum};
use turtle::{
    debugger::{
        config::RunConfig,
        interface::{Terminal, VSCode},
        window::{SdlWindow, Window},
    },
    features::{Feature, FeatureConf, FeatureState},
    pos::FilePos,
    TProgram,
};

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
        #[command(flatten)]
        opt: RunOpt,
    },
    /// Start debugger
    Debug {
        #[command(flatten)]
        source: Source,
        /// set breakpoints in format line,column
        #[arg(short, long)]
        breakpoint: Vec<FilePos>,
        /// select debugging interface
        #[arg(short, long, default_value = "terminal")]
        interface: Interf,
        #[command(flatten)]
        opt: RunOpt,
    },
    /// Compile to C
    Compile {
        #[command(flatten)]
        source: Source,
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

#[derive(Args)]
#[group()]
struct RunOpt {
    /// display mechanism
    #[arg(short, long, default_value = "sdl")]
    window: Display,
    /// args passed to turtle
    #[arg(last = true)]
    args: Vec<String>,
}

impl RunOpt {
    fn config<'a>(&'a self, title: &str) -> RunConfig<'a, Box<dyn Window>, Terminal> {
        RunConfig::new(&self.args).window(match self.window {
            Display::SDL => Box::new(SdlWindow::new(title)),
            Display::Void => Box::new((20.0, 15.0)),
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum Display {
    /// use SDL2 to draw turtles
    #[default]
    SDL,
    /// no display
    Void,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum Interf {
    /// run as standalone debugger
    #[default]
    Terminal,
    /// run as vscode extension
    VSCode,
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        TCommand::Run {
            source,
            optimized,
            opt,
        } => {
            let mut prog = source.get_prog();
            if optimized {
                prog.optimize();
            }
            opt.config(&prog.title("Interpreter")).exec(&prog);
        }
        TCommand::Debug {
            source,
            breakpoint,
            interface,
            opt,
        } => {
            let prog = source.get_prog();
            let conf = opt
                .config(&prog.title("Debugger"))
                .debug_in(Terminal)
                .breakpoints(breakpoint);
            match interface {
                Interf::Terminal => conf.exec(&prog),
                Interf::VSCode => conf.debug_in(VSCode).exec(&prog),
            }
        }
        TCommand::Compile { source } => compile(source.get_prog(), &source.file),
        TCommand::Check {
            source,
            print_symbols,
        } => source.check(print_symbols),
    }
}

fn compile(prog: TProgram, filename: &str) {
    let mut cc = turtle::CComp::new(prog);
    let resfile = format!(
        "ccomp\\{}",
        filename.replace(".tg", ".c").split('\\').last().unwrap()
    );
    cc.set_filename(&resfile);
    cc.compile();
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
