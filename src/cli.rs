use clap::{Args, Parser, Subcommand, ValueEnum};
use turtle::{features::Feature, pos::FilePos};

#[derive(Parser)]
#[command(version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: TCommand,
}

#[derive(Subcommand)]
pub enum TCommand {
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
    /// Start turtle shell
    Shell {
        /// display mechanism
        #[arg(short, long, default_value = "sdl")]
        window: Display,
        /// source file to load functions from
        #[arg(short, long)]
        base: Option<String>,
        #[command(flatten)]
        features: Features,
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
pub struct Source {
    /// turtle source file
    pub file: String,
    #[command(flatten)]
    pub features: Features,
}

#[derive(Args)]
#[group()]
pub struct Features {
    /// enabled features
    #[arg(short, long = "feature")]
    pub features: Vec<Feature>,
    /// disabled features
    #[arg(short = 'F', long)]
    pub disabled: Vec<Feature>,
}

#[derive(Args)]
#[group()]
pub struct RunOpt {
    /// display mechanism
    #[arg(short, long, default_value = "sdl")]
    pub window: Display,
    /// args passed to turtle
    #[arg(last = true)]
    pub args: Vec<String>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum Display {
    /// use SDL2 to draw turtles
    #[default]
    Sdl,
    /// no display
    Void,
    /// buffered window
    /// 
    /// redraws every line on changes to @max_x / @max_y
    Buffered,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum Interf {
    /// run as standalone debugger
    #[default]
    Terminal,
    /// run as vscode extension
    VSCode,
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
