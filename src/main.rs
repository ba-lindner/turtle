use clap::Parser;
use turtle::{
    debugger::{
        config::RunConfig,
        interface::{Shell, Terminal, VSCode},
        window::{SdlWindow, VoidWindow, Window},
    },
    features::{FeatureConf, FeatureState},
    TProgram,
};

use cli::*;

mod cli;

impl Features {
    fn feature_conf(&self) -> FeatureConf {
        let mut res = FeatureConf::default();
        for f in &self.features {
            res[*f] = FeatureState::Enabled;
        }
        for f in &self.disabled {
            res[*f] = FeatureState::Disabled;
        }
        res
    }
}

impl Source {
    fn get_prog(&self) -> TProgram {
        match TProgram::from_file(&self.file, false, self.features.feature_conf()) {
            Ok(prog) => prog,
            Err(why) => {
                eprintln!("invalid turtle program: {why}");
                std::process::exit(1)
            }
        }
    }

    fn check(&self, print_symbols: bool) {
        if let Err(why) =
            TProgram::from_file(&self.file, print_symbols, self.features.feature_conf())
        {
            eprintln!("invalid turtle program: {why}");
            std::process::exit(1)
        }
    }
}

impl Display {
    fn as_boxed(&self, title: &str) -> Box<dyn Window> {
        match self {
            Display::Sdl => Box::new(SdlWindow::create(title.to_string())),
            Display::Void => Box::new(VoidWindow::default()),
            Display::Buffered => Box::new(SdlWindow::create(title.to_string()).buffered()),
        }
    }
}

impl RunOpt {
    fn config<'a>(&'a self, title: &str) -> RunConfig<'a, Box<dyn Window>, Terminal> {
        RunConfig::new(&self.args).window(self.window.as_boxed(title))
    }
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
        TCommand::Shell {
            features,
            base,
            window,
        } => {
            let prog = if let Some(file) = base {
                Source { features, file }.get_prog()
            } else {
                TProgram::parse("begin end", false, features.feature_conf()).unwrap()
            };
            RunConfig::new(&[])
                .window(window.as_boxed("Turtle Shell"))
                .debug_in(Shell)
                .exec(&prog);
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
