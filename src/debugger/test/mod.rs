use std::sync::mpsc;

use crate::{features::FeatureConf, TProgram};

use super::{config::RunConfig, interface::Strings, window::{ChannelWindow, WindowCmd, WindowEvent}};

const TEST_SRC_CIRCLE: &str = "
path circle(r,n)
  jump r

  store 180-360/n in beta
  store 2*(2*@pi*r/2)/n in a

  turn right 180-beta/2
  counter x from 0 to n do
    walk a
    turn right 180-beta
  done
endpath

begin
  store @max_y * 0.9 in radius
  if @1 > 0 then
    store @1 in radius
  endif
  store 100 in steps
  if @2 > 0 then
    store @2 in steps
  endif

  store 1 in @delay

  path circle(radius,steps)
  jump home
  stop
end
";

enum TestInput<'c> {
    Stdin(&'c str),
    Event(WindowEvent),
}

#[derive(Debug, PartialEq)]
enum TestResult {
    Stdout(String),
    Stderr(String),
    Window(WindowCmd),
}

fn debug_test(code: &str, inputs: &[TestInput<'_>]) -> Vec<TestResult> {
    let prog = TProgram::parse(code, false, FeatureConf::default()).unwrap();
    let mut results = Vec::new();
    let (window, cmds, events) = ChannelWindow::construct();
    let (dbg_tx, dbg_rx) = mpsc::channel();
    let mut collect_results = || {
        for cmd in cmds.try_iter() {
            results.push(TestResult::Window(cmd));
        }
        for out in dbg_rx.try_iter() {
            match out {
                Ok(msg) => results.push(TestResult::Stdout(msg)),
                Err(msg) => results.push(TestResult::Stderr(msg)),
            }
        }
    };
    let itf = Strings::new(inputs.iter().filter_map(|inp| {
        match inp {
            TestInput::Stdin(cmd) => {
                collect_results();
                Some(cmd.to_string())
            },
            TestInput::Event(evt) => {
                events.send(*evt).unwrap();
                None
            }
        }
    }), dbg_tx);
    let cfg = RunConfig::new(&[])
        .debug_in(itf)
        .window(window);
    cfg.exec(&prog);
    collect_results();
    results
}

#[test]
fn example_test() {
    let inputs = &[
        TestInput::Stdin("test"),
        TestInput::Stdin("step draw"),
        TestInput::Event(WindowEvent::WindowExited),
    ];
    let results = debug_test(TEST_SRC_CIRCLE, inputs);
    assert_eq!(results, &[
        TestResult::Stderr("unknown command test".to_string()),
        TestResult::Window(WindowCmd::Draw((0.675, 0.0), (0.6736678225654924, -0.05652076441067613), (100.0, 100.0, 0.0))),
        TestResult::Stdout("executed 10 statements".to_string()),
    ]);
}