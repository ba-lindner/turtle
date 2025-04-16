use std::{sync::{
    mpsc::{self, Receiver, Sender},
    Arc,
}, thread};

use actix_web::{get, post, web::{Data, Json}, App, HttpServer};
use parking_lot::Mutex;
use serde_json::{json, Value};
use turtle::debugger::{
    config::RunConfig,
    interface::Strings,
    window::{ChannelWindow, WindowCmd, WindowEvent},
};

const TEST_CODE: &str = "
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

struct SharedState {
    commands: Receiver<WindowCmd>,
    events: Sender<WindowEvent>,
    inputs: Sender<String>,
    outputs: Receiver<Result<String, String>>,
}

type AppState = Data<Arc<Mutex<SharedState>>>;

type EmptyResponse = &'static str;
const EMPTY_RESPONSE: EmptyResponse = "";

#[actix_web::main]
async fn main() -> Result<(), std::io::Error> {
    let prog = TEST_CODE.parse().unwrap();
    let (window, commands, events) = ChannelWindow::construct();
    let (in_tx, in_rx) = mpsc::channel();
    let (interface, outputs) = Strings::construct(in_rx);
    thread::spawn(move || {
        RunConfig::new(&[])
            .window(window)
            .debug_in(interface)
            .exec(&prog);
    });
    let state = SharedState {
        commands,
        events,
        inputs: in_tx,
        outputs,
    };
    let state = Arc::new(Mutex::new(state));
    HttpServer::new(move || {
        App::new()
            .service(exec_cmd)
            .service(get_output)
            .service(event)
            .service(get_drawings)
            .app_data(Data::new(Arc::clone(&state)))
    })
    .bind("127.0.0.1:8000")?
    .run()
    .await
}

#[post("/exec")]
async fn exec_cmd(state: AppState, cmd: String) -> EmptyResponse {
    state.lock().inputs.send(cmd).unwrap();
    EMPTY_RESPONSE
}

#[get("/output")]
async fn get_output(state: AppState) -> Json<Value> {
    Json(Value::Array(
        state
            .lock()
            .outputs
            .try_iter()
            .map(|out| match out {
                Ok(msg) => json!({"kind":"ok","msg":msg}),
                Err(msg) => json!({"kind":"err","msg":msg}),
            })
            .collect(),
    ))
}

#[post("/event")]
async fn event(state: AppState, Json(coord): Json<(f64, f64)>) -> EmptyResponse {
    state.lock().events.send(WindowEvent::MouseClicked(coord, true)).unwrap();
    EMPTY_RESPONSE
}

#[get("/lines")]
async fn get_drawings(state: Data<AppState>) -> Json<Value> {
    Json(Value::Array(
        state
            .lock()
            .commands
            .try_iter()
            .filter_map(|cmd| match cmd {
                WindowCmd::Draw(from, to, _) => Some(json!({
                    "start_x": from.0,
                    "start_y": from.1,
                    "end_x": to.0,
                    "end_y": to.1,
                })),
                _ => None
            })
            .collect()
    ))
}
