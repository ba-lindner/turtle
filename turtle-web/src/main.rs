use std::{collections::HashMap, sync::Arc, thread, time::Duration};

use anyhow::anyhow;
use axum::{
    Router,
    extract::State,
    http::{Method, StatusCode},
    routing::{get, post},
};
use parking_lot::Mutex;
use prog::Prog;
use run::Run;
use tokio::net::TcpListener;
use tower_http::cors::{Any, CorsLayer};

use uuid::Uuid;

mod api;
mod err;
mod prog;
mod run;

use err::*;

/*
/progs
    programs.json
    <prog_name_1>.tg
    <prog_name_2>.tg
/static
    index.html
    script.js
    styles.css

*/

type AppState = State<Arc<Mutex<FullState>>>;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let state = FullState::new();
    gc_job(state.clone(), Duration::from_secs(60 * 15));
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST])
        .allow_origin(Any);
    let app = Router::new()
        .route("/examples", get(api::example_list))
        .route("/prog", post(api::new_prog))
        .route("/examples/{name}/run", post(api::run_example))
        .route("/prog/{id}/run", post(api::run_prog))
        .route("/run/{id}/window", get(api::window_output))
        .route("/run/{id}/events", post(api::report_event))
        .route("/run/{id}/stop", post(api::stop_run))
        .route("/run/{id}/debug", post(api::debug))
        .layer(cors)
        .with_state(state);
    let listener = TcpListener::bind(("0.0.0.0", 8000)).await?;
    axum::serve(listener, app).await
}

#[derive(Default)]
struct FullState {
    progs: HashMap<Uuid, Prog>,
    runs: HashMap<Uuid, Run>,
}

impl FullState {
    fn new() -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Self::default()))
    }

    fn new_prog(&mut self, prog: Prog) -> Uuid {
        let id = Uuid::new_v4();
        self.progs.insert(id, prog);
        id
    }

    fn start_run(&mut self, prog: turtle::TProgram, args: Vec<String>) -> Uuid {
        let id = Uuid::new_v4();
        self.runs.insert(id, Run::start(prog, args));
        id
    }

    fn get_prog(&mut self, id: &Uuid) -> WebResult<&mut Prog> {
        self.progs
            .get_mut(id)
            .ok_or(anyhow!("no program with id {id}"))
            .err_status(StatusCode::NOT_FOUND)
    }

    fn get_run(&mut self, id: &Uuid) -> WebResult<&mut Run> {
        self.runs
            .get_mut(id)
            .ok_or(anyhow!("no run with id {id}"))
            .err_status(StatusCode::NOT_FOUND)
    }
}

fn gc_job(state: Arc<Mutex<FullState>>, max_idle: Duration) {
    thread::spawn(move || {
        loop {
            {
                let mut state = state.lock();
                for run in state.runs.values() {
                    run.gc(max_idle);
                }
                state.runs.retain(|_, r| !r.finished());
            }
            thread::sleep(Duration::from_secs(15));
        }
    });
}
