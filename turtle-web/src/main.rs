use std::{collections::HashMap, sync::Arc, thread, time::Duration};

use anyhow::anyhow;
use axum::{
    Router,
    extract::State,
    http::{Method, StatusCode},
    routing::{get, post, put},
};
use parking_lot::Mutex;
use prog::Prog;
use run::Run;
use tokio::net::TcpListener;
use tower_http::{
    cors::{Any, CorsLayer},
    services::ServeDir,
};

use uuid::Uuid;

mod api;
mod err;
mod prog;
mod run;

use err::*;

use crate::prog::{Auth, Permission};

/*
/data
    programs.json
    <prog_id_1>.tg
    <prog_id_2>.tg
/static
    index.html
    script.js
    styles.css

*/

type AppState = State<Arc<Mutex<FullState>>>;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let state = FullState::load();
    std::fs::create_dir("data")?;
    gc_job(state.clone(), Duration::from_secs(60 * 15));
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST])
        .allow_origin(Any);
    let app = Router::new()
        .route("/examples", get(api::example_list))
        .route("/prog", post(api::new_prog))
        .route("/prog/{if}", get(api::get_prog_meta))
        .route("/prog/{if}", put(api::set_prog_meta))
        .route("/prog/{if}/code", get(api::get_prog_code))
        .route("/prog/{if}/code", put(api::set_prog_code))
        .route("/prog/{if}/check", post(api::check_prog))
        .route("/examples/{name}/run", post(api::run_example))
        .route("/prog/{id}/run", post(api::run_prog))
        .route("/run/{id}/window", get(api::window_output))
        .route("/run/{id}/events", post(api::report_event))
        .route("/run/{id}/stop", post(api::stop_run))
        .route("/run/{id}/debug", post(api::debug))
        .fallback_service(ServeDir::new("assets"))
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
    fn load() -> Arc<Mutex<Self>> {
        let progs = match std::fs::read("data/programs.json") {
            Ok(content) => serde_json::from_slice(&content).unwrap_or_else(|e| {
                eprintln!("failed to load programs: {e}");
                HashMap::new()
            }),
            Err(not_found) if not_found.kind() == std::io::ErrorKind::NotFound => HashMap::new(),
            Err(other) => {
                eprintln!("failed to load programs: {other}");
                HashMap::new()
            }
        };
        Arc::new(Mutex::new(Self {
            progs,
            runs: HashMap::new(),
        }))
    }

    fn save(&self) {
        let content = match serde_json::to_vec(&self.progs) {
            Ok(v) => v,
            Err(why) => {
                eprintln!("failed to save programs: {why}");
                return;
            }
        };
        if let Err(why) = std::fs::write("data/programs.json", &content) {
            eprintln!("failed to save programs: {why}");
        }
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

    fn _get_prog_impl(&mut self, id: &Uuid) -> WebResult<&mut Prog> {
        let prog = self
            .progs
            .get_mut(id)
            .ok_or(anyhow!("no program with id {id}"))
            .err_status(StatusCode::NOT_FOUND)?;
        prog.load_code(id)?;
        Ok(prog)
    }

    fn get_prog(&mut self, id: &Uuid, auth: Auth) -> WebResult<&Prog> {
        let prog = self._get_prog_impl(id)?;
        prog.assert(Permission::Read, auth)?;
        Ok(&*prog)
    }

    fn get_prog_mut(&mut self, id: &Uuid, auth: Auth) -> WebResult<&mut Prog> {
        let prog = self._get_prog_impl(id)?;
        prog.assert(Permission::Write, auth)?;
        Ok(prog)
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
                state.save();
                for run in state.runs.values() {
                    run.gc(max_idle);
                }
                state.runs.retain(|_, r| !r.finished());
            }
            thread::sleep(Duration::from_secs(5));
        }
    });
}
