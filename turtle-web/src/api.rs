use anyhow::{Result, anyhow, bail};
use axum::{
    Json,
    extract::{Path, Query, State},
    http::StatusCode,
};
use serde::Deserialize;
use serde_json::{Value, json};
use turtle::{
    Disp, TProgram,
    debugger::{
        interface::commands::{CmdOutput, NoCmdReason, parse_command},
        window::{WindowCmd, WindowEvent},
    },
    examples::Example,
    pos::FilePos,
};
use uuid::Uuid;

use crate::{
    AppState,
    err::{ResultExt, WebResult},
    prog::Prog,
};

// ##########################
//     Examples
// ##########################

pub async fn example_list() -> Json<Value> {
    Json(Value::Array(
        turtle::examples::Example::ALL
            .iter()
            .map(|ex| {
                json!({
                    "name": ex.name,
                    "summary": ex.summary,
                    "description": ex.description,
                    "code": ex.code,
                    "group": ex.group.to_string(),
                })
            })
            .collect(),
    ))
}

// ##########################
//     Programming
// ##########################

pub async fn new_prog(State(state): AppState) -> Json<Uuid> {
    Json(state.lock().new_prog(Prog::new()))
}

// ##########################
//     Running - New Run
// ##########################

pub async fn run_example(
    State(state): AppState,
    Path(name): Path<String>,
    Json(args): Json<Vec<String>>,
) -> WebResult<Json<Uuid>> {
    let ex = Example::ALL
        .iter()
        .find(|e| e.name == name)
        .ok_or(anyhow!("no example named '{name}'"))
        .err_status(StatusCode::NOT_FOUND)?;
    let prog = TProgram::from_example(ex)?;
    let id = state.lock().start_run(prog, args);
    Ok(Json(id))
}

pub async fn run_prog(
    State(state): AppState,
    Path(id): Path<Uuid>,
    Json(args): Json<Vec<String>>,
) -> WebResult<Json<Uuid>> {
    let mut state = state.lock();
    let prog = state.get_prog(&id)?.get_prog()?;
    let id = state.start_run(prog, args);
    Ok(Json(id))
}

// ##########################
//     Running - Interpreter
// ##########################

#[derive(Deserialize)]
pub struct WindowQuery {
    skip: Option<usize>,
}

fn serialize_window_cmd(cmd: &WindowCmd) -> Value {
    match cmd {
        WindowCmd::Draw(from, to, col) => json!({
            "kind": "draw",
            "from": from,
            "to": to,
            "col": col,
        }),
        WindowCmd::Clear => json!({"kind": "clear"}),
        WindowCmd::Print(msg) => json!({"kind": "print", "msg": msg}),
    }
}

pub async fn window_output(
    State(state): AppState,
    Path(id): Path<Uuid>,
    Query(WindowQuery { skip }): Query<WindowQuery>,
) -> WebResult<Json<Value>> {
    let mut state = state.lock();
    let outp = state
        .get_run(&id)?
        .get_window_output()
        .iter()
        .skip(skip.unwrap_or_default())
        .map(serialize_window_cmd)
        .collect();
    Ok(Json(Value::Array(outp)))
}

fn parse_event(evt: Value) -> Result<WindowEvent> {
    #[derive(Deserialize)]
    struct MouseEvent {
        pos: (f64, f64),
        left: bool,
    }

    #[derive(Deserialize)]
    struct KeyEvent {
        key: char,
    }

    match evt["kind"].as_str() {
        None => bail!("field 'kind' is missing or not a string"),
        Some("exit") => Ok(WindowEvent::WindowExited),
        Some("mouse") => {
            let evt: MouseEvent = serde_json::from_value(evt)?;
            Ok(WindowEvent::MouseClicked(evt.pos, evt.left))
        }
        Some("key") => {
            let KeyEvent { key } = serde_json::from_value(evt)?;
            Ok(WindowEvent::KeyPressed(key))
        }
        Some(other) => bail!("unknown event kind '{other}'"),
    }
}

pub async fn report_event(
    State(state): AppState,
    Path(id): Path<Uuid>,
    Json(evt): Json<Value>,
) -> WebResult<()> {
    let evt = parse_event(evt)?;
    state.lock().get_run(&id)?.report_event(evt)?;
    Ok(())
}

pub async fn stop_run(State(state): AppState, Path(id): Path<Uuid>) -> WebResult<()> {
    state.lock().get_run(&id)?.stop();
    Ok(())
}

// ##########################
//     Running - Debugger
// ##########################

#[derive(Deserialize)]
pub struct DebugQuery {
    formatted: Option<bool>,
}

pub async fn debug(
    State(state): AppState,
    Path(id): Path<Uuid>,
    Query(DebugQuery { formatted }): Query<DebugQuery>,
    Json(cmd): Json<String>,
) -> WebResult<Json<Value>> {
    let cmd = match parse_command(&cmd) {
        Ok(cmd) => cmd,
        Err(NoCmdReason::Empty) => return Ok(Json(json!({}))),
        Err(NoCmdReason::Quit) => {
            state.lock().get_run(&id)?.stop();
            return Ok(Json(json!({"quit": true})));
        }
        Err(NoCmdReason::Help(help)) => return Ok(Json(json!({"help": help}))),
        Err(NoCmdReason::Err(why)) => return Ok(Json(json!({"parse_err": why.to_string()}))),
    };
    let mut state = state.lock();
    let run = state.get_run(&id)?;
    let res = run.exec_cmd(cmd)?;
    let mut ret = serde_json::Map::new();
    if formatted == Some(true)
        && let Some(outp) = res.output
    {
        ret.insert(
            "output".to_string(),
            Value::String(outp.with_symbols(&run.prog.symbols).to_string()),
        );
    } else if let Some(outp) = res.output {
        use turtle::tokens::Value as TValue;
        fn serialize_value(val: TValue) -> Value {
            match val {
                TValue::Number(n) => serde_json::Number::from_f64(n)
                    .map(Value::Number)
                    .unwrap_or_default(),
                TValue::String(s) => Value::String(s),
                TValue::Boolean(b) => Value::Bool(b),
            }
        }
        fn serialize_pos(pos: FilePos) -> Value {
            json!({"line": pos.line, "column": pos.column})
        }

        let (kind, data) = match outp {
            CmdOutput::ToggleNarrate(enabled) => ("narrator", json!({"enabled": enabled})),
            CmdOutput::Vardump(vars) => {
                let mut res = serde_json::Map::new();
                for (name, val) in vars.locals {
                    res.insert(name, serialize_value(val));
                }
                for (name, val) in vars.globals {
                    res.insert(format!("@{name}"), serialize_value(val));
                }
                for (pdv, val) in vars.predef {
                    res.insert(format!("@{}", pdv.get_str()), serialize_value(val));
                }
                ("vardump", Value::Object(res))
            }
            CmdOutput::CurrPos(id, ft, fp) => (
                "curr_pos",
                json!({
                    "active": id,
                    "func": ft.with_symbols(&run.prog.symbols).to_string(),
                    "pos": serialize_pos(fp)
                }),
            ),
            CmdOutput::ListTurtles(sync, ttls) => {
                let ttls = Value::Array(ttls.into_iter().map(|ttl| {
                    let args = Value::Array(ttl.start_task.1.into_iter().map(serialize_value).collect());
                    json!({
                        "id": ttl.id,
                        "active": ttl.is_active,
                        "start_task": {
                            "func": ttl.start_task.0.with_symbols(&run.prog.symbols).to_string(),
                            "args": args,
                        }
                    })
                }).collect());
                ("list_turtles", json!({"sync": sync, "turtles": ttls}))
            }
            CmdOutput::ListBreakpoints(bps) => (
                "list_breakpoints",
                Value::Array(
                    bps.into_iter()
                        .map(|bp| {
                            json!({
                                "id": bp.id,
                                "enabled": bp.enabled,
                                "pos": serialize_pos(bp.pos)
                            })
                        })
                        .collect(),
                ),
            ),
            CmdOutput::AddBreakpoint(id) => ("add_breakpoint", json!({"id": id})),
            CmdOutput::Stacktrace(frames) => (
                "stacktrace",
                Value::Array(
                    frames
                        .into_iter()
                        .map(|frame| {
                            json!({
                                "func": frame.func.with_symbols(&run.prog.symbols).to_string(),
                                "pos": serialize_pos(frame.pos)
                            })
                        })
                        .collect(),
                ),
            ),
            CmdOutput::Evaluate(val) => ("evaluate", json!({"result": serialize_value(val)})),
            CmdOutput::Execute(finished) => ("execute", json!({"finished": finished})),
        };
        ret.insert("kind".to_string(), Value::String(kind.to_string()));
        ret.insert("data".to_string(), data);
    }
    drop(state);
    if let Some(err) = res.error {
        ret.insert("error".to_string(), Value::String(err.to_string()));
    }
    if res.stmt_count > 0 {
        ret.insert("stmt_count".to_string(), json!(res.stmt_count));
    }
    let events = Value::Array(
        res.run_events
            .into_iter()
            .map(|e| match e {
                turtle::debugger::DbgEvent::TurtleFinished(id) => json!({"turtle_finished": id}),
                turtle::debugger::DbgEvent::BreakpointHit(id) => json!({"breakpoint": id}),
            })
            .collect(),
    );
    ret.insert("events".to_string(), events);
    Ok(Json(Value::Object(ret)))
}
