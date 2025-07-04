use anyhow::Error;
use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};

pub type WebResult<T> = Result<T, TurtleWebError>;

pub struct TurtleWebError(Error, StatusCode);

impl<E: Into<Error>> From<E> for TurtleWebError {
    fn from(value: E) -> Self {
        Self(value.into(), StatusCode::INTERNAL_SERVER_ERROR)
    }
}

impl IntoResponse for TurtleWebError {
    fn into_response(self) -> Response {
        Response::builder()
            .status(self.1)
            .body(axum::body::Body::new(format!("{:?}", self.0)))
            .expect("no headers configured")
    }
}

pub trait ResultExt<T> {
    fn err_status(self, code: StatusCode) -> WebResult<T>;
}

impl<T, E: Into<Error>> ResultExt<T> for Result<T, E> {
    fn err_status(self, code: StatusCode) -> WebResult<T> {
        self.map_err(|e| TurtleWebError(e.into(), code))
    }
}
