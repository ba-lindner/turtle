use std::convert::Infallible;

use anyhow::{Result, anyhow};
use axum::{extract::FromRequestParts, http::StatusCode};
use serde::{Deserialize, Serialize};
use turtle::{
    TProgram,
    features::{Feature as TFeature, FeatureConf, FeatureState},
};
use uuid::Uuid;

use crate::err::{ResultExt, WebResult};

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Permission {
    None,
    Read,
    Write,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Feature {
    Types,
    Events,
    Multithreading,
    Parameters,
}

impl From<Feature> for TFeature {
    fn from(value: Feature) -> Self {
        match value {
            Feature::Types => Self::Types,
            Feature::Events => Self::Events,
            Feature::Multithreading => Self::Multithreading,
            Feature::Parameters => Self::Parameters,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Prog {
    name: String,
    key: Option<String>,
    public: Permission,
    features: Vec<Feature>,
    #[serde(skip)]
    code: Option<String>,
}

impl Prog {
    pub fn new() -> Self {
        Self {
            name: String::from("New Program"),
            key: None,
            public: Permission::None,
            features: Vec::new(),
            code: Some(String::new()),
        }
    }

    pub fn assert(&self, perm: Permission, auth: Auth) -> WebResult<()> {
        if self.public >= perm {
            return Ok(());
        }
        match (&self.key, &auth.key) {
            (Some(exp), Some(given)) if exp != given => {
                Err(anyhow!("wrong key")).err_status(StatusCode::UNAUTHORIZED)
            }
            (Some(_), None) => Err(anyhow!("missing key")).err_status(StatusCode::UNAUTHORIZED),
            _ => Ok(()),
        }
    }

    pub fn load_code(&mut self, id: &Uuid) -> WebResult<()> {
        if self.code.is_none() {
            let path = format!("data/prog_{id}.tg");
            let code = std::fs::read_to_string(path)?;
            self.code = Some(code);
        }
        Ok(())
    }

    // ##########################
    //     API functions
    // ##########################

    pub fn update(&mut self, other: Prog) {
        self.name = other.name;
        self.key = other.key;
        self.public = other.public;
        self.features = other.features;
    }

    pub fn get_code(&self) -> Result<&str> {
        self.code.as_deref().ok_or(anyhow!("failed to load code"))
    }

    pub fn set_code(&mut self, code: String) {
        self.code = Some(code);
    }

    fn feature_conf(&self) -> FeatureConf {
        let mut conf = FeatureConf::default();
        for feat in &self.features {
            conf[(*feat).into()] = FeatureState::Enabled;
        }
        conf.finalize();
        conf
    }

    pub fn check(&self) -> WebResult<Option<turtle::TurtleError>> {
        Ok(TProgram::parse(self.get_code()?, false, self.feature_conf()).err())
    }

    pub fn get_prog(&self) -> Result<TProgram> {
        Ok(TProgram::parse(
            self.get_code()?,
            false,
            self.feature_conf(),
        )?)
    }
}

pub struct Auth {
    key: Option<String>,
}

impl<S> FromRequestParts<S> for Auth {
    type Rejection = Infallible;

    fn from_request_parts(
        parts: &mut axum::http::request::Parts,
        _: &S,
    ) -> impl Future<Output = Result<Self, Self::Rejection>> + Send {
        async {
            let key = (|| -> Option<_> {
                Some(
                    parts
                        .headers
                        .get(axum::http::header::AUTHORIZATION)?
                        .to_str()
                        .ok()?
                        .to_string(),
                )
            })();
            Ok(Auth { key })
        }
    }
}
