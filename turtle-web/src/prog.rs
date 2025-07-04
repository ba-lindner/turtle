use anyhow::Result;
use serde::{Deserialize, Serialize};
use turtle::{
    TProgram,
    features::{Feature as TFeature, FeatureConf, FeatureState},
};

#[derive(Serialize, Deserialize)]
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

    fn code_path(&self) -> String {
        format!("data/{}.tg", self.name)
    }

    pub fn get_code(&mut self) -> Result<&str> {
        if self.code.is_none() {
            self.code = Some(std::fs::read_to_string(self.code_path())?);
        }
        Ok(self.code.as_deref().unwrap())
    }

    fn feature_conf(&self) -> FeatureConf {
        let mut conf = FeatureConf::default();
        for feat in &self.features {
            conf[(*feat).into()] = FeatureState::Enabled;
        }
        conf.finalize();
        conf
    }

    pub fn get_prog(&mut self) -> Result<TProgram> {
        let features = self.feature_conf();
        Ok(TProgram::parse(self.get_code()?, false, features)?)
    }
}
