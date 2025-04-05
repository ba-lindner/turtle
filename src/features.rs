use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

use clap::ValueEnum;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct FeatureConf([FeatureState; 4]);

impl FeatureConf {
    pub fn expect(&mut self, feature: Feature) -> Result<(), Feature> {
        match self[feature] {
            FeatureState::Auto => self[feature] = FeatureState::Enabled,
            FeatureState::Enabled => {}
            FeatureState::Disabled => return Err(feature),
        }
        Ok(())
    }

    pub fn set_auto(&mut self, feature: Feature, enabled: bool) -> bool {
        if self[feature] == FeatureState::Auto {
            self[feature] = if enabled {
                FeatureState::Enabled
            } else {
                FeatureState::Disabled
            };
            true
        } else {
            false
        }
    }

    pub fn finalize(&mut self) {
        for f in Feature::value_variants() {
            self.set_auto(*f, false);
        }
    }
}

impl Index<Feature> for FeatureConf {
    type Output = FeatureState;

    fn index(&self, index: Feature) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<Feature> for FeatureConf {
    fn index_mut(&mut self, index: Feature) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Feature {
    /// introduces types: strings, booleans and numbers
    Types = 0,
    /// parallel execution with multiple turtles drawing concurrently
    Multithreading,
    /// react to users clicking or typing
    Events,
    /// easier command-line arguments
    Parameters,
}

impl Display for Feature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Feature::Types => write!(f, "types"),
            Feature::Multithreading => write!(f, "multithreading"),
            Feature::Events => write!(f, "events"),
            Feature::Parameters => write!(f, "parameters"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum FeatureState {
    Enabled,
    #[default]
    Auto,
    Disabled,
}
