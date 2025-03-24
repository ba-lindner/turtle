use std::str::FromStr;

use crate::features::{Feature, FeatureConf, FeatureState};

use super::ValType;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PredefVar {
    Dir,
    Dist,
    X,
    Y,
    Arg(usize),
    Pi,
    MaxX,
    MaxY,
    Delay,
    Red,
    Green,
    Blue,
}

impl PredefVar {
    pub fn get_str(&self) -> &'static str {
        match self {
            PredefVar::Dir => "dir",
            PredefVar::Dist => "dist",
            PredefVar::X => "x",
            PredefVar::Y => "y",
            PredefVar::Arg(1) => "1",
            PredefVar::Arg(2) => "2",
            PredefVar::Arg(3) => "3",
            PredefVar::Arg(4) => "4",
            PredefVar::Arg(5) => "5",
            PredefVar::Arg(6) => "6",
            PredefVar::Arg(7) => "7",
            PredefVar::Arg(8) => "8",
            PredefVar::Arg(9) => "9",
            PredefVar::Arg(_) => unreachable!(),
            PredefVar::Pi => "pi",
            PredefVar::MaxX => "max_x",
            PredefVar::MaxY => "max_y",
            PredefVar::Delay => "delay",
            PredefVar::Red => "red",
            PredefVar::Green => "green",
            PredefVar::Blue => "blue",
        }
    }

    pub fn get_all() -> Vec<PredefVar> {
        vec![
            PredefVar::Dir,
            PredefVar::Dist,
            PredefVar::X,
            PredefVar::Y,
            PredefVar::Arg(1),
            PredefVar::Arg(2),
            PredefVar::Arg(3),
            PredefVar::Arg(4),
            PredefVar::Arg(5),
            PredefVar::Arg(6),
            PredefVar::Arg(7),
            PredefVar::Arg(8),
            PredefVar::Arg(9),
            PredefVar::Pi,
            PredefVar::MaxX,
            PredefVar::MaxY,
            PredefVar::Delay,
            PredefVar::Red,
            PredefVar::Green,
            PredefVar::Blue,
        ]
    }

    pub fn is_writeable(&self) -> bool {
        match self {
            PredefVar::Dir => false,
            PredefVar::Dist => false,
            PredefVar::X => false,
            PredefVar::Y => false,
            PredefVar::Arg(_) => false,
            PredefVar::Pi => false,
            PredefVar::MaxX => true,
            PredefVar::MaxY => true,
            PredefVar::Delay => true,
            PredefVar::Red => true,
            PredefVar::Blue => true,
            PredefVar::Green => true,
        }
    }

    pub fn val_type(&self, features: &FeatureConf) -> ValType {
        if features[Feature::Types] == FeatureState::Disabled {
            return ValType::Number
        }
        match self {
            PredefVar::Dir
            | PredefVar::Dist
            | PredefVar::X
            | PredefVar::Y
            | PredefVar::Pi
            | PredefVar::MaxX
            | PredefVar::MaxY
            | PredefVar::Delay
            | PredefVar::Red
            | PredefVar::Green
            | PredefVar::Blue => ValType::Number,
            PredefVar::Arg(_) => ValType::String,
        }
    }
}

impl FromStr for PredefVar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for pdv in PredefVar::get_all() {
            if pdv.get_str() == s {
                return Ok(pdv);
            }
        }
        Err(())
    }
}
