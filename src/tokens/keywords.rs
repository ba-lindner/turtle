use crate::features::*;

macro_rules! count_tts {
    () => { 0 };
    ($odd:tt $($a:tt $b:tt)*) => { (count_tts!($($a)*) << 1) | 1 };
    ($($a:tt $even:tt)*) => { count_tts!($($a)*) << 1 };
}

macro_rules! keyword_enum {
    ($($kw:ident $(if $($feat:ident)|+)?),+ $(,)?) => {
        #[derive(Debug, PartialEq)]
        #[derive(Clone, Copy)]
        pub enum Keyword {
            $($kw,)+
        }

        impl Keyword {
            pub fn all(features: &FeatureConf) -> Vec<Keyword> {
                let mut res = Vec::with_capacity(count_tts!($($kw)+));
                $(
                    if true $(&& ( $(features[Feature::$feat] != FeatureState::Disabled)||+ ))? {
                        res.push(Keyword::$kw);
                    }
                )+
                res
            }

            pub fn enabled(&self, features: &FeatureConf) -> bool {
                match self {
                    $(
                        Keyword::$kw => true $(&& ( $(features[Feature::$feat] != FeatureState::Disabled)||+ ))?,
                    )+
                }
            }
        }

        impl std::str::FromStr for Keyword {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                paste::paste! {
                    Ok(match s {
                        $(
                            stringify!([<$kw:lower>]) => Keyword::$kw,
                        )+
                        _ => return Err(()),
                    })
                }
            }
        }

        impl std::fmt::Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                paste::paste! {
                    match self {
                        $(
                            Keyword::$kw => write!(f, "{}", stringify!([<$kw:lower>])),
                        )+
                    }
                }
            }
        }
    };
}

keyword_enum! {
    Walk,
    Back,
    Jump,
    Home,
    Turn,
    Left,
    Right,
    Direction,
    Color,
    Clear,
    Stop,
    Finish,
    Path,
    Store,
    In,
    Add,
    To,
    Sub,
    From,
    Mul,
    Div,
    By,
    Mark,
    If,
    Then,
    Else,
    Endif,
    Do,
    Times,
    Done,
    Counter,
    Downto,
    Step,
    While,
    Repeat,
    Until,
    Endpath,
    Calculation,
    Returns,
    Endcalc,
    Begin,
    End,
    Sin,
    Cos,
    Tan,
    Sqrt,
    Rand,
    And,
    Or,
    Not,
    Arctan,
    String if Types,
    Num if Types,
    Bool if Types,
    Append if Types,
    True if Types,
    False if Types,
    Print if Types,
    Substr if Types,
    Strlen if Types,
    Split if Multithreading,
    Wait if Multithreading,
    Event if Events,
    EndEvent if Events,
    Key if Events,
    Mouse if Events,
    Param if Parameters,
}
