macro_rules! keyword_enum {
    ($($kw:ident),+ $(,)?) => {
        #[derive(Debug, PartialEq)]
        #[derive(Clone, Copy)]
        pub enum Keyword {
            $($kw,)+
        }

        impl Keyword {
            pub fn all() -> Vec<Keyword> {
                vec![
                    $(Keyword::$kw,)+
                ]
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
}
