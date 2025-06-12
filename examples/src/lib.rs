use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Group {
    /// Basic examples
    Basic,
    /// Advanced examples
    Advanced,
    /// Fractals and other cool stuff
    Fractals,
    /// Showcasing some features of turtle
    Features,
}

impl Group {
    pub fn all() -> Vec<Self> {
        vec![
            Group::Basic,
            Group::Advanced,
            Group::Fractals,
            Group::Features,
        ]
    }

    pub fn examples(&self) -> Vec<Example> {
        Example::all()
            .into_iter()
            .filter(|e| e.group == *self)
            .collect()
    }
}

impl Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Group::Basic => write!(f, "Basic examples"),
            Group::Advanced => write!(f, "Advanced examples"),
            Group::Fractals => write!(f, "Fractals"),
            Group::Features => write!(f, "Feature showcases"),
        }
    }
}

pub struct Example {
    pub name: &'static str,
    pub summary: &'static str,
    pub description: &'static str,
    pub code: &'static str,
    pub group: Group,
}

macro_rules! examples {
    ($(
        #[doc = $first_doc:expr]
        $(#[doc = $doc:expr])*
        $const:ident ( $file:literal as $name:literal ) $group:ident
    ),+ $(,)?) => {
        impl Example {
            $(
                pub const $const: Example = Example {
                    name: $name,
                    summary: $first_doc,
                    description: concat!(
                        $first_doc,
                        $("\n", $doc,)*
                    ),
                    code: include_str!($file),
                    group: Group::$group,
                };
            )+

            pub fn all() -> Vec<Example> {
                vec![
                    $(Self::$const),+
                ]
            }
        }
    };
}

examples! {

    // ##########################
    //           Basic
    // ##########################

    /// A simple circle
    CIRCLE("../circle.tg" as "Circle") Basic,
    /// A spiral of squares
    SPIRAL("../spirale.tg" as "Spiral") Basic,
    /// Random strokes in a radial arrangement
    ///
    /// It looks a bit like an eye
    RANDOM("../random.tg" as "Random Strokes") Basic,

    // ##########################
    //          Advanced
    // ##########################

    /// A diagramm plotting the sinus and cosinus functions
    SIN_COS("../sin_cos.tg" as "Sinus & Cosinus Plot") Advanced,
    /// A turtle trying not to fall of the edge of the canvas
    RAND_PATH("../rand_path.tg" as "Random Path") Advanced,
    /// A diagram showing the architecture of turtle itself
    DIAGRAM("../pap.tg" as "Turtle UML") Advanced,

    // ##########################
    //          Fractals
    // ##########################

    /// The Pythagoras tree
    PYTH_TREE("../pyth_frak.tg" as "Pythagoras tree") Fractals,
    /// A parallel version of the Pythagoras tree
    PYTH_TREE_SPLIT("../pyth_frak_split.tg" as "Pythagoras tree (Parallel)") Fractals,
    /// The dragon curve
    DRAGON_CURVE("../drachenkurve.tg" as "Dragon curve") Fractals,
    /// Multiple Koch snowflakes inside each other
    SNOWFLAKE("../koch_kurve.tg" as "Koch snowflake") Fractals,
    /// The Lévy C curve
    LEVY_C_CURVE("../levy_c.tg" as "Lévy C curve") Fractals,
    /// The Sierpiński triangle
    SIERPINSKI("../dreieck.tg" as "Sierpiński triangle") Fractals,

    // ##########################
    //          Features
    // ##########################

    /// Static typing for the win!
    TYPES("../types.tg" as "Types") Features,
    /// Let it draw faster!
    MULTITHREADING("../multithreading.tg" as "Multithreading") Features,
    /// Now with user interaction!
    EVENTS("../events.tg" as "Events") Features,
    /// Move the turtle with key events
    KEY_CONTROL("../button_control.tg" as "Key control") Features,
    /// Move the turtle with mouse events
    BUTTON_CONTROL("../mouse_control.tg" as "Mouse control") Features,
    /// Easier Parameters
    PARAMETERS("../params.tg" as "Parameters") Features,
}
