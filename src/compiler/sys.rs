use std::env;

pub struct System {
    pub args: Vec<String>,
}

lazy_static! {
    pub static ref SYS: System = System {
        args: env::args().collect(),
    };
}
