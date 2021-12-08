use std::env;

pub trait System {
    fn args(&self) -> &Vec<String>;
    fn get_current_directory(&self) -> String;
}

struct SystemConcrete {
    args: Vec<String>,
}

impl System for SystemConcrete {
    fn args(&self) -> &Vec<String> {
        &self.args
    }

    fn get_current_directory(&self) -> String {
        env::current_dir()
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap()
    }
}

lazy_static! {
    static ref SYS: SystemConcrete = SystemConcrete {
        args: env::args().collect(),
    };
}

pub fn get_sys() -> &'static impl System {
    &*SYS
}
