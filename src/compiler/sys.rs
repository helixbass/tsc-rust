use std::{env, fs};

pub trait System {
    fn args(&self) -> &Vec<String>;
    fn read_file(&self, path: &str) -> Option<String>;
    fn get_current_directory(&self) -> String;
}

struct SystemConcrete {
    args: Vec<String>,
}

impl SystemConcrete {
    fn read_file_worker(&self, file_name: &str) -> Option<String> {
        fs::read_to_string(file_name).ok()
    }
}

impl System for SystemConcrete {
    fn args(&self) -> &Vec<String> {
        &self.args
    }

    fn read_file(&self, file_name: &str) -> Option<String> {
        self.read_file_worker(file_name)
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
