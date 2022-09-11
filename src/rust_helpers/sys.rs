use std::env;

/*const*/
pub fn is_windows() -> bool {
    env::consts::OS == "windows"
}

pub fn process_cwd() -> String {
    env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap()
}
