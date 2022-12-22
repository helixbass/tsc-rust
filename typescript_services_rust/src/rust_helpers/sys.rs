use std::env;

pub fn os_platform() -> &'static str {
    match env::consts::OS {
        "macos" => "darwin",
        "linux" => "linux",
        "windows" => "win32",
        _ => unimplemented!()
    }
}
