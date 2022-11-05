pub mod vpath {
    use typescript_rust::{get_base_file_name, normalize_slashes};

    pub fn normalize_separators(path: &str) -> String {
        normalize_slashes(path)
    }

    pub fn basename(path: &str, extensions: Option<&[&str]>, ignore_case: Option<bool>) -> String {
        get_base_file_name(path, extensions, ignore_case)
    }
}
