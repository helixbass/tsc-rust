pub mod vpath {
    use typescript_rust::{combine_paths, get_base_file_name, normalize_slashes};

    pub fn normalize_separators(path: &str) -> String {
        normalize_slashes(path)
    }

    pub fn combine(path: &str, paths: &[Option<&str>]) -> String {
        combine_paths(path, paths)
    }

    pub fn basename(path: &str, extensions: Option<&[&str]>, ignore_case: Option<bool>) -> String {
        get_base_file_name(path, extensions, ignore_case)
    }
}
