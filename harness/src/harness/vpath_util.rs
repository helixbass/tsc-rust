pub mod vpath {
    use typescript_rust::{
        combine_paths, get_base_file_name, get_directory_path, normalize_slashes, resolve_path,
    };

    pub fn normalize_separators(path: &str) -> String {
        normalize_slashes(path)
    }

    pub fn combine(path: &str, paths: &[Option<&str>]) -> String {
        combine_paths(path, paths)
    }

    pub fn resolve(path: &str, paths: &[Option<&str>]) -> String {
        resolve_path(path, paths)
    }

    pub fn dirname(path: &str) -> String {
        get_directory_path(path)
    }

    pub fn basename(path: &str, extensions: Option<&[&str]>, ignore_case: Option<bool>) -> String {
        get_base_file_name(path, extensions, ignore_case)
    }
}
