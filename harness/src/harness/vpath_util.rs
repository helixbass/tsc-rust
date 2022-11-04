pub mod vpath {
    use typescript_rust::get_base_file_name;

    pub fn basename(path: &str, extensions: Option<&[&str]>, ignore_case: Option<bool>) -> String {
        get_base_file_name(path, extensions, ignore_case)
    }
}
