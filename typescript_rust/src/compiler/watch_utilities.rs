use std::io;

pub trait DirectoryStructureHost {
    fn file_exists(&self, path: &str) -> bool;
    fn read_file(&self, path: &str, encoding: Option<&str>) -> io::Result<Option<String>>;

    fn directory_exists(&self, path: &str) -> Option<bool> {
        None
    }
    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        None
    }
    fn read_directory(
        &self,
        path: &str,
        extensions: &[&str],
        exclude: Option<&[String]>,
        include: Option<&[String]>,
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        None
    }
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String> {
        None
    }

    fn create_directory(&self, path: &str) {}
    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {}
}
