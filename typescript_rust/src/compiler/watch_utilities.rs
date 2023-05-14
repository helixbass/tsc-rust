use gc::{Finalize, Trace};
use std::io;

pub trait DirectoryStructureHost: Trace + Finalize {
    fn file_exists(&self, path: &str) -> bool;
    fn read_file(&self, path: &str, encoding: Option<&str>) -> io::Result<Option<String>>;

    fn directory_exists(&self, _path: &str) -> Option<bool> {
        None
    }
    fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
        None
    }
    fn read_directory(
        &self,
        _path: &str,
        _extensions: &[&str],
        _exclude: Option<&[String]>,
        _include: Option<&[String]>,
        _depth: Option<usize>,
    ) -> Option<io::Result<Vec<String>>> {
        None
    }
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, _path: &str) -> Option<String> {
        None
    }

    fn create_directory(&self, _path: &str) -> io::Result<()> {
        Ok(())
    }
    fn write_file(
        &self,
        _path: &str,
        _data: &str,
        _write_byte_order_mark: Option<bool>,
    ) -> io::Result<()> {
        Ok(())
    }
}
