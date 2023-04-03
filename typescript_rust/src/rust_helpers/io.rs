use std::io;

pub fn io_error_from_name<TSuccess>(name: &'static str) -> io::Result<TSuccess> {
    Err(io::Error::new(
        match name {
            "ENOENT" => io::ErrorKind::NotFound,
            // "EISDIR" => io::ErrorKind::IsADirectory,
            // "EBADF => io::ErrorKind::Uncategorized,
            // _ => io::ErrorKind::Uncategorized,
            _ => io::ErrorKind::Other,
        },
        name,
    ))
}
