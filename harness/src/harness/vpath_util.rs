pub mod vpath {
    use bitflags::bitflags;
    use typescript_rust::{
        combine_paths, compare_paths_case_insensitive, compare_paths_case_sensitive,
        get_base_file_name, get_directory_path, is_disk_path_root, normalize_slashes, resolve_path,
        Comparison,
    };

    pub fn normalize_separators(path: &str) -> String {
        normalize_slashes(path)
    }

    pub fn is_root(path: &str) -> bool {
        is_disk_path_root(path)
    }

    pub fn combine(path: &str, paths: &[Option<&str>]) -> String {
        combine_paths(path, paths)
    }

    pub fn resolve(path: &str, paths: &[Option<&str>]) -> String {
        resolve_path(path, paths)
    }

    pub fn compare_case_sensitive(a: &str, b: &str) -> Comparison {
        compare_paths_case_sensitive(a, b)
    }

    pub fn compare_case_insensitive(a: &str, b: &str) -> Comparison {
        compare_paths_case_insensitive(a, b)
    }

    pub fn dirname(path: &str) -> String {
        get_directory_path(path)
    }

    pub fn basename(path: &str, extensions: Option<&[&str]>, ignore_case: Option<bool>) -> String {
        get_base_file_name(path, extensions, ignore_case)
    }

    bitflags! {
        pub struct ValidationFlags: u32 {
            const None = 0;

            const RequireRoot = 1 << 0;
            const RequireDirname = 1 << 1;
            const RequireBasename = 1 << 2;
            const RequireExtname = 1 << 3;
            const RequireTrailingSeparator = 1 << 4;

            const AllowRoot = 1 << 5;
            const AllowDirname = 1 << 6;
            const AllowBasename = 1 << 7;
            const AllowExtname = 1 << 8;
            const AllowTrailingSeparator = 1 << 9;
            const AllowNavigation = 1 << 10;
            const AllowWildcard = 1 << 11;

            const Root = Self::RequireRoot.bits | Self::AllowRoot.bits | Self::AllowTrailingSeparator.bits;

            const Absolute = Self::RequireRoot.bits | Self::AllowRoot.bits | Self::AllowDirname.bits | Self::AllowBasename.bits | Self::AllowExtname.bits | Self::AllowTrailingSeparator.bits | Self::AllowNavigation.bits;

            const RelativeOrAbsolute = Self::AllowRoot.bits | Self::AllowDirname.bits | Self::AllowBasename.bits | Self::AllowExtname.bits | Self::AllowTrailingSeparator.bits | Self::AllowNavigation.bits;

            const Basename = Self::RequireBasename.bits | Self::AllowExtname.bits;
        }
    }

    pub fn validate(path: &str, flags: Option<ValidationFlags>) -> String {
        let flags = flags.unwrap_or(ValidationFlags::RelativeOrAbsolute);
        unimplemented!()
    }
}
