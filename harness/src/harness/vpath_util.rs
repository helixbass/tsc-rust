pub mod vpath {
    use bitflags::bitflags;
    use fancy_regex::Regex as FancyRegex;
    use regex::Regex;
    use typescript_rust::{
        change_any_extension, combine_paths, compare_paths_case_insensitive,
        compare_paths_case_sensitive, directory_separator, directory_separator_str,
        ensure_trailing_directory_separator, file_extension_is_one_of, get_any_extension_from_path,
        get_base_file_name, get_directory_path, get_path_components, get_path_from_path_components,
        get_relative_path_from_directory, has_js_file_extension, has_trailing_directory_separator,
        has_ts_file_extension, is_disk_path_root, normalize_slashes, reduce_path_components,
        resolve_path, Comparison, Extension,
    };

    pub const sep: char = directory_separator;
    pub const sep_str: &'static str = directory_separator_str;

    pub fn normalize_separators(path: &str) -> String {
        normalize_slashes(path)
    }

    pub fn is_root(path: &str) -> bool {
        is_disk_path_root(path)
    }

    pub fn has_trailing_separator(path: &str) -> bool {
        has_trailing_directory_separator(path)
    }

    pub fn add_trailing_separator(path: &str) -> String {
        ensure_trailing_directory_separator(path)
    }

    pub fn combine(path: &str, paths: &[Option<&str>]) -> String {
        combine_paths(path, paths)
    }

    pub fn parse(path: &str, current_directory: Option<&str>) -> Vec<String> {
        get_path_components(path, current_directory)
    }

    pub fn reduce(components: &[String]) -> Vec<String> {
        reduce_path_components(components)
    }

    pub fn format(path_components: &[String]) -> String {
        get_path_from_path_components(path_components)
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

    pub fn extname<TExtension: AsRef<str>>(
        path: &str,
        extensions: Option<&[TExtension]>,
        ignore_case: Option<bool>,
    ) -> String {
        get_any_extension_from_path(path, extensions, ignore_case)
    }

    pub fn relative<TGetCanonicalFileName: Fn(&str) -> String>(
        from_directory: &str,
        to: &str,
        get_canonical_file_name: Option<TGetCanonicalFileName>,
        ignore_case: Option<bool>,
    ) -> String {
        get_relative_path_from_directory(from_directory, to, get_canonical_file_name, ignore_case)
    }

    pub fn change_extension<TExtension: AsRef<str>>(
        path: &str,
        ext: &str,
        extensions: Option<&[TExtension]>,
        ignore_case: Option<bool>,
    ) -> String {
        change_any_extension(path, ext, extensions, ignore_case)
    }

    pub fn is_type_script(file_name: &str) -> bool {
        has_ts_file_extension(file_name)
    }

    pub fn is_java_script(file_name: &str) -> bool {
        has_js_file_extension(file_name)
    }

    lazy_static! {
        static ref invalid_root_component_reg_exp: FancyRegex =
            FancyRegex::new(r"^(?!(/|//\w+/|[a-zA-Z]:/?|)$)").unwrap();
        static ref invalid_navigable_component_reg_exp: Regex = Regex::new(r#"[:*?"<>|]"#).unwrap();
        static ref invalid_navigable_component_with_wildcards_reg_exp: Regex =
            Regex::new(r#"[:"<>|]"#).unwrap();
        static ref invalid_non_navigable_component_with_wildcards_reg_exp: Regex =
            Regex::new(r#"^\.{1,2}$|[:*?"<>|]"#).unwrap();
        static ref invalid_non_navigable_component_reg_exp: Regex =
            Regex::new(r#"^\.{1,2}$|[:"<>|]"#).unwrap();
        static ref ext_reg_exp: Regex = Regex::new(r"\.\w+$").unwrap();
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

    fn validate_components(
        components: &[String],
        mut flags: ValidationFlags,
        has_trailing_separator: bool,
    ) -> bool {
        let has_root = !components[0].is_empty();
        let has_dirname = components.len() > 2;
        let has_basename = components.len() > 1;
        let has_extname = has_basename && ext_reg_exp.is_match(&components[components.len() - 1]);
        let invalid_component_reg_exp: &Regex =
            if flags.intersects(ValidationFlags::AllowNavigation) {
                if flags.intersects(ValidationFlags::AllowWildcard) {
                    &invalid_navigable_component_with_wildcards_reg_exp
                } else {
                    &invalid_navigable_component_reg_exp
                }
            } else {
                if flags.intersects(ValidationFlags::AllowWildcard) {
                    &invalid_non_navigable_component_with_wildcards_reg_exp
                } else {
                    &invalid_non_navigable_component_reg_exp
                }
            };

        if flags.intersects(ValidationFlags::RequireRoot) && !has_root {
            return false;
        }
        if flags.intersects(ValidationFlags::RequireDirname) && !has_dirname {
            return false;
        }
        if flags.intersects(ValidationFlags::RequireBasename) && !has_basename {
            return false;
        }
        if flags.intersects(ValidationFlags::RequireExtname) && !has_extname {
            return false;
        }
        if flags.intersects(ValidationFlags::RequireTrailingSeparator) && !has_trailing_separator {
            return false;
        }

        if flags.intersects(ValidationFlags::RequireRoot) {
            flags |= ValidationFlags::AllowRoot;
        }
        if flags.intersects(ValidationFlags::RequireDirname) {
            flags |= ValidationFlags::AllowDirname;
        }
        if flags.intersects(ValidationFlags::RequireBasename) {
            flags |= ValidationFlags::AllowBasename;
        }
        if flags.intersects(ValidationFlags::RequireExtname) {
            flags |= ValidationFlags::AllowExtname;
        }
        if flags.intersects(ValidationFlags::RequireTrailingSeparator) {
            flags |= ValidationFlags::AllowTrailingSeparator;
        }

        if (!flags).intersects(ValidationFlags::AllowRoot) && has_root {
            return false;
        }
        if (!flags).intersects(ValidationFlags::AllowDirname) && has_dirname {
            return false;
        }
        if (!flags).intersects(ValidationFlags::AllowBasename) && has_basename {
            return false;
        }
        if (!flags).intersects(ValidationFlags::AllowExtname) && has_extname {
            return false;
        }
        if (!flags).intersects(ValidationFlags::AllowTrailingSeparator) && has_trailing_separator {
            return false;
        }

        if invalid_root_component_reg_exp
            .is_match(&components[0])
            .unwrap()
        {
            return false;
        }
        for component in components.into_iter().skip(1) {
            if invalid_component_reg_exp.is_match(component) {
                return false;
            }
        }

        true
    }

    pub fn validate(path: &str, flags: Option<ValidationFlags>) -> String {
        let flags = flags.unwrap_or(ValidationFlags::RelativeOrAbsolute);
        let components = parse(path, None);
        let trailing = has_trailing_separator(path);
        if !validate_components(&components, flags, trailing) {
            // throw vfs.createIOError("ENOENT");
            panic!("ENOENT");
        }
        if components.len() > 1 && trailing {
            format!("{}{}", format(&reduce(&components)), sep,)
        } else {
            format(&reduce(&components))
        }
    }

    pub fn is_declaration(path: &str) -> bool {
        file_extension_is_one_of(path, &[Extension::Dmts, Extension::Dcts, Extension::Dts])
    }

    pub fn is_source_map(path: &str) -> bool {
        !extname(path, Some(&[".map"]), Some(false)).is_empty()
    }

    pub fn is_json(path: &str) -> bool {
        !extname(path, Some(&[".json"]), Some(false)).is_empty()
    }
}
