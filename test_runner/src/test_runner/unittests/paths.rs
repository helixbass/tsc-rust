#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use speculoos::prelude::*;

    use typescript_rust::{
        combine_paths, get_any_extension_from_path, get_base_file_name, get_directory_path,
        get_path_components, get_relative_path_from_directory, get_root_length,
        is_rooted_disk_path, is_url, normalize_slashes, reduce_path_components, resolve_path,
        to_file_name_lower_case,
    };

    #[test]
    fn test_normalize_slashes() {
        assert_eq!(normalize_slashes("a"), "a");
        assert_eq!(normalize_slashes("a/b"), "a/b");
        assert_eq!(normalize_slashes("a\\b"), "a/b");
        assert_eq!(normalize_slashes("\\\\server\\path"), "//server/path");
    }

    #[test]
    fn test_get_root_length() {
        assert_eq!(get_root_length("a"), 0);
        assert_eq!(get_root_length("/"), 1);
        assert_eq!(get_root_length("/path"), 1);
        assert_eq!(get_root_length("c:"), 2);
        assert_eq!(get_root_length("c:d"), 0);
        assert_eq!(get_root_length("c:/"), 3);
        assert_eq!(get_root_length("c:\\"), 3);
        assert_eq!(get_root_length("//server"), 8);
        assert_eq!(get_root_length("//server/share"), 9);
        assert_eq!(get_root_length("\\\\server"), 8);
        assert_eq!(get_root_length("\\\\server\\share"), 9);
        assert_eq!(get_root_length("file:///"), 8);
        assert_eq!(get_root_length("file:///path"), 8);
        assert_eq!(get_root_length("file:///c:"), 10);
        assert_eq!(get_root_length("file:///c:d"), 8);
        assert_eq!(get_root_length("file:///c:/path"), 11);
        assert_eq!(get_root_length("file:///c%3a"), 12);
        assert_eq!(get_root_length("file:///c%3ad"), 8);
        assert_eq!(get_root_length("file:///c%3a/path"), 13);
        assert_eq!(get_root_length("file:///c%3A"), 12);
        assert_eq!(get_root_length("file:///c%3Ad"), 8);
        assert_eq!(get_root_length("file:///c%3A/path"), 13);
        assert_eq!(get_root_length("file://localhost"), 16);
        assert_eq!(get_root_length("file://localhost/"), 17);
        assert_eq!(get_root_length("file://localhost/path"), 17);
        assert_eq!(get_root_length("file://localhost/c:"), 19);
        assert_eq!(get_root_length("file://localhost/c:d"), 17);
        assert_eq!(get_root_length("file://localhost/c:/path"), 20);
        assert_eq!(get_root_length("file://localhost/c%3a"), 21);
        assert_eq!(get_root_length("file://localhost/c%3ad"), 17);
        assert_eq!(get_root_length("file://localhost/c%3a/path"), 22);
        assert_eq!(get_root_length("file://localhost/c%3A"), 21);
        assert_eq!(get_root_length("file://localhost/c%3Ad"), 17);
        assert_eq!(get_root_length("file://localhost/c%3A/path"), 22);
        assert_eq!(get_root_length("file://server"), 13);
        assert_eq!(get_root_length("file://server/"), 14);
        assert_eq!(get_root_length("file://server/path"), 14);
        assert_eq!(get_root_length("file://server/c:"), 14);
        assert_eq!(get_root_length("file://server/c:d"), 14);
        assert_eq!(get_root_length("file://server/c:/d"), 14);
        assert_eq!(get_root_length("file://server/c%3a"), 14);
        assert_eq!(get_root_length("file://server/c%3ad"), 14);
        assert_eq!(get_root_length("file://server/c%3a/d"), 14);
        assert_eq!(get_root_length("file://server/c%3A"), 14);
        assert_eq!(get_root_length("file://server/c%3Ad"), 14);
        assert_eq!(get_root_length("file://server/c%3A/d"), 14);
        assert_eq!(get_root_length("http://server"), 13);
        assert_eq!(get_root_length("http://server/path"), 14);
    }

    #[test]
    fn test_is_url() {
        assert_that(&is_url("a")).is_false();
        assert_that(&is_url("/")).is_false();
        assert_that(&is_url("c:")).is_false();
        assert_that(&is_url("c:d")).is_false();
        assert_that(&is_url("c:/")).is_false();
        assert_that(&is_url("c:\\")).is_false();
        assert_that(&is_url("//server")).is_false();
        assert_that(&is_url("//server/share")).is_false();
        assert_that(&is_url("\\\\server")).is_false();
        assert_that(&is_url("\\\\server\\share")).is_false();
        assert_that(&is_url("file:///path")).is_true();
        assert_that(&is_url("file:///c:")).is_true();
        assert_that(&is_url("file:///c:d")).is_true();
        assert_that(&is_url("file:///c:/path")).is_true();
        assert_that(&is_url("file://server")).is_true();
        assert_that(&is_url("file://server/path")).is_true();
        assert_that(&is_url("http://server")).is_true();
        assert_that(&is_url("http://server/path")).is_true();
    }

    #[test]
    fn test_is_rooted_disk_path() {
        assert_that(&is_rooted_disk_path("a")).is_false();
        assert_that(&is_rooted_disk_path("/")).is_true();
        assert_that(&is_rooted_disk_path("c:")).is_true();
        assert_that(&is_rooted_disk_path("c:d")).is_false();
        assert_that(&is_rooted_disk_path("c:/")).is_true();
        assert_that(&is_rooted_disk_path("c:\\")).is_true();
        assert_that(&is_rooted_disk_path("//server")).is_true();
        assert_that(&is_rooted_disk_path("//server/share")).is_true();
        assert_that(&is_rooted_disk_path("\\\\server")).is_true();
        assert_that(&is_rooted_disk_path("\\\\server\\share")).is_true();
        assert_that(&is_rooted_disk_path("file:///path")).is_false();
        assert_that(&is_rooted_disk_path("file:///c:")).is_false();
        assert_that(&is_rooted_disk_path("file:///c:d")).is_false();
        assert_that(&is_rooted_disk_path("file:///c:/path")).is_false();
        assert_that(&is_rooted_disk_path("file://server")).is_false();
        assert_that(&is_rooted_disk_path("file://server/path")).is_false();
        assert_that(&is_rooted_disk_path("http://server")).is_false();
        assert_that(&is_rooted_disk_path("http://server/path")).is_false();
    }

    #[test]
    fn test_get_directory_path() {
        assert_eq!(&get_directory_path(""), "");
        assert_eq!(&get_directory_path("a"), "");
        assert_eq!(&get_directory_path("a/b"), "a");
        assert_eq!(&get_directory_path("/"), "/");
        assert_eq!(&get_directory_path("/a"), "/");
        assert_eq!(&get_directory_path("/a/"), "/");
        assert_eq!(&get_directory_path("/a/b"), "/a");
        assert_eq!(&get_directory_path("/a/b/"), "/a");
        assert_eq!(&get_directory_path("c:"), "c:");
        assert_eq!(&get_directory_path("c:d"), "");
        assert_eq!(&get_directory_path("c:/"), "c:/");
        assert_eq!(&get_directory_path("c:/path"), "c:/");
        assert_eq!(&get_directory_path("c:/path/"), "c:/");
        assert_eq!(&get_directory_path("//server"), "//server");
        assert_eq!(&get_directory_path("//server/"), "//server/");
        assert_eq!(&get_directory_path("//server/share"), "//server/");
        assert_eq!(&get_directory_path("//server/share/"), "//server/");
        assert_eq!(&get_directory_path("\\\\server"), "//server");
        assert_eq!(&get_directory_path("\\\\server\\"), "//server/");
        assert_eq!(&get_directory_path("\\\\server\\share"), "//server/");
        assert_eq!(&get_directory_path("\\\\server\\share\\"), "//server/");
        assert_eq!(&get_directory_path("file:///"), "file:///");
        assert_eq!(&get_directory_path("file:///path"), "file:///");
        assert_eq!(&get_directory_path("file:///path/"), "file:///");
        assert_eq!(&get_directory_path("file:///c:"), "file:///c:");
        assert_eq!(&get_directory_path("file:///c:d"), "file:///");
        assert_eq!(&get_directory_path("file:///c:/"), "file:///c:/");
        assert_eq!(&get_directory_path("file:///c:/path"), "file:///c:/");
        assert_eq!(&get_directory_path("file:///c:/path/"), "file:///c:/");
        assert_eq!(&get_directory_path("file://server"), "file://server");
        assert_eq!(&get_directory_path("file://server/"), "file://server/");
        assert_eq!(&get_directory_path("file://server/path"), "file://server/");
        assert_eq!(&get_directory_path("file://server/path/"), "file://server/");
        assert_eq!(&get_directory_path("http://server"), "http://server");
        assert_eq!(&get_directory_path("http://server/"), "http://server/");
        assert_eq!(&get_directory_path("http://server/path"), "http://server/");
        assert_eq!(&get_directory_path("http://server/path/"), "http://server/");
    }

    #[test]
    fn test_get_base_file_name() {
        assert_eq!(&get_base_file_name("", None, None), "");
        assert_eq!(&get_base_file_name("a", None, None), "a");
        assert_eq!(&get_base_file_name("a/", None, None), "a");
        assert_eq!(&get_base_file_name("/", None, None), "");
        assert_eq!(&get_base_file_name("/a", None, None), "a");
        assert_eq!(&get_base_file_name("/a/", None, None), "a");
        assert_eq!(&get_base_file_name("/a/b", None, None), "b");
        assert_eq!(&get_base_file_name("c:", None, None), "");
        assert_eq!(&get_base_file_name("c:d", None, None), "c:d");
        assert_eq!(&get_base_file_name("c:/", None, None), "");
        assert_eq!(&get_base_file_name("c:\\", None, None), "");
        assert_eq!(&get_base_file_name("c:/path", None, None), "path");
        assert_eq!(&get_base_file_name("c:/path/", None, None), "path");
        assert_eq!(&get_base_file_name("//server", None, None), "");
        assert_eq!(&get_base_file_name("//server/", None, None), "");
        assert_eq!(&get_base_file_name("//server/share", None, None), "share");
        assert_eq!(&get_base_file_name("//server/share/", None, None), "share");
        assert_eq!(&get_base_file_name("file:///", None, None), "");
        assert_eq!(&get_base_file_name("file:///path", None, None), "path");
        assert_eq!(&get_base_file_name("file:///path/", None, None), "path");
        assert_eq!(&get_base_file_name("file:///c:", None, None), "");
        assert_eq!(&get_base_file_name("file:///c:/", None, None), "");
        assert_eq!(&get_base_file_name("file:///c:d", None, None), "c:d");
        assert_eq!(&get_base_file_name("file:///c:/d", None, None), "d");
        assert_eq!(&get_base_file_name("file:///c:/d/", None, None), "d");
        assert_eq!(&get_base_file_name("http://server", None, None), "");
        assert_eq!(&get_base_file_name("http://server/", None, None), "");
        assert_eq!(&get_base_file_name("http://server/a", None, None), "a");
        assert_eq!(&get_base_file_name("http://server/a/", None, None), "a");
        assert_eq!(
            &get_base_file_name("/path/a.ext", Some(&[".ext"]), Some(false)),
            "a"
        );
        assert_eq!(
            &get_base_file_name("/path/a.ext", Some(&[".EXT"]), Some(true)),
            "a"
        );
        assert_eq!(
            &get_base_file_name("/path/a.ext", Some(&["ext"]), Some(false)),
            "a"
        );
        assert_eq!(
            &get_base_file_name("/path/a.b", Some(&[".ext"]), Some(false)),
            "a.b"
        );
        assert_eq!(
            &get_base_file_name("/path/a.b", Some(&[".b", ".c"]), Some(false)),
            "a"
        );
        assert_eq!(
            &get_base_file_name("/path/a.c", Some(&[".b", ".c"]), Some(false)),
            "a"
        );
        assert_eq!(
            &get_base_file_name("/path/a.d", Some(&[".b", ".c"]), Some(false)),
            "a.d"
        );
    }

    #[test]
    fn test_get_any_extension_from_path() {
        assert_eq!(
            &get_any_extension_from_path("", Option::<&[&str]>::None, None),
            ""
        );
        assert_eq!(
            &get_any_extension_from_path(".ext", Option::<&[&str]>::None, None),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.ext", Option::<&[&str]>::None, None),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("/a.ext", Option::<&[&str]>::None, None),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.ext/", Option::<&[&str]>::None, None),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.ext", Some(&[".ext"]), Some(false)),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.ext", Some(&[".EXT"]), Some(true)),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.ext", Some(&["ext"]), Some(false)),
            ".ext"
        );
        assert_eq!(
            &get_any_extension_from_path("a.b", Some(&[".ext"]), Some(false)),
            ""
        );
        assert_eq!(
            &get_any_extension_from_path("a.b", Some(&[".b", ".c"]), Some(false)),
            ".b"
        );
        assert_eq!(
            &get_any_extension_from_path("a.c", Some(&[".b", ".c"]), Some(false)),
            ".c"
        );
        assert_eq!(
            &get_any_extension_from_path("a.d", Some(&[".b", ".c"]), Some(false)),
            ""
        );
    }

    fn check_get_path_components(args: (&str, Option<&str>), expected: &[&str]) {
        assert_that(&get_path_components(args.0, args.1)).equals_iterator(
            &expected
                .into_iter()
                .map(|str_| (*str_).to_owned())
                .collect::<Vec<_>>()
                .iter(),
        );
    }

    #[test]
    fn test_get_path_components() {
        check_get_path_components(("", None), &[""]);
        check_get_path_components(("a", None), &["", "a"]);
        check_get_path_components(("./a", None), &["", ".", "a"]);
        check_get_path_components(("/", None), &["/"]);
        check_get_path_components(("/a", None), &["/", "a"]);
        check_get_path_components(("/a/", None), &["/", "a"]);
        check_get_path_components(("c:", None), &["c:"]);
        check_get_path_components(("c:d", None), &["", "c:d"]);
        check_get_path_components(("c:/", None), &["c:/"]);
        check_get_path_components(("c:/path", None), &["c:/", "path"]);
        check_get_path_components(("//server", None), &["//server"]);
        check_get_path_components(("//server/", None), &["//server/"]);
        check_get_path_components(("//server/share", None), &["//server/", "share"]);
        check_get_path_components(("file:///", None), &["file:///"]);
        check_get_path_components(("file:///path", None), &["file:///", "path"]);
        check_get_path_components(("file:///c:", None), &["file:///c:"]);
        check_get_path_components(("file:///c:d", None), &["file:///", "c:d"]);
        check_get_path_components(("file:///c:/", None), &["file:///c:/"]);
        check_get_path_components(("file:///c:/path", None), &["file:///c:/", "path"]);
        check_get_path_components(("file://server", None), &["file://server"]);
        check_get_path_components(("file://server/", None), &["file://server/"]);
        check_get_path_components(("file://server/path", None), &["file://server/", "path"]);
        check_get_path_components(("http://server", None), &["http://server"]);
        check_get_path_components(("http://server/", None), &["http://server/"]);
        check_get_path_components(("http://server/path", None), &["http://server/", "path"]);
    }

    fn check_reduce_path_components(components: &[&str], expected: &[&str]) {
        assert_that(&reduce_path_components(
            &components
                .into_iter()
                .map(|str_| (*str_).to_owned())
                .collect::<Vec<_>>(),
        ))
        .equals_iterator(
            &expected
                .into_iter()
                .map(|str_| (*str_).to_owned())
                .collect::<Vec<_>>()
                .iter(),
        );
    }

    #[test]
    fn test_reduce_path_components() {
        check_reduce_path_components(&[], &[]);
        check_reduce_path_components(&[""], &[""]);
        check_reduce_path_components(&["", "."], &[""]);
        check_reduce_path_components(&["", ".", "a"], &["", "a"]);
        check_reduce_path_components(&["", "a", "."], &["", "a"]);
        check_reduce_path_components(&["", ".."], &["", ".."]);
        check_reduce_path_components(&["", "..", ".."], &["", "..", ".."]);
        check_reduce_path_components(&["", "..", ".", ".."], &["", "..", ".."]);
        check_reduce_path_components(&["", "a", ".."], &[""]);
        check_reduce_path_components(&["", "..", "a"], &["", "..", "a"]);
        check_reduce_path_components(&["/"], &["/"]);
        check_reduce_path_components(&["/", "."], &["/"]);
        check_reduce_path_components(&["/", ".."], &["/"]);
        check_reduce_path_components(&["/", "a", ".."], &["/"]);
    }

    fn check_combine_paths(args: (&str, &str), expected: &str) {
        assert_that(&combine_paths(args.0, &[Some(args.1)])).is_equal_to(expected.to_owned());
    }

    #[test]
    fn test_combine_paths() {
        check_combine_paths(("/", "/node_modules/@types"), "/node_modules/@types");
        check_combine_paths(("/a/..", ""), "/a/..");
        check_combine_paths(("/a/..", "b"), "/a/../b");
        check_combine_paths(("/a/..", "b/"), "/a/../b/");
        check_combine_paths(("/a/..", "/"), "/");
        check_combine_paths(("/a/..", "/b"), "/b");
    }

    fn check_resolve_path<const TPathsLen: usize>(args: (&str, [&str; TPathsLen]), expected: &str) {
        assert_that(&resolve_path(
            args.0,
            &args.1.into_iter().map(Option::Some).collect_vec(),
        ))
        .is_equal_to(expected.to_owned());
    }

    #[test]
    fn test_resolve_path() {
        check_resolve_path(("", []), "");
        check_resolve_path((".", []), "");
        check_resolve_path(("./", []), "");
        check_resolve_path(("..", []), "..");
        check_resolve_path(("../", []), "../");
        check_resolve_path(("/", []), "/");
        check_resolve_path(("/.", []), "/");
        check_resolve_path(("/./", []), "/");
        check_resolve_path(("/../", []), "/");
        check_resolve_path(("/a", []), "/a");
        check_resolve_path(("/a/", []), "/a/");
        check_resolve_path(("/a/.", []), "/a");
        check_resolve_path(("/a/./", []), "/a/");
        check_resolve_path(("/a/./b", []), "/a/b");
        check_resolve_path(("/a/./b/", []), "/a/b/");
        check_resolve_path(("/a/..", []), "/");
        check_resolve_path(("/a/../", []), "/");
        check_resolve_path(("/a/../b", []), "/b");
        check_resolve_path(("/a/../b/", []), "/b/");
        check_resolve_path(("/a/..", ["b"]), "/b");
        check_resolve_path(("/a/..", ["/"]), "/");
        check_resolve_path(("/a/..", ["b/"]), "/b/");
        check_resolve_path(("/a/..", ["/b"]), "/b");
        check_resolve_path(("/a/.", ["b"]), "/a/b");
        check_resolve_path(("/a/.", ["."]), "/a");
        check_resolve_path(("a", ["b", "c"]), "a/b/c");
        check_resolve_path(("a", ["b", "/c"]), "/c");
        check_resolve_path(("a", ["b", "../c"]), "a/c");
    }

    fn check_get_path_relative_to(args: (&str, &str, bool), expected: &str) {
        assert_that(&get_relative_path_from_directory(
            args.0,
            args.1,
            Option::<fn(&str) -> String>::None,
            Some(args.2),
        ))
        .is_equal_to(expected.to_owned());
    }

    #[test]
    fn test_get_path_relative_to() {
        check_get_path_relative_to(("/", "/", false), "");
        check_get_path_relative_to(("/a", "/a", false), "");
        check_get_path_relative_to(("/a/", "/a", false), "");
        check_get_path_relative_to(("/a", "/", false), "..");
        check_get_path_relative_to(("/a", "/b", false), "../b");
        check_get_path_relative_to(("/a/b", "/b", false), "../../b");
        check_get_path_relative_to(("/a/b/c", "/b", false), "../../../b");
        check_get_path_relative_to(("/a/b/c", "/b/c", false), "../../../b/c");
        check_get_path_relative_to(("/a/b/c", "/a/b", false), "..");
        check_get_path_relative_to(("c:", "d:", false), "d:/");
        check_get_path_relative_to(("file:///", "file:///", false), "");
        check_get_path_relative_to(("file:///a", "file:///a", false), "");
        check_get_path_relative_to(("file:///a/", "file:///a", false), "");
        check_get_path_relative_to(("file:///a", "file:///", false), "..");
        check_get_path_relative_to(("file:///a", "file:///b", false), "../b");
        check_get_path_relative_to(("file:///a/b", "file:///b", false), "../../b");
        check_get_path_relative_to(("file:///a/b/c", "file:///b", false), "../../../b");
        check_get_path_relative_to(("file:///a/b/c", "file:///b/c", false), "../../../b/c");
        check_get_path_relative_to(("file:///a/b/c", "file:///a/b", false), "..");
        check_get_path_relative_to(("file:///c:", "file:///d:", false), "file:///d:/");
    }

    fn check_to_file_name_lower_case(x: &str, expected: &str) {
        assert_that(&to_file_name_lower_case(x)).is_equal_to(expected.to_owned());
    }

    #[test]
    fn test_to_file_name_lower_case() {
        check_to_file_name_lower_case(
            "/user/UserName/projects/Project/file.ts",
            "/user/username/projects/project/file.ts",
        );
        check_to_file_name_lower_case(
            "/user/UserName/projects/projectß/file.ts",
            "/user/username/projects/projectß/file.ts",
        );
        check_to_file_name_lower_case(
            "/user/UserName/projects/İproject/file.ts",
            "/user/username/projects/İproject/file.ts",
        );
        check_to_file_name_lower_case(
            "/user/UserName/projects/ı/file.ts",
            "/user/username/projects/ı/file.ts",
        );
    }
}
