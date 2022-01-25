use regex::Regex;
use std::cmp;
use std::convert::{TryFrom, TryInto};

use crate::{compare_values, last_or_undefined, some, CharacterCodes, Comparison, Debug_, Path};

#[allow(non_upper_case_globals)]
pub const directory_separator: char = '/';

#[allow(non_upper_case_globals)]
pub const directory_separator_str: &str = "/";

#[allow(non_upper_case_globals)]
pub const alt_directory_separator: char = '\\';

#[allow(non_upper_case_globals)]
pub const url_scheme_separator: &str = "://";

lazy_static! {
    static ref backslash_reg_exp: Regex = Regex::new(r"\\"/*/g*/).unwrap();
}

pub fn is_any_directory_separator(char_code: char) -> bool {
    matches!(char_code, CharacterCodes::slash | CharacterCodes::backslash)
}

pub fn is_url(path: &str) -> bool {
    get_encoded_root_length(path) < 0
}

pub fn is_rooted_disk_path(path: &str) -> bool {
    get_encoded_root_length(path) > 0
}

pub fn is_disk_path_root(path: &str) -> bool {
    let root_length = get_encoded_root_length(path);
    root_length > 0 && usize::try_from(root_length).unwrap() == path.len()
}

pub fn path_is_absolute(path: &str) -> bool {
    get_encoded_root_length(path) != 0
}

pub fn path_is_relative(path: &str) -> bool {
    lazy_static! {
        static ref regex: Regex = Regex::new(r"^\.\.?($|[\\/])").unwrap();
    }
    regex.is_match(path)
}

pub fn path_is_bare_specifier(path: &str) -> bool {
    !path_is_absolute(path) && !path_is_relative(path)
}

pub fn has_extension(file_name: &str) -> bool {
    string_contains(get_base_file_name(file_name, None, None), ".")
}

pub fn file_extension_is(path: &str, extension: &str) -> bool {
    path.len() > extension.len() && ends_with(path, extension)
}

pub fn file_extension_is_one_of(path: &str, extensions: &[&str]) -> bool {
    for extension in extensions {
        if file_extension_is(path, extension) {
            return true;
        }
    }

    false
}

pub fn has_trailing_directory_separator(path: &str) -> bool {
    !path.is_empty() && is_any_directory_separator(path.chars().last().unwrap())
}

fn is_volume_character(char_code: char) -> bool {
    char_code >= CharacterCodes::a && char_code <= CharacterCodes::z
        || char_code >= CharacterCodes::A && char_code <= CharacterCodes::Z
}

fn get_file_url_volume_separator_end(url: &[char], start: usize) -> isize {
    let ch0 = url.get(start).map(|ch| *ch);
    if matches!(ch0, Some(CharacterCodes::colon)) {
        return (start + 1).try_into().unwrap();
    }
    if matches!(ch0, Some(CharacterCodes::percent))
        && matches!(url.get(start + 1).map(|ch| *ch), Some(CharacterCodes::_3))
    {
        let ch2 = url.get(start + 2).map(|ch| *ch);
        if matches!(ch2, Some(CharacterCodes::a | CharacterCodes::A)) {
            return (start + 3).try_into().unwrap();
        }
    }
    -1
}

fn get_encoded_root_length(path: &str) -> isize {
    let path_str = path;
    let path: Vec<char> = path_str.chars().collect();
    if path.is_empty() {
        return 0;
    }
    let ch0 = path[0];

    if matches!(ch0, CharacterCodes::slash | CharacterCodes::backslash) {
        if path.get(1).map(|ch| *ch) != Some(ch0) {
            return 1;
        }

        let p1 = path
            .iter()
            .skip(2)
            .position(|ch| {
                *ch == if ch0 == CharacterCodes::slash {
                    directory_separator
                } else {
                    alt_directory_separator
                }
            })
            .map(|position| position + 2);
        if p1.is_none() {
            return path.len().try_into().unwrap();
        }
        let p1 = p1.unwrap();

        return (p1 + 1).try_into().unwrap();
    }

    if is_volume_character(ch0) && matches!(path.get(1).map(|ch| *ch), Some(CharacterCodes::colon))
    {
        let ch2 = path.get(2).map(|ch| *ch);
        if matches!(ch2, Some(CharacterCodes::slash | CharacterCodes::backslash)) {
            return 3;
        }
        if path.len() == 2 {
            return 2;
        }
    }

    let scheme_end = path_str
        .find(url_scheme_separator)
        .map(|byte_index| path.len() - path_str[byte_index..].chars().count());
    if let Some(scheme_end) = scheme_end {
        let authority_start = scheme_end + url_scheme_separator.len() /*byte length == chars length in this case*/;
        let authority_end = path
            .iter()
            .skip(authority_start)
            .position(|ch| *ch == directory_separator);
        if let Some(authority_end) = authority_end {
            let scheme: String = path[0..scheme_end].iter().collect();
            let authority: String = path[authority_start..authority_end].iter().collect();
            if &scheme == "file"
                && (&authority == "" || &authority == "localhost")
                && matches!(path.get(authority_end + 1), Some(ch) if is_volume_character(*ch))
            {
                let volume_separator_end =
                    get_file_url_volume_separator_end(&path, authority_end + 2);
                if volume_separator_end != -1 {
                    let volume_separator_end_as_usize: usize =
                        volume_separator_end.try_into().unwrap();
                    if matches!(
                        path.get(volume_separator_end_as_usize).map(|ch| *ch),
                        Some(CharacterCodes::slash)
                    ) {
                        return !(volume_separator_end + 1);
                    }
                    if volume_separator_end_as_usize == path.len() {
                        return !volume_separator_end;
                    }
                }
            }
            return (!(authority_end + 1)).try_into().unwrap();
        }
        return !isize::try_from(path.len()).unwrap();
    }

    0
}

fn get_root_length(path: &str) -> usize {
    let root_length = get_encoded_root_length(path);
    if root_length < 0 {
        (!root_length).try_into().unwrap()
    } else {
        root_length.try_into().unwrap()
    }
}

pub fn get_directory_path(path: &str) -> String {
    let mut path = normalize_slashes(path);

    let root_length = get_root_length(&path);
    if root_length == path.len() {
        return path.to_string();
    }

    path = remove_trailing_directory_separator(&path);
    path[0..cmp::max(root_length, path.rfind(directory_separator).unwrap_or(0))].to_string()
}

pub fn get_base_file_name(
    path: &str,
    extensions: Option<&[&str]>,
    ignore_case: Option<bool>,
) -> String {
    let mut path = normalize_slashes(path);

    let root_length = get_root_length(&path);
    if root_length == path.len() {
        return "".to_string();
    }

    path = remove_trailing_directory_separator(&path);
    let name = &path[cmp::max(
        get_root_length(&path),
        path.rfind(directory_separator).map_or(0, |index| index + 1),
    )..];
    let extension = match (extensions, ignore_case) {
        (Some(extensions), Some(ignore_case)) => Some(get_any_extension_from_path(
            name,
            Some(extensions),
            Some(ignore_case),
        )),
        _ => None,
    };
    match extension {
        Some(extension) => name[0..name.len() - extension.len()].to_string(),
        None => name.to_string(),
    }
}

fn try_get_extension_from_path<TStringEqualityComparer: FnOnce(&str, &str) -> bool>(
    path: &str,
    extension: &str,
    string_equality_comparer: TStringEqualityComparer,
) -> Option<String> {
    let mut extension = extension.to_string();
    if !starts_with(&extension, ".") {
        extension = format!(".{}", extension);
    }
    let path_chars: Vec<char> = path.chars().collect();
    let extension_chars: Vec<char> = extension.chars().collect();
    if path.len() >= extension.len()
        && path_chars.len() >= extension_chars.len()
        && path_chars[path_chars.len() - extension_chars.len()] == CharacterCodes::dot
    {
        let path_extension = &path[path.len() - extension.len()..];
        if string_equality_comparer(path_extension, &extension) {
            return Some(path_extension.to_string());
        }
    }
    None
}

fn get_any_extension_from_path_worker<TStringEqualityComparer: FnOnce(&str, &str) -> bool>(
    path: &str,
    extensions: &[&str],
    string_equality_comparer: TStringEqualityComparer,
) -> String {
    // if (typeof extensions === "string") {
    // }
    for extension in extensions {
        let result = try_get_extension_from_path(path, extension, string_equality_comparer);
        if let Some(result) = result {
            return result;
        }
    }
    "".to_string()
}

pub fn get_any_extension_from_path(
    path: &str,
    extensions: Option<&[&str]>,
    ignore_case: Option<bool>,
) -> String {
    let ignore_case = ignore_case.unwrap_or(false);
    if let Some(extensions) = extensions {
        return get_any_extension_from_path_worker(
            &remove_trailing_directory_separator(path),
            extensions,
            if ignore_case {
                equate_strings_case_insensitive
            } else {
                equate_strings_case_sensitive
            },
        );
    }
    let base_file_name = get_base_file_name(path, None, None);
    let extension_index = base_file_name.rfind('.');
    if let Some(extension_index) = extension_index {
        return base_file_name[extension_index..].to_string();
    }
    "".to_string()
}

fn path_components(path: &str, root_length: usize) -> Vec<String> {
    let path: Vec<char> = path.chars().collect();
    let root: String = path[0..root_length].into_iter().collect();
    let mut rest: Vec<String> = path[root_length..]
        .iter()
        .collect::<String>()
        .split(directory_separator)
        .map(|str| str.to_string())
        .collect();
    if !rest.is_empty() && !matches!(last_or_undefined(&rest), Some(last) if !last.is_empty()) {
        rest.pop();
    }
    let mut ret = rest;
    ret.insert(0, root);
    ret
}

fn get_path_components(path: &str, current_directory: Option<&str>) -> Vec<String> {
    let current_directory = current_directory.unwrap_or("");
    let path = combine_paths(current_directory, &[Some(&path)]);
    path_components(&path, get_root_length(&path))
}

pub fn get_path_from_path_components(path_components: &Vec<String>) -> String {
    if path_components.len() == 0 {
        return "".into();
    }

    let first = &path_components[0];
    let root = if first.len() > 0 {
        ensure_trailing_directory_separator(first)
    } else {
        first.to_string()
    };
    let mut ret = root;
    ret.push_str(&path_components[1..].join(directory_separator_str));
    ret
}

pub fn normalize_slashes(path: &str) -> String {
    let index = path.find("\\");
    if index.is_none() {
        return path.to_string();
    }
    let index = index.unwrap();
    // backslashRegExp.lastIndex = index;
    backslash_reg_exp
        .replace_all(path, directory_separator_str)
        .to_string()
}

pub fn reduce_path_components(components: &[String]) -> Vec<String> {
    if !some(Some(components), Option::<fn(&String) -> bool>::None) {
        return vec![];
    }
    let mut reduced = vec![components[0].clone()];
    for (i, component) in components.iter().enumerate().take(1) {
        if component.is_empty() {
            continue;
        }
        if component == "." {
            continue;
        }
        if component == ".." {
            if reduced.len() > 1 {
                if &reduced[reduced.len() - 1] != ".." {
                    reduced.pop();
                    continue;
                }
            } else if !reduced[0].is_empty() {
                continue;
            }
        }
        reduced.push(component.clone());
    }
    reduced
}

pub fn combine_paths(path: &str, paths: &[Option<&str>]) -> String {
    let mut path = path.to_string();
    if !path.is_empty() {
        path = normalize_slashes(&path);
    }
    for relative_path in paths {
        if match relative_path {
            Some(relative_path) => relative_path.is_empty(),
            None => true,
        } {
            continue;
        }
        let relative_path = relative_path.unwrap();
        let relative_path = normalize_slashes(relative_path);
        if path.is_empty() || get_root_length(&relative_path) != 0 {
            path = relative_path;
        } else {
            path = ensure_trailing_directory_separator(&path);
            path.push_str(&relative_path);
        }
    }
    path
}

pub fn resolve_path(path: &str, paths: &[Option<&str>]) -> String {
    normalize_path(&if some(Some(paths), Option::<fn(&&str) -> bool>::None) {
        combine_paths(path, paths)
    } else {
        normalize_slashes(path)
    })
}

fn get_normalized_path_components(path: &str, current_directory: Option<&str>) -> Vec<String> {
    reduce_path_components(&get_path_components(path, current_directory))
}

fn get_normalized_absolute_path(file_name: &str, current_directory: Option<&str>) -> String {
    get_path_from_path_components(&get_normalized_path_components(
        file_name,
        current_directory,
    ))
}

pub fn normalize_path(path: &str) -> String {
    let mut path = normalize_slashes(path);
    if !relative_path_segment_reg_exp.is_match(&path) {
        return path;
    }
    lazy_static! {
        static ref slash_dot_slash_regex: Regex = Regex::new(r"/\./").unwrap();
    }
    lazy_static! {
        static ref leading_dot_slash_regex: Regex = Regex::new(r"^/\./").unwrap();
    }
    let simplified = leading_dot_slash_regex
        .replace(
            &slash_dot_slash_regex.replace_all(&path, "/").to_string(),
            "",
        )
        .to_string();
    if simplified.len() != path.len() {
        path = simplified;
        if !relative_path_segment_reg_exp.is_match(&path) {
            return path;
        }
    }
    let normalized =
        get_path_from_path_components(&reduce_path_components(&get_path_components(&path, None)));
    if !normalized.is_empty() && has_trailing_directory_separator(&path) {
        ensure_trailing_directory_separator(&normalized)
    } else {
        normalized
    }
}

fn get_path_without_root<TStr: AsRef<str>>(path_components: &[TStr]) -> String {
    if path_components.is_empty() {
        return "".to_string();
    }
    path_components[1..]
        .iter()
        .map(|component| component.as_ref())
        .collect::<Vec<_>>()
        .join(directory_separator_str)
}

pub fn get_normalized_absolute_path_without_root(
    file_name: &str,
    current_directory: Option<&str>,
) -> String {
    get_path_without_root(&get_normalized_path_components(
        file_name,
        current_directory,
    ))
}

pub fn to_path<TGetCanonicalFileName: FnMut(&str) -> String>(
    file_name: &str,
    base_path: Option<&str>,
    get_canonical_file_name: TGetCanonicalFileName,
) -> Path {
    let non_canonicalized_path = if is_rooted_disk_path(file_name) {
        normalize_path(file_name)
    } else {
        get_normalized_absolute_path(file_name, base_path)
    };
    Path::new(get_canonical_file_name(&non_canonicalized_path))
}

pub struct PathAndParts {
    pub path: String,
    pub parts: Vec<String>,
}

pub fn normalize_path_and_parts(path: &str) -> PathAndParts {
    let path = normalize_slashes(path);
    let parts = reduce_path_components(&get_path_components(&path, None));
    let root = parts.iter().next().unwrap().clone();
    let parts: Vec<String> = parts.iter().skip(1).map(Clone::clone).collect();
    if !parts.is_empty() {
        let joined_parts = format!("{}{}", root, parts.join(directory_separator_str));
        PathAndParts {
            path: if has_trailing_directory_separator(&path) {
                ensure_trailing_directory_separator(&joined_parts)
            } else {
                joined_parts
            },
            parts,
        }
    } else {
        PathAndParts { path: root, parts }
    }
}

pub fn remove_trailing_directory_separator(path: &str) -> String {
    if has_trailing_directory_separator(path) {
        return path[0..path.len() - 1].to_string();
    }

    path.to_string()
}

pub fn ensure_trailing_directory_separator(path: &str) -> String {
    if !has_trailing_directory_separator(path) {
        return format!("{}{}", path, directory_separator_str);
    }
    path.to_string()
}

pub fn ensure_path_is_non_module_name(path: &str) -> String {
    if !path_is_absolute(path) && !path_is_relative(path) {
        format!("./{}", path)
    } else {
        path.to_string()
    }
}

pub fn change_any_extension(
    path: &str,
    ext: &str,
    extensions: Option<&[&str]>,
    ignore_case: Option<bool>,
) -> String {
    let pathext = match (extensions, ignore_case) {
        (Some(extensions), Some(ignore_case)) => {
            get_any_extension_from_path(path, Some(extensions), Some(ignore_case))
        }
        _ => get_any_extension_from_path(path, None, None),
    };
    if !pathext.is_empty() {
        format!(
            "{}{}",
            &path[0..path.len() - pathext.len()],
            if starts_with(ext, ".") {
                ext
            } else {
                &format!(".{}", ext)
            }
        )
    } else {
        path.to_string()
    }
}

lazy_static! {
    static ref relative_path_segment_reg_exp: Regex =
        Regex::new(r"(?://)|(?:^|/)\.\.?(?:$|/)").unwrap();
}

fn compare_paths_worker(
    a: &str,
    b: &str,
    component_comparer: fn(&str, &str) -> Comparison,
) -> Comparison {
    if a == b {
        return Comparison::EqualTo;
    }
    // if (a === undefined) return Comparison.LessThan;
    // if (b === undefined) return Comparison.GreaterThan;

    // TODO: mimic performance optimizations? Looked like that will require translating char
    // indexes -> byte indexes

    let a_components = reduce_path_components(&get_path_components(a, None));
    let b_components = reduce_path_components(&get_path_components(b, None));
    let shared_length = cmp::min(a_components.len(), b_components.len());
    let mut i = 1;
    while i < shared_length {
        let result = component_comparer(&a_components[i], &b_components[i]);
        if result != Comparison::EqualTo {
            return result;
        }
        i += 1;
    }
    compare_values(Some(a_components.len()), Some(b_components.len()))
}

pub fn compare_paths_case_sensitive(a: &str, b: &str) -> Comparison {
    compare_paths_worker(a, b, compare_strings_case_sensitive)
}

pub fn compare_paths_case_insensitive(a: &str, b: &str) -> Comparison {
    compare_paths_worker(a, b, compare_strings_case_insensitive)
}

pub enum StringOrBool {
    String(String),
    Bool(bool),
}

impl From<String> for StringOrBool {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<bool> for StringOrBool {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

pub fn compare_paths<TCurrentDirectory: Into<StringOrBool>>(
    a: &str,
    b: &str,
    current_directory: Option<TCurrentDirectory>,
    ignore_case: Option<bool>,
) -> Comparison {
    let mut ignore_case = ignore_case.unwrap_or(false);
    let current_directory = current_directory.map(|current_directory| current_directory.into());
    let mut a = a.to_string();
    let mut b = b.to_string();
    if let Some(StringOrBool::String(current_directory)) = current_directory {
        a = combine_paths(&current_directory, &[Some(&a)]);
        b = combine_paths(&current_directory, &[Some(&b)]);
    } else if let Some(StringOrBool::Bool(current_directory)) = current_directory {
        ignore_case = current_directory;
    }
    compare_paths_worker(&a, &b, get_string_comparer(ignore_case))
}

pub fn contains_path<TCurrentDirectory: Into<StringOrBool>>(
    parent: &str,
    child: &str,
    current_directory: Option<TCurrentDirectory>,
    ignore_case: Option<bool>,
) -> bool {
    let mut ignore_case = ignore_case.unwrap_or(false);
    let current_directory = current_directory.map(|current_directory| current_directory.into());
    let mut parent = parent.to_string();
    let mut child = child.to_string();
    if let Some(StringOrBool::String(current_directory)) = current_directory {
        parent = combine_paths(&current_directory, &[Some(&parent)]);
        child = combine_paths(&current_directory, &[Some(&child)]);
    } else if let Some(StringOrBool::Bool(current_directory)) = current_directory {
        ignore_case = current_directory;
    }
    // if (parent === undefined || child === undefined) return false;
    if parent == child {
        return true;
    }
    let parent_components = reduce_path_components(&get_path_components(&parent, None));
    let child_components = reduce_path_components(&get_path_components(&child, None));
    if child_components.len() < parent_components.len() {
        return false;
    }

    let component_equality_comparer = if ignore_case {
        equate_strings_case_insensitive
    } else {
        equate_strings_case_sensitive
    };
    for (i, parent_component) in parent_components.iter().enumerate() {
        let equality_comparer = if i == 0 {
            equate_strings_case_insensitive
        } else {
            component_equality_comparer
        };
        if !equality_comparer(parent_component, &child_components[i]) {
            return false;
        }
    }

    true
}

pub fn starts_with_directory(
    file_name: &str,
    directory_name: &str,
    get_canonical_file_name: GetCanonicalFileName,
) -> bool {
    let canonical_file_name = get_canonical_file_name(file_name);
    let canonical_directory_name = get_canonical_file_name(directory_name);
    starts_with(
        &canonical_file_name,
        &format!("{}/", canonical_directory_name),
    ) || starts_with(
        &canonical_file_name,
        &format!("{}\\", canonical_directory_name),
    )
}

pub fn get_path_components_relative_to(
    from: &str,
    to: &str,
    string_equality_comparer: fn(&str, &str) -> bool,
    get_canonical_file_name: GetCanonicalFileName,
) -> Vec<String> {
}

fn str_to_string(str_: &str) -> String {
    str_.to_string()
}

pub enum GetCanonicalFileNameOrBool {
    GetCanonicalFileName(GetCanonicalFileName),
    Bool(bool),
}

impl From<GetCanonicalFileName> for GetCanonicalFileNameOrBool {
    fn from(value: GetCanonicalFileName) -> Self {
        Self::GetCanonicalFileName(value)
    }
}

impl From<bool> for GetCanonicalFileNameOrBool {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

pub fn get_relative_path_from_directory<
    TGetCanonicalFileNameOrIgnoreCase: Into<GetCanonicalFileNameOrBool>,
>(
    from_directory: &str,
    to: &str,
    get_canonical_file_name_or_ignore_case: TGetCanonicalFileNameOrIgnoreCase,
) -> String {
    let get_canonical_file_name_or_ignore_case = get_canonical_file_name_or_ignore_case.into();
    Debug_.assert(
        (get_root_length(from_directory) > 0) == (get_root_length(to) > 0),
        Some("Paths must either both be absolute or both be relative"),
    );
    let get_canonical_file_name = match get_canonical_file_name_or_ignore_case {
        GetCanonicalFileNameOrBool::GetCanonicalFileName(
            get_canonical_file_name_or_ignore_case,
        ) => get_canonical_file_name_or_ignore_case,
        _ =>
        /*identity*/
        {
            str_to_string
        }
    };
    let ignore_case = match get_canonical_file_name_or_ignore_case {
        GetCanonicalFileNameOrBool::Bool(get_canonical_file_name_or_ignore_case) => {
            get_canonical_file_name_or_ignore_case
        }
        _ => false,
    };
    let path_components = get_path_components_relative_to(
        from_directory,
        to,
        if ignore_case {
            equate_strings_case_insensitive
        } else {
            equate_strings_case_sensitive
        },
        get_canonical_file_name,
    );
    get_path_from_path_components(&path_components)
}

pub fn convert_to_relative_path(
    absolute_or_relative_path: &str,
    base_path: &str,
    get_canonical_file_name: GetCanonicalFileName,
) -> String {
    if !is_rooted_disk_path(absolute_or_relative_path) {
        absolute_or_relative_path.to_string()
    } else {
        get_relative_path_to_directory_or_url(
            base_path,
            absolute_or_relative_path,
            base_path,
            get_canonical_file_name,
            false,
        )
    }
}

pub fn get_relative_path_from_file(
    from: &str,
    to: &str,
    get_canonical_file_name: GetCanonicalFileName,
) -> String {
    ensure_path_is_non_module_name(&get_relative_path_from_directory(
        &get_directory_path(from),
        to,
        get_canonical_file_name,
    ))
}

pub fn get_relative_path_to_directory_or_url(
    directory_path_or_url: &str,
    relative_or_absolute_path: &str,
    current_directory: &str,
    get_canonical_file_name: GetCanonicalFileName,
    is_absolute_path_an_url: bool,
) -> String {
    let mut path_components = get_path_components_relative_to(
        &resolve_path(current_directory, &[Some(directory_path_or_url)]),
        &resolve_path(current_directory, &[Some(relative_or_absolute_path)]),
        equate_strings_case_sensitive,
        get_canonical_file_name,
    );

    let first_component = &path_components[0];
    if is_absolute_path_an_url && is_rooted_disk_path(first_component) {
        let prefix = if matches!(first_component.chars().next(), Some(directory_separator)) {
            "file://"
        } else {
            "file:///"
        };
        path_components[0] = format!("{}{}", prefix, first_component);
    }

    get_path_from_path_components(&path_components)
}

pub fn for_each_ancestor_directory<TReturn, TCallback: FnMut(&Path) -> Option<TReturn>>(
    directory: &Path,
    callback: TCallback,
) -> Option<TReturn> {
    let mut directory = (*directory).clone();
    loop {
        let result = callback(&directory);
        if result.is_some() {
            return result;
        }

        let parent_path = Path::new(get_directory_path(&directory));
        if parent_path == directory {
            return None;
        }

        directory = parent_path;
    }
}

pub fn is_node_modules_directory(dir_path: &Path) -> bool {
    ends_with(dir_path, "/node_modules")
}
