use crate::{last_or_undefined, CharacterCodes, Path};

static directory_separator: &str = "/";

fn is_any_directory_separator(char_code: char) -> bool {
    char_code == CharacterCodes::slash
}

fn has_trailing_directory_separator(path: &str) -> bool {
    path.len() > 0 && is_any_directory_separator(path.chars().last().unwrap())
}

fn get_encoded_root_length(path: &str) -> usize {
    if path.len() == 0 {
        return 0;
    }
    let ch0 = path.chars().nth(0).unwrap();
    if ch0 == CharacterCodes::slash {
        return 1;
    }
    0
}

fn get_root_length(path: &str) -> usize {
    get_encoded_root_length(path)
}

fn path_components(path: &str, root_length: usize) -> Vec<String> {
    let root: String = path.chars().take(root_length).collect();
    let mut rest: Vec<String> = path
        .chars()
        .skip(root_length)
        .collect::<String>()
        .split(directory_separator)
        .map(|str| str.to_string())
        .collect();
    if rest.len() > 0
        && (last_or_undefined(&rest).is_none() || last_or_undefined(&rest).unwrap().len() == 0)
    {
        rest.pop();
    }
    let mut ret = rest;
    ret.insert(0, root);
    ret
}

fn get_path_components(path: &str, current_directory: Option<&str>) -> Vec<String> {
    let current_directory = current_directory.unwrap_or("");
    let path = combine_paths(current_directory, path);
    path_components(&path, get_root_length(&path))
}

fn get_path_from_path_components(path_components: &Vec<String>) -> String {
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
    ret.push_str(&path_components[1..].join(directory_separator));
    ret
}

fn normalize_slashes(path: &str) -> String {
    path.into()
}

fn reduce_path_components(components: Vec<String>) -> Vec<String> {
    components
}

fn combine_paths(path: &str, paths: &str) -> String {
    let paths = [paths];
    let mut path = path.to_string();
    if path.len() > 0 {
        path = normalize_slashes(&path);
    }
    for relative_path in paths {
        if relative_path.len() == 0 {
            continue;
        }
        let relative_path = normalize_slashes(relative_path);
        if path.len() == 0 || get_root_length(&relative_path) != 0 {
            path = relative_path;
        } else {
            path = ensure_trailing_directory_separator(&path);
            path.push_str(&relative_path);
        }
    }
    path
}

fn get_normalized_path_components(path: &str, current_directory: Option<&str>) -> Vec<String> {
    reduce_path_components(get_path_components(path, current_directory))
}

fn get_normalized_absolute_path(file_name: &str, current_directory: Option<&str>) -> String {
    get_path_from_path_components(&get_normalized_path_components(
        file_name,
        current_directory,
    ))
}

pub fn normalize_path(path: &str) -> String {
    normalize_slashes(path)
}

pub fn to_path(
    file_name: &str,
    base_path: Option<&str>,
    get_canonical_file_name: &mut dyn FnMut(&str) -> String,
) -> Path {
    let non_canonicalized_path = get_normalized_absolute_path(file_name, base_path);
    Path::new(get_canonical_file_name(&non_canonicalized_path))
}

fn ensure_trailing_directory_separator(path: &str) -> String {
    if !has_trailing_directory_separator(path) {
        let mut path = path.to_string();
        path.push_str(directory_separator);
        return path;
    }
    path.to_string()
}
