use crate::{combine_paths, file_extension_is, Extension, ResolvedConfigFileName};

pub fn resolve_config_file_project_name(project: &str) -> ResolvedConfigFileName {
    if file_extension_is(project, Extension::Json.to_str()) {
        return project.to_owned().into();
    }

    combine_paths(project, &[Some("tsconfig.json")]).into()
}
