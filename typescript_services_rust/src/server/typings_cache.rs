use gc::{Finalize, Trace};
use std::future::Future;
use std::rc::Rc;

use typescript_rust::{Path, TypeAcquisition};

use crate::{ApplyCodeActionCommandResult, ProjectInterface, ProjectService};

pub struct InstallPackageOptionsWithProject {
    pub file_name: Path,
    pub package_name: String,
    pub project_name: String,
    pub project_root_path: Path,
}

pub trait ITypingsInstaller: Trace + Finalize {
    fn is_known_types_package_name(&self, name: &str) -> bool;
    fn install_package(
        &self,
        options: &InstallPackageOptionsWithProject,
    ) -> Rc<dyn Future<Output = ApplyCodeActionCommandResult>>;
    fn enqueue_install_typings_request(
        &self,
        p: &dyn ProjectInterface,
        type_acquisition: &TypeAcquisition,
        unresolved_imports: Option<&[String] /*SortedReadonlyArray*/>,
    );
    fn attach(&self, project_service: &ProjectService);
    fn on_project_closed(&self, p: &dyn ProjectInterface);
    fn global_typings_cache_location(&self) -> Option<String>;
}
