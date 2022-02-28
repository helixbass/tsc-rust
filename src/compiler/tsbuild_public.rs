use std::marker::PhantomData;
use std::rc::Rc;

use crate::{
    BuilderProgram, CancellationToken, CreateProgram, CustomTransformers, DiagnosticReporter,
    ExitStatus, System, WatchHost, WatchOptions, WatchStatusReporter, WriteFileCallback,
};

pub struct BuildOptions {
    pub(crate) clean: Option<bool>,
    pub(crate) watch: Option<bool>,
    pub(crate) help: Option<bool>,

    pub(crate) pretty: Option<bool>,

    pub(crate) locale: Option<String>,
    pub(crate) generate_cpu_profile: Option<String>,
}

pub type ReportEmitErrorSummary = ();

pub trait SolutionBuilderHostBase<TBuilderProgram: BuilderProgram> {}

pub trait SolutionBuilderHost<TBuilderProgram: BuilderProgram>:
    SolutionBuilderHostBase<TBuilderProgram>
{
}

pub trait SolutionBuilderWithWatchHost<TBuilderProgram: BuilderProgram>:
    SolutionBuilderHostBase<TBuilderProgram> + WatchHost
{
}

pub struct SolutionBuilder<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilder<TBuilderProgram> {
    pub fn build<TGetCustomTransformers: FnMut(&str) -> CustomTransformers>(
        &self,
        project: Option<&str>,
        cancellation_token: Option<Rc<dyn CancellationToken>>,
        write_file: Option<WriteFileCallback>,
        get_custom_transformers: Option<TGetCustomTransformers>,
    ) -> ExitStatus {
        unimplemented!()
    }

    pub fn clean(&self, project: Option<&str>) -> ExitStatus {
        unimplemented!()
    }
}

pub fn create_builder_status_reporter(
    system: &dyn System,
    pretty: Option<bool>,
) -> Rc<dyn DiagnosticReporter> {
    unimplemented!()
}

pub fn create_solution_builder_host<
    TBuilderProgram: BuilderProgram,
    TCreateProgram: CreateProgram<TBuilderProgram>,
>(
    system: Option<&dyn System>,
    create_program: Option<TCreateProgram>,
    report_diagnostic: Option<Rc<dyn DiagnosticReporter>>,
    report_solution_builder_status: Option<Rc<dyn DiagnosticReporter>>,
    report_error_summary: Option<ReportEmitErrorSummary>,
) -> SolutionBuilderHostConcrete<TBuilderProgram> {
    unimplemented!()
}

pub struct SolutionBuilderHostConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderHostBase<TBuilderProgram>
    for SolutionBuilderHostConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderHost<TBuilderProgram>
    for SolutionBuilderHostConcrete<TBuilderProgram>
{
}

pub fn create_solution_builder_with_watch_host<
    TBuilderProgram: BuilderProgram,
    TCreateProgram: CreateProgram<TBuilderProgram>,
>(
    system: Option<&dyn System>,
    create_program: Option<TCreateProgram>,
    report_diagnostic: Option<Rc<dyn DiagnosticReporter>>,
    report_solution_builder_status: Option<Rc<dyn DiagnosticReporter>>,
    report_watch_status: Option<WatchStatusReporter>,
) -> SolutionBuilderWithWatchHostConcrete<TBuilderProgram> {
    unimplemented!()
}

pub struct SolutionBuilderWithWatchHostConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderHostBase<TBuilderProgram>
    for SolutionBuilderWithWatchHostConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderWithWatchHost<TBuilderProgram>
    for SolutionBuilderWithWatchHostConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> WatchHost
    for SolutionBuilderWithWatchHostConcrete<TBuilderProgram>
{
}

pub fn create_solution_builder<
    TBuilderProgram: BuilderProgram,
    THost: SolutionBuilderHost<TBuilderProgram>,
>(
    host: &THost,
    root_names: &[String],
    default_options: &BuildOptions,
) -> SolutionBuilder<TBuilderProgram> {
    unimplemented!()
}

pub fn create_solution_builder_with_watch<
    TBuilderProgram: BuilderProgram,
    THost: SolutionBuilderWithWatchHost<TBuilderProgram>,
>(
    host: &THost,
    root_names: &[String],
    default_options: &BuildOptions,
    base_options: Option<&WatchOptions>,
) -> SolutionBuilder<TBuilderProgram> {
    unimplemented!()
}
