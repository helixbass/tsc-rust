use gc::Gc;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::{
    BuilderProgram, CancellationToken, CreateProgram, CustomTransformers, DiagnosticReporter,
    ExitStatus, ProgramHost, System, WatchHost, WatchOptions, WatchStatusReporter,
    WriteFileCallback,
};

#[derive(Debug, Default)]
pub struct BuildOptions {
    pub dry: Option<bool>,
    pub force: Option<bool>,
    pub verbose: Option<bool>,

    pub(crate) clean: Option<bool>,
    pub(crate) watch: Option<bool>,
    pub(crate) help: Option<bool>,

    pub(crate) preserve_watch_output: Option<bool>,
    pub(crate) list_emitted_files: Option<bool>,
    pub(crate) list_files: Option<bool>,
    pub(crate) explain_files: Option<bool>,
    pub(crate) pretty: Option<bool>,
    pub incremental: Option<bool>,
    pub assume_changes_only_affect_direct_dependencies: Option<bool>,

    pub trace_resolution: Option<bool>,
    pub(crate) diagnostics: Option<bool>,
    pub(crate) extended_diagnostics: Option<bool>,
    pub(crate) locale: Option<String>,
    pub(crate) generate_cpu_profile: Option<String>,
    pub(crate) generate_trace: Option<String>,
    // [option: string]: CompilerOptionsValue | undefined;
}

pub trait ReportEmitErrorSummary {
    fn call(&self, error_count: usize);
}

pub trait SolutionBuilderHostBase<TBuilderProgram: BuilderProgram>:
    ProgramHost<TBuilderProgram>
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram>;
}

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
        cancellation_token: Option<Gc<Box<dyn CancellationToken>>>,
        write_file: Option<&dyn WriteFileCallback>,
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
    report_error_summary: Option<Rc<dyn ReportEmitErrorSummary>>,
) -> SolutionBuilderHostConcrete<TBuilderProgram> {
    unimplemented!()
}

pub struct SolutionBuilderHostConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> ProgramHost<TBuilderProgram>
    for SolutionBuilderHostConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderHostBase<TBuilderProgram>
    for SolutionBuilderHostConcrete<TBuilderProgram>
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram> {
        self
    }
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
    report_watch_status: Option<Rc<dyn WatchStatusReporter>>,
) -> SolutionBuilderWithWatchHostConcrete<TBuilderProgram> {
    unimplemented!()
}

pub struct SolutionBuilderWithWatchHostConcrete<TBuilderProgram: BuilderProgram> {
    phantom: PhantomData<TBuilderProgram>,
}

impl<TBuilderProgram: BuilderProgram> ProgramHost<TBuilderProgram>
    for SolutionBuilderWithWatchHostConcrete<TBuilderProgram>
{
}

impl<TBuilderProgram: BuilderProgram> SolutionBuilderHostBase<TBuilderProgram>
    for SolutionBuilderWithWatchHostConcrete<TBuilderProgram>
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram> {
        self
    }
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
