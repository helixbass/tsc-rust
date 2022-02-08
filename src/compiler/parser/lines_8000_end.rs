use std::rc::Rc;

use crate::{file_extension_is_one_of, DiagnosticMessage, Extension, Node, TextChangeRange};

pub fn IncrementalParser() -> IncrementalParserType {
    IncrementalParserType::new()
}

pub struct IncrementalParserType {}

impl IncrementalParserType {
    pub fn new() -> Self {
        Self {}
    }

    pub fn update_source_file(
        &self,
        source_file: &Node, /*SourceFile*/
        new_text: String,
        text_change_range: TextChangeRange,
        aggressive_checks: bool,
    ) -> Rc<Node /*SourceFile*/> {
        unimplemented!()
    }
}

pub type IncrementalParserSyntaxCursor = ();

pub(crate) fn is_declaration_file_name(file_name: &str) -> bool {
    file_extension_is_one_of(
        file_name,
        &vec![
            Extension::Dts.to_str(),
            Extension::Dmts.to_str(),
            Extension::Dcts.to_str(),
        ],
    )
}

pub(crate) trait PragmaContext {}

pub(crate) fn process_comment_pragmas<TContext: PragmaContext>(
    context: &TContext,
    source_text: &str,
) {
}

pub(crate) fn process_pragmas_into_fields<
    TContext: PragmaContext,
    TReportDiagnostic: FnMut(isize, isize, &DiagnosticMessage),
>(
    context: &TContext,
    report_diagnostic: TReportDiagnostic,
) {
}
