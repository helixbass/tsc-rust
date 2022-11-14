use typescript_rust::Node;

pub fn split_content_by_newlines(content: &str) -> Vec<&str> {
    let mut lines = content.split("\r\n").collect::<Vec<_>>();
    if lines.len() == 1 {
        lines = content.split("\n").collect::<Vec<_>>();

        if lines.len() == 1 {
            lines = content.split("\r").collect::<Vec<_>>();
        }
    }
    lines
}

pub fn assert_invariants(node: Option<&Node>, parent: Option<&Node>) {
    unimplemented!()
}
