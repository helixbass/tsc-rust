use tsc_rust::{execute_command_line, get_sys, System};

fn main() {
    let sys = get_sys();
    execute_command_line(sys, sys.args());
}
