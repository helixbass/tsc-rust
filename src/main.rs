use tsc_rust::{execute_command_line, SYS};

fn main() {
    println!("Hello, world!");
    execute_command_line(&SYS, &SYS.args);
}
