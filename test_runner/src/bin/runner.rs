use clap::Parser;

use test_runner::{run, Args};

fn main() {
    let args = Args::parse();

    run(&args)
}
