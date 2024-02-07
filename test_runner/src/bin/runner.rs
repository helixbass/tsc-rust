use clap::Parser;
use harness::AllArenasHarness;
use test_runner::{run, Args};

fn main() {
    let args = Args::parse();

    let arena = AllArenasHarness::default();

    run(&args, &arena)
}
