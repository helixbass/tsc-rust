use clap::Parser;

use test_runner::{run, Args};

#[tokio::main]
async fn main() {
    let args = Args::parse();

    run(&args).await
}
