use clap::Parser;
use futures::future::FutureExt;
use once_cell::sync::OnceCell;
use std::cell::{Cell, RefCell};
use std::io::{self, Write};
use std::panic;
use tokio::task;
use tokio_stream::{StreamExt, StreamMap};

#[derive(Clone, Debug, Parser)]
pub struct MochaArgs {
    #[arg(long)]
    pub start_index: Option<usize>,

    #[arg(long)]
    pub end_index: Option<usize>,
}

static CONFIG: OnceCell<MochaArgs> = OnceCell::new();

pub fn register_config(args: &MochaArgs) {
    CONFIG
        .set(args.clone())
        .expect("Should only initialize Mocha config once");
    panic::set_hook(Box::new(|_| {}));
}

fn config() -> &'static MochaArgs {
    CONFIG
        .get()
        .expect("Tried to get Mocha config before it was set")
}

struct It {
    pub description: String,
    pub callback: Box<dyn FnOnce() + Send>,
    pub global_index: usize,
}

struct DescribeContext {
    its: Vec<It>,
    befores: Vec<Box<dyn FnOnce()>>,
    afters: Vec<Box<dyn FnOnce()>>,
}

impl DescribeContext {
    pub fn new() -> Self {
        Self {
            its: vec![],
            befores: vec![],
            afters: vec![],
        }
    }
}

thread_local! {
    static describe_contexts_: RefCell<Vec<DescribeContext>> = RefCell::new(vec![]);
}

thread_local! {
    static num_its_: Cell<usize> = Cell::new(0);
}

fn get_next_it_index() -> usize {
    num_its_.with(|num_its| {
        let ret = num_its.get();
        num_its.set(ret + 1);
        ret
    })
}

pub fn describe<TCallback: FnOnce()>(description: &str, callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        describe_contexts.borrow_mut().push(DescribeContext::new());
    });
    // println!("{description}:");
    callback();
    let DescribeContext {
        its,
        befores,
        afters,
    } = describe_contexts_.with(|describe_contexts| describe_contexts.borrow_mut().pop().unwrap());
    for before in befores {
        before()
    }
    for it in its {
        if !should_run_it(&it) {
            continue;
        }
        let It {
            description,
            callback,
            ..
        } = it;
        // println!("{description}: ");
        let join_handle = task::spawn_blocking(callback);
        register_it_join_handle(join_handle);
    }
    for after in afters {
        after()
    }
}

fn should_run_it(it: &It) -> bool {
    if let Some(config_start_index) = config().start_index {
        if it.global_index < config_start_index {
            return false;
        }
    }
    if let Some(config_end_index) = config().end_index {
        if it.global_index > config_end_index {
            return false;
        }
    }
    true
}

thread_local! {
    static it_join_handles_: RefCell<Option<Vec<task::JoinHandle<()>>>> = RefCell::new(Some(vec![]));
}

fn register_it_join_handle(join_handle: task::JoinHandle<()>) {
    it_join_handles_.with(|it_join_handles| {
        it_join_handles
            .borrow_mut()
            .as_mut()
            .unwrap()
            .push(join_handle);
    });
}

pub fn it<TCallback: FnOnce() + Send + 'static>(description: &str, callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].its.push(It {
            description: description.to_owned(),
            callback: Box::new(callback),
            global_index: get_next_it_index(),
        });
    });
}

pub fn before<TCallback: FnOnce() + 'static>(callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].befores.push(Box::new(callback));
    });
}

pub fn after<TCallback: FnOnce() + 'static>(callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].afters.push(Box::new(callback));
    });
}

pub async fn collect_results() {
    let it_join_handles = it_join_handles_
        .with(|it_join_handles| it_join_handles.borrow_mut().take())
        .unwrap();
    let single_future_streams = it_join_handles
        .into_iter()
        .map(|join_handle| join_handle.into_stream());
    let mut stream_map = StreamMap::new();
    for (index, stream) in single_future_streams.enumerate() {
        stream_map.insert(index, stream);
    }
    while let Some((_, join_handle_result)) = stream_map.next().await {
        match join_handle_result {
            Ok(_) => {
                print!(".");
                io::stdout().flush();
            }
            Err(err) => {
                print!("F");
                io::stdout().flush();
            }
        }
    }
}
