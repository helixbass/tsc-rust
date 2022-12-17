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
    description: String,
    its: Vec<It>,
    befores: Vec<Box<dyn FnOnce()>>,
    afters: Vec<Box<dyn FnOnce()>>,
}

impl DescribeContext {
    pub fn new(description: String) -> Self {
        Self {
            description,
            its: vec![],
            befores: vec![],
            afters: vec![],
        }
    }
}

thread_local! {
    static describe_contexts_: RefCell<Vec<DescribeContext>> = RefCell::new(vec![]);
}

fn get_enclosing_describe_descriptions() -> Vec<String> {
    describe_contexts_.with(|describe_contexts| {
        describe_contexts
            .borrow()
            .iter()
            .map(|describe_context| describe_context.description.clone())
            .collect()
    })
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
        describe_contexts
            .borrow_mut()
            .push(DescribeContext::new(description.to_owned()));
    });
    callback();
    let enclosing_descriptions = get_enclosing_describe_descriptions();
    let DescribeContext {
        its,
        befores,
        afters,
        ..
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
        let mut enclosing_descriptions = enclosing_descriptions.clone();
        enclosing_descriptions.push(description);
        let join_handle = task::spawn_blocking(callback);
        register_it_join_handle(join_handle, enclosing_descriptions);
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
    static it_join_handles_: RefCell<Option<Vec<ItJoinHandle>>> = RefCell::new(Some(vec![]));
}

struct ItJoinHandle {
    join_handle: task::JoinHandle<()>,
    enclosing_descriptions: Vec<String>,
}

fn register_it_join_handle(join_handle: task::JoinHandle<()>, enclosing_descriptions: Vec<String>) {
    it_join_handles_.with(|it_join_handles| {
        it_join_handles
            .borrow_mut()
            .as_mut()
            .unwrap()
            .push(ItJoinHandle {
                join_handle,
                enclosing_descriptions,
            });
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
    let single_future_streams = it_join_handles.into_iter().map(
        |ItJoinHandle {
             join_handle,
             enclosing_descriptions,
         }| (enclosing_descriptions, join_handle.into_stream()),
    );
    let mut stream_map = StreamMap::new();
    for (enclosing_descriptions, stream) in single_future_streams {
        stream_map.insert(enclosing_descriptions, stream);
    }
    let mut num_passes = 0;
    let mut failures: Vec<FailureInfo> = vec![];
    while let Some((enclosing_descriptions, join_handle_result)) = stream_map.next().await {
        match join_handle_result {
            Ok(_) => {
                print!(".");
                io::stdout().flush().unwrap();
                num_passes += 1;
            }
            Err(err) => {
                print!("F");
                io::stdout().flush().unwrap();
                failures.push(FailureInfo::new(err, enclosing_descriptions));
            }
        }
    }
    println!("\n{} passes, {} failures", num_passes, failures.len());
    for failure in failures {
        for (enclosing_level, description) in failure.enclosing_descriptions.iter().enumerate() {
            println!("{}{}:", "  ".repeat(enclosing_level), description);
        }
        println!("{}", failure.panic_message);
    }
}

struct FailureInfo {
    panic_message: String,
    enclosing_descriptions: Vec<String>,
}

impl FailureInfo {
    pub fn new(join_error: task::JoinError, enclosing_descriptions: Vec<String>) -> Self {
        let panic = join_error.into_panic();
        let panic_type_id = panic.type_id();
        Self {
            enclosing_descriptions,
            panic_message: match panic.downcast::<String>() {
                Ok(panic_string) => *panic_string,
                Err(panic) => match panic.downcast::<&str>() {
                    Ok(panic_string) => (*panic_string).to_owned(),
                    Err(_) => format!("Got non-string panic: {:?}", panic_type_id),
                },
            },
        }
    }
}
