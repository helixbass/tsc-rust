use std::{
    any::Any,
    cell::{Cell, RefCell},
    io::{self, Write},
    panic::{self, UnwindSafe},
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread,
};

use clap::Parser;
use once_cell::sync::{Lazy, OnceCell};
use regex::Regex;
use threadpool::ThreadPool;

#[derive(Clone, Debug, Parser)]
pub struct MochaArgs {
    #[arg(long)]
    pub grep: Option<Regex>,

    #[arg(long)]
    pub start_index: Option<usize>,

    #[arg(long)]
    pub end_index: Option<usize>,

    #[arg(long)]
    pub enable_panic_hook: bool,

    #[arg(long)]
    pub stream_results: bool,
}

static CONFIG: OnceCell<MochaArgs> = OnceCell::new();

type Job = Box<dyn FnOnce() + Send + UnwindSafe>;
struct JobChannelMessage {
    job: Job,
    enclosing_descriptions: Vec<String>,
}

impl JobChannelMessage {
    fn new(job: Job, enclosing_descriptions: Vec<String>) -> Self {
        Self {
            job,
            enclosing_descriptions,
        }
    }
}

thread_local! {
    static JOB_CHANNEL_SENDER: RefCell<Option<Sender<JobChannelMessage>>> = Default::default();
}

pub fn register_config(args: &MochaArgs) {
    CONFIG
        .set(args.clone())
        .expect("Should only initialize Mocha config once");
    if !args.enable_panic_hook {
        panic::set_hook(Box::new(|_| {}));
    }
    set_up_channel();
}

fn config() -> &'static MochaArgs {
    CONFIG
        .get()
        .expect("Tried to get Mocha config before it was set")
}

static THREAD_POOL: Lazy<Arc<Mutex<ThreadPool>>> = Lazy::new(|| {
    Arc::new(Mutex::new(
        threadpool::Builder::new()
            .thread_stack_size(8_000_000)
            .build(),
    ))
});
fn get_thread_pool() -> ThreadPool {
    THREAD_POOL.lock().unwrap().clone()
}

fn set_job_channel_sender(sender: Sender<JobChannelMessage>) {
    JOB_CHANNEL_SENDER.with(|job_channel_sender| {
        *job_channel_sender.borrow_mut() = Some(sender);
    });
}

fn get_job_channel_sender() -> Sender<JobChannelMessage> {
    JOB_CHANNEL_SENDER.with(|channel_sender| channel_sender.borrow().clone().unwrap())
}

fn send_job(job: JobChannelMessage) {
    get_job_channel_sender().send(job).unwrap();
}

fn set_up_channel() {
    let (sender, receiver) = channel::<JobChannelMessage>();
    set_job_channel_sender(sender);
    set_up_thread_pool_listener(receiver);
}

static NUM_PASSES: AtomicUsize = AtomicUsize::new(0);

static FAILURES: Lazy<Arc<Mutex<Vec<FailureInfo>>>> = Lazy::new(|| Default::default());

fn get_failures() -> Arc<Mutex<Vec<FailureInfo>>> {
    FAILURES.clone()
}

fn set_up_thread_pool_listener(job_channel_receiver: Receiver<JobChannelMessage>) {
    thread::spawn(move || {
        while let Ok(job_message) = job_channel_receiver.recv() {
            get_thread_pool().execute(move || {
                let JobChannelMessage {
                    job,
                    enclosing_descriptions,
                } = job_message;
                match panic::catch_unwind(job) {
                    Ok(_) => {
                        if config().stream_results {
                            println!("PASS #{}", NUM_PASSES.load(Ordering::Relaxed) + 1);
                        } else {
                            print!(".");
                        }
                        io::stdout().flush().unwrap();
                        NUM_PASSES.fetch_add(1, Ordering::Relaxed);
                    }
                    Err(test_failure) => {
                        let failure = FailureInfo::new(&test_failure, enclosing_descriptions);
                        if config().stream_results {
                            println!("FAIL #{}", get_failures().lock().unwrap().len() + 1);
                            print_failure(&failure);
                        } else {
                            print!("F");
                        }
                        io::stdout().flush().unwrap();
                        get_failures().lock().unwrap().push(failure);
                        // panic::resume_unwind(test_failure);
                    }
                }
            });
        }
    });
}

pub fn print_results() {
    get_thread_pool().join();
    let failures = get_failures();
    let failures = failures.lock().unwrap();
    println!(
        "\n{} passes, {} failures",
        NUM_PASSES.load(Ordering::Relaxed),
        failures.len()
    );
    if !config().stream_results {
        for failure in failures.iter() {
            print_failure(failure);
        }
    }
}

fn print_failure(failure: &FailureInfo) {
    for (enclosing_level, description) in failure.enclosing_descriptions.iter().enumerate() {
        println!("{}{}:", "  ".repeat(enclosing_level), description);
    }
    println!("{}", failure.panic_message);
}

struct It {
    pub description: String,
    pub callback: Job,
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
    static DESCRIBE_CONTEXTS: RefCell<Vec<DescribeContext>> = RefCell::new(vec![]);
}

fn get_enclosing_describe_descriptions() -> Vec<String> {
    DESCRIBE_CONTEXTS.with(|describe_contexts| {
        describe_contexts
            .borrow()
            .iter()
            .map(|describe_context| describe_context.description.clone())
            .collect()
    })
}

thread_local! {
    static NUM_ITS: Cell<usize> = Cell::new(0);
}

fn get_next_it_index() -> usize {
    NUM_ITS.with(|num_its| {
        let ret = num_its.get();
        num_its.set(ret + 1);
        ret
    })
}

pub fn describe(description: &str, callback: impl FnOnce()) {
    DESCRIBE_CONTEXTS.with(|describe_contexts| {
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
    } = DESCRIBE_CONTEXTS.with(|describe_contexts| describe_contexts.borrow_mut().pop().unwrap());
    for before in befores {
        before()
    }
    for it in its {
        if !should_run_it(&it, &enclosing_descriptions) {
            continue;
        }
        let It {
            description,
            callback,
            ..
        } = it;
        let mut enclosing_descriptions = enclosing_descriptions.clone();
        enclosing_descriptions.push(description);
        send_job(JobChannelMessage::new(callback, enclosing_descriptions));
    }
    for after in afters {
        after()
    }
}

fn should_run_it(it: &It, enclosing_descriptions: &[String]) -> bool {
    if let Some(config_grep) = config().grep.as_ref() {
        if !(config_grep.is_match(&it.description)
            || enclosing_descriptions
                .into_iter()
                .any(|enclosing_description| config_grep.is_match(enclosing_description)))
        {
            return false;
        }
    }
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

pub fn it<TCallback: FnOnce() + Send + UnwindSafe + 'static>(
    description: &str,
    callback: TCallback,
) {
    DESCRIBE_CONTEXTS.with(|describe_contexts| {
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
    DESCRIBE_CONTEXTS.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].befores.push(Box::new(callback));
    });
}

pub fn after<TCallback: FnOnce() + 'static>(callback: TCallback) {
    DESCRIBE_CONTEXTS.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].afters.push(Box::new(callback));
    });
}

struct FailureInfo {
    panic_message: String,
    enclosing_descriptions: Vec<String>,
}

impl FailureInfo {
    pub fn new(panic: &Box<dyn Any + Send>, enclosing_descriptions: Vec<String>) -> Self {
        let panic_type_id = panic.type_id();
        Self {
            enclosing_descriptions,
            panic_message: if let Some(panic_string) = panic.downcast_ref::<String>() {
                panic_string.clone()
            } else if let Some(panic_str) = panic.downcast_ref::<&str>() {
                (*panic_str).to_owned()
            } else {
                format!("Got non-string panic: {:?}", panic_type_id)
            },
        }
    }
}
