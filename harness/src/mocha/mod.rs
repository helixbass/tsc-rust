use clap::Parser;
use once_cell::sync::OnceCell;
use std::any::Any;
use std::cell::{Cell, RefCell};
use std::io::{self, Write};
use std::panic::{self, UnwindSafe};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use threadpool::ThreadPool;

#[derive(Clone, Debug, Parser)]
pub struct MochaArgs {
    #[arg(long)]
    pub start_index: Option<usize>,

    #[arg(long)]
    pub end_index: Option<usize>,
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
    static THREAD_POOL: ThreadPool = Default::default();
    static JOB_CHANNEL_SENDER: RefCell<Option<Sender<JobChannelMessage>>> = Default::default();
}

pub fn register_config(args: &MochaArgs) {
    CONFIG
        .set(args.clone())
        .expect("Should only initialize Mocha config once");
    panic::set_hook(Box::new(|_| {}));
    set_up_channel();
}

fn config() -> &'static MochaArgs {
    CONFIG
        .get()
        .expect("Tried to get Mocha config before it was set")
}

fn get_thread_pool() -> ThreadPool {
    THREAD_POOL.with(|thread_pool| thread_pool.clone())
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

thread_local! {
    static FAILURES: Arc<Mutex<Vec<FailureInfo>>> = Default::default();
}

fn get_failures() -> Arc<Mutex<Vec<FailureInfo>>> {
    FAILURES.with(|failures| failures.clone())
}

fn set_up_thread_pool_listener(job_channel_receiver: Receiver<JobChannelMessage>) {
    while let Ok(job_message) = job_channel_receiver.recv() {
        get_thread_pool().execute(move || {
            let JobChannelMessage {
                job,
                enclosing_descriptions,
            } = job_message;
            match panic::catch_unwind(job) {
                Ok(_) => {
                    print!(".");
                    io::stdout().flush().unwrap();
                    NUM_PASSES.fetch_add(1, Ordering::Relaxed);
                }
                Err(test_failure) => {
                    print!("F");
                    io::stdout().flush().unwrap();
                    get_failures()
                        .lock()
                        .unwrap()
                        .push(FailureInfo::new(test_failure, enclosing_descriptions));
                }
            }
        });
    }
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
    for failure in failures.iter() {
        for (enclosing_level, description) in failure.enclosing_descriptions.iter().enumerate() {
            println!("{}{}:", "  ".repeat(enclosing_level), description);
        }
        println!("{}", failure.panic_message);
    }
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
        send_job(JobChannelMessage::new(callback, enclosing_descriptions));
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
    pub fn new(panic: Box<dyn Any + Send>, enclosing_descriptions: Vec<String>) -> Self {
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
