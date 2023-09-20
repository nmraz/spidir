use std::{
    cell::RefCell,
    mem,
    panic::{self, PanicInfo, UnwindSafe},
};

use log::Log;

pub fn init_log_capture() {
    log::set_logger(&CapturingLogger).unwrap();
    log::set_max_level(log::LevelFilter::Trace);
}

pub fn capture_logs<R>(f: impl FnOnce() -> R) -> (R, Vec<String>) {
    clear_captured_logs();
    let ret = f();
    let logs = take_captured_logs();
    (ret, logs)
}

pub fn catch_panic_message<R>(f: impl FnOnce() -> R + UnwindSafe) -> Result<R, String> {
    clear_panic_message();
    panic::set_hook(Box::new(panic_hook));
    let ret = panic::catch_unwind(f);
    let _ = panic::take_hook();
    ret.map_err(|_| take_panic_message().unwrap_or("unknown panic".to_owned()))
}

thread_local! {
    static CAPTURED_LOGS: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

fn clear_captured_logs() {
    CAPTURED_LOGS.with(|captured_logs| captured_logs.borrow_mut().clear());
}

fn take_captured_logs() -> Vec<String> {
    CAPTURED_LOGS.with(|captured_logs| mem::take(&mut *captured_logs.borrow_mut()))
}

struct CapturingLogger;
impl Log for CapturingLogger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        let formatted = format!(
            "[{} {}] {}",
            record.level(),
            record.module_path().unwrap_or_default(),
            record.args()
        );
        CAPTURED_LOGS.with(|captured_logs| {
            captured_logs.borrow_mut().push(formatted);
        })
    }

    fn flush(&self) {}
}

thread_local! {
    static PANIC_MESSAGE: RefCell<Option<String>> = RefCell::new(None);
}

fn clear_panic_message() {
    PANIC_MESSAGE.with(|message| *message.borrow_mut() = None)
}

fn take_panic_message() -> Option<String> {
    PANIC_MESSAGE.with(|message| message.borrow_mut().take())
}

fn panic_hook(info: &PanicInfo<'_>) {
    let message = info.to_string();
    PANIC_MESSAGE.with(|stashed_message| *stashed_message.borrow_mut() = Some(message));
}
