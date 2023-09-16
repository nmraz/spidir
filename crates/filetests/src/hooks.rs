use std::{
    cell::RefCell,
    panic::{self, PanicInfo, UnwindSafe},
};

thread_local! {
    static PANIC_MESSAGE: RefCell<Option<String>> = RefCell::new(None);
}

pub fn catch_panic_message<R>(f: impl FnOnce() -> R + UnwindSafe) -> Result<R, String> {
    clear_panic_message();
    panic::set_hook(Box::new(panic_hook));
    let ret = panic::catch_unwind(f);
    let _ = panic::take_hook();
    ret.map_err(|_| take_panic_message().unwrap_or("unknown panic".to_owned()))
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
