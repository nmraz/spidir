use core::{
    alloc::{GlobalAlloc, Layout},
    ffi::c_char,
    panic::PanicInfo,
    sync::atomic::{AtomicBool, Ordering},
};

use alloc::string::ToString;

extern "C" {
    fn spidir_alloc(size: usize, align: usize) -> *mut u8;
    fn spidir_free(ptr: *mut u8, size: usize, align: usize);
    fn spidir_realloc(ptr: *mut u8, old_size: usize, align: usize, new_size: usize) -> *mut u8;
    fn spidir_panic(message: *const c_char, message_len: usize) -> !;
}

struct Allocator;

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe { spidir_alloc(layout.size(), layout.align()) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { spidir_free(ptr, layout.size(), layout.align()) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        unsafe { spidir_realloc(ptr, layout.size(), layout.align(), new_size) }
    }
}

#[global_allocator]
static ALLOC: Allocator = Allocator;

static PANICKING: AtomicBool = AtomicBool::new(false);

fn do_panic(message: &str) -> ! {
    unsafe {
        spidir_panic(message.as_ptr() as *const c_char, message.len());
    }
}

#[panic_handler]
fn handle_panic(info: &PanicInfo<'_>) -> ! {
    if PANICKING.swap(true, Ordering::Relaxed) {
        do_panic("nested panic");
    }

    do_panic(&info.to_string());
}

// TODO: liballoc still brings in unwind info referencing `rust_eh_personality`?
#[no_mangle]
fn rust_eh_personality() {}
