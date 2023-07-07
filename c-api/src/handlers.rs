use core::{
    alloc::{GlobalAlloc, Layout},
    ffi::c_char,
    panic::PanicInfo,
    ptr,
};

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

#[panic_handler]
fn handle_panic(info: &PanicInfo<'_>) -> ! {
    unsafe {
        // TODO
        spidir_panic(ptr::null(), 0);
    }
}
