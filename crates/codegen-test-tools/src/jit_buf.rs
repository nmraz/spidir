use core::slice;
use std::{
    io,
    ptr::{self, NonNull},
};

use anyhow::{bail, Error, Result};
use libc::{MAP_ANONYMOUS, MAP_FAILED, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE};

pub struct JitBuf {
    base: NonNull<u8>,
    len: usize,
}

impl Drop for JitBuf {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.base.as_ptr().cast(), self.len);
        }
    }
}

impl JitBuf {
    pub fn new(len: usize) -> Result<Self> {
        let base = unsafe {
            libc::mmap(
                ptr::null_mut(),
                len,
                PROT_READ | PROT_WRITE,
                MAP_ANONYMOUS | MAP_PRIVATE,
                -1,
                0,
            )
        };

        if base == MAP_FAILED {
            bail!(last_error().context("failed to map memory"));
        }

        Ok(Self {
            base: unsafe { NonNull::new_unchecked(base.cast()) },
            len,
        })
    }

    pub fn make_exec(&mut self) -> Result<()> {
        let ret = unsafe {
            libc::mprotect(
                self.base.as_ptr().cast(),
                (self.len + PAGE_SIZE - 1) & 0usize.wrapping_sub(PAGE_SIZE),
                PROT_READ | PROT_EXEC,
            )
        };
        if ret != 0 {
            bail!(last_error().context("`mprotect` failed"));
        }
        Ok(())
    }

    pub fn base(&self) -> NonNull<u8> {
        self.base
    }

    pub unsafe fn buf_mut(&mut self) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(self.base.as_ptr(), self.len) }
    }
}

fn last_error() -> Error {
    Error::new(io::Error::from_raw_os_error(unsafe {
        *libc::__errno_location()
    }))
}

const PAGE_SIZE: usize = 0x1000;
