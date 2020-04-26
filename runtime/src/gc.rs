use std::alloc::{handle_alloc_error, GlobalAlloc, Layout, System};

/*
macro_rules! print_{
    ( $( $x:expr ),* ) => {
        {
            use std::fs::File;
            use std::io::Write;
            use std::os::unix::io::{FromRawFd, IntoRawFd};
            let mut stdout = unsafe { File::from_raw_fd(1) };

            write!(stdout, $($x, )*).unwrap();
            write!(stdout, "\n").unwrap();
            let _ = stdout.into_raw_fd();
        }
    };
}
*/

struct Allocation {
    ptr: *mut u8,
    refcount: AtomicUsize,
    size: usize,
    align: usize,
}

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

thread_local! {
    static ALLOC_TABLE: RefCell<HashMap<*const u8,Allocation>> = RefCell::new(HashMap::default());
}

#[no_mangle]
pub extern "C" fn alloc__(array_size: libc::size_t, element_size: libc::size_t) -> *const u8 {
    unsafe {
        let layout = Layout::from_size_align(array_size * (element_size), element_size).unwrap();
        let ptr: *mut u8 = System.alloc_zeroed(layout);
        if ptr.is_null() {
            // abort
            handle_alloc_error(layout);
        }

        ALLOC_TABLE.with(|tbl| {
            let mut tbl_ref = tbl.borrow_mut();
            tbl_ref.insert(
                ptr,
                Allocation {
                    ptr,
                    refcount: AtomicUsize::new(1),
                    size: array_size * element_size,
                    align: element_size,
                },
            );
        });
        ptr
    }
}

#[no_mangle]
pub extern "C" fn incref__(ptr: *const u8) {
    if ptr.is_null() {
        return;
    }
    ALLOC_TABLE.with(|tbl| {
        let tbl_ref = tbl.borrow();
        if !tbl_ref.contains_key(&ptr) {
            return;
        }
        tbl_ref[&ptr].refcount.fetch_add(1, Ordering::SeqCst);
    });
}

#[no_mangle]
pub extern "C" fn decref__(ptr: *const u8) {
    if ptr.is_null() {
        return;
    }
    ALLOC_TABLE.with(|tbl| {
        let mut tbl_ref = tbl.borrow_mut();
        if !tbl_ref.contains_key(&ptr) {
            return;
        }
        let c = tbl_ref[&ptr].refcount.fetch_sub(1, Ordering::SeqCst);
        if c == 1 {
            tbl_ref[&ptr].dealloc();
            // TODO: check if mem::forget works
            tbl_ref.remove(&ptr).unwrap();
        }
    });
}

impl Drop for Allocation {
    fn drop(&mut self) {
        let c = self.refcount.load(Ordering::SeqCst);
        if c > 0 {
            self.dealloc();
        }
    }
}

impl Allocation {
    fn dealloc(&self) {
        self.refcount.store(0, Ordering::SeqCst);
        let layout = Layout::from_size_align(self.size, self.align).unwrap();
        unsafe {
            System.dealloc(self.ptr, layout);
        }
    }
}
