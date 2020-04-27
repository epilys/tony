use super::*;
use std::convert::TryInto;

#[no_mangle]
pub extern "C" fn list_cons__(head: *const u8, tail: *const u8, element_size: usize) -> *const u8 {
    unsafe {
        ALLOC_TABLE.with(|tbl| {
            if !tail.is_null() {
                let tbl_ref = tbl.borrow();
                if !tbl_ref.contains_key(&tail) {
                    panic!();
                }
                let alloc = &tbl_ref[&tail];
                let bytes_to_copy = alloc.size;
                debug_assert_eq!(element_size, alloc.align);
                drop(tbl_ref);
                let dst = alloc__(bytes_to_copy / element_size + 1, element_size);
                let dst_offset = dst.offset(element_size.try_into().unwrap());
                std::ptr::copy_nonoverlapping(tail, dst_offset, bytes_to_copy);
                std::ptr::copy_nonoverlapping(head, dst, element_size);
                dst
            } else {
                alloc__(1, element_size)
            }
        })
    }
}

#[no_mangle]
pub extern "C" fn list_head__(ptr: *const u8) -> *const u8 {
    assert!(!ptr.is_null());
    ptr
}

#[no_mangle]
pub extern "C" fn list_tail__(ptr: *const u8) -> *const u8 {
    unsafe {
        assert!(!ptr.is_null());
        ALLOC_TABLE.with(|tbl| {
            let tbl_ref = tbl.borrow();
            if !tbl_ref.contains_key(&ptr) {
                panic!();
            }
            let element_size = tbl_ref[&ptr].align;
            let src = ptr.offset(element_size.try_into().unwrap());
            let src_len = tbl_ref[&ptr].size - element_size;
            if src_len == 0 {
                return std::ptr::null();
            }
            drop(tbl_ref);
            let dst = alloc__(src_len / element_size, element_size);
            std::ptr::copy_nonoverlapping(src, dst, src_len);
            dst as *const u8
        })
    }
}

#[no_mangle]
pub extern "C" fn list_nil__(ptr: *const u8) -> bool {
    ptr.is_null()
}
