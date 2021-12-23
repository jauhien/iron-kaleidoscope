// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Support
// LLVM-C header Support.h

use std;
use libc::{c_char, c_void};

mod wrappers;

// function is marked as unsafe as user can trigger execution of an
// arbitrary memory address using it
pub unsafe fn add_symbol(name: &str, ptr: *const ()) {
    let name = std::ffi::CString::new(name).unwrap();
    let addr = ptr as *const c_void;
    wrappers::LLVM_AddSymbol(name.as_ptr() as *const c_char, addr)
}
