// Copyright 2016 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM BitReader and BitWriter
// LLVM-C headers BitReader.h and BitWriter.h

use std::ffi::CString;
use std::os::unix::io::RawFd;

use libc::c_int;

use llvm_sys::prelude::*;
use llvm_sys::bit_writer::*;

use ::{LLVMRef, LLVMRefCtor};
use core::Module;

pub fn write_bitcode_to_file(module: &Module, path: &str) -> Result<(), ()> {
    let path = CString::new(path).unwrap();

    if unsafe {
        LLVMWriteBitcodeToFile(module.to_ref(), path.as_ptr()) == 0
    } {
        Ok(())
    } else {
        Err(())
    }
}

pub fn write_bitcode_to_memory(module: &Module) -> LLVMMemoryBufferRef {
    unsafe {
        LLVMWriteBitcodeToMemoryBuffer(module.to_ref())
    }
}
