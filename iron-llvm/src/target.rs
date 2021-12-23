// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Target
// LLVM-C header Target.h

use llvm_sys::target::*;

pub fn initilalize_native_target() -> bool {
    unsafe {
        LLVM_InitializeNativeTarget() > 0
    }
}

pub fn initilalize_native_asm_parser() -> bool {
    unsafe {
        LLVM_InitializeNativeAsmParser() > 0
    }
}

pub fn initilalize_native_asm_printer() -> bool {
    unsafe {
        LLVM_InitializeNativeAsmPrinter() > 0
    }
}

pub fn initilalize_native_disassembler() -> bool {
    unsafe {
        LLVM_InitializeNativeDisassembler() > 0
    }
}
