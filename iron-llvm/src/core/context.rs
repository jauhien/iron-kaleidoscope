// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Context
// LLVM-C header Core.h

use llvm_sys::prelude::*;
use llvm_sys::core::*;

use ::{LLVMRef, LLVMRefCtor};

pub struct Context {
    context: LLVMContextRef,
    owned: bool
}

impl Context {
    pub fn new() -> Context {
        let context = unsafe {
            LLVMContextCreate()
        };

        Context{
            context: context,
            owned: true
        }
    }

    pub fn get_global() -> Context {
        let context = unsafe {
            LLVMGetGlobalContext()
        };

        Context{
            context: context,
            owned: false
        }
    }
}

impl LLVMRef<LLVMContextRef> for Context {
    fn to_ref(&self) -> LLVMContextRef {
        self.context
    }
}

impl LLVMRefCtor<LLVMContextRef> for Context {
    unsafe fn from_ref(context_ref : LLVMContextRef) -> Context {
        Context{
            context: context_ref,
            owned: false
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if self.owned {
            unsafe {
                LLVMContextDispose(self.context);
            }
        }
    }
}
