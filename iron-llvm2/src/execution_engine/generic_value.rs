// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Generic Value
// LLVM-C header ExecutionEngine.h

use libc::c_void;

use llvm_sys::prelude::*;
use llvm_sys::execution_engine::*;

use ::{LLVMRef, LLVMRefCtor};
use core::types;

pub struct GenericValue {
    generic_value: LLVMGenericValueRef
}

impl GenericValue {
    pub fn new_from_int<T:types::IntType>(ty: &T, n: u64, is_signed: bool) -> GenericValue {
        let generic_value = unsafe {
            LLVMCreateGenericValueOfInt(ty.to_ref(), n, is_signed as LLVMBool)
        };
        GenericValue{ generic_value: generic_value }
    }

    pub fn new_from_pointer(p: *mut c_void) -> GenericValue {
        let generic_value = unsafe {
            LLVMCreateGenericValueOfPointer(p)
        };
        GenericValue{ generic_value: generic_value }
    }

    pub fn new_from_float<T:types::RealType>(ty: &T, n: f64) -> GenericValue {
        let generic_value = unsafe {
            LLVMCreateGenericValueOfFloat(ty.to_ref(), n)
        };
        GenericValue{ generic_value: generic_value }
    }

    pub fn to_int(&self, is_signed: bool) -> u64 {
        unsafe {
            LLVMGenericValueToInt(self.to_ref(), is_signed as LLVMBool)
        }
    }

    pub fn to_pointer(&self) -> *mut c_void {
        unsafe {
            LLVMGenericValueToPointer(self.to_ref())
        }
    }

    pub fn to_float<T:types::RealType>(&self, ty: &T) -> f64 {
        unsafe {
            LLVMGenericValueToFloat(ty.to_ref(), self.to_ref())
        }
    }

    pub fn get_int_width(&self) -> u32 {
        unsafe {
            LLVMGenericValueIntWidth(self.to_ref())
        }
    }
}

impl LLVMRef<LLVMGenericValueRef> for GenericValue {
    fn to_ref(&self) -> LLVMGenericValueRef {
        self.generic_value
    }
}

impl LLVMRefCtor<LLVMGenericValueRef> for GenericValue {
    unsafe fn from_ref(generic_value_ref: LLVMGenericValueRef) -> GenericValue {
        GenericValue{
            generic_value: generic_value_ref
        }
    }
}

impl Drop for GenericValue {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeGenericValue(self.to_ref())
        }
    }
}
