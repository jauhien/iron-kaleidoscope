// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Module
// LLVM-C header Core.h

use std;
use std::ffi::CString;

use libc::c_char;

use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::analysis::*;

use ::{LLVMRef, LLVMRefCtor};
use core::context;
use core::value::FunctionRef;

pub struct Module {
    module: LLVMModuleRef,
    owned: bool
}

//TODO: add print to file and metadata methods
impl Module {
    pub fn new(module_id: &str) -> Module {
        let module_id = CString::new(module_id).unwrap();
        unsafe {
            Module {
                module: LLVMModuleCreateWithName(module_id.as_ptr() as *const c_char),
                owned: true
            }
        }
    }

    pub fn new_in_context(module_id: &str, ctx: &context::Context) -> Module {
        let module_id = CString::new(module_id).unwrap();
        unsafe {
            Module {
                module: LLVMModuleCreateWithNameInContext(module_id.as_ptr() as *const c_char, ctx.to_ref()),
                owned: true
            }
        }
    }

    pub fn get_data_layout(&self) -> String {
        unsafe {
            let buf = std::ffi::CStr::from_ptr(LLVMGetDataLayout(self.to_ref()));
            String::from_utf8_lossy(buf.to_bytes()).into_owned()
        }
    }

    pub fn set_data_layout(&mut self, triple: &str) {
        let triple = CString::new(triple).unwrap();
        unsafe {
            LLVMSetDataLayout(self.to_ref(), triple.as_ptr() as *const c_char);
        }
    }

    pub fn get_target(&self) -> String {
        unsafe {
            let buf = std::ffi::CStr::from_ptr(LLVMGetTarget(self.to_ref()));
            String::from_utf8_lossy(buf.to_bytes()).into_owned()
        }
    }

    pub fn set_target(&mut self, triple: &str) {
        let triple = CString::new(triple).unwrap();
        unsafe {
            LLVMSetTarget(self.to_ref(), triple.as_ptr() as *const c_char);
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.to_ref());
        }
    }

    pub fn print_to_string(&self) -> String {
        unsafe {
            let buf = LLVMPrintModuleToString(self.to_ref());
            let cstr_buf = std::ffi::CStr::from_ptr(buf);
            let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
            LLVMDisposeMessage(buf);
            result
        }
    }

    pub fn set_inline_asm(&mut self, asm: &str) {
        let asm = CString::new(asm).unwrap();
        unsafe {
            LLVMSetModuleInlineAsm(self.to_ref(), asm.as_ptr() as *const c_char);
        }
    }

    pub fn get_context(&self) -> context::Context {
        unsafe {
            let ctx = LLVMGetModuleContext(self.to_ref());
            context::Context::from_ref(ctx)
        }
    }

    pub fn get_type_by_name(&self, name: &str) -> Option<LLVMTypeRef> {
        let name = CString::new(name).unwrap();
        let ty = unsafe {
            LLVMGetTypeByName(self.to_ref(), name.as_ptr() as *const c_char)
        };
        if !ty.is_null() {
            Some(ty)
        } else {
            None
        }
    }

    pub fn get_function_by_name(&self, name: &str) -> Option<FunctionRef> {
        let name = CString::new(name).unwrap();
        let function = unsafe {
            LLVMGetNamedFunction(self.to_ref(), name.as_ptr() as *const c_char)
        };
        if !function.is_null() {
            unsafe {
                Some(FunctionRef::from_ref(function))
            }
        } else {
            None
        }
    }

    pub fn verify(&self, action: LLVMVerifierFailureAction) -> Result<(), String> {
        let mut error = 0 as *mut c_char;
        unsafe {
            if LLVMVerifyModule(self.to_ref(), action, &mut error) > 0 {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                Ok(())
            }
        }
    }

    pub fn function_iter(&self) -> FunctionIter {
        let first = unsafe {
            LLVMGetFirstFunction(self.to_ref())
        };

        let current = if first.is_null() {
            None
        } else {
            unsafe {
                Some(FunctionRef::from_ref(first))
            }
        };

        FunctionIter{current: current}
    }

    pub unsafe fn unown(&mut self) {
        self.owned = false;
    }
}

pub struct FunctionIter {
    current: Option<FunctionRef>
}

impl Iterator for FunctionIter {
    type Item = FunctionRef;

    fn next(&mut self) -> Option<FunctionRef> {
        match self.current {
            Some(cur) => {
                let next = unsafe {
                    LLVMGetNextFunction(cur.to_ref())
                };
                self.current = if next.is_null() {
                    None
                } else {
                    unsafe {
                        Some(FunctionRef::from_ref(next))
                    }
                };

                self.current
            },
            None => None
        }
    }
}

impl LLVMRef<LLVMModuleRef> for Module {
    fn to_ref(&self) -> LLVMModuleRef {
        self.module
    }
}

impl LLVMRefCtor<LLVMModuleRef> for Module {
    unsafe fn from_ref(module_ref : LLVMModuleRef) -> Module {
        Module{
            module: module_ref,
            owned: false
        }
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        unsafe {
            Module {
                module: LLVMCloneModule(self.to_ref()),
                owned: true
            }
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        if self.owned {
            unsafe {
                LLVMDisposeModule(self.to_ref());
            }
        }
    }
}
