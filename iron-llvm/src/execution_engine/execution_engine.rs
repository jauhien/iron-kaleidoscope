// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Execution Engine
// LLVM-C header ExecutionEngine.h

use std;
use std::ffi::CString;

use libc::{c_char, c_uint, size_t};

use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;

use core;
use execution_engine::GenericValue;
use execution_engine::MCJITMemoryManager;
use {LLVMRef, LLVMRefCtor};

pub struct FrozenModule {
    module: core::Module,
}

impl FrozenModule {
    pub fn get(&self) -> &core::Module {
        &self.module
    }
}

pub struct ExecutionEngine {
    ee: LLVMExecutionEngineRef,
    _memory_manager: Option<Box<dyn MCJITMemoryManager>>,
}

impl ExecutionEngine {
    pub fn new(mut module: core::Module) -> Result<(ExecutionEngine, FrozenModule), String> {
        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        unsafe {
            module.unown();
            if LLVMCreateExecutionEngineForModule(&mut ee, module.to_ref(), &mut error) > 0 {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                Ok((
                    ExecutionEngine {
                        ee: ee,
                        _memory_manager: None,
                    },
                    FrozenModule { module: module },
                ))
            }
        }
    }

    pub fn new_interpreter(
        mut module: core::Module,
    ) -> Result<(ExecutionEngine, FrozenModule), String> {
        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        unsafe {
            module.unown();
            if LLVMCreateInterpreterForModule(&mut ee, module.to_ref(), &mut error) > 0 {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                Ok((
                    ExecutionEngine {
                        ee: ee,
                        _memory_manager: None,
                    },
                    FrozenModule { module: module },
                ))
            }
        }
    }

    pub fn new_jit_compiler(
        mut module: core::Module,
        opt_level: u32,
    ) -> Result<(ExecutionEngine, FrozenModule), String> {
        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        unsafe {
            module.unown();
            if LLVMCreateJITCompilerForModule(&mut ee, module.to_ref(), opt_level, &mut error) > 0 {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                Ok((
                    ExecutionEngine {
                        ee: ee,
                        _memory_manager: None,
                    },
                    FrozenModule { module: module },
                ))
            }
        }
    }

    pub fn add_module(&mut self, mut module: core::Module) -> FrozenModule {
        unsafe {
            LLVMAddModule(self.to_ref(), module.to_ref());
            module.unown();
        }
        FrozenModule { module: module }
    }

    pub fn remove_module(&mut self, module: FrozenModule) -> Result<core::Module, String> {
        let mut module_ref = 0 as LLVMModuleRef;
        let mut error = 0 as *mut c_char;
        unsafe {
            if LLVMRemoveModule(
                self.to_ref(),
                module.get().to_ref(),
                &mut module_ref,
                &mut error,
            ) > 0
            {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                Ok(core::Module::from_ref(module_ref))
            }
        }
    }

    pub fn run_static_constructors(&mut self) {
        unsafe { LLVMRunStaticConstructors(self.to_ref()) }
    }

    pub fn run_static_destructors(&mut self) {
        unsafe { LLVMRunStaticDestructors(self.to_ref()) }
    }

    pub fn run_function<T: core::Function>(
        &self,
        f: &T,
        args: &mut [LLVMGenericValueRef],
    ) -> GenericValue {
        unsafe {
            GenericValue::from_ref(LLVMRunFunction(
                self.to_ref(),
                f.to_ref(),
                args.len() as c_uint,
                args.as_mut_ptr(),
            ))
        }
    }

    pub fn get_function_address(&self, name: &str) -> u64 {
        let name = CString::new(name).unwrap();
        unsafe { LLVMGetFunctionAddress(self.to_ref(), name.as_ptr() as *const c_char) }
    }

    pub fn find_function(&self, name: &str) -> Option<core::FunctionRef> {
        let name = CString::new(name).unwrap();
        let mut f = 0 as LLVMValueRef;
        unsafe {
            if LLVMFindFunction(self.to_ref(), name.as_ptr() as *const c_char, &mut f) > 0 {
                None
            } else {
                Some(core::FunctionRef::from_ref(f))
            }
        }
    }
}

impl LLVMRef<LLVMExecutionEngineRef> for ExecutionEngine {
    fn to_ref(&self) -> LLVMExecutionEngineRef {
        self.ee
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        unsafe { LLVMDisposeExecutionEngine(self.to_ref()) }
    }
}

pub struct MCJITBuilder {
    options: LLVMMCJITCompilerOptions,
    memory_manager: Option<Box<dyn MCJITMemoryManager>>,
}

impl MCJITBuilder {
    pub fn new() -> MCJITBuilder {
        let mut options;
        unsafe {
            options = std::mem::uninitialized();
            LLVMInitializeMCJITCompilerOptions(
                &mut options,
                std::mem::size_of::<LLVMMCJITCompilerOptions>() as size_t,
            );
        }

        MCJITBuilder {
            options: options,
            memory_manager: None,
        }
    }

    pub fn set_opt_level(mut self, opt_level: u32) -> Self {
        self.options.OptLevel = opt_level;
        self
    }

    pub fn set_code_model(mut self, code_model: LLVMCodeModel) -> Self {
        self.options.CodeModel = code_model;
        self
    }

    pub fn no_frame_pointer_elim(mut self) -> Self {
        self.options.NoFramePointerElim = 1;
        self
    }

    pub fn enable_fast_isel(mut self) -> Self {
        self.options.EnableFastISel = 1;
        self
    }

    pub fn set_mcjit_memory_manager(mut self, memory_manager: Box<dyn MCJITMemoryManager>) -> Self {
        self.options.MCJMM = memory_manager.to_ref();
        self.memory_manager = Some(memory_manager);
        self
    }

    pub fn create(
        mut self,
        mut module: core::Module,
    ) -> Result<(ExecutionEngine, FrozenModule), String> {
        let mut ee = 0 as LLVMExecutionEngineRef;
        let mut error = 0 as *mut c_char;
        unsafe {
            module.unown();
            self.memory_manager = self.memory_manager.map(|mut mm| {
                mm.unown();
                mm
            });
            if LLVMCreateMCJITCompilerForModule(
                &mut ee,
                module.to_ref(),
                &mut self.options,
                std::mem::size_of::<LLVMMCJITCompilerOptions>() as size_t,
                &mut error,
            ) > 0
            {
                let cstr_buf = std::ffi::CStr::from_ptr(error);
                let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                LLVMDisposeMessage(error);
                Err(result)
            } else {
                match self.memory_manager {
                    Some(memory_manager) => Ok((
                        ExecutionEngine {
                            ee: ee,
                            _memory_manager: Some(memory_manager),
                        },
                        FrozenModule { module: module },
                    )),
                    None => Ok((
                        ExecutionEngine {
                            ee: ee,
                            _memory_manager: None,
                        },
                        FrozenModule { module: module },
                    )),
                }
            }
        }
    }
}

pub fn link_in_mcjit() {
    unsafe {
        LLVMLinkInMCJIT();
    }
}

pub fn link_in_interpreter() {
    unsafe {
        LLVMLinkInInterpreter();
    }
}
