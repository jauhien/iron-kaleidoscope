// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM MCJIT Memory Manager
// LLVM-C header ExecutionEngine.h

use std;

use libc::{c_char, c_uint, c_void, uintptr_t};

use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;

use LLVMRef;

use execution_engine::wrappers;

pub trait MCJITMemoryManager: LLVMRef<LLVMMCJITMemoryManagerRef> {
    unsafe fn unown(&mut self);
}

pub trait SimpleMCJITMemoryManagerImpl {
    fn allocate_code_section(
        &mut self,
        size: uintptr_t,
        alignment: u32,
        section_id: u32,
        section_name: &str,
    ) -> *mut u8;
    fn allocate_data_section(
        &mut self,
        size: uintptr_t,
        alignment: u32,
        section_id: u32,
        section_name: &str,
        is_read_only: bool,
    ) -> *mut u8;
    fn finalize_memory(&mut self) -> Result<(), String>;
    fn destroy(&mut self);
}

pub struct SimpleMCJITMemoryManager<T: SimpleMCJITMemoryManagerImpl> {
    memory_manager: LLVMMCJITMemoryManagerRef,
    _implementation: Box<T>,
    owned: bool,
}

impl<T: SimpleMCJITMemoryManagerImpl> SimpleMCJITMemoryManager<T> {
    pub fn new(mut implementation: T) -> SimpleMCJITMemoryManager<T> {
        extern "C" fn allocate_code_section_callback<T: SimpleMCJITMemoryManagerImpl>(
            opaque: *mut c_void,
            size: uintptr_t,
            alignment: c_uint,
            section_id: c_uint,
            section_name: *const c_char,
        ) -> *mut u8 {
            let implementation = opaque as *mut T;
            unsafe {
                let cstr_buf = std::ffi::CStr::from_ptr(section_name);
                let section_name = std::str::from_utf8_unchecked(cstr_buf.to_bytes());
                (*implementation).allocate_code_section(size, alignment, section_id, section_name)
            }
        }

        extern "C" fn allocate_data_section_callback<T: SimpleMCJITMemoryManagerImpl>(
            opaque: *mut c_void,
            size: uintptr_t,
            alignment: c_uint,
            section_id: c_uint,
            section_name: *const c_char,
            is_read_only: LLVMBool,
        ) -> *mut u8 {
            let implementation = opaque as *mut T;
            unsafe {
                let cstr_buf = std::ffi::CStr::from_ptr(section_name);
                let section_name = std::str::from_utf8_unchecked(cstr_buf.to_bytes());
                (*implementation).allocate_data_section(
                    size,
                    alignment,
                    section_id,
                    section_name,
                    is_read_only > 0,
                )
            }
        }

        extern "C" fn finalize_memory_callback<T: SimpleMCJITMemoryManagerImpl>(
            opaque: *mut c_void,
            error_msg: *mut *mut c_char,
        ) -> LLVMBool {
            let implementation = opaque as *mut T;
            match unsafe { (*implementation).finalize_memory() } {
                Ok(_) => 0,
                Err(error) => {
                    let error = std::ffi::CString::new(error).unwrap();
                    unsafe {
                        *error_msg = error.as_ptr() as *mut c_char;
                    }
                    std::mem::forget(error);
                    1
                }
            }
        }

        extern "C" fn destroy_callback<T: SimpleMCJITMemoryManagerImpl>(opaque: *mut c_void) {
            let implementation = opaque as *mut T;
            unsafe {
                (*implementation).destroy();
            }
        }

        let memory_manager = unsafe {
            LLVMCreateSimpleMCJITMemoryManager(
                &mut implementation as *mut _ as *mut c_void,
                allocate_code_section_callback::<T>,
                allocate_data_section_callback::<T>,
                finalize_memory_callback::<T>,
                destroy_callback::<T>,
            )
        };

        SimpleMCJITMemoryManager {
            memory_manager: memory_manager,
            _implementation: Box::new(implementation),
            owned: true,
        }
    }
}

impl<T: SimpleMCJITMemoryManagerImpl> LLVMRef<LLVMMCJITMemoryManagerRef>
    for SimpleMCJITMemoryManager<T>
{
    fn to_ref(&self) -> LLVMMCJITMemoryManagerRef {
        self.memory_manager
    }
}

impl<T: SimpleMCJITMemoryManagerImpl> MCJITMemoryManager for SimpleMCJITMemoryManager<T> {
    unsafe fn unown(&mut self) {
        self.owned = false
    }
}

impl<T: SimpleMCJITMemoryManagerImpl> Drop for SimpleMCJITMemoryManager<T> {
    fn drop(&mut self) {
        if self.owned {
            unsafe { LLVMDisposeMCJITMemoryManager(self.memory_manager) }
        }
    }
}

pub struct SectionMemoryManager {
    memory_manager: LLVMMCJITMemoryManagerRef,
    owned: bool,
}

impl SectionMemoryManager {
    pub fn new() -> Self {
        let memory_manager = unsafe { wrappers::LLVM_CreateSectionMemoryManager() };

        SectionMemoryManager {
            memory_manager: memory_manager,
            owned: true,
        }
    }
}

impl LLVMRef<LLVMMCJITMemoryManagerRef> for SectionMemoryManager {
    fn to_ref(&self) -> LLVMMCJITMemoryManagerRef {
        self.memory_manager
    }
}

impl MCJITMemoryManager for SectionMemoryManager {
    unsafe fn unown(&mut self) {
        self.owned = false
    }
}

impl Drop for SectionMemoryManager {
    fn drop(&mut self) {
        if self.owned {
            unsafe { LLVMDisposeMCJITMemoryManager(self.memory_manager) }
        }
    }
}

pub struct ParentBSMM(LLVMMCJITMemoryManagerRef);

impl ParentBSMM {
    pub fn allocate_code_section(
        &mut self,
        size: uintptr_t,
        alignment: c_uint,
        section_id: c_uint,
        section_name: &str,
    ) -> *mut u8 {
        let name = std::ffi::CString::new(section_name).unwrap();
        unsafe {
            wrappers::LLVM_BSMMCallParentAllocateCodeSection(
                self.0,
                size,
                alignment,
                section_id,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn allocate_date_section(
        &mut self,
        size: uintptr_t,
        alignment: c_uint,
        section_id: c_uint,
        section_name: &str,
        is_read_only: bool,
    ) -> *mut u8 {
        let name = std::ffi::CString::new(section_name).unwrap();
        unsafe {
            wrappers::LLVM_BSMMCallParentAllocateDataSection(
                self.0,
                size,
                alignment,
                section_id,
                name.as_ptr() as *const c_char,
                is_read_only as LLVMBool,
            )
        }
    }

    pub fn finalize_memory(&mut self) -> Result<(), String> {
        let mut error = 0 as *mut c_char;
        unsafe {
            if wrappers::LLVM_BSMMCallParentFinalizeMemory(self.0, &mut error) > 0 {
                if error.is_null() {
                    let cstr_buf = std::ffi::CStr::from_ptr(error);
                    let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
                    LLVMDisposeMessage(error);
                    Err(result)
                } else {
                    Err("unknown error during memory finalization".to_string())
                }
            } else {
                Ok(())
            }
        }
    }

    pub fn invalidate_instruction_cache(&mut self) {
        unsafe { wrappers::LLVM_BSMMCallParentInvalidateInstructionCache(self.0) }
    }

    pub fn get_symbol_address(&mut self, name: &str) -> u64 {
        let name = std::ffi::CString::new(name).unwrap();
        unsafe {
            wrappers::LLVM_BSMMCallParentGetSymbolAddress(self.0, name.as_ptr() as *const c_char)
        }
    }
}

pub struct BSMMFunctions<'a, 'b, 'c, 'd, 'e, 'f> {
    allocate_code_section:
        Option<Box<dyn FnMut(ParentBSMM, uintptr_t, u32, u32, &str) -> *mut u8 + 'a>>,

    allocate_data_section:
        Option<Box<dyn FnMut(ParentBSMM, uintptr_t, u32, u32, &str, bool) -> *mut u8 + 'b>>,

    finalize_memory: Option<Box<dyn FnMut(ParentBSMM) -> Result<(), String> + 'c>>,

    invalidate_instruction_cache: Option<Box<dyn FnMut(ParentBSMM) + 'd>>,

    get_symbol_address: Option<Box<dyn FnMut(ParentBSMM, &str) -> u64 + 'e>>,

    destroy: Option<Box<dyn FnMut() + 'f>>,
}

pub struct BindingSectionMemoryManager<'a, 'b, 'c, 'd, 'e, 'f> {
    memory_manager: LLVMMCJITMemoryManagerRef,
    owned: bool,
    _bsmm_functions: Box<BSMMFunctions<'a, 'b, 'c, 'd, 'e, 'f>>,
}

impl<'a, 'b, 'c, 'd, 'e, 'f> LLVMRef<LLVMMCJITMemoryManagerRef>
    for BindingSectionMemoryManager<'a, 'b, 'c, 'd, 'e, 'f>
{
    fn to_ref(&self) -> LLVMMCJITMemoryManagerRef {
        self.memory_manager
    }
}

impl<'a, 'b, 'c, 'd, 'e, 'f> MCJITMemoryManager
    for BindingSectionMemoryManager<'a, 'b, 'c, 'd, 'e, 'f>
{
    unsafe fn unown(&mut self) {
        self.owned = false
    }
}

impl<'a, 'b, 'c, 'd, 'e, 'f> Drop for BindingSectionMemoryManager<'a, 'b, 'c, 'd, 'e, 'f> {
    fn drop(&mut self) {
        if self.owned {
            unsafe { LLVMDisposeMCJITMemoryManager(self.memory_manager) }
        }
    }
}

pub struct BindingSectionMemoryManagerBuilder<'a, 'b, 'c, 'd, 'e, 'f> {
    bsmm_functions: Box<BSMMFunctions<'a, 'b, 'c, 'd, 'e, 'f>>,
}

impl<'a, 'b, 'c, 'd, 'e, 'f> BindingSectionMemoryManagerBuilder<'a, 'b, 'c, 'd, 'e, 'f> {
    pub fn new() -> Self {
        BindingSectionMemoryManagerBuilder {
            bsmm_functions: Box::new(BSMMFunctions {
                allocate_code_section: None,
                allocate_data_section: None,
                finalize_memory: None,
                invalidate_instruction_cache: None,
                get_symbol_address: None,
                destroy: None,
            }),
        }
    }

    pub fn set_allocate_code_section<F>(mut self, allocate_code_section: F) -> Self
    where
        F: 'a + FnMut(ParentBSMM, uintptr_t, u32, u32, &str) -> *mut u8,
    {
        self.bsmm_functions.allocate_code_section = Some(Box::new(allocate_code_section));
        self
    }

    pub fn set_allocate_data_section<F>(mut self, allocate_data_section: F) -> Self
    where
        F: 'b + FnMut(ParentBSMM, uintptr_t, u32, u32, &str, bool) -> *mut u8,
    {
        self.bsmm_functions.allocate_data_section = Some(Box::new(allocate_data_section));
        self
    }

    pub fn set_finalize_memory<F>(mut self, finalize_memory: F) -> Self
    where
        F: 'c + FnMut(ParentBSMM) -> Result<(), String>,
    {
        self.bsmm_functions.finalize_memory = Some(Box::new(finalize_memory));
        self
    }

    pub fn set_invalidate_instruction_cache<F>(mut self, invalidate_instruction_cache: F) -> Self
    where
        F: 'd + FnMut(ParentBSMM),
    {
        self.bsmm_functions.invalidate_instruction_cache =
            Some(Box::new(invalidate_instruction_cache));
        self
    }

    pub fn set_get_symbol_address<F>(mut self, get_symbol_address: F) -> Self
    where
        F: 'e + FnMut(ParentBSMM, &str) -> u64,
    {
        self.bsmm_functions.get_symbol_address = Some(Box::new(get_symbol_address));
        self
    }

    pub fn set_destroy<F>(mut self, destroy: F) -> Self
    where
        F: 'f + FnMut(),
    {
        self.bsmm_functions.destroy = Some(Box::new(destroy));
        self
    }

    pub fn create(mut self) -> BindingSectionMemoryManager<'a, 'b, 'c, 'd, 'e, 'f> {
        extern "C" fn allocate_code_section_callback(
            opaque: *mut c_void,
            mm: LLVMMCJITMemoryManagerRef,
            size: uintptr_t,
            alignment: c_uint,
            section_id: c_uint,
            section_name: *const c_char,
        ) -> *mut u8 {
            let parent = ParentBSMM(mm);
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                let cstr_buf = std::ffi::CStr::from_ptr(section_name);
                let section_name = std::str::from_utf8_unchecked(cstr_buf.to_bytes());
                bsmm_functions
                    .allocate_code_section
                    .iter_mut()
                    .next()
                    .unwrap()(parent, size, alignment, section_id, section_name)
            }
        }

        extern "C" fn allocate_data_section_callback(
            opaque: *mut c_void,
            mm: LLVMMCJITMemoryManagerRef,
            size: uintptr_t,
            alignment: c_uint,
            section_id: c_uint,
            section_name: *const c_char,
            is_read_only: LLVMBool,
        ) -> *mut u8 {
            let parent = ParentBSMM(mm);
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                let cstr_buf = std::ffi::CStr::from_ptr(section_name);
                let section_name = std::str::from_utf8_unchecked(cstr_buf.to_bytes());
                bsmm_functions
                    .allocate_data_section
                    .iter_mut()
                    .next()
                    .unwrap()(
                    parent,
                    size,
                    alignment,
                    section_id,
                    section_name,
                    is_read_only > 0,
                )
            }
        }

        extern "C" fn finalize_memory_callback(
            opaque: *mut c_void,
            mm: LLVMMCJITMemoryManagerRef,
            error_msg: *mut *mut c_char,
        ) -> LLVMBool {
            let parent = ParentBSMM(mm);
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                match bsmm_functions.finalize_memory.iter_mut().next().unwrap()(parent) {
                    Ok(_) => 0,
                    Err(error) => {
                        let error = std::ffi::CString::new(error).unwrap();
                        *error_msg = error.as_ptr() as *mut c_char;
                        std::mem::forget(error);
                        1
                    }
                }
            }
        }

        extern "C" fn invalidate_instruction_cache_callback(
            opaque: *mut c_void,
            mm: LLVMMCJITMemoryManagerRef,
        ) {
            let parent = ParentBSMM(mm);
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                bsmm_functions
                    .invalidate_instruction_cache
                    .iter_mut()
                    .next()
                    .unwrap()(parent)
            }
        }

        extern "C" fn get_symbol_address_callback(
            opaque: *mut c_void,
            mm: LLVMMCJITMemoryManagerRef,
            name: *const ::libc::c_char,
        ) -> u64 {
            let parent = ParentBSMM(mm);
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                let cstr_buf = std::ffi::CStr::from_ptr(name);
                let name = std::str::from_utf8_unchecked(cstr_buf.to_bytes());
                bsmm_functions.get_symbol_address.iter_mut().next().unwrap()(parent, name)
            }
        }

        extern "C" fn destroy_callback(opaque: *mut c_void) {
            unsafe {
                let bsmm_functions = &mut *(opaque as *mut BSMMFunctions);
                bsmm_functions.destroy.iter_mut().next().unwrap()()
            }
        }

        let allocate_code_section = unsafe {
            match self.bsmm_functions.allocate_code_section {
                Some(_) => {
                    allocate_code_section_callback as wrappers::LLVM_BSMMAllocateCodeSectionCallback
                }
                None => std::mem::transmute::<
                    *mut c_void,
                    wrappers::LLVM_BSMMAllocateCodeSectionCallback,
                >(0 as *mut c_void),
            }
        };

        let allocate_data_section = unsafe {
            match self.bsmm_functions.allocate_data_section {
                Some(_) => {
                    allocate_data_section_callback as wrappers::LLVM_BSMMAllocateDataSectionCallback
                }
                None => std::mem::transmute::<
                    *mut c_void,
                    wrappers::LLVM_BSMMAllocateDataSectionCallback,
                >(0 as *mut c_void),
            }
        };

        let finalize_memory = unsafe {
            match self.bsmm_functions.finalize_memory {
                Some(_) => finalize_memory_callback as wrappers::LLVM_BSMMFinalizeMemoryCallback,
                None => {
                    std::mem::transmute::<*mut c_void, wrappers::LLVM_BSMMFinalizeMemoryCallback>(
                        0 as *mut c_void,
                    )
                }
            }
        };

        let invalidate_instruction_cache = unsafe {
            match self.bsmm_functions.invalidate_instruction_cache {
                Some(_) => {
                    invalidate_instruction_cache_callback
                        as wrappers::LLVM_BSMMInvalidateInstructionCacheCallback
                }
                None => std::mem::transmute::<
                    *mut c_void,
                    wrappers::LLVM_BSMMInvalidateInstructionCacheCallback,
                >(0 as *mut c_void),
            }
        };

        let get_symbol_address = unsafe {
            match self.bsmm_functions.get_symbol_address {
                Some(_) => {
                    get_symbol_address_callback as wrappers::LLVM_BSMMGetSymbolAddressCallback
                }
                None => std::mem::transmute::<
                    *mut c_void,
                    wrappers::LLVM_BSMMGetSymbolAddressCallback,
                >(0 as *mut c_void),
            }
        };

        let destroy = unsafe {
            match self.bsmm_functions.destroy {
                Some(_) => destroy_callback as wrappers::LLVM_BSMMDestroyCallback,
                None => std::mem::transmute::<*mut c_void, wrappers::LLVM_BSMMDestroyCallback>(
                    0 as *mut c_void,
                ),
            }
        };

        let memory_manager = unsafe {
            wrappers::LLVM_CreateBindingSectionMemoryManager(
                &mut *self.bsmm_functions as *mut _ as *mut c_void,
                allocate_code_section,
                allocate_data_section,
                finalize_memory,
                invalidate_instruction_cache,
                get_symbol_address,
                destroy,
            )
        };

        BindingSectionMemoryManager {
            memory_manager: memory_manager,
            owned: true,
            _bsmm_functions: self.bsmm_functions,
        }
    }
}

pub fn get_symbol_address_in_process(name: &str) -> u64 {
    let name = std::ffi::CString::new(name).unwrap();
    unsafe { wrappers::LLVM_GetSymbolAddressInProcess(name.as_ptr() as *const c_char) }
}
