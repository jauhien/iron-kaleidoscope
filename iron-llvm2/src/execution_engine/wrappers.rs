// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Execution Engine Wrappers

use libc::{c_char, c_void, c_uint, uintptr_t};

use llvm_sys::prelude::*;
use llvm_sys::execution_engine::*;

#[allow(non_camel_case_types)]
pub type LLVM_BSMMAllocateCodeSectionCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void,
                  MM: LLVMMCJITMemoryManagerRef,
                  Size: uintptr_t,
                  Alignment: ::libc::c_uint,
                  SectionID: ::libc::c_uint,
                  SectionName: *const ::libc::c_char) -> *mut u8;

#[allow(non_camel_case_types)]
pub type LLVM_BSMMAllocateDataSectionCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void,
                  MM: LLVMMCJITMemoryManagerRef,
                  Size: uintptr_t,
                  Alignment: ::libc::c_uint,
                  SectionID: ::libc::c_uint,
                  SectionName: *const ::libc::c_char,
                  IsReadOnly: LLVMBool) -> *mut u8;

#[allow(non_camel_case_types)]
pub type LLVM_BSMMFinalizeMemoryCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void,
                  MM: LLVMMCJITMemoryManagerRef,
                  ErrMsg: *mut *mut ::libc::c_char) -> LLVMBool;

#[allow(non_camel_case_types)]
pub type LLVM_BSMMDestroyCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void);

#[allow(non_camel_case_types)]
pub type LLVM_BSMMInvalidateInstructionCacheCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void,
                  MM: LLVMMCJITMemoryManagerRef);

#[allow(non_camel_case_types)]
pub type LLVM_BSMMGetSymbolAddressCallback =
    extern "C" fn(Opaque: *mut ::libc::c_void,
                  MM: LLVMMCJITMemoryManagerRef,
                  Name: *const ::libc::c_char) -> u64;

#[allow(improper_ctypes)]
extern "C" {
    pub fn LLVM_CreateSectionMemoryManager() -> LLVMMCJITMemoryManagerRef;

    pub fn LLVM_CreateBindingSectionMemoryManager(
        Opaque: *mut c_void,
        AllocateCodeSection: LLVM_BSMMAllocateCodeSectionCallback,
        AllocateDataSection: LLVM_BSMMAllocateDataSectionCallback,
        FinalizeMemory: LLVM_BSMMFinalizeMemoryCallback,
        InvalidateInstructionCache: LLVM_BSMMInvalidateInstructionCacheCallback,
        GetSymbolAddress: LLVM_BSMMGetSymbolAddressCallback,
        Destroy: LLVM_BSMMDestroyCallback) -> LLVMMCJITMemoryManagerRef;

    pub fn LLVM_BSMMCallParentAllocateCodeSection(
        MM: LLVMMCJITMemoryManagerRef,
        Size: uintptr_t,
        Alignment: c_uint,
        SectionID: c_uint,
        SectionName: *const c_char) -> *mut u8;

    pub fn LLVM_BSMMCallParentAllocateDataSection(
        MM: LLVMMCJITMemoryManagerRef,
        Size: uintptr_t,
        Alignment: c_uint,
        SectionID: c_uint,
        SectionName: *const c_char,
        isReadOnly: LLVMBool) -> *mut u8;

    pub fn LLVM_BSMMCallParentFinalizeMemory(
        MM: LLVMMCJITMemoryManagerRef,
        ErrMsg: *mut *mut c_char) -> LLVMBool;

    pub fn LLVM_BSMMCallParentInvalidateInstructionCache(MM: LLVMMCJITMemoryManagerRef);

    pub fn LLVM_BSMMCallParentGetSymbolAddress(
        MM: LLVMMCJITMemoryManagerRef,
        Name: *const c_char) -> u64;

    pub fn LLVM_GetSymbolAddressInProcess(Name: *const c_char) -> u64;
}
