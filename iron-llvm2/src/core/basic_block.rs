// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Basic block
// LLVM-C header Core.h

use std;
use std::ffi::CString;

use libc::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;

use core::context::Context;
use core::instruction::InstructionRef;
use core::value::{FunctionRef, Value};
use {LLVMRef, LLVMRefCtor};

pub trait BasicBlockCtor: LLVMRefCtor<LLVMBasicBlockRef> {
    fn from_value(value: &dyn Value) -> Option<Self> {
        unsafe {
            if LLVMValueIsBasicBlock(value.to_ref()) > 0 {
                Some(Self::from_ref(LLVMValueAsBasicBlock(value.to_ref())))
            } else {
                None
            }
        }
    }
}

pub trait BasicBlock: LLVMRef<LLVMBasicBlockRef> + Value {
    fn get_parent(&self) -> FunctionRef {
        unsafe { FunctionRef::from_ref(LLVMGetBasicBlockParent(self.to_ref())) }
    }

    // TODO: change return type to terminator, when it will be introduced
    fn get_terminator(&self) -> LLVMValueRef {
        unsafe { LLVMGetBasicBlockTerminator(self.to_ref()) }
    }

    fn insert_basic_block_in_context(
        &self,
        context: &mut Context,
        name: &str,
    ) -> LLVMBasicBlockRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMInsertBasicBlockInContext(
                context.to_ref(),
                self.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    fn insert_basic_block(&self, name: &str) -> LLVMBasicBlockRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMInsertBasicBlock(self.to_ref(), name.as_ptr() as *const c_char) }
    }

    // TODO make this function safe and somehow inform compiler about
    // ref invalidation
    unsafe fn delete(&mut self) {
        LLVMDeleteBasicBlock(self.to_ref())
    }

    fn remove_from_parent(&mut self) {
        unsafe { LLVMRemoveBasicBlockFromParent(self.to_ref()) }
    }

    fn move_before(&self, move_pos: &dyn BasicBlock) {
        unsafe { LLVMMoveBasicBlockBefore(self.to_ref(), move_pos.to_ref()) }
    }

    fn move_after(&self, move_pos: &dyn BasicBlock) {
        unsafe { LLVMMoveBasicBlockAfter(self.to_ref(), move_pos.to_ref()) }
    }

    fn get_first_instruction(&self) -> InstructionRef {
        unsafe { InstructionRef::from_ref(LLVMGetFirstInstruction(self.to_ref())) }
    }

    fn get_last_instruction(&self) -> InstructionRef {
        unsafe { InstructionRef::from_ref(LLVMGetLastInstruction(self.to_ref())) }
    }
}

impl LLVMRef<LLVMBasicBlockRef> for LLVMBasicBlockRef {
    fn to_ref(&self) -> LLVMBasicBlockRef {
        *self
    }
}

impl LLVMRef<LLVMValueRef> for LLVMBasicBlockRef {
    fn to_ref(&self) -> LLVMValueRef {
        unsafe { LLVMBasicBlockAsValue(*self) }
    }
}

impl LLVMRefCtor<LLVMBasicBlockRef> for LLVMBasicBlockRef {
    unsafe fn from_ref(rf: LLVMBasicBlockRef) -> LLVMBasicBlockRef {
        rf
    }
}

impl Value for LLVMBasicBlockRef {}
impl BasicBlock for LLVMBasicBlockRef {}
impl BasicBlockCtor for LLVMBasicBlockRef {}

pub struct BasicBlocksIter {
    // TODO make it private, but allow access from value mod
    pub current: Option<LLVMBasicBlockRef>,
}

impl Iterator for BasicBlocksIter {
    type Item = LLVMBasicBlockRef;

    fn next(&mut self) -> Option<LLVMBasicBlockRef> {
        match self.current {
            Some(cur) => {
                let next = unsafe { LLVMGetNextBasicBlock(cur) };
                self.current = if next.is_null() { None } else { Some(next) };

                self.current
            }
            None => None,
        }
    }
}
