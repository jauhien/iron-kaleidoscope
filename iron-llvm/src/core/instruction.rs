// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Instructions hierarchy
// LLVM-C header Core.h

use std;

use libc::c_uint;

use llvm_sys::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

use ::{LLVMRef, LLVMRefCtor};
use core::value::{Value, ValueCtor};

pub trait Instruction : Value {
    fn get_parent(&self) -> LLVMBasicBlockRef {
        unsafe {
            LLVMGetInstructionParent(self.to_ref())
        }
    }

    fn get_next(&self) -> InstructionRef {
        unsafe {
            InstructionRef::from_ref(LLVMGetNextInstruction(self.to_ref()))
        }
    }

    fn get_previous(&self) -> InstructionRef {
        unsafe {
            InstructionRef::from_ref(LLVMGetPreviousInstruction(self.to_ref()))
        }
    }

    //TODO fix ownership
    fn erase_from_parent(&mut self) {
        unsafe {
            LLVMInstructionEraseFromParent(self.to_ref())
        }
    }

    fn get_opcode(&self) -> LLVMOpcode {
        unsafe {
            LLVMGetInstructionOpcode(self.to_ref())
        }
    }
}

new_ref_type!(InstructionRef for LLVMValueRef
              implementing
              Value,
              ValueCtor,
              Instruction
              );

pub trait PHINode : Instruction {
    fn add_incoming(&mut self, incoming_values: &mut [LLVMValueRef], incoming_blocks: &mut [LLVMBasicBlockRef]) {
        assert!(incoming_values.len() == incoming_blocks.len());
        unsafe {
            LLVMAddIncoming(self.to_ref(),
                            incoming_values.as_mut_ptr(),
                            incoming_blocks.as_mut_ptr(),
                            incoming_values.len() as c_uint)
        }
    }

    fn count_incoming(&self) -> u32 {
        unsafe {
            LLVMCountIncoming(self.to_ref())
        }
    }

    fn get_incoming_value(&self, index: u32) -> Option<LLVMValueRef> {
        if index < self.count_incoming() {
            unsafe {
                Some(LLVMGetIncomingValue(self.to_ref(), index))
            }
        } else {
            None
        }
    }

    fn get_incoming_block(&self, index: u32) -> Option<LLVMBasicBlockRef> {
        if index < self.count_incoming() {
            unsafe {
                Some(LLVMGetIncomingBlock(self.to_ref(), index))
            }
        } else {
            None
        }
    }
}

new_ref_type!(PHINodeRef for LLVMValueRef
              implementing
              Value,
              ValueCtor,
              Instruction,
              PHINode
              );
