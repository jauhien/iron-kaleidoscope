// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Instruction Builders
// LLVM-C header Core.h

use std;
use std::ffi::CString;

use libc::{c_char, c_uint};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;

use core::basic_block::BasicBlock;
use core::context::Context;
use core::instruction::Instruction;
use core::types::Type;
use {LLVMRef, LLVMRefCtor};

pub struct Builder {
    builder: LLVMBuilderRef,
}

//TODO debug locations, aggregate ret, better instruction types
impl Builder {
    pub fn new() -> Builder {
        unsafe {
            Builder {
                builder: LLVMCreateBuilder(),
            }
        }
    }

    pub fn new_in_context(ctx: &mut Context) -> Builder {
        unsafe {
            Builder {
                builder: LLVMCreateBuilderInContext(ctx.to_ref()),
            }
        }
    }

    pub fn position(&mut self, block: &mut dyn BasicBlock, instr: &dyn Instruction) {
        unsafe { LLVMPositionBuilder(self.to_ref(), block.to_ref(), instr.to_ref()) }
    }

    pub fn position_before(&mut self, instr: &dyn Instruction) {
        unsafe { LLVMPositionBuilderBefore(self.to_ref(), instr.to_ref()) }
    }

    pub fn position_at_end(&mut self, block: &mut dyn BasicBlock) {
        unsafe { LLVMPositionBuilderAtEnd(self.to_ref(), block.to_ref()) }
    }

    pub fn get_insert_block(&self) -> LLVMBasicBlockRef {
        unsafe { LLVMGetInsertBlock(self.to_ref()) }
    }

    pub fn clear_insertion_position(&mut self) {
        unsafe { LLVMClearInsertionPosition(self.to_ref()) }
    }

    pub fn insert(&mut self, instr: &mut dyn Instruction) {
        unsafe { LLVMInsertIntoBuilder(self.to_ref(), instr.to_ref()) }
    }

    pub fn insert_with_name(&mut self, instr: &mut dyn Instruction, name: &str) {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMInsertIntoBuilderWithName(
                self.to_ref(),
                instr.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_ret_void(&mut self) -> LLVMValueRef {
        unsafe { LLVMBuildRetVoid(self.to_ref()) }
    }

    pub fn build_ret(&mut self, v: &LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildRet(self.to_ref(), v.to_ref()) }
    }

    pub fn build_br(&mut self, dest: &dyn BasicBlock) -> LLVMValueRef {
        unsafe { LLVMBuildBr(self.to_ref(), dest.to_ref()) }
    }

    pub fn build_cond_br(
        &mut self,
        cond: LLVMValueRef,
        then_bl: &dyn BasicBlock,
        else_bl: &dyn BasicBlock,
    ) -> LLVMValueRef {
        unsafe { LLVMBuildCondBr(self.to_ref(), cond, then_bl.to_ref(), else_bl.to_ref()) }
    }

    pub fn build_switch(
        &mut self,
        v: LLVMValueRef,
        else_bl: &dyn BasicBlock,
        num_cases: u32,
    ) -> LLVMValueRef {
        unsafe { LLVMBuildSwitch(self.to_ref(), v, else_bl.to_ref(), num_cases) }
    }

    pub fn build_indirect_br(&mut self, addr: LLVMValueRef, num_dests: u32) -> LLVMValueRef {
        unsafe { LLVMBuildIndirectBr(self.to_ref(), addr, num_dests) }
    }

    pub fn build_invoke(
        &mut self,
        func: LLVMValueRef,
        args: &mut [LLVMValueRef],
        then_bl: &dyn BasicBlock,
        catch_bl: &dyn BasicBlock,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildInvoke(
                self.to_ref(),
                func,
                args.as_mut_ptr(),
                args.len() as c_uint,
                then_bl.to_ref(),
                catch_bl.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_landing_pad(
        &mut self,
        func: LLVMValueRef,
        args: &mut [LLVMValueRef],
        then_bl: &dyn BasicBlock,
        catch_bl: &dyn BasicBlock,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildInvoke(
                self.to_ref(),
                func,
                args.as_mut_ptr(),
                args.len() as c_uint,
                then_bl.to_ref(),
                catch_bl.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_resume(&mut self, exn: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildResume(self.to_ref(), exn) }
    }

    pub fn build_unreachable(&mut self) -> LLVMValueRef {
        unsafe { LLVMBuildUnreachable(self.to_ref()) }
    }

    //TODO move it to appropriate instruction trait
    pub fn add_case(switch: LLVMValueRef, on_val: LLVMValueRef, dest: &dyn BasicBlock) {
        unsafe { LLVMAddCase(switch, on_val, dest.to_ref()) }
    }

    pub fn add_destination(indirect_br: LLVMValueRef, dest: &dyn BasicBlock) {
        unsafe { LLVMAddDestination(indirect_br, dest.to_ref()) }
    }

    pub fn add_clause(landing_pad: LLVMValueRef, clause_val: LLVMValueRef) {
        unsafe { LLVMAddClause(landing_pad, clause_val) }
    }

    pub fn set_cleanup(landing_pad: LLVMValueRef, val: bool) {
        unsafe { LLVMSetCleanup(landing_pad, val as LLVMBool) }
    }

    pub fn build_add(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildAdd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nsw_add(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNSWAdd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nuw_add(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNUWAdd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fadd(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFAdd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_sub(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSub(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nsw_sub(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNSWSub(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nuw_sub(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNUWSub(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fsub(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFSub(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_mul(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildMul(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nsw_mul(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNSWMul(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_nuw_mul(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNUWMul(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fmul(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFMul(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_udiv(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildUDiv(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_sdiv(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSDiv(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_exact_sdiv(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildExactSDiv(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_urem(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildURem(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_srem(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSRem(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_frem(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFRem(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_shl(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildShl(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_lshr(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildLShr(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_ashr(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildAShr(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_and(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildAnd(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_or(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildOr(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_xor(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildXor(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_binop(
        &mut self,
        op: LLVMOpcode,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildBinOp(self.to_ref(), op, lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_neg(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNeg(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_nsw_neg(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNSWNeg(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_nuw_neg(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNUWNeg(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_fneg(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFNeg(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_not(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildNot(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_malloc(&mut self, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildMalloc(self.to_ref(), ty.to_ref(), name.as_ptr() as *const c_char) }
    }

    pub fn build_array_malloc(
        &mut self,
        ty: LLVMTypeRef,
        v: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildArrayMalloc(
                self.to_ref(),
                ty.to_ref(),
                v,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_alloca(&mut self, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildAlloca(self.to_ref(), ty.to_ref(), name.as_ptr() as *const c_char) }
    }

    pub fn build_array_alloca(
        &mut self,
        ty: LLVMTypeRef,
        v: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildArrayAlloca(
                self.to_ref(),
                ty.to_ref(),
                v,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_free(&mut self, pointer: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildFree(self.to_ref(), pointer) }
    }

    pub fn build_load(&mut self, pointer: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildLoad(self.to_ref(), pointer, name.as_ptr() as *const c_char) }
    }

    pub fn build_store(&mut self, v: LLVMValueRef, pointer: LLVMValueRef) -> LLVMValueRef {
        unsafe { LLVMBuildStore(self.to_ref(), v, pointer) }
    }

    pub fn build_gep(
        &mut self,
        pointer: LLVMValueRef,
        indices: &mut [LLVMValueRef],
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildGEP(
                self.to_ref(),
                pointer,
                indices.as_mut_ptr(),
                indices.len() as c_uint,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_in_bounds_gep(
        &mut self,
        pointer: LLVMValueRef,
        indices: &mut [LLVMValueRef],
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildInBoundsGEP(
                self.to_ref(),
                pointer,
                indices.as_mut_ptr(),
                indices.len() as c_uint,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_struct_gep(
        &mut self,
        pointer: LLVMValueRef,
        idx: u32,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildStructGEP(self.to_ref(), pointer, idx, name.as_ptr() as *const c_char) }
    }

    pub fn build_global_string(&mut self, string: &str, name: &str) -> LLVMValueRef {
        let string = CString::new(string).unwrap();
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildGlobalString(
                self.to_ref(),
                string.as_ptr() as *const c_char,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_global_string_ptr(&mut self, string: &str, name: &str) -> LLVMValueRef {
        let string = CString::new(string).unwrap();
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildGlobalStringPtr(
                self.to_ref(),
                string.as_ptr() as *const c_char,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn get_volatile(memory_access_inst: LLVMValueRef) -> bool {
        unsafe { LLVMGetVolatile(memory_access_inst) > 0 }
    }

    pub fn set_volatile(memory_access_inst: LLVMValueRef, is_volatile: bool) {
        unsafe { LLVMSetVolatile(memory_access_inst, is_volatile as LLVMBool) }
    }

    pub fn build_trunc(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildTrunc(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_zext(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildZExt(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_sext(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSExt(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_fp_to_ui(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFPToUI(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_fp_to_si(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFPToSI(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_ui_to_fp(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildUIToFP(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_si_to_fp(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSIToFP(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_fp_trunc(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFPTrunc(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_fp_ext(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFPExt(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_ptr_to_int(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildPtrToInt(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_int_to_ptr(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildIntToPtr(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_bit_cast(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildBitCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_addr_space_cast(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildAddrSpaceCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_zext_or_bit_cast(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildZExtOrBitCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_sext_or_bit_cast(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildSExtOrBitCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_trunc_or_bit_cast(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildTruncOrBitCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_cast(
        &mut self,
        op: LLVMOpcode,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildCast(self.to_ref(), op, v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_pointer_cast(
        &mut self,
        v: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildPointerCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_int_cast(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildIntCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_fp_cast(&mut self, v: LLVMValueRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFPCast(self.to_ref(), v, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_icmp(
        &mut self,
        op: LLVMIntPredicate,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildICmp(self.to_ref(), op, lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fcmp(
        &mut self,
        op: LLVMRealPredicate,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildFCmp(self.to_ref(), op, lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_phi(&mut self, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildPhi(self.to_ref(), ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_call(
        &mut self,
        func: LLVMValueRef,
        args: &mut [LLVMValueRef],
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildCall(
                self.to_ref(),
                func,
                args.as_mut_ptr(),
                args.len() as c_uint,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_select(
        &mut self,
        cond: LLVMValueRef,
        then_bl: &dyn BasicBlock,
        else_bl: &dyn BasicBlock,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildSelect(
                self.to_ref(),
                cond,
                then_bl.to_ref(),
                else_bl.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_va_arg(
        &mut self,
        list: LLVMValueRef,
        ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildVAArg(self.to_ref(), list, ty, name.as_ptr() as *const c_char) }
    }

    pub fn build_extract_element(
        &mut self,
        vec_val: LLVMValueRef,
        idx: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildExtractElement(self.to_ref(), vec_val, idx, name.as_ptr() as *const c_char)
        }
    }

    pub fn build_insert_element(
        &mut self,
        vec_val: LLVMValueRef,
        v: LLVMValueRef,
        idx: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildInsertElement(
                self.to_ref(),
                vec_val,
                v,
                idx,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_shuffle_vector(
        &mut self,
        v1: LLVMValueRef,
        v2: LLVMValueRef,
        mask: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildShuffleVector(self.to_ref(), v1, v2, mask, name.as_ptr() as *const c_char)
        }
    }

    pub fn build_extract_value(
        &mut self,
        agg_val: LLVMValueRef,
        idx: u32,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildExtractValue(self.to_ref(), agg_val, idx, name.as_ptr() as *const c_char)
        }
    }

    pub fn build_insert_value(
        &mut self,
        agg_val: LLVMValueRef,
        v: LLVMValueRef,
        idx: u32,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildInsertValue(
                self.to_ref(),
                agg_val,
                v,
                idx,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_is_null(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildIsNull(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_is_not_null(&mut self, v: LLVMValueRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildIsNotNull(self.to_ref(), v, name.as_ptr() as *const c_char) }
    }

    pub fn build_ptr_diff(
        &mut self,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMBuildPtrDiff(self.to_ref(), lhs, rhs, name.as_ptr() as *const c_char) }
    }

    pub fn build_fence(
        &mut self,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMBuildFence(
                self.to_ref(),
                ordering,
                single_thread as LLVMBool,
                name.as_ptr() as *const c_char,
            )
        }
    }

    pub fn build_atomic_rmw(
        &mut self,
        op: LLVMAtomicRMWBinOp,
        ptr: LLVMValueRef,
        v: LLVMValueRef,
        ordering: LLVMAtomicOrdering,
        single_thread: bool,
    ) -> LLVMValueRef {
        unsafe {
            LLVMBuildAtomicRMW(
                self.to_ref(),
                op,
                ptr,
                v,
                ordering,
                single_thread as LLVMBool,
            )
        }
    }
}

impl LLVMRef<LLVMBuilderRef> for Builder {
    fn to_ref(&self) -> LLVMBuilderRef {
        self.builder
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.to_ref());
        }
    }
}
