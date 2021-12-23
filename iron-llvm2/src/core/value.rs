// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Value hierarchy
// LLVM-C header Core.h

use std;
use std::ffi::CString;

use libc::{c_char, c_uint, c_ulonglong};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;

use core::basic_block::{BasicBlock, BasicBlocksIter};
use core::context::Context;
use core::module;
use core::types::{FunctionType, IntType, RealType, Type};
use {LLVMRef, LLVMRefCtor};

pub trait ValueCtor: LLVMRefCtor<LLVMValueRef> {}

pub trait Value: LLVMRef<LLVMValueRef> {
    fn get_type(&self) -> LLVMTypeRef {
        unsafe { LLVMTypeOf(self.to_ref()) }
    }

    fn get_name(&self) -> String {
        let buf = unsafe { std::ffi::CStr::from_ptr(LLVMGetValueName(self.to_ref())) };
        let result = String::from_utf8_lossy(buf.to_bytes()).into_owned();
        result
    }

    fn set_name(&self, name: &str) {
        let name = CString::new(name).unwrap();
        unsafe { LLVMSetValueName(self.to_ref(), name.as_ptr() as *const i8) }
    }

    fn dump(&self) {
        unsafe { LLVMDumpValue(self.to_ref()) }
    }

    fn print_to_string(&self) -> String {
        unsafe {
            let buf = LLVMPrintValueToString(self.to_ref());
            let cstr_buf = std::ffi::CStr::from_ptr(buf);
            let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
            LLVMDisposeMessage(buf);
            result
        }
    }

    fn replace_all_uses_with(&self, new_val: &dyn Value) {
        unsafe { LLVMReplaceAllUsesWith(self.to_ref(), new_val.to_ref()) }
    }

    fn is_constant(&self) -> bool {
        unsafe { LLVMIsConstant(self.to_ref()) > 0 }
    }

    fn is_undef(&self) -> bool {
        unsafe { LLVMIsUndef(self.to_ref()) > 0 }
    }

    fn use_iter(&self) -> UseIter {
        let first = unsafe { LLVMGetFirstUse(self.to_ref()) };

        let current = if first.is_null() { None } else { Some(first) };

        UseIter { current: current }
    }
}

impl LLVMRef<LLVMValueRef> for LLVMValueRef {
    fn to_ref(&self) -> LLVMValueRef {
        *self
    }
}

impl LLVMRefCtor<LLVMValueRef> for LLVMValueRef {
    unsafe fn from_ref(rf: LLVMValueRef) -> LLVMValueRef {
        rf
    }
}

impl Value for LLVMValueRef {}
impl ValueCtor for LLVMValueRef {}

pub trait UseCtor: LLVMRefCtor<LLVMUseRef> {}

pub trait Use: LLVMRef<LLVMUseRef> {
    fn get_user(&self) -> UserRef {
        unsafe { UserRef::from_ref(LLVMGetUser(self.to_ref())) }
    }

    fn get_used_value(&self) -> LLVMValueRef {
        unsafe { LLVMGetUsedValue(self.to_ref()) }
    }
}

impl LLVMRef<LLVMUseRef> for LLVMUseRef {
    fn to_ref(&self) -> LLVMUseRef {
        *self
    }
}

impl LLVMRefCtor<LLVMUseRef> for LLVMUseRef {
    unsafe fn from_ref(rf: LLVMUseRef) -> LLVMUseRef {
        rf
    }
}

impl Use for LLVMUseRef {}
impl UseCtor for LLVMUseRef {}

pub struct UseIter {
    current: Option<LLVMUseRef>,
}

impl Iterator for UseIter {
    type Item = LLVMUseRef;

    fn next(&mut self) -> Option<LLVMUseRef> {
        match self.current {
            Some(cur) => {
                let next = unsafe { LLVMGetNextUse(cur) };
                self.current = if next.is_null() { None } else { Some(next) };

                self.current
            }
            None => None,
        }
    }
}

// TODO: implement iterator and some better indexing for User

pub trait User: Value {
    fn get_operand(&self, index: u32) -> LLVMValueRef {
        unsafe { LLVMGetOperand(self.to_ref(), index) }
    }

    fn get_operand_use(&self, index: u32) -> LLVMUseRef {
        unsafe { LLVMUseRef::from_ref(LLVMGetOperandUse(self.to_ref(), index)) }
    }

    fn set_operand(&self, index: u32, op: &dyn Value) {
        unsafe { LLVMSetOperand(self.to_ref(), index, op.to_ref()) }
    }

    fn get_num_operands(&self) -> i32 {
        unsafe { LLVMGetNumOperands(self.to_ref()) }
    }
}

new_ref_type!(UserRef for LLVMValueRef implementing Value);

pub trait ConstCtor<Ty: Type + ?Sized>: ValueCtor {
    fn get_null(ty: &Ty) -> Self {
        unsafe { Self::from_ref(LLVMConstNull(ty.to_ref())) }
    }

    fn get_undef(ty: &Ty) -> Self {
        unsafe { Self::from_ref(LLVMGetUndef(ty.to_ref())) }
    }

    fn get_pointer_null(ty: &Ty) -> Self {
        unsafe { Self::from_ref(LLVMConstPointerNull(ty.to_ref())) }
    }
}

pub trait Const: User {
    fn is_null(&self) -> bool {
        unsafe { LLVMIsNull(self.to_ref()) > 0 }
    }
}

pub trait IntConstCtor: ConstCtor<dyn IntType> {
    fn get_all_ones(ty: &dyn IntType) -> Self {
        unsafe { Self::from_ref(LLVMConstAllOnes(ty.to_ref())) }
    }

    fn get(ty: &dyn IntType, val: u64, sign_extend: bool) -> Self {
        unsafe { Self::from_ref(LLVMConstInt(ty.to_ref(), val, sign_extend as LLVMBool)) }
    }

    fn get_arbitrary_precision(ty: &dyn IntType, words: &[u64]) -> Self {
        unsafe {
            Self::from_ref(LLVMConstIntOfArbitraryPrecision(
                ty.to_ref(),
                words.len() as c_uint,
                words.as_ptr(),
            ))
        }
    }

    fn get_from_string(ty: &dyn IntType, text: &str, radix: u8) -> Self {
        let text = CString::new(text).unwrap();
        unsafe {
            Self::from_ref(LLVMConstIntOfString(
                ty.to_ref(),
                text.as_ptr() as *const c_char,
                radix,
            ))
        }
    }
}

pub trait IntConst: Const {
    fn get_z_ext_value(&self) -> u64 {
        unsafe { LLVMConstIntGetZExtValue(self.to_ref()) }
    }

    fn get_s_ext_value(&self) -> i64 {
        unsafe { LLVMConstIntGetSExtValue(self.to_ref()) }
    }
}

new_ref_type!(IntConstRef for LLVMValueRef
implementing
Value,
User,
Const,
ValueCtor,
ConstCtor<dyn IntType>,
IntConstCtor,
IntConst
);

pub trait RealConstCtor: ConstCtor<dyn RealType> {
    fn get(ty: &dyn RealType, val: f64) -> Self {
        unsafe { Self::from_ref(LLVMConstReal(ty.to_ref(), val)) }
    }

    fn get_from_string(ty: &dyn RealType, text: &str) -> Self {
        let text = CString::new(text).unwrap();
        unsafe {
            Self::from_ref(LLVMConstRealOfString(
                ty.to_ref(),
                text.as_ptr() as *const c_char,
            ))
        }
    }
}

pub trait RealConst: Const {
    fn get_double(&self) -> (f64, bool) {
        let mut info_lost: LLVMBool = 0;
        let val = unsafe { LLVMConstRealGetDouble(self.to_ref(), &mut info_lost) };

        (val, info_lost > 0)
    }
}

new_ref_type!(RealConstRef for LLVMValueRef
implementing
Value,
User,
Const,
ValueCtor,
ConstCtor<dyn RealType>,
RealConstCtor,
RealConst
);

pub trait FunctionCtor: ValueCtor {
    fn new(module: &mut module::Module, name: &str, ty: &dyn FunctionType) -> Self {
        let name = CString::new(name).unwrap();
        unsafe {
            Self::from_ref(LLVMAddFunction(
                module.to_ref(),
                name.as_ptr() as *const c_char,
                ty.to_ref(),
            ))
        }
    }
}

pub trait Function: Const {
    fn get_intrinsic_id(&self) -> u32 {
        unsafe { LLVMGetIntrinsicID(self.to_ref()) }
    }

    fn get_call_conv(&self) -> LLVMCallConv {
        unsafe { std::mem::transmute(LLVMGetFunctionCallConv(self.to_ref())) }
    }

    fn set_call_conv(&mut self, cc: LLVMCallConv) {
        unsafe { LLVMSetFunctionCallConv(self.to_ref(), cc as c_uint) }
    }

    fn add_attr(&mut self, attr: LLVMAttribute) {
        unsafe { LLVMAddFunctionAttr(self.to_ref(), attr) }
    }

    fn get_attr(&self) -> LLVMAttribute {
        unsafe { LLVMGetFunctionAttr(self.to_ref()) }
    }

    fn remove_attr(&mut self, attr: LLVMAttribute) {
        unsafe { LLVMRemoveFunctionAttr(self.to_ref(), attr) }
    }

    fn count_params(&self) -> u32 {
        unsafe { LLVMCountParams(self.to_ref()) }
    }

    fn get_params(&self) -> Vec<LLVMValueRef> {
        let params_count = self.count_params();
        let mut buf: Vec<LLVMValueRef> = Vec::with_capacity(params_count as usize);
        let p = buf.as_mut_ptr();
        unsafe {
            std::mem::forget(buf);
            LLVMGetParams(self.to_ref(), p);
            Vec::from_raw_parts(p, params_count as usize, params_count as usize)
        }
    }

    fn get_param(&self, index: u32) -> ArgumentRef {
        unsafe { ArgumentRef::from_ref(LLVMGetParam(self.to_ref(), index)) }
    }

    fn params_iter(&self) -> ArgsIter {
        ArgsIter {
            current: None,
            func: self.to_ref(),
            pos: 0,
            size: 0,
        }
    }

    fn count_basic_blocks(&self) -> u32 {
        unsafe { LLVMCountBasicBlocks(self.to_ref()) }
    }

    fn basic_blocks_iter(&self) -> BasicBlocksIter {
        let first = unsafe { LLVMGetFirstBasicBlock(self.to_ref()) };

        let current = if first.is_null() { None } else { Some(first) };

        BasicBlocksIter { current: current }
    }

    fn get_entry(&self) -> LLVMBasicBlockRef {
        unsafe { LLVMGetEntryBasicBlock(self.to_ref()) }
    }

    fn append_basic_block_in_context(
        &mut self,
        context: &mut Context,
        name: &str,
    ) -> LLVMBasicBlockRef {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMAppendBasicBlockInContext(
                context.to_ref(),
                self.to_ref(),
                name.as_ptr() as *const c_char,
            )
        }
    }

    fn append_basic_block(&mut self, name: &str) -> LLVMBasicBlockRef {
        let name = CString::new(name).unwrap();
        unsafe { LLVMAppendBasicBlock(self.to_ref(), name.as_ptr() as *const c_char) }
    }

    fn verify(&self, action: LLVMVerifierFailureAction) -> bool {
        unsafe { LLVMVerifyFunction(self.to_ref(), action) > 0 }
    }

    fn view_cfg(&self) {
        unsafe { LLVMViewFunctionCFG(self.to_ref()) }
    }

    fn view_cfg_only(&self) {
        unsafe { LLVMViewFunctionCFGOnly(self.to_ref()) }
    }
}

new_ref_type!(FunctionRef for LLVMValueRef
implementing
Value,
User,
Const,
Function,
ValueCtor,
FunctionCtor
);

//TODO Add args back iter

pub trait Argument: Value {
    fn get_parent(&self) -> FunctionRef {
        unsafe { FunctionRef::from_ref(LLVMGetParamParent(self.to_ref())) }
    }

    fn add_attr(&self, attr: LLVMAttribute) {
        unsafe { LLVMAddAttribute(self.to_ref(), attr) }
    }

    fn get_attr(&self) -> LLVMAttribute {
        unsafe { LLVMGetAttribute(self.to_ref()) }
    }

    fn remove_attr(&self, attr: LLVMAttribute) {
        unsafe { LLVMRemoveAttribute(self.to_ref(), attr) }
    }

    fn set_alignment(&self, align: u32) {
        unsafe { LLVMSetParamAlignment(self.to_ref(), align) }
    }
}

new_ref_type!(ArgumentRef for LLVMValueRef
implementing
Value,
Argument
);

pub struct ArgsIter {
    current: Option<LLVMValueRef>,
    func: LLVMValueRef,
    pos: u32,
    size: u32,
}

impl Iterator for ArgsIter {
    type Item = ArgumentRef;

    fn next(&mut self) -> Option<ArgumentRef> {
        let cur = match self.current {
            Some(cur) => cur,
            None => unsafe {
                self.size = LLVMCountParams(self.func);
                self.pos = 0;
                if self.size == 0 {
                    return None;
                }
                LLVMGetFirstParam(self.func)
            },
        };

        if self.pos >= self.size {
            return None;
        }

        unsafe {
            if self.pos < self.size - 1 {
                self.current = Some(LLVMGetNextParam(cur))
            }

            self.pos += 1;

            Some(ArgumentRef::from_ref(cur))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (pos, size) = match self.current {
            Some(_) => (self.pos, self.size),
            None => unsafe { (0, LLVMCountParams(self.func)) },
        };

        ((size - pos) as usize, Some((size - pos) as usize))
    }
}
