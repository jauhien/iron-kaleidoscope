// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Type hierarchy
// LLVM-C header Core.h

use std;
use std::ffi::CString;

use libc::{c_char, c_uint};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;

use core::context;
use {LLVMRef, LLVMRefCtor};

pub trait Type: LLVMRef<LLVMTypeRef> {
    fn get_kind(&self) -> LLVMTypeKind {
        unsafe { LLVMGetTypeKind(self.to_ref()) }
    }

    fn is_sized(&self) -> bool {
        unsafe { LLVMTypeIsSized(self.to_ref()) > 0 }
    }

    fn get_context(&self) -> context::Context {
        unsafe {
            let ctx = LLVMGetTypeContext(self.to_ref());
            context::Context::from_ref(ctx)
        }
    }

    fn dump(&self) {
        unsafe {
            LLVMDumpType(self.to_ref());
        }
    }

    fn print_to_string(&self) -> String {
        unsafe {
            let buf = LLVMPrintTypeToString(self.to_ref());
            let cstr_buf = std::ffi::CStr::from_ptr(buf);
            let result = String::from_utf8_lossy(cstr_buf.to_bytes()).into_owned();
            LLVMDisposeMessage(buf);
            result
        }
    }
}

pub trait TypeCtor: LLVMRefCtor<LLVMTypeRef> {}

impl LLVMRef<LLVMTypeRef> for LLVMTypeRef {
    fn to_ref(&self) -> LLVMTypeRef {
        *self
    }
}

impl LLVMRefCtor<LLVMTypeRef> for LLVMTypeRef {
    unsafe fn from_ref(rf: LLVMTypeRef) -> LLVMTypeRef {
        rf
    }
}

impl Type for LLVMTypeRef {}
impl TypeCtor for LLVMTypeRef {}

pub trait IntTypeCtor: TypeCtor {
    fn get_int1_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMInt1TypeInContext(ctx.to_ref())) }
    }

    fn get_int8_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMInt8TypeInContext(ctx.to_ref())) }
    }

    fn get_int16_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMInt16TypeInContext(ctx.to_ref())) }
    }

    fn get_int32_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMInt32TypeInContext(ctx.to_ref())) }
    }

    fn get_int64_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMInt64TypeInContext(ctx.to_ref())) }
    }

    fn get_int_in_context(ctx: &context::Context, num_bits: u32) -> Self {
        unsafe { Self::from_ref(LLVMIntTypeInContext(ctx.to_ref(), num_bits)) }
    }

    fn get_int1() -> Self {
        unsafe { Self::from_ref(LLVMInt1Type()) }
    }

    fn get_int8() -> Self {
        unsafe { Self::from_ref(LLVMInt8Type()) }
    }

    fn get_int16() -> Self {
        unsafe { Self::from_ref(LLVMInt16Type()) }
    }

    fn get_int32() -> Self {
        unsafe { Self::from_ref(LLVMInt32Type()) }
    }

    fn get_int64() -> Self {
        unsafe { Self::from_ref(LLVMInt64Type()) }
    }

    fn get_int(num_bits: u32) -> Self {
        unsafe { Self::from_ref(LLVMIntType(num_bits)) }
    }
}

pub trait IntType: Type {
    fn get_width(&self) -> u32 {
        unsafe { LLVMGetIntTypeWidth(self.to_ref()) }
    }
}

new_ref_type!(IntTypeRef for LLVMTypeRef implementing Type, IntType, TypeCtor, IntTypeCtor);

pub trait RealTypeCtor: TypeCtor {
    fn get_half_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMHalfTypeInContext(ctx.to_ref())) }
    }

    fn get_float_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMFloatTypeInContext(ctx.to_ref())) }
    }

    fn get_double_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMDoubleTypeInContext(ctx.to_ref())) }
    }

    fn get_x86fp80_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMX86FP80TypeInContext(ctx.to_ref())) }
    }

    fn get_fp128_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMFP128TypeInContext(ctx.to_ref())) }
    }

    fn get_ppcfp128_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMPPCFP128TypeInContext(ctx.to_ref())) }
    }

    fn get_half() -> Self {
        unsafe { Self::from_ref(LLVMHalfType()) }
    }

    fn get_float() -> Self {
        unsafe { Self::from_ref(LLVMFloatType()) }
    }

    fn get_double() -> Self {
        unsafe { Self::from_ref(LLVMDoubleType()) }
    }

    fn get_x86fp80() -> Self {
        unsafe { Self::from_ref(LLVMX86FP80Type()) }
    }

    fn get_fp128() -> Self {
        unsafe { Self::from_ref(LLVMFP128Type()) }
    }

    fn get_ppcfp128() -> Self {
        unsafe { Self::from_ref(LLVMPPCFP128Type()) }
    }
}

pub trait RealType: Type {}

new_ref_type!(RealTypeRef for LLVMTypeRef implementing Type, RealType, TypeCtor, RealTypeCtor);

pub trait FunctionTypeCtor: TypeCtor {
    fn get(return_type: &dyn Type, param_types: &mut [LLVMTypeRef], is_var_arg: bool) -> Self {
        unsafe {
            Self::from_ref(LLVMFunctionType(
                return_type.to_ref(),
                param_types.as_mut_ptr(),
                param_types.len() as c_uint,
                is_var_arg as LLVMBool,
            ))
        }
    }
}

pub trait FunctionType: Type {
    fn is_var_arg(&self) -> bool {
        unsafe { LLVMIsFunctionVarArg(self.to_ref()) > 0 }
    }

    fn get_return_type(&self) -> LLVMTypeRef {
        unsafe { LLVMGetReturnType(self.to_ref()) }
    }

    fn count_param_types(&self) -> u32 {
        unsafe { LLVMCountParamTypes(self.to_ref()) }
    }

    fn get_param_types(&self) -> Vec<LLVMTypeRef> {
        let params_count = self.count_param_types();
        let mut buf: Vec<LLVMTypeRef> = Vec::with_capacity(params_count as usize);
        let p = buf.as_mut_ptr();
        unsafe {
            std::mem::forget(buf);
            LLVMGetParamTypes(self.to_ref(), p);
            Vec::from_raw_parts(p, params_count as usize, params_count as usize)
        }
    }
}

new_ref_type!(FunctionTypeRef for LLVMTypeRef implementing Type, FunctionType, TypeCtor, FunctionTypeCtor);

pub trait StructTypeCtor: TypeCtor {
    fn get_in_context(
        ctx: &context::Context,
        element_types: &mut [LLVMTypeRef],
        packed: bool,
    ) -> Self {
        unsafe {
            Self::from_ref(LLVMStructTypeInContext(
                ctx.to_ref(),
                element_types.as_mut_ptr(),
                element_types.len() as c_uint,
                packed as LLVMBool,
            ))
        }
    }

    fn get(element_types: &mut [LLVMTypeRef], packed: bool) -> Self {
        unsafe {
            Self::from_ref(LLVMStructType(
                element_types.as_mut_ptr(),
                element_types.len() as c_uint,
                packed as LLVMBool,
            ))
        }
    }

    fn new_named(ctx: &context::Context, name: &str) -> Self {
        let name = CString::new(name).unwrap();
        unsafe {
            Self::from_ref(LLVMStructCreateNamed(
                ctx.to_ref(),
                name.as_ptr() as *const c_char,
            ))
        }
    }
}

pub trait StructType: Type {
    fn get_name(&self) -> String {
        let buf = unsafe { std::ffi::CStr::from_ptr(LLVMGetStructName(self.to_ref())) };
        String::from_utf8_lossy(buf.to_bytes()).into_owned()
    }

    fn set_body(&self, element_types: &mut [LLVMTypeRef], packed: bool) {
        unsafe {
            LLVMStructSetBody(
                self.to_ref(),
                element_types.as_mut_ptr(),
                element_types.len() as c_uint,
                packed as LLVMBool,
            )
        }
    }

    fn count_element_types(&self) -> u32 {
        unsafe { LLVMCountStructElementTypes(self.to_ref()) }
    }

    fn get_element_types(&self) -> Vec<LLVMTypeRef> {
        let element_count = self.count_element_types();
        let mut buf: Vec<LLVMTypeRef> = Vec::with_capacity(element_count as usize);
        let p = buf.as_mut_ptr();
        unsafe {
            std::mem::forget(buf);
            LLVMGetStructElementTypes(self.to_ref(), p);
            Vec::from_raw_parts(p, element_count as usize, element_count as usize)
        }
    }

    fn is_packed(&self) -> bool {
        unsafe { LLVMIsPackedStruct(self.to_ref()) > 0 }
    }

    fn is_opaque(&self) -> bool {
        unsafe { LLVMIsOpaqueStruct(self.to_ref()) > 0 }
    }
}

new_ref_type!(StructTypeRef for LLVMTypeRef implementing Type, StructType, TypeCtor, StructTypeCtor);

pub trait SequentialTypeCtor: TypeCtor {}

pub trait SequentialType: Type {
    fn get_element_type(&self) -> LLVMTypeRef {
        unsafe { LLVMGetElementType(self.to_ref()) }
    }
}

pub trait ArrayTypeCtor: SequentialTypeCtor {
    fn get(element_type: &dyn Type, element_count: u32) -> Self {
        unsafe { Self::from_ref(LLVMArrayType(element_type.to_ref(), element_count)) }
    }
}

pub trait ArrayType: SequentialType {
    fn get_length(&self) -> u32 {
        unsafe { LLVMGetArrayLength(self.to_ref()) }
    }
}

pub trait PointerTypeCtor: SequentialTypeCtor {
    fn get(element_type: &dyn Type, address_space: u32) -> Self {
        unsafe { Self::from_ref(LLVMPointerType(element_type.to_ref(), address_space)) }
    }
}

pub trait PointerType: SequentialType {
    fn get_address_space(&self) -> u32 {
        unsafe { LLVMGetPointerAddressSpace(self.to_ref()) }
    }
}

pub trait VectorTypeCtor: SequentialTypeCtor {
    fn get(element_type: &dyn Type, element_count: u32) -> Self {
        unsafe { Self::from_ref(LLVMVectorType(element_type.to_ref(), element_count)) }
    }
}

pub trait VectorType: SequentialType {
    fn get_size(&self) -> u32 {
        unsafe { LLVMGetVectorSize(self.to_ref()) }
    }
}

new_ref_type!(ArrayTypeRef for LLVMTypeRef
              implementing
              Type,
              SequentialType,
              ArrayType,
              TypeCtor,
              SequentialTypeCtor,
              ArrayTypeCtor);

new_ref_type!(PointerTypeRef for LLVMTypeRef
              implementing
              Type,
              SequentialType,
              PointerType,
              TypeCtor,
              SequentialTypeCtor,
              PointerTypeCtor);

new_ref_type!(VectorTypeRef for LLVMTypeRef
              implementing
              Type,
              SequentialType,
              VectorType,
              TypeCtor,
              SequentialTypeCtor,
              VectorTypeCtor);

pub trait VoidTypeCtor: TypeCtor {
    fn get_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMVoidTypeInContext(ctx.to_ref())) }
    }

    fn get() -> Self {
        unsafe { Self::from_ref(LLVMVoidType()) }
    }
}

pub trait VoidType {}

pub trait LabelTypeCtor: TypeCtor {
    fn get_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMLabelTypeInContext(ctx.to_ref())) }
    }

    fn get() -> Self {
        unsafe { Self::from_ref(LLVMLabelType()) }
    }
}

pub trait LabelType {}

pub trait X86MMXTypeCtor: TypeCtor {
    fn get_in_context(ctx: &context::Context) -> Self {
        unsafe { Self::from_ref(LLVMX86MMXTypeInContext(ctx.to_ref())) }
    }

    fn get() -> Self {
        unsafe { Self::from_ref(LLVMX86MMXType()) }
    }
}

pub trait X86MMXType {}

new_ref_type!(VoidTypeRef for LLVMTypeRef implementing Type, VoidType, TypeCtor, VoidTypeCtor);
new_ref_type!(LabelTypeRef for LLVMTypeRef implementing Type, LabelType, TypeCtor, LabelTypeCtor);
new_ref_type!(X86MMXTypeRef for LLVMTypeRef implementing Type, X86MMXType, TypeCtor, X86MMXTypeCtor);
