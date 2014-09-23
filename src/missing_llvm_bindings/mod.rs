#![allow(non_camel_case_types)]

use libc::{c_char, c_uint};

use rustc::lib::llvm::{ExecutionEngineRef, ModuleRef, TargetDataRef, TypeRef, ValueRef};

pub use self::llvmdeps::LLVMInitializeNativeTarget;

pub enum GenericValue_opaque {}
pub type GenericValueRef = *mut GenericValue_opaque;

extern {
    pub fn LLVMCreateExecutionEngineForModule(OutEE: *mut ExecutionEngineRef,
                                              M: ModuleRef,
                                              OutError: *mut *const c_char);

    pub fn LLVMDisposeExecutionEngine(EE: ExecutionEngineRef);

    pub fn LLVMGetExecutionEngineTargetData(EE: ExecutionEngineRef)
                                            -> TargetDataRef;

    pub fn LLVMCopyStringRepOfTargetData(TD: TargetDataRef)
                                         -> *const c_char;

    pub fn LLVMDisposeMessage(Message: *const c_char);

    pub fn LLVMRunFunction(EE: ExecutionEngineRef,
                           F: ValueRef,
                           NumArgs: c_uint,
                           Args: *const GenericValueRef)
                           -> GenericValueRef;

    pub fn LLVMGenericValueToFloat(TyRef: TypeRef,
                                   GenVal: GenericValueRef)
                                   -> f64;
}

mod llvmdeps;
