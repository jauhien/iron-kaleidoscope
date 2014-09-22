use libc::c_char;

use rustc::lib::llvm::{ExecutionEngineRef, ModuleRef, TargetDataRef};

pub use self::llvmdeps::LLVMInitializeNativeTarget;

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
}

mod llvmdeps;
