use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::LLVMRefCtor;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::FunctionRef;
use iron_llvm::execution_engine::ExecutionEngine;

use builder;

pub struct MCJITter {
    execution_engine: ExecutionEngine
}

impl MCJITter {
    pub fn new(ir_builder_context: builder::Context) -> MCJITter {
        let execution_engine = match ExecutionEngine::new_jit_compiler(ir_builder_context.get_module(), 0) {
            Ok(ee) => ee,
            Err(msg) => panic!(msg)
        };

        MCJITter {
            execution_engine: execution_engine
        }
    }

    pub fn run_function(&mut self, f: LLVMValueRef) -> f64 {
        let f = unsafe {FunctionRef::from_ref(f)};
        let mut args = vec![];
        let res = self.execution_engine.run_function(&f, args.as_mut_slice());
        let ty = RealTypeRef::get_double();
        res.to_float(&ty)
    }
}
