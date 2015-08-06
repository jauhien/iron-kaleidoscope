use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::LLVMRefCtor;
use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::FunctionRef;
use iron_llvm::execution_engine::ExecutionEngine;
use iron_llvm::execution_engine::execution_engine::FrozenModule;

pub struct Context {
    module: core::Module,
    function_passmanager: core::FunctionPassManager
}

impl Context {
    pub fn new(name: &str) -> Context {
        let module = core::Module::new(name);
        let mut function_passmanager = core::FunctionPassManager::new(&module);
        function_passmanager.add_verifier_pass();
        function_passmanager.add_promote_memory_to_register_pass();
        function_passmanager.add_basic_alias_analysis_pass();
        function_passmanager.add_instruction_combining_pass();
        function_passmanager.add_reassociate_pass();
        function_passmanager.add_GVN_pass();
        function_passmanager.add_CFG_simplification_pass();
        function_passmanager.initialize();

        Context {
            module: module,
            function_passmanager: function_passmanager
        }
    }

    pub fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    pub fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }
}

pub struct MCJITter {
    execution_engines: Vec<ExecutionEngine>,
    modules: Vec<FrozenModule>
}

impl MCJITter {
    pub fn new() -> MCJITter {
        MCJITter {
            execution_engines: vec![],
            modules: vec![]
        }
    }

    pub fn add(&mut self, context: Context) {
        let (execution_engine, module) = match ExecutionEngine::new_jit_compiler(context.module, 0) {
            Ok((ee, module)) => (ee, module),
            Err(msg) => panic!(msg)
        };

        self.execution_engines.push(execution_engine);
        self.modules.push(module);
    }

    pub fn run_function(&mut self, f: LLVMValueRef) -> f64 {
        let f = unsafe {FunctionRef::from_ref(f)};
        let mut args = vec![];
        let res = match self.execution_engines.last_mut() {
            Some(ee) => ee.run_function(&f, args.as_mut_slice()),
            None => panic!("MCJITter went crazy")
        };
        let ty = RealTypeRef::get_double();
        res.to_float(&ty)
    }

    pub fn dump(&self) {
        for module in self.modules.iter() {
            module.get().dump()
        }
    }
}
