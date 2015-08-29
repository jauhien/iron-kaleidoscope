use std;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core;
use iron_llvm::core::types::{FunctionType, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value};
use iron_llvm::execution_engine::ExecutionEngine;
use iron_llvm::execution_engine::execution_engine::FrozenModule;

pub struct MCJITter {
    module_name: String,
    current_module: core::Module,
    function_passmanager: core::FunctionPassManager,

    execution_engines: Vec<ExecutionEngine>,
    modules: Vec<FrozenModule>
}

impl MCJITter {
    fn new_module(name: &str) -> (core::Module, core::FunctionPassManager) {
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

        (module, function_passmanager)
    }

    pub fn new(name: &str) -> MCJITter {
        let (current_module, function_passmanager) = MCJITter::new_module(name);

        MCJITter {
            module_name: String::from(name),
            current_module: current_module,
            function_passmanager: function_passmanager,
            execution_engines: vec![],
            modules: vec![]
        }
    }

    pub fn get_current_module(&mut self) -> &mut core::Module {
        &mut self.current_module
    }

    pub fn get_function(&mut self, name: &str) -> Result<FunctionRef, String> {
        for ee in &self.execution_engines {
            let funct = match ee.find_function(name) {
                Some(f) => {
                    f
                },
                None => continue
            };

            let proto = match self.current_module.get_function_by_name(name) {
                Some(f) => {
                    if f.count_basic_blocks() != 0 {
                        return Err("redefinition of function across modules".to_string())
                    }
                    f
                },
                None => {
                    // TODO: fix iron-llvm get_type
                    let fty = unsafe { FunctionTypeRef::from_ref(funct.get_type().to_ref()) };
                    let fty = unsafe { FunctionTypeRef::from_ref(fty.get_return_type().to_ref()) };
                    FunctionRef::new(&mut self.current_module, name, &fty)
                }
            };

            return Ok(proto)
        }

        match self.current_module.get_function_by_name(name) {
            Some(f) => Ok(f),
            None => Err("function not found: ".to_string() + name)
        }
    }

    pub fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }

    pub fn close_current_module(&mut self) {
        let (new_module, new_function_passmanager) = MCJITter::new_module(&self.module_name);
        self.function_passmanager = new_function_passmanager;
        let current_module = std::mem::replace(&mut self.current_module, new_module);
        let (execution_engine, module) = match ExecutionEngine::new_jit_compiler(current_module, 0) {
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
