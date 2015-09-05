use std;
use std::cell::RefCell;
use std::rc::Rc;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core;
use iron_llvm::core::types::{FunctionType, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value};
use iron_llvm::execution_engine::{BindingSectionMemoryManagerBuilder, ExecutionEngine, MCJITBuilder};
use iron_llvm::execution_engine::execution_engine::FrozenModule;
use iron_llvm::support::add_symbol;

pub extern fn printd(x: f64) -> f64 {
    println!("> {} <", x);
    x
}

pub extern fn putchard(x: f64) -> f64 {
    print!("{}", x as u8 as char);
    x
}

pub fn init() {
    unsafe {
        add_symbol("printd", printd as *const ());
        add_symbol("putchard", putchard as *const ());
    }
}

struct ModulesContainer {
    execution_engines: Vec<ExecutionEngine>,
    modules: Vec<FrozenModule>
}

impl ModulesContainer {
    fn get_function_address(&self, name: &str) -> u64 {
        for ee in &self.execution_engines {
            let addr = ee.get_function_address(name);
            if addr != 0 {
                return addr;
            }
        }

        0
    }
}

pub struct MCJITter {
    module_name: String,
    current_module: core::Module,
    function_passmanager: core::FunctionPassManager,

    container: Rc<RefCell<ModulesContainer>>
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

            container: Rc::new(RefCell::new(ModulesContainer {
                execution_engines: vec![],
                modules: vec![]
            }))
        }
    }

    pub fn get_current_module(&mut self) -> &mut core::Module {
        &mut self.current_module
    }

    pub fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        for ee in &self.container.borrow().execution_engines {
            let funct = match ee.find_function(name) {
                Some(f) => {
                    f
                },
                None => continue
            };

            let proto = match self.current_module.get_function_by_name(name) {
                Some(f) => {
                    if f.count_basic_blocks() != 0 {
                        panic!("redefinition of function across modules".to_string())
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

            return Some((proto, true))
        }

        match self.current_module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }

    pub fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }

    pub fn close_current_module(& mut self) {
        let (new_module, new_function_passmanager) = MCJITter::new_module(&self.module_name);
        self.function_passmanager = new_function_passmanager;
        let current_module = std::mem::replace(&mut self.current_module, new_module);

        let container = self.container.clone();
        let memory_manager = BindingSectionMemoryManagerBuilder::new()
            .set_get_symbol_address(move |mut parent_mm, name| {
                let addr = parent_mm.get_symbol_address(name);
                if addr != 0 {
                    return addr;
                }

                container.borrow().get_function_address(name)
            })
            .create();

        let (execution_engine, module) = match MCJITBuilder::new()
            .set_mcjit_memory_manager(Box::new(memory_manager))
            .create(current_module) {
                Ok((ee, module)) => (ee, module),
                Err(msg) => panic!(msg)
            };

        self.container.borrow_mut().execution_engines.push(execution_engine);
        self.container.borrow_mut().modules.push(module);
    }

    pub fn run_function(&mut self, f: LLVMValueRef) -> f64 {
        let f = unsafe {FunctionRef::from_ref(f)};
        let mut args = vec![];
        let res = match self.container.borrow().execution_engines.last() {
            Some(ee) => ee.run_function(&f, args.as_mut_slice()),
            None => panic!("MCJITter went crazy")
        };
        let ty = RealTypeRef::get_double();
        res.to_float(&ty)
    }

    pub fn dump(&self) {
        for module in self.container.borrow().modules.iter() {
            module.get().dump()
        }
    }
}
