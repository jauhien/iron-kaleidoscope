//< ch-3
use std;
use std::cell::RefCell;
use std::rc::Rc;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::types::{FunctionType, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value};
use iron_llvm::execution_engine::execution_engine::FrozenModule;
use iron_llvm::execution_engine::{
    BindingSectionMemoryManagerBuilder, ExecutionEngine, MCJITBuilder,
};
use iron_llvm::support::add_symbol;
use iron_llvm::{LLVMRef, LLVMRefCtor};

use builder;

//< jit-builtin
pub extern "C" fn printd(x: f64) -> f64 {
    println!("> {} <", x);
    x
}

pub extern "C" fn putchard(x: f64) -> f64 {
    print!("{}", x as u8 as char);
    x
}

pub fn init() {
    unsafe {
        add_symbol("printd", printd as *const ());
        add_symbol("putchard", putchard as *const ());
    }
}
//> jit-builtin

//< jit-jitter
pub trait JITter: builder::ModuleProvider {
    // TODO: fix https://github.com/rust-lang/rust/issues/5665
    fn get_module_provider(&mut self) -> &mut dyn builder::ModuleProvider;

    fn run_function(&mut self, f: LLVMValueRef) -> f64;
}
//> jit-jitter

//< jit-mc
struct ModulesContainer {
    execution_engines: Vec<ExecutionEngine>,
    modules: Vec<FrozenModule>,
}
//> jit-mc

//< jit-mc-address
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
//> jit-mc-address

//< jit-mcjitter
pub struct MCJITter {
    module_name: String,
    current_module: core::Module,
    function_passmanager: core::FunctionPassManager,

    container: Rc<RefCell<ModulesContainer>>,
}
//> jit-mcjitter

impl MCJITter {
    //< jit-mcjitter-ctor
    pub fn new(name: &str) -> MCJITter {
        let (current_module, function_passmanager) = builder::new_module(name);

        MCJITter {
            module_name: String::from(name),
            current_module: current_module,
            function_passmanager: function_passmanager,

            container: Rc::new(RefCell::new(ModulesContainer {
                execution_engines: vec![],
                modules: vec![],
            })),
        }
    }
    //> jit-mcjitter-ctor

    //< jit-mcjitter-close-module
    fn close_current_module(&mut self) {
        //< jit-mcjitter-new-module
        let (new_module, new_function_passmanager) = builder::new_module(&self.module_name);
        self.function_passmanager = new_function_passmanager;
        let current_module = std::mem::replace(&mut self.current_module, new_module);
        //> jit-mcjitter-new-module

        //< jit-mcjitter-mm
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
        //> jit-mcjitter-mm

        //< jit-mcjitter-ee
        let (execution_engine, module) = match MCJITBuilder::new()
            .set_mcjit_memory_manager(Box::new(memory_manager))
            .create(current_module)
        {
            Ok((ee, module)) => (ee, module),
            Err(msg) => panic!("{}", msg),
        };
        //> jit-mcjitter-ee

        //< jit-mcjitter-container-update
        self.container
            .borrow_mut()
            .execution_engines
            .push(execution_engine);
        self.container.borrow_mut().modules.push(module);
        //> jit-mcjitter-container-update
    }
    //> jit-mcjitter-close-module
}

impl builder::ModuleProvider for MCJITter {
    //< jit-mcjitter-mp-simple
    fn dump(&self) {
        for module in self.container.borrow().modules.iter() {
            module.get().dump()
        }
        self.current_module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.current_module
    }
    //> jit-mcjitter-mp-simple

    //< jit-mcjitter-mp-gf
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        for module in &self.container.borrow().modules {
            let funct = match module.get().get_function_by_name(name) {
                Some(f) => f,
                None => continue,
            };

            let proto = match self.current_module.get_function_by_name(name) {
                Some(f) => {
                    if funct.count_basic_blocks() != 0 && f.count_basic_blocks() != 0 {
                        panic!("redefinition of function across modules")
                    }
                    f
                }
                None => {
                    // TODO: fix iron-llvm get_type
                    let fty = unsafe { FunctionTypeRef::from_ref(funct.get_type().to_ref()) };
                    let fty = unsafe { FunctionTypeRef::from_ref(fty.get_return_type().to_ref()) };
                    FunctionRef::new(&mut self.current_module, name, &fty)
                }
            };

            if funct.count_basic_blocks() > 0 {
                return Some((proto, true));
            }
        }

        match self.current_module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None,
        }
    }
    //> jit-mcjitter-mp-gf

    //< jit-mcjitter-mp-simple
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }
    //> jit-mcjitter-mp-simple
}

//< jit-mcjitter-jitter
impl JITter for MCJITter {
    fn get_module_provider(&mut self) -> &mut dyn builder::ModuleProvider {
        self
    }

    fn run_function(&mut self, f: LLVMValueRef) -> f64 {
        self.close_current_module();
        let f = unsafe { FunctionRef::from_ref(f) };
        let mut args = vec![];
        let res = self
            .container
            .borrow()
            .execution_engines
            .last()
            .expect("MCJITter went crazy")
            .run_function(&f, args.as_mut_slice());
        let ty = RealTypeRef::get_double();
        res.to_float(&ty)
    }
}
//> jit-mcjitter-jitter

//< jit-smp
impl JITter for builder::SimpleModuleProvider {
    fn get_module_provider(&mut self) -> &mut dyn builder::ModuleProvider {
        self
    }

    fn run_function(&mut self, _f: LLVMValueRef) -> f64 {
        panic!("not implemented (and will not be)")
    }
}
//> ch-3 jit-smp
