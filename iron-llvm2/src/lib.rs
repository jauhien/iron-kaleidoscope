// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(non_upper_case_globals)]
#![allow(unused_imports)]
#![allow(deprecated)]
#![allow(invalid_value)]
#![feature(log_syntax)]
#![feature(trace_macros)]

extern crate libc;
extern crate llvm_sys;

use std::io;
use std::io::Write;

use libc::{c_uint, uintptr_t};

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use core::types::{
    FunctionType, FunctionTypeCtor, IntType, IntTypeCtor, StructType, StructTypeCtor, Type,
};
use core::{context, types};

macro_rules! new_ref_type(
    ($ref_type:ident for $llvm_ref_type:ident implementing $($base_trait:ty),+) => (
        #[derive(Copy, Clone)]
        pub struct $ref_type ($llvm_ref_type);

        impl LLVMRef<$llvm_ref_type> for $ref_type {
            fn to_ref(&self) -> $llvm_ref_type {
                self.0
            }
        }

        impl LLVMRefCtor<$llvm_ref_type> for $ref_type {
            unsafe fn from_ref(rf: $llvm_ref_type) -> $ref_type {
                $ref_type(rf)
            }
        }

        $(
            impl $base_trait for $ref_type {}
        )*
        )
);

pub mod bitcode;
pub mod core;
pub mod execution_engine;
pub mod support;
pub mod target;

pub trait LLVMRef<Ref> {
    fn to_ref(&self) -> Ref;
}

pub trait LLVMRefCtor<Ref>: Sized {
    unsafe fn from_ref(rf: Ref) -> Self;
}

#[test]
fn it_works() {
    let c1 = context::Context::new();

    let c1_ref = c1.to_ref();

    let gc1 = context::Context::get_global();
    let gc1_ref = gc1.to_ref();

    let gc = unsafe { LLVMGetGlobalContext() };

    assert!(gc == gc1_ref);
    assert!(c1_ref != gc);

    let ty = unsafe { LLVMInt64TypeInContext(gc1.to_ref()) };

    assert!(ty.get_context().to_ref() == gc1_ref);

    let mut stderr = io::stderr();

    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "Testing Type").unwrap();
    //writeln!(&mut stderr, "kind: {:?}", ty.get_kind()).unwrap();
    writeln!(&mut stderr, "is sized: {:?}", ty.is_sized()).unwrap();
    write!(&mut stderr, "dump: ").unwrap();
    ty.dump();
    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "string rep: {:?}", ty.print_to_string()).unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "").unwrap();

    let int10 = types::IntTypeRef::get_int(10);
    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "Testing int10").unwrap();
    writeln!(&mut stderr, "type width: {:?}", int10.get_width()).unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "").unwrap();

    let mut args = vec![ty.to_ref(), int10.to_ref()];
    let func = types::FunctionTypeRef::get(&ty, args.as_mut_slice(), false);

    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "Testing function type").unwrap();
    writeln!(&mut stderr, "string rep: {:?}", func.print_to_string()).unwrap();
    writeln!(&mut stderr, "is var arg: {:?}", func.is_var_arg()).unwrap();
    writeln!(
        &mut stderr,
        "return type: {:?}",
        func.get_return_type().print_to_string()
    )
    .unwrap();
    writeln!(
        &mut stderr,
        "number of parameters: {:?}",
        func.count_param_types()
    )
    .unwrap();

    let params = func.get_param_types();
    for param in params {
        writeln!(&mut stderr, "  param type: {:?}", param.print_to_string()).unwrap();
    }

    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "").unwrap();

    let struct1 = types::StructTypeRef::new_named(&gc1, "testing");
    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "Testing struct type").unwrap();
    writeln!(&mut stderr, "string rep: {:?}", struct1.print_to_string()).unwrap();
    writeln!(&mut stderr, "name: {:?}", struct1.get_name()).unwrap();
    writeln!(&mut stderr, "is opaque: {:?}", struct1.is_opaque()).unwrap();
    writeln!(&mut stderr, "setting body...").unwrap();

    struct1.set_body(args.as_mut_slice(), false);

    writeln!(&mut stderr, "string rep: {:?}", struct1.print_to_string()).unwrap();
    writeln!(&mut stderr, "is opaque: {:?}", struct1.is_opaque()).unwrap();
    writeln!(
        &mut stderr,
        "count of elements: {:?}",
        struct1.count_element_types()
    )
    .unwrap();

    let elements = struct1.get_element_types();
    for element in elements {
        writeln!(
            &mut stderr,
            "  element type: {:?}",
            element.print_to_string()
        )
        .unwrap();
    }

    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "").unwrap();
}

#[test]
fn test_ee() {
    let mut stderr = io::stderr();

    writeln!(&mut stderr, "").unwrap();
    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "Testing execution engine").unwrap();

    let m = core::Module::new("test0");
    let ee = execution_engine::ExecutionEngine::new(m);
    let (mut either, either_module) = match ee {
        Ok((ee, module)) => {
            writeln!(&mut stderr, "either: ok!").unwrap();
            (ee, module)
        }
        Err(error) => {
            panic!("either: {:?}", error)
        }
    };

    let m = core::Module::new("test1");
    let ee = execution_engine::ExecutionEngine::new_interpreter(m);
    let (mut interpreter, interpreter_module) = match ee {
        Ok((ee, module)) => {
            writeln!(&mut stderr, "interpreter: ok!").unwrap();
            (ee, module)
        }
        Err(error) => {
            panic!("interpreter: {:?}", error)
        }
    };

    target::initilalize_native_target();
    let m = core::Module::new("test2");
    let ee = execution_engine::ExecutionEngine::new_jit_compiler(m, 0);
    let (mut jit, jit_module) = match ee {
        Ok((ee, module)) => {
            writeln!(&mut stderr, "jit: ok!").unwrap();
            (ee, module)
        }
        Err(error) => {
            panic!("jit: {:?}", error)
        }
    };

    match either.remove_module(either_module) {
        Ok(_) => writeln!(&mut stderr, "either module removed").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    }

    match interpreter.remove_module(interpreter_module) {
        Ok(_) => writeln!(&mut stderr, "interpreter module removed").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    }

    match jit.remove_module(jit_module) {
        Ok(_) => writeln!(&mut stderr, "jit module removed").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    }

    struct TestMM;

    impl execution_engine::SimpleMCJITMemoryManagerImpl for TestMM {
        fn allocate_code_section(
            &mut self,
            _size: uintptr_t,
            _alignment: u32,
            _section_id: u32,
            _section_name: &str,
        ) -> *mut u8 {
            0 as *mut u8
        }

        fn allocate_data_section(
            &mut self,
            _size: uintptr_t,
            _alignment: u32,
            _section_id: u32,
            _section_name: &str,
            _is_read_only: bool,
        ) -> *mut u8 {
            0 as *mut u8
        }

        fn finalize_memory(&mut self) -> Result<(), String> {
            Ok(())
        }

        fn destroy(&mut self) {
            let mut stderr = io::stderr();
            writeln!(&mut stderr, "simple MM: destroy called").unwrap();
        }
    }

    impl Drop for TestMM {
        fn drop(&mut self) {
            let mut stderr = io::stderr();
            writeln!(&mut stderr, "simple MM: drop called").unwrap();
        }
    }

    let mm_impl = TestMM;
    let mm = execution_engine::SimpleMCJITMemoryManager::new(mm_impl);

    let m = core::Module::new("test_builder");

    match execution_engine::MCJITBuilder::new()
        .set_opt_level(1)
        .set_mcjit_memory_manager(Box::new(mm))
        .create(m)
    {
        Ok(_) => writeln!(&mut stderr, "mcjit ok!").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    };

    let mm = execution_engine::SectionMemoryManager::new();

    let m = core::Module::new("test_builder");

    match execution_engine::MCJITBuilder::new()
        .set_opt_level(1)
        .set_mcjit_memory_manager(Box::new(mm))
        .create(m)
    {
        Ok(_) => writeln!(&mut stderr, "mcjit with section MM ok!").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    };

    struct DropTester;

    impl DropTester {
        fn print(&self) {
            let mut stderr = io::stderr();
            writeln!(&mut stderr, "binding section MM: destroy called").unwrap();
        }
    }

    impl Drop for DropTester {
        fn drop(&mut self) {
            let mut stderr = io::stderr();
            writeln!(&mut stderr, "binding section MM: drop called").unwrap();
        }
    }

    let drop_tester = DropTester;

    let mm_builder = execution_engine::BindingSectionMemoryManagerBuilder::new();
    let mm = mm_builder
        .set_destroy(move || {
            drop_tester.print();
        })
        .create();

    let m = core::Module::new("test_builder");

    match execution_engine::MCJITBuilder::new()
        .set_opt_level(1)
        .set_mcjit_memory_manager(Box::new(mm))
        .create(m)
    {
        Ok(_) => writeln!(&mut stderr, "mcjit with binding section MM ok!").unwrap(),
        Err(error) => writeln!(&mut stderr, "{:?}", error).unwrap(),
    };

    writeln!(&mut stderr, "========").unwrap();
    writeln!(&mut stderr, "").unwrap();
}
