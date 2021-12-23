// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM
// LLVM-C header Core.h

pub use self::builder::Builder;
pub use self::context::Context;
pub use self::module::Module;
pub use self::pass_manager::{FunctionPassManager, PassManager};
pub use self::value::{Function, FunctionRef, Value};

pub mod basic_block;
pub mod builder;
pub mod context;
pub mod instruction;
pub mod module;
pub mod pass_manager;
pub mod types;
pub mod value;
