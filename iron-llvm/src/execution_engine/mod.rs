// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Execution Engine
// LLVM-C header ExecutionEngine.h

pub use self::execution_engine::{ExecutionEngine, MCJITBuilder};
pub use self::generic_value::GenericValue;
pub use self::memory_manager::{BindingSectionMemoryManagerBuilder,
                               MCJITMemoryManager,
                               SectionMemoryManager,
                               SimpleMCJITMemoryManager,
                               SimpleMCJITMemoryManagerImpl};

pub mod execution_engine;
pub mod generic_value;
pub mod memory_manager;

mod wrappers;
