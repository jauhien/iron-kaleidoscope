// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Support Wrappers

use libc::{c_char, c_void};

extern "C" {
    pub fn LLVM_AddSymbol(symbolName: *const c_char, symbolValue: *const c_void);
}
