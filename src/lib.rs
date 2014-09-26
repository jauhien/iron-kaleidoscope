#![feature(globs)]
#![feature(link_args)]
#![feature(phase)]
#![feature(struct_variant)]

extern crate libc;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;
extern crate rustc;

pub mod builder;
pub mod driver;
pub mod lexer;
pub mod missing_llvm_bindings;
pub mod parser;
