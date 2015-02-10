#![feature(box_syntax)]
#![feature(link_args)]

#![feature(plugin)]
#[plugin] #[no_link]
extern crate regex_macros;

extern crate libc;
extern crate regex;
extern crate rustc;

pub mod builder;
pub mod driver;
pub mod lexer;
pub mod missing_llvm_bindings;
pub mod parser;
