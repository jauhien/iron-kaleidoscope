#![feature(box_syntax)]
#![feature(convert)]
#![feature(libc)]
#![feature(link_args)]
#![feature(plugin)]
#![plugin(regex_macros)]

extern crate libc;
extern crate regex;
//extern crate rustc;

//pub mod builder;
pub mod driver;
pub mod lexer;
//pub mod missing_llvm_bindings;
pub mod parser;
