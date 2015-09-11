#![feature(box_syntax)]
#![feature(convert)]
#![feature(plugin)]
#![plugin(regex_macros)]

extern crate regex;

extern crate iron_llvm;
extern crate llvm_sys;

pub mod builder;
pub mod driver;
pub mod lexer;
pub mod parser;
