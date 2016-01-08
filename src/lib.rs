//< ch-1 ch-2 ch-3
#![feature(box_syntax)]
//< ch-0
#![feature(convert)]
#![feature(plugin)]
#![plugin(regex_macros)]

extern crate regex;
//> ch-0 ch-1

//< ir-import
extern crate iron_llvm;
extern crate llvm_sys;
//> ir-import

pub mod builder;
//< ch-0 ch-1
pub mod driver;
//> ch-0 ch-1 ch-2
pub mod jitter;
//< ch-0 ch-1 ch-2
pub mod lexer;
//> ch-0
pub mod parser;
//> ch-1 ch-2 ch-3
pub mod filer;
