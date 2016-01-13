use std::env;
use std::error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::Path;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::bitcode::write_bitcode_to_file;

use builder;
use builder::IRBuilder;
use lexer;
use parser;

#[derive(Debug)]
pub enum FilerError {
    Io(io::Error),
    Build(String)
}

impl fmt::Display for FilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FilerError::Io(ref err) => write!(f, "IO error: {}", err),
            FilerError::Build(ref err) => write!(f, "Build error: {}", err)
        }
    }
}

impl error::Error for FilerError {
    fn description(&self) -> &str {
         match *self {
            FilerError::Io(ref err) => err.description(),
            FilerError::Build(ref err) => err
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            FilerError::Io(ref err) => Some(err),
            FilerError::Build(_) => Some(self)
        }
    }
}

impl From<io::Error> for FilerError {
    fn from(err: io::Error) -> FilerError {
        FilerError::Io(err)
    }
}

impl From<String> for FilerError {
    fn from(err: String) -> FilerError {
        FilerError::Build(err)
    }
}

pub fn load_ks<P: AsRef<Path>>(path: P,
                               parser_settings: &mut parser::ParserSettings,
                               context: &mut builder::Context,
                               module_provider: &mut builder::ModuleProvider) -> Result<(LLVMValueRef, bool), FilerError> {
    let mut f = try!(File::open(path));
    let mut buf = String::new();
    try!(f.read_to_string(&mut buf));

    let tokens = lexer::tokenize(&buf);

    let (ast, rest) = try!(parser::parse(tokens.as_slice(), vec![].as_slice(), parser_settings));
    if !rest.is_empty() {
        return Err(FilerError::Build(format!("Not complete expression: {:?}", rest)));
    }

    Ok(try!(ast.codegen(context, module_provider)))
}

pub fn load_stdlib(parser_settings: &mut parser::ParserSettings,
                   context: &mut builder::Context,
                   module_provider: &mut builder::ModuleProvider) -> Result<(LLVMValueRef, bool), FilerError> {
    let path = match env::var("KALEIDOSCOPE_PATH") {
        Ok(path) => path,
        Err(_) => "stdlib".to_string()
    };
    let path = Path::new(&path).join("stdlib.ks");

    Ok(try!(load_ks(path, parser_settings, context, module_provider)))
}

pub fn dump_bitcode(path: &str, module_provider: &mut builder::ModuleProvider) -> Result<(), ()> {
    write_bitcode_to_file(module_provider.get_module(), path)
}
