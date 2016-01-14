extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;

use iron_kaleidoscope::{parser,builder,filer};

const USAGE: &'static str = "
Usage: ikt [--no-stdlib] <input>... -o <output>

Options:
    --no-stdlib  Do not include stdlib
    -o <output>  Output bitcode file
";

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_no_stdlib: bool,
    arg_input: Vec<String>,
    flag_o: String
}

extern crate iron_kaleidoscope;

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let mut parser_settings = parser::default_parser_settings();
    let mut ir_container = builder::SimpleModuleProvider::new("main");
    let mut context = builder::Context::new();

    if !args.flag_no_stdlib {
        match filer::load_stdlib(&mut parser_settings, &mut context, &mut ir_container) {
            Ok(_) => (),
            Err(err) => panic!("Error occured during stdlib loading: {}\n", err)
        }
    };

    for input in &args.arg_input {
        match filer::load_ks(input, &mut parser_settings, &mut context, &mut ir_container) {
            Ok(_) => (),
            Err(err) => panic!("Error occured during '{}' loading: {}\n", input, err)
        }
    };

    match filer::dump_bitcode(&args.flag_o, &mut ir_container) {
        Ok(_) => (),
        Err(_) => panic!("Error occured during output writing\n")
    };
}
