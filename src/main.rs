//< ch-0 ch-1 ch-2 ch-3
extern crate rustc_serialize;
extern crate docopt;

extern crate iron_kaleidoscope;

use iron_kaleidoscope::driver::{main_loop,
//> ch-0 ch-1 ch-2
                                Exec,
//< ch-2
                                IR,
//< ch-1
                                AST,
//< ch-0
                                Tokens
};

//< parser-main
use docopt::Docopt;

const USAGE: &'static str = "
Usage: iron_kaleidoscope [(-l | -p | -i)]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -i  Run only IR builder and show its output.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_l: bool,
    flag_p: bool,
    flag_i: bool
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

//> ch-0
    let stage = if args.flag_l {
        Tokens
//> ch-1 parser-main
    } else if args.flag_p {
        AST
//< ch-1
    } else if args.flag_i {
//> ch-1 ch-2 ch-3
/*
//< ch-1
        unimplemented!();
//> ch-1
*/
//< ch-2 ch-3
        IR
//< ch-1 parser-main
    } else {
//> ch-1 ch-2 ch-3 parser-main
/*
//< ch-0
    if args.flag_p || args.flag_i {
        unimplemented!();
    }
    let stage = Tokens;
//> ch-0
//< ch-1 parser-main
        AST
//> ch-1 parser-main
//< ch-2
        IR
//> ch-2
*/
//< ch-3
        Exec
//< ch-1 ch-2 parser-main
    };
//< ch-0

    main_loop(stage);
}
//> ch-0 ch-1 ch-2 ch-3 parser-main
