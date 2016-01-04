extern crate rustc_serialize;
extern crate docopt;

extern crate iron_kaleidoscope;

use iron_kaleidoscope::driver::{main_loop,
                                Tokens
};

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

    if args.flag_p || args.flag_i {
        unimplemented!();
    }
    let stage = Tokens;

    main_loop(stage);
}
