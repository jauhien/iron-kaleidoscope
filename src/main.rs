#![feature(phase)]

extern crate serialize;
#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;

extern crate "iron-kaleidoscope" as iron_kaleidoscope;

#[cfg(not(test))]
use iron_kaleidoscope::driver::{main_loop, Tokens, AST, IR, Exec};

docopt!(Args, "
Usage: iron_kaleidoscope [(-l | -p | -i)]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -i  Run only IR builder and show its output.
")

#[cfg(not(test))]
fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Tokens
    } else if args.flag_p {
        AST
    } else if args.flag_i {
        IR
    } else {
        Exec
    };

    main_loop(stage);
}
