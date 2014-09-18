extern crate iron_kaleidoscope;

#[cfg(not(test))]
use iron_kaleidoscope::driver::main_loop;

#[cfg(not(test))]
fn main() {
    main_loop();
}
