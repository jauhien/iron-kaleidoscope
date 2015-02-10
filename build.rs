use std::old_io::Command;
use std::os;

fn main() {
    let out_dir = os::getenv("OUT_DIR").unwrap();

    Command::new("python2").arg("src/etc/mklldeps.py")
        .arg("src/missing_llvm_bindings/llvmdeps.rs")
        .arg(format!("{}", out_dir))
        .arg("jit")
        .arg("llvm-config")
        .status()
        .unwrap();

    Command::new("make")
        .arg(format!("OUT_DIR={}", out_dir))
        .status()
        .unwrap();

    println!("cargo:rustc-flags=-L {} ", out_dir);
}
