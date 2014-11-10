use std::io::Command;

fn main() {
    Command::new("python2").arg("src/etc/mklldeps.py")
        .arg("src/missing_llvm_bindings/llvmdeps.rs")
        .arg("jit")
        .arg("llvm-config")
        .status()
        .unwrap();
}
