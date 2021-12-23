extern crate cmake;

fn main() {
    let dst = cmake::Config::new("wrappers")
        .define("CMAKE_CXX_STANDARD", "17")
        .build();
    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=static=llvm-wrappers");
}

// cmake -G Ninja "-DCMAKE_C_COMPILER=$(command -v clang-12)" "-DCMAKE_CXX_COMPILER=$(command -v clang++-12)" -DUSE_SNAPPY=1 -DCMAKE_BUILD_TYPE=RelWithDebInfo ..
