use std::path::Path;

mod sourcegen;

fn main() {
    let debug = cfg!(debug_assertions);
    let dst = cmake::Config::new(".")
        // TODO: remove it
        .env("CMAKE_BUILD_PARALLEL_LEVEL", "16")
        .define("SLANG_MASTER_PROJECT", "OFF")
        .define("CMAKE_BUILD_TYPE", if debug { "Debug" } else { "Release" })
        .define("SLANG_BOOST_SINGLE_HEADER", "")
        .build()
        .join("build");
    let dst = dst.display();

    println!("cargo:rustc-link-search=native={dst}/lib");
    println!("cargo:rustc-link-lib=static=svlang");
    if cfg!(debug_assertions) {
        println!("cargo:rustc-link-lib=static=fmtd");
        println!("cargo:rustc-link-lib=static=mimalloc-debug");
    } else {
        println!("cargo:rustc-link-lib=static=fmt");
        println!("cargo:rustc-link-lib=static=mimalloc");
    }

    println!("cargo:rerun-if-changed=bindings/rust/lib.rs");
    println!("cargo:rerun-if-changed=bindings/rust/ffi.rs");
    println!("cargo:rerun-if-changed=bindings/rust/ffi/cxx_sv.rs");
    println!("cargo:rerun-if-changed=bindings/rust/ffi/string_view.h");
    println!("cargo:rerun-if-changed=bindings/rust/ffi/wrapper.h");
    let generated = format!("{dst}/source");
    let fmt_include = format!("{dst}/_deps/fmt-src/include");
    let includes = [
        &generated,
        &fmt_include,
        "include",
        "source",
        "external",
        "bindings/rust",
    ];

    let mut builder = cxx_build::bridges(["bindings/rust/ffi.rs", "bindings/rust/ffi/cxx_sv.rs"]);
    let builder = builder
        .includes(includes.into_iter().map(Path::new))
        .std("c++20")
        .flag_if_supported("-stdlib=libstdc++")
        .flag_if_supported("-DSLANG_BOOST_SINGLE_HEADER");
    if debug {
        builder.flag_if_supported("-DDEBUG");
    }
    builder.compile("slang_binding");

    // generate rs
    println!("cargo:rerun-if-changed=bindings/rust/sourcegen.rs");
    println!("cargo:rerun-if-changed=scripts/syntax.txt");
    println!("cargo:rerun-if-changed=scripts/tokenkinds.txt");
    println!("cargo:rerun-if-changed=scripts/triviakinds.txt");
    generate_rs();
}

fn generate_rs() {
    let (all_types, kind_map) = sourcegen::loader::load_types();
    sourcegen::generator::generate_syntax_kind(&kind_map);
    sourcegen::generator::generate_ast_file(&all_types, &kind_map);
}
