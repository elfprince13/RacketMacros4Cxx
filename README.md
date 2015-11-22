# Racket Macros for C++
This repository provides a toolchain for writing C++ macros using [Racket](http://racket-lang.org/)'s excellent macro facilities.

This requires building our fork of llvm / clang. Once you've done so, you can use `compile-cxx.rkt` to work with the toolchain. An example invocation is as follows:

`compile-cxx.rkt -t -u -f -v -f -stdlib=libstdc++ -f --std=c++11 -S "SkelImpls" -@ Min Math.rkt -@ Repeat LexicalUnroll.rkt -@ CuFunc CuFunc.rkt -@ Reinclude Reinclude.rkt -b "/Users/thomas/Documents/Brown/Proteins/llvm/build/Debug/bin/" -I /Developer/NVIDIA/CUDA-7.0/include test-params.json demos/reduce.skel`

The `-t` flag instructs the toolchain to leave temporary files in place, so you can inspect intermediate products. This is useful for interactively debugging individual components in DrRacket.
