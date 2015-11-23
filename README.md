# Racket Macros for C++
This repository provides a toolchain for writing C++ macros using [Racket](http://racket-lang.org/)'s excellent macro facilities.

This requires building our fork of llvm / clang. See [this page](http://clang.llvm.org/get_started.html) for help doing so, substituting the forks referenced by this repository where necessary. You'll also need to add the `Cxx` directory from this repository as a collection to your installation of racket. See [this page](http://docs.racket-lang.org/guide/module-basics.html#%28part._link-collection%29) for helping doing so.

Once you've done both, you can use `compile-cxx.rkt` to work with the toolchain. An example invocation is as follows:

`compile-cxx.rkt -t -u -f -v -f -stdlib=libstdc++ -f --std=c++11 -S "SkelImpls" -@ Min Math.rkt -@ Repeat LexicalUnroll.rkt -@ CuFunc CuFunc.rkt -@ Reinclude Reinclude.rkt -b "llvm/build/Debug/bin/" -I /Developer/NVIDIA/CUDA-7.0/include test-params.json demos/reduce.skel`

The `-t` flag instructs the toolchain to leave temporary files in place, so you can inspect intermediate products. This is useful for interactively debugging individual components in DrRacket.
