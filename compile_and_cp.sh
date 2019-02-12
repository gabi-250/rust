#!/bin/sh

CARGO_CFG_REGEX_DISABLE_AUTO_OPTIMIZATIONS=1 ./x.py build -i --stage 1 src/rustc --keep-stage 1

cp -r ./llvm_build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/lib ./build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/

