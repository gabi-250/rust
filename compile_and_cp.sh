#!/bin/sh

# Build the IronOx backend:
PATH=/opt/gdb-8.2/bin:${PATH} RUST_BACKTRACE=1 \
./x.py build --config .buildbot-ironox.toml --stage 1

# Copy the libs compiled by the LLVM backend:
cp -r ./llvm_build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/lib \
./build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/

