#!/bin/sh

# Build the LLVM backend:
RUST_BACKTRACE=1 ./x.py build --config .buildbot.toml --stage 1

mv build llvm_build

# Build the IronOx backend:
RUST_BACKTRACE=1 ./x.py build --config .buildbot-ironox.toml --stage 1

# Copy the libs compiled by the LLVM backend:
cp -r ./llvm_build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/lib \
./build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/
