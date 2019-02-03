#!/bin/sh
#
# Build script for continuous integration.

./x.py clean  # We don't clone afresh to save time and bandwidth.
git clean -dffx # If upstream removes a submodule, remove the files from disk.

# Note that the gdb must be Python enabled.
PATH=/opt/gdb-8.2/bin:${PATH} RUST_BACKTRACE=1 \
./x.py test --config .buildbot.toml

# Build the LLVM backend:
 PATH=/opt/gdb-8.2/bin:${PATH} RUST_BACKTRACE=1 \
 ./x.py build --config .buildbot.toml --stage 1

mv build llvm_build

# Build the IronOx backend:
PATH=/opt/gdb-8.2/bin:${PATH} RUST_BACKTRACE=1 \
./x.py build --config .buildbot-ironox.toml --stage 1

# Copy the libs compiled by the LLVM backend:
cp -r ./llvm_build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/lib \
./build/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/

cd src/test_iron && python3.5 test_output.py

