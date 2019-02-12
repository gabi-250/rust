#!/bin/sh
#
# Build script for continuous integration.

./x.py clean  # We don't clone afresh to save time and bandwidth.
git clean -dffx # If upstream removes a submodule, remove the files from disk.

# Note that the gdb must be Python enabled.
PATH=/opt/gdb-8.2/bin:${PATH} RUST_BACKTRACE=1 \
./x.py test --config .buildbot.toml

# Make sure there are no compile errors
RUST_BACKTRACE=1 ./x.py check --config .buildbot-ironox.toml

mv build llvm_build

# Run the ironox tests:
./compile_and_cp.sh || rustup toolchain link ironox ./build/x86_64-unknown-linux-gnu/stage1

cd ./src/test_iron
pytest
