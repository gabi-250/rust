#!/bin/sh

./x.py clean
RUST_BACKTRACE=1 ./x.py check --config .buildbot.toml
