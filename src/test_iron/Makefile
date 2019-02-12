TOOLCHAIN := ironox
STDLIB_SO := ../../llvm_build/x86_64-unknown-linux-gnu/stage1-std/x86_64-unknown-linux-gnu/release/libstd.so
PROJ_DIR := ./cargo_projs/$(proj)
TEST_NAME := test_$(proj)
CARGO_BUILD_DIR := $(PROJ_DIR)/target/debug/deps
BUILD_DIR := build
MANIFEST_PATH := --manifest-path $(PROJ_DIR)/Cargo.toml

.PHONY: all clean rust_proj

all: $(TEST_NAME)

$(BUILD_DIR):
	mkdir -p $@

$(TEST_NAME): $(PROJ_DIR)/$(TEST_NAME).c rust_proj | $(BUILD_DIR)
	$(CC) -o $@ $(STDLIB_SO) $(CARGO_BUILD_DIR)/*.s $(PROJ_DIR)/$@.c && mv $@ ./$(BUILD_DIR)

rust_proj:
	cargo +$(TOOLCHAIN) build $(MANIFEST_PATH)

clean:
	rm -f $(BUILD_DIR)/$(TEST_NAME); cargo clean $(MANIFEST_PATH)
