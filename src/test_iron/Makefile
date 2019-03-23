TOOLCHAIN := ironox
STDLIB_SO := ../../llvm_build/x86_64-unknown-linux-gnu/stage1-std/x86_64-unknown-linux-gnu/release/libstd.so
LIBS := ../../build/x86_64-unknown-linux-gnu/stage1/lib
IRONOX_STAGE1 := ../../build/x86_64-unknown-linux-gnu/stage1/bin/rustc
PROJ_DIR := ./cargo_projs/$(proj)
TEST_NAME := test_$(proj)
CARGO := ../../build/x86_64-unknown-linux-gnu/stage0/bin/cargo
IRONOX_SO := librustc_codegen_ironox-ironox.so
IRONOX_BUILD_DIR := ../../build
IRONOX_DIR := $(realpath $(IRONOX_BUILD_DIR))
NIGHTLY_FLAGS :=-Zcodegen-backend=$(IRONOX_DIR)/x86_64-unknown-linux-gnu/stage1/lib/rustlib/x86_64-unknown-linux-gnu/codegen-backends/$(IRONOX_SO) -Zno-landing-pads
CODEGEN_FLAGS := -Cdebuginfo=0 -Cno-integrated-as -Cpanic=abort
RUSTFLAGS := $(NIGHTLY_FLAGS) $(CODEGEN_FLAGS)
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
	LD_LIBRARY_PATH=$(LIBS) RUSTFLAGS="$(RUSTFLAGS)" RUSTC=$(IRONOX_STAGE1) $(CARGO) build $(MANIFEST_PATH)

clean:
	rm -f $(BUILD_DIR)/$(TEST_NAME); $(CARGO) clean $(MANIFEST_PATH)
