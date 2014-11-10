CC ?= gcc
AR ?= ar

LLVM_CFLAGS := $(shell llvm-config --cflags)
CFLAGS += $(LLVM_CFLAGS)

all: libllvm_initialization.a

llvm_initialization.o: src/missing_llvm_bindings/llvm_initialization.c
	@$(CC) -c $(CFLAGS) $< -o $(OUT_DIR)/$@

libllvm_initialization.a: llvm_initialization.o
	@cd $(OUT_DIR) && $(AR) crus $@ $<
