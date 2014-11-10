#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>

void llvm_initialize_native_target() {
	LLVMInitializeNativeTarget();
}
