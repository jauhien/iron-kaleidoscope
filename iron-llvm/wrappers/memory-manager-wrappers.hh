// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Missing Memory Manager C bindings

#include <llvm-c/ExecutionEngine.h>

extern "C" {

    typedef uint8_t *(*LLVM_BSMMAllocateCodeSectionCallback)(
        void *Opaque, LLVMMCJITMemoryManagerRef MM, uintptr_t Size, unsigned Alignment, unsigned SectionID,
        const char *SectionName);
    typedef uint8_t *(*LLVM_BSMMAllocateDataSectionCallback)(
        void *Opaque, LLVMMCJITMemoryManagerRef MM, uintptr_t Size, unsigned Alignment, unsigned SectionID,
        const char *SectionName, LLVMBool IsReadOnly);
    typedef LLVMBool (*LLVM_BSMMFinalizeMemoryCallback)(
        void *Opaque, LLVMMCJITMemoryManagerRef MM, char **ErrMsg);
    typedef void     (*LLVM_BSMMDestroyCallback)(void *Opaque);
    typedef void     (*LLVM_BSMMInvalidateInstructionCacheCallback)(
        void *Opaque, LLVMMCJITMemoryManagerRef MM);
    typedef uint64_t (*LLVM_BSMMGetSymbolAddressCallback)(
        void *Opaque, LLVMMCJITMemoryManagerRef MM, const char *Name);

    LLVMMCJITMemoryManagerRef LLVM_CreateSectionMemoryManager();

    LLVMMCJITMemoryManagerRef LLVM_CreateBindingSectionMemoryManager(
        void *Opaque,
        LLVM_BSMMAllocateCodeSectionCallback AllocateCodeSection,
        LLVM_BSMMAllocateDataSectionCallback AllocateDataSection,
        LLVM_BSMMFinalizeMemoryCallback FinalizeMemory,
        LLVM_BSMMInvalidateInstructionCacheCallback InvalidateInstructionCache,
        LLVM_BSMMGetSymbolAddressCallback GetSymbolAddress,
        LLVM_BSMMDestroyCallback Destroy);

    uint8_t *LLVM_BSMMCallParentAllocateCodeSection(
        LLVMMCJITMemoryManagerRef MM,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        const char *SectionName);

    uint8_t *LLVM_BSMMtCallParentAllocateDataSection(
        LLVMMCJITMemoryManagerRef MM,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        const char *SectionName,
        LLVMBool isReadOnly);

    LLVMBool LLVM_BSMMCallParentFinalizeMemory(
        LLVMMCJITMemoryManagerRef MM,
        char **ErrMsg);

    void LLVM_BSMMCallParentInvalidateInstructionCache(LLVMMCJITMemoryManagerRef MM);

    uint64_t LLVM_BSMMCallParentGetSymbolAddress(
        LLVMMCJITMemoryManagerRef MM,
        const char *Name);

    uint64_t LLVM_GetSymbolAddressInProcess(const char *Name);
}
