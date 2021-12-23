// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Missing Memory Manager C bindings

#include "memory-manager-wrappers.hh"

#include <algorithm>
#include <string>

#include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

namespace {

using namespace llvm;

struct BSMMFunctions {
    LLVM_BSMMAllocateCodeSectionCallback AllocateCodeSection;
    LLVM_BSMMAllocateDataSectionCallback AllocateDataSection;
    LLVM_BSMMFinalizeMemoryCallback FinalizeMemory;

    LLVM_BSMMInvalidateInstructionCacheCallback InvalidateInstructionCache;
    LLVM_BSMMGetSymbolAddressCallback GetSymbolAddress;

    LLVM_BSMMDestroyCallback Destroy;
};

class BindingSectionMemoryManager : public SectionMemoryManager {
public:
    BindingSectionMemoryManager(const BSMMFunctions& Functions, void *Opaque);
    ~BindingSectionMemoryManager() override;

    uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                 unsigned SectionID,
                                 StringRef SectionName) override;

    uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                 unsigned SectionID, StringRef SectionName,
                                 bool isReadOnly) override;

    bool finalizeMemory(std::string *ErrMsg = nullptr) override;

    void invalidateInstructionCache() override;

    uint64_t getSymbolAddress(const std::string &Name) override;

private:
    BSMMFunctions Functions;
    void *Opaque;
};

BindingSectionMemoryManager::BindingSectionMemoryManager(const BSMMFunctions& Functions, void *Opaque)
    : Functions(Functions), Opaque(Opaque) {}

BindingSectionMemoryManager::~BindingSectionMemoryManager() {
    if (Functions.Destroy) {
        Functions.Destroy(Opaque);
    }
}

uint8_t *BindingSectionMemoryManager::allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                                          unsigned SectionID,
                                                          StringRef SectionName) {
    if (Functions.AllocateCodeSection) {
        return Functions.AllocateCodeSection(Opaque, wrap(this), Size, Alignment, SectionID,
                                             SectionName.str().c_str());
    } else {
        return SectionMemoryManager::allocateCodeSection(Size, Alignment, SectionID, SectionName);
    }
}

uint8_t *BindingSectionMemoryManager::allocateDataSection(uintptr_t Size, unsigned Alignment,
                                                          unsigned SectionID, StringRef SectionName,
                                                          bool isReadOnly) {
    if (Functions.AllocateDataSection) {
        return Functions.AllocateDataSection(Opaque, wrap(this), Size, Alignment, SectionID,
                                             SectionName.str().c_str(),
                                             isReadOnly);
    } else {
        return SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID, SectionName, isReadOnly);
    }
}

bool BindingSectionMemoryManager::finalizeMemory(std::string *ErrMsg) {
    if (Functions.FinalizeMemory) {
        char *errMsgCString = nullptr;
        bool result = Functions.FinalizeMemory(Opaque, wrap(this), &errMsgCString);
        assert((result || !errMsgCString) &&
               "Did not expect an error message if FinalizeMemory succeeded");
        if (errMsgCString) {
            if (ErrMsg)
                *ErrMsg = errMsgCString;
            free(errMsgCString);
        }
        return result;
    } else {
        return SectionMemoryManager::finalizeMemory(ErrMsg);
    }
}

void BindingSectionMemoryManager::invalidateInstructionCache() {
    if (Functions.InvalidateInstructionCache) {
        Functions.InvalidateInstructionCache(Opaque, wrap(this));
    } else {
        SectionMemoryManager::invalidateInstructionCache();
    }
}

uint64_t BindingSectionMemoryManager::getSymbolAddress(const std::string &Name) {
    if (Functions.GetSymbolAddress) {
        return Functions.GetSymbolAddress(Opaque, wrap(this), Name.c_str());
    } else {
        return SectionMemoryManager::getSymbolAddress(Name);
    }
}

} // anonymous namespace

extern "C" {
    LLVMMCJITMemoryManagerRef LLVM_CreateSectionMemoryManager() {
        return wrap(new SectionMemoryManager());
    }

    LLVMMCJITMemoryManagerRef LLVM_CreateBindingSectionMemoryManager(
        void *Opaque,
        LLVM_BSMMAllocateCodeSectionCallback AllocateCodeSection,
        LLVM_BSMMAllocateDataSectionCallback AllocateDataSection,
        LLVM_BSMMFinalizeMemoryCallback FinalizeMemory,
        LLVM_BSMMInvalidateInstructionCacheCallback InvalidateInstructionCache,
        LLVM_BSMMGetSymbolAddressCallback GetSymbolAddress,
        LLVM_BSMMDestroyCallback Destroy) {
        BSMMFunctions functions;
        functions.AllocateCodeSection = AllocateCodeSection;
        functions.AllocateDataSection = AllocateDataSection;
        functions.FinalizeMemory = FinalizeMemory;
        functions.InvalidateInstructionCache = InvalidateInstructionCache;
        functions.GetSymbolAddress = GetSymbolAddress;
        functions.Destroy = Destroy;
        return wrap(new BindingSectionMemoryManager(functions, Opaque));
    }

    uint8_t *LLVM_BSMMCallParentAllocateCodeSection(
        LLVMMCJITMemoryManagerRef MM,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        const char *SectionName) {
        return dynamic_cast<SectionMemoryManager*>(unwrap(MM))->SectionMemoryManager::allocateCodeSection(
            Size, Alignment, SectionID, SectionName);
    }

    uint8_t *LLVM_BSMMCallParentAllocateDataSection(
        LLVMMCJITMemoryManagerRef MM,
        uintptr_t Size,
        unsigned Alignment,
        unsigned SectionID,
        const char *SectionName,
        LLVMBool isReadOnly) {
        return dynamic_cast<SectionMemoryManager*>(unwrap(MM))->SectionMemoryManager::allocateDataSection(
            Size, Alignment, SectionID, SectionName, isReadOnly);
    }

    LLVMBool LLVM_BSMMCallParentFinalizeMemory(
        LLVMMCJITMemoryManagerRef MM,
        char **ErrMsg) {
        std::string errMsg;
        bool result = dynamic_cast<SectionMemoryManager*>(unwrap(MM))->SectionMemoryManager::finalizeMemory(&errMsg);
        if (result && !errMsg.empty()) {
            *ErrMsg = new char[errMsg.size() + 1];
            std::copy(&(errMsg.c_str()[0]), &(errMsg.c_str()[errMsg.size() - 1]), *ErrMsg);
            (*ErrMsg)[errMsg.size()] = '\0';
        }

        return result;
    }

    void LLVM_BSMMCallParentInvalidateInstructionCache(LLVMMCJITMemoryManagerRef MM) {
        dynamic_cast<SectionMemoryManager*>(unwrap(MM))->SectionMemoryManager::invalidateInstructionCache();
    }

    uint64_t LLVM_BSMMCallParentGetSymbolAddress(
        LLVMMCJITMemoryManagerRef MM,
        const char *Name) {
        return dynamic_cast<SectionMemoryManager*>(unwrap(MM))->SectionMemoryManager::getSymbolAddress(Name);
    }

    uint64_t LLVM_GetSymbolAddressInProcess(const char *Name) {
        return RTDyldMemoryManager::getSymbolAddressInProcess(Name);
    }
}
