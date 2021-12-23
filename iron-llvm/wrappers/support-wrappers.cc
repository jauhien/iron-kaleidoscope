// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Missing support C bindings
// Available in the trunk, r239411, patch by Antoine Pitrou

#include "support-wrappers.hh"

#include <llvm/ADT/StringMap.h>
#include <llvm/Support/DynamicLibrary.h>

extern "C" {
    void LLVM_AddSymbol(const char *symbolName, void *symbolValue) {
        return llvm::sys::DynamicLibrary::AddSymbol(symbolName, symbolValue);
    }
}
