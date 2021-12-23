// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Missing support C bindings
// Available in the trunk, r239411, patch by Antoine Pitrou

extern "C" {
    void LLVM_AddSymbol(const char *symbolName, void *symbolValue);
}
