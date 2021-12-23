// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Core LLVM: Pass managers
// LLVM-C header Core.h

use std;

use libc::c_int;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::transforms::scalar::*;

use core::module::Module;
use core::value::Function;
use LLVMRef;

pub struct PassManager {
    manager: LLVMPassManagerRef,
}

impl PassManager {
    pub fn new() -> PassManager {
        unsafe {
            PassManager {
                manager: LLVMCreatePassManager(),
            }
        }
    }

    pub fn run(&mut self, module: &mut Module) -> bool {
        unsafe { LLVMRunPassManager(self.to_ref(), module.to_ref()) > 0 }
    }
}

impl LLVMRef<LLVMPassManagerRef> for PassManager {
    fn to_ref(&self) -> LLVMPassManagerRef {
        self.manager
    }
}

impl Drop for PassManager {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.to_ref());
        }
    }
}

pub struct FunctionPassManager {
    manager: LLVMPassManagerRef,
}

impl FunctionPassManager {
    pub fn new(module: &Module) -> FunctionPassManager {
        unsafe {
            FunctionPassManager {
                manager: LLVMCreateFunctionPassManagerForModule(module.to_ref()),
            }
        }
    }

    pub fn initialize(&mut self) -> bool {
        unsafe { LLVMInitializeFunctionPassManager(self.to_ref()) > 0 }
    }

    pub fn run(&mut self, function: &mut dyn Function) -> bool {
        unsafe { LLVMRunFunctionPassManager(self.to_ref(), function.to_ref()) > 0 }
    }

    pub fn finalize(&mut self) -> bool {
        unsafe { LLVMFinalizeFunctionPassManager(self.to_ref()) > 0 }
    }

    #[allow(non_snake_case)]
    pub fn add_aggressive_DCE_pass(&mut self) {
        unsafe { LLVMAddAggressiveDCEPass(self.to_ref()) }
    }

    pub fn add_alignment_from_assumptions_pass(&mut self) {
        unsafe { LLVMAddAlignmentFromAssumptionsPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_CFG_simplification_pass(&mut self) {
        unsafe { LLVMAddCFGSimplificationPass(self.to_ref()) }
    }

    pub fn add_dead_store_elimination_pass(&mut self) {
        unsafe { LLVMAddDeadStoreEliminationPass(self.to_ref()) }
    }

    pub fn add_scalarizer_pass(&mut self) {
        unsafe { LLVMAddScalarizerPass(self.to_ref()) }
    }

    pub fn add_merged_load_store_motion_pass(&mut self) {
        unsafe { LLVMAddMergedLoadStoreMotionPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_GVN_pass(&mut self) {
        unsafe { LLVMAddGVNPass(self.to_ref()) }
    }

    pub fn add_ind_var_simplify_pass(&mut self) {
        unsafe { LLVMAddIndVarSimplifyPass(self.to_ref()) }
    }

    pub fn add_instruction_combining_pass(&mut self) {
        unsafe { LLVMAddInstructionCombiningPass(self.to_ref()) }
    }

    pub fn add_jump_threading_pass(&mut self) {
        unsafe { LLVMAddJumpThreadingPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_LICM_pass(&mut self) {
        unsafe { LLVMAddLICMPass(self.to_ref()) }
    }

    pub fn add_loop_deletion_pass(&mut self) {
        unsafe { LLVMAddLoopDeletionPass(self.to_ref()) }
    }

    pub fn add_loop_idiom_pass(&mut self) {
        unsafe { LLVMAddLoopIdiomPass(self.to_ref()) }
    }

    pub fn add_loop_rotate_pass(&mut self) {
        unsafe { LLVMAddLoopRotatePass(self.to_ref()) }
    }

    pub fn add_loop_reroll_pass(&mut self) {
        unsafe { LLVMAddLoopRerollPass(self.to_ref()) }
    }

    pub fn add_loop_unroll_pass(&mut self) {
        unsafe { LLVMAddLoopUnrollPass(self.to_ref()) }
    }

    pub fn add_loop_unswitch_pass(&mut self) {
        unsafe { LLVMAddLoopUnswitchPass(self.to_ref()) }
    }

    pub fn add_mem_cpy_opt_pass(&mut self) {
        unsafe { LLVMAddMemCpyOptPass(self.to_ref()) }
    }

    pub fn add_partially_inline_lib_calls_pass(&mut self) {
        unsafe { LLVMAddPartiallyInlineLibCallsPass(self.to_ref()) }
    }

    pub fn add_lower_switch_pass(&mut self) {
        unsafe { LLVMAddLowerSwitchPass(self.to_ref()) }
    }

    pub fn add_promote_memory_to_register_pass(&mut self) {
        unsafe { LLVMAddPromoteMemoryToRegisterPass(self.to_ref()) }
    }

    pub fn add_reassociate_pass(&mut self) {
        unsafe { LLVMAddReassociatePass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_SCCP_pass(&mut self) {
        unsafe { LLVMAddSCCPPass(self.to_ref()) }
    }

    pub fn add_scalar_repl_aggregates_pass(&mut self) {
        unsafe { LLVMAddScalarReplAggregatesPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_scalar_repl_aggregates_pass_SSA(&mut self) {
        unsafe { LLVMAddScalarReplAggregatesPassSSA(self.to_ref()) }
    }

    pub fn add_scalar_repl_aggregates_pass_with_threshold(&mut self, threshold: c_int) {
        unsafe { LLVMAddScalarReplAggregatesPassWithThreshold(self.to_ref(), threshold) }
    }

    pub fn add_simplify_lib_calls_pass(&mut self) {
        unsafe { LLVMAddSimplifyLibCallsPass(self.to_ref()) }
    }

    pub fn add_tail_call_elimination_pass(&mut self) {
        unsafe { LLVMAddTailCallEliminationPass(self.to_ref()) }
    }

    pub fn add_constant_propagation_pass(&mut self) {
        unsafe { LLVMAddConstantPropagationPass(self.to_ref()) }
    }

    pub fn add_demote_memory_to_register_pass(&mut self) {
        unsafe { LLVMAddDemoteMemoryToRegisterPass(self.to_ref()) }
    }

    pub fn add_verifier_pass(&mut self) {
        unsafe { LLVMAddVerifierPass(self.to_ref()) }
    }

    pub fn add_correlated_value_propagation_pass(&mut self) {
        unsafe { LLVMAddCorrelatedValuePropagationPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_early_CSE_pass(&mut self) {
        unsafe { LLVMAddEarlyCSEPass(self.to_ref()) }
    }

    pub fn add_lower_expect_intrinsic_pass(&mut self) {
        unsafe { LLVMAddLowerExpectIntrinsicPass(self.to_ref()) }
    }

    pub fn add_type_based_alias_analysis_pass(&mut self) {
        unsafe { LLVMAddTypeBasedAliasAnalysisPass(self.to_ref()) }
    }

    #[allow(non_snake_case)]
    pub fn add_scoped_no_alias_AA_pass(&mut self) {
        unsafe { LLVMAddScopedNoAliasAAPass(self.to_ref()) }
    }

    pub fn add_basic_alias_analysis_pass(&mut self) {
        unsafe { LLVMAddBasicAliasAnalysisPass(self.to_ref()) }
    }
}

impl LLVMRef<LLVMPassManagerRef> for FunctionPassManager {
    fn to_ref(&self) -> LLVMPassManagerRef {
        self.manager
    }
}

impl Drop for FunctionPassManager {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposePassManager(self.to_ref());
        }
    }
}
