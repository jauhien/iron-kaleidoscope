use std::collections::HashMap;
use libc::{c_char, c_uint};

use rustc::lib::llvm;

use missing_llvm_bindings::*;
use parser::*;

pub struct Context {
    context: llvm::ContextRef,
    module: llvm::ModuleRef,
    builder: llvm::BuilderRef,
    exec_engine: llvm::ExecutionEngineRef,
    function_passmanager: llvm::PassManagerRef,
    named_values: HashMap<String, llvm::ValueRef>
}

pub type IRBuildingResult = Result<(llvm::ValueRef, bool), String>;

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult;
}

pub fn dump_value(value: llvm::ValueRef) {
    unsafe {
        llvm::LLVMDumpValue(value);
    }
}

#[no_mangle]
pub extern fn print(x: f64) -> f64 {
    println!("> {} <", x);
    x
}

#[allow(unused_variable)]
pub fn run(value: llvm::ValueRef, context: &Context) -> f64 {
    //hack for make linker not to remove print function
    let x = print;

    unsafe {
        let result = LLVMRunFunction(context.exec_engine,
                                     value,
                                     0,
                                     0 as *const GenericValueRef);
        let ty = llvm::LLVMDoubleTypeInContext(context.context);
        LLVMGenericValueToFloat(ty, result)
    }
}

impl Context {
    pub fn new(module_name : &str) -> Context {
        unsafe {
            LLVMInitializeNativeTarget();

            let context = llvm::LLVMContextCreate();
            let module = module_name.with_c_str(|buf| {
                llvm::LLVMModuleCreateWithNameInContext(buf, context)
            });
            let builder = llvm::LLVMCreateBuilderInContext(context);
            let named_values = HashMap::new();

            let mut exec_engine = 0 as llvm::ExecutionEngineRef;
            let mut error = 0 as *const c_char;
            LLVMCreateExecutionEngineForModule(&mut exec_engine, module, &mut error);
            assert!(exec_engine != 0 as llvm::ExecutionEngineRef);

            let function_passmanager = llvm::LLVMCreateFunctionPassManagerForModule(module);

            let target_data = LLVMGetExecutionEngineTargetData(exec_engine);
            let data_layout = LLVMCopyStringRepOfTargetData(target_data);
            llvm::LLVMSetDataLayout(module, data_layout);
            llvm::LLVMAddTargetData(target_data, function_passmanager);
            LLVMDisposeMessage(data_layout);

            llvm::LLVMAddVerifierPass(function_passmanager);
            llvm::LLVMAddBasicAliasAnalysisPass(function_passmanager);
            llvm::LLVMAddInstructionCombiningPass(function_passmanager);
            llvm::LLVMAddReassociatePass(function_passmanager);
            llvm::LLVMAddGVNPass(function_passmanager);
            llvm::LLVMAddCFGSimplificationPass(function_passmanager);
            llvm::LLVMInitializeFunctionPassManager(function_passmanager);

            Context{context: context,
                    module: module,
                    builder: builder,
                    exec_engine: exec_engine,
                    function_passmanager:function_passmanager,
                    named_values: named_values}
        }
    }

    pub fn dump(&self) {
        unsafe {
            llvm::LLVMDumpModule(self.module);
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            llvm::LLVMDisposePassManager(self.function_passmanager);
            LLVMDisposeExecutionEngine(self.exec_engine);
            llvm::LLVMDisposeBuilder(self.builder);
            llvm::LLVMContextDispose(self.context);
        }
    }
}

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

impl IRBuilder for Expression {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            match self {
                &Literal(ref value) => {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    Ok((llvm::LLVMConstReal(ty, *value), false))
                },
                &Variable(ref name) => {
                    match context.named_values.find(name) {
                        Some(value) => Ok((*value, false)),
                        None => error("unknown variable name")
                    }
                },
                &Binary(ref name, ref lhs, ref rhs) => {
                    let lhs_value = match lhs.codegen(context) {
                        Ok((value, _)) => value,
                        Err(message) => return Err(message)
                    };
                    let rhs_value = match rhs.codegen(context) {
                        Ok((value, _)) => value,
                        Err(message) => return Err(message)
                    };

                    match name.as_slice() {
                        "+" => "addtmp".with_c_str(|buf| {
                            Ok((llvm::LLVMBuildFAdd(context.builder,
                                                   lhs_value,
                                                   rhs_value,
                                                   buf),
                                false))
                        }),
                        "-" => "subtmp".with_c_str(|buf| {
                            Ok((llvm::LLVMBuildFSub(context.builder,
                                                   lhs_value,
                                                   rhs_value,
                                                   buf),
                                false))
                        }),
                        "*" => "multmp".with_c_str(|buf| {
                            Ok((llvm::LLVMBuildFMul(context.builder,
                                                   lhs_value,
                                                   rhs_value,
                                                   buf),
                                false))
                        }),
                        "<" => {
                            let cmp = "cmptmp".with_c_str(|buf| {
                                llvm::LLVMBuildFCmp(context.builder,
                                              llvm::RealOLT as c_uint,
                                              lhs_value,
                                              rhs_value,
                                              buf)
                                });
                            let ty = llvm::LLVMDoubleTypeInContext(context.context);
                            "booltmp".with_c_str(|buf| {
                                Ok((llvm::LLVMBuildUIToFP(context.builder,
                                                         cmp,
                                                         ty,
                                                         buf),
                                    false))
                            })
                        },
                        _ => error("invalid binary operator")
                    }
                },
                &Call(ref name, ref args) => {
                    let function = name.with_c_str(|buf| {
                        llvm::LLVMGetNamedFunction(context.module, buf)
                    });
                    if function.is_null() {
                        return error("unknown function referenced")
                    }
                    if llvm::LLVMCountParams(function) as uint != args.len() {
                        return error("incorrect number of arguments passed")
                    }
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        let arg_value = match arg.codegen(context) {
                            Ok((value, _)) => value,
                            Err(message) => return Err(message)
                        };
                        args_value.push(arg_value);
                    }
                    "calltmp".with_c_str(|buf| {
                        Ok((llvm::LLVMBuildCall(context.builder,
                                               function,
                                               args_value.as_ptr(),
                                               args_value.len() as c_uint,
                                               buf),
                            false))
                    })
                },
                &Conditional{ref cond_expr, ref then_expr, ref else_expr} => return error("not implemented")
            }
        }
    }
}

impl IRBuilder for Prototype {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            let prev_definition = self.name.with_c_str(|buf| {
                llvm::LLVMGetNamedFunction(context.module, buf)
            });

            let function =
                if !prev_definition.is_null() {
                    if llvm::LLVMCountParams(prev_definition) as uint != self.args.len() {
                        return error("redefinition of function with different number of args")
                    }
                    if llvm::LLVMCountBasicBlocks(prev_definition) != 0 {
                        return error("redefinition of function");
                    }

                    prev_definition

                } else {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    let param_types = Vec::from_elem(self.args.len(), ty);
                    let fty = llvm::LLVMFunctionType(ty, param_types.as_ptr(), param_types.len() as c_uint, false as c_uint);

                    self.name.with_c_str(|buf| {
                        llvm::LLVMAddFunction(context.module,
                                              buf,
                                              fty)
                    })
                };

            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.args.iter() {
                arg.with_c_str(|buf| {
                     llvm::LLVMSetValueName(param, buf);
                });
                param = llvm::LLVMGetNextParam(param);
            }

            Ok((function, false))
        }
    }
}

impl IRBuilder for Function {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        context.named_values.clear();

        let function = match self.prototype.codegen(context) {
            Ok((func, _)) => func,
            Err(message) => return Err(message)
        };

        unsafe {
            let basic_block = "entry".with_c_str(|buf| {
                llvm::LLVMAppendBasicBlockInContext(context.context,
                                                    function,
                                                    buf)
            });

            llvm::LLVMPositionBuilderAtEnd(context.builder, basic_block);

            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.prototype.args.iter() {
                context.named_values.insert(arg.clone(), param);
                param = llvm::LLVMGetNextParam(param);
            }

            let body = match self.body.codegen(context) {
                Ok((value, _)) => value,
                Err(message) => {
                    llvm:: LLVMDeleteFunction(function);
                    return Err(message);
                }
            };

            llvm::LLVMBuildRet(context.builder, body);
            llvm::LLVMRunFunctionPassManager(context.function_passmanager, function);
        }

        context.named_values.clear();
        Ok((function, self.prototype.name.as_slice() == ""))
    }
}

impl IRBuilder for ASTNode {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &ExternNode(ref prototype) => prototype.codegen(context),
            &FunctionNode(ref function) => function.codegen(context)
        }
    }
}

impl IRBuilder for Vec<ASTNode> {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = match node.codegen(context) {
                Ok((value, runnable)) => Ok((value, runnable)),
                Err(message) => return Err(message)
            }
        }

        result
    }
}

impl IRBuilder for ParsingResult {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context),
            &Err(ref message) => Err(message.clone())
        }
    }
}
