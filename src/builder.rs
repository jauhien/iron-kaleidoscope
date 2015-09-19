//< ch-2 ch-3 ir-context
use std::collections::HashMap;
//> ir-context
use std::iter;

use llvm_sys::LLVMRealPredicate::
//> ch-2 ch-3
{
      LLVMRealONE,
//< ch-2 ch-3
/*j*/ LLVMRealOLT
//> ch-2 ch-3
}
//< ch-2 ch-3
/*j*/;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::core::LLVMDeleteFunction;
//< ir-context
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
//> ch-2 ch-3 ir-context
use iron_llvm::core::basic_block::{BasicBlock};
use iron_llvm::core::instruction::{PHINode, PHINodeRef};
//< ch-2 ch-3
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value, RealConstRef, RealConstCtor};
//< ir-context
use iron_llvm::core::types::{
//> ir-context
/*j*/  FunctionTypeCtor, 
/*j*/  FunctionTypeRef, 
//< ir-context
/*j*/  RealTypeCtor,
/*jw*/ RealTypeRef
/*j*/};
use iron_llvm::{LLVMRef, LLVMRefCtor};
//> ir-context

use parser;
//< ir-context

pub struct Context {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: RealTypeRef
}

impl Context {
    pub fn new() -> Context {

        let context = core::Context::get_global();
        let builder = core::Builder::new();
        let named_values = HashMap::new();
        let ty = RealTypeRef::get_double();

        Context { context: context,
                  builder: builder,
                  named_values: named_values,
                  ty: ty
        }
    }
}
//> ir-context

//< ir-module-provider jit-mp
pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
//> ch-2 ir-module-provider
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager;
//< ch-2 ir-module-provider
}
//> ch-2 ir-module-provider jit-mp

//< jit-fpm
pub fn new_module(name: &str) -> (core::Module, core::FunctionPassManager) {
    let module = core::Module::new(name);
    let mut function_passmanager = core::FunctionPassManager::new(&module);
//> ch-3 jit-fpm
    function_passmanager.add_promote_memory_to_register_pass();
//< ch-3 jit-fpm
    function_passmanager.add_basic_alias_analysis_pass();
    function_passmanager.add_instruction_combining_pass();
    function_passmanager.add_reassociate_pass();
    function_passmanager.add_GVN_pass();
    function_passmanager.add_CFG_simplification_pass();
    function_passmanager.initialize();

    (module, function_passmanager)
}
//> jit-fpm
//< ch-2 ir-module-provider jit-mp

pub struct SimpleModuleProvider {
//> ch-2 ir-module-provider
    function_passmanager: core::FunctionPassManager,
//< ch-2 ir-module-provider
    module: core::Module
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
//> ch-2 ir-module-provider
        let (module, function_passmanager) = new_module(name);
//> ch-3 jit-mp
/*
//< ch-2 ir-module-provider
        let module = core::Module::new(name);
//> ch-2 ir-module-provider
*/
//< ch-2 ch-3 ir-module-provider jit-mp

        SimpleModuleProvider {
//> ch-2 ir-module-provider
            function_passmanager: function_passmanager,
//< ch-2 ir-module-provider
            module: module
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }
//> ch-2 ir-module-provider

    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }
//< ch-2 ir-module-provider
}
//> ir-module-provider jit-mp

//< ir-builder-trait
pub type IRBuildingResult = Result<(LLVMValueRef, bool), String>;

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult;
}
//> ir-builder-trait

//< ir-top-level
impl IRBuilder for parser::ParsingResult {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context, module_provider),
            &Err(ref message) => Err(message.clone())
        }
    }
}

impl IRBuilder for Vec<parser::ASTNode> {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context, module_provider)));
        }

        result
    }
}

impl IRBuilder for parser::ASTNode {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &parser::ExternNode(ref prototype) => prototype.codegen(context, module_provider),
            &parser::FunctionNode(ref function) => function.codegen(context, module_provider)
        }
    }
}
//> ir-top-level

//< ir-prototype
impl IRBuilder for parser::Prototype {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        // check if declaration with this name was already done
        let function = match module_provider.get_function(&self.name) {
            Some((prev_definition, redefinition)) => {
                // we do not allow to redeclare functions with
                // other signatures
                if prev_definition.count_params() as usize != self.args.len() {
                    return error("redefinition of function with different number of args")
                }

                // we do not allow to redefine/redeclare already
                // defined functions (those that have the body)
                if redefinition {
                    return error("redefinition of function");
                }

                prev_definition
            },
            None => {
                // function type is defined by number and types of
                // the arguments
                let mut param_types = iter::repeat(context.ty.to_ref()).take(self.args.len()).collect::<Vec<_>>();
                let fty = FunctionTypeRef::get(&context.ty, param_types.as_mut_slice(), false);
                FunctionRef::new(&mut module_provider.get_module(), &self.name, &fty)
            }
        };

        // set correct parameters names
        for (param, arg) in function.params_iter().zip(&self.args) {
            param.set_name(arg);
        }

        Ok((function.to_ref(), false))
    }
}
//> ir-prototype

//< ir-function
impl IRBuilder for parser::Function {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        // we have no global variables, so we can clear all the
        // previously defined named values as they come from other functions
        context.named_values.clear();

        let (function, _) = try!(self.prototype.codegen(context, module_provider));
        let mut function = unsafe {FunctionRef::from_ref(function)};

        // basic block that will contain generated instructions
        let mut bb = function.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);

        // set function parameters
        for (param, arg) in function.params_iter().zip(&self.prototype.args) {
//> ch-2 ch-3 ir-function
            let arg_alloca = create_entry_block_alloca(context, &function, arg);
            context.builder.build_store(param.to_ref(), arg_alloca);
            context.named_values.insert(arg.clone(), arg_alloca);
            /*
//< ch-2 ch-3 ir-function
            context.named_values.insert(arg.clone(), param.to_ref());
//> ch-2 ch-3 ir-function
            */
//< ch-2 ch-3 ir-function
        }

//< jit-run-passes
        // emit function body
        // if error occured, remove the function, so user can
        // redefine it
        let body = match self.body.codegen(context, module_provider) {
            Ok((value, _)) => value,
            Err(message) => {
                unsafe {LLVMDeleteFunction(function.to_ref())};
                return Err(message);
            }
        };

        // the last instruction should be return
        context.builder.build_ret(&body);

        function.verify(LLVMAbortProcessAction);
//> ch-2 ch-3 ir-function
        module_provider.get_pass_manager().run(&mut function);
//< ch-2 ch-3 ir-function

        // clear local variables
        context.named_values.clear();
        Ok((function.to_ref(), self.prototype.name.as_str() == ""))
    }
}
//> ch-2 ch-3 ir-function jit-run-passes

fn create_entry_block_alloca(context: &mut Context, function: &FunctionRef, var_name: &str) -> LLVMValueRef {
    let mut builder = core::Builder::new();
    let mut bb = function.get_entry();
    let fi = bb.get_first_instruction();
    builder.position(&mut bb, &fi);
    builder.build_alloca(context.ty.to_ref(), var_name)
}


//< ch-2 ch-3 ir-expression
impl IRBuilder for parser::Expression {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {


//< ir-literal
            &parser::LiteralExpr(ref value) => {
                Ok((RealConstRef::get(&context.ty, *value).to_ref(), false))
            },
//> ir-literal


//< ir-variable
            &parser::VariableExpr(ref name) => {
                match context.named_values.get(name) {
                    Some(value) => {
//> ch-2 ch-3 ir-expression ir-variable
                        let var = context.builder.build_load(*value, name);
                        Ok((var, false))
/*
//< ch-2 ch-3 ir-expression ir-variable
                        Ok((*value, false))
//> ch-2 ch-3 ir-expression ir-variable
*/
//< ch-2 ch-3 ir-expression ir-variable
                    },
                    None => error("unknown variable name")
                }
            },
//> ch-2 ch-3 ir-expression ir-variable


            &parser::UnaryExpr(ref operator, ref operand) => {
                let (operand, _) = try!(operand.codegen(context, module_provider));

                let name = "unary".to_string() + operator;
                let (function, _) = match module_provider.get_function(name.as_str()) {
                    Some(function) => function,
                    None => return error("unary operator not found")
                };

                let mut args_value = vec![operand];

                Ok((context.builder.build_call(function.to_ref(),
                                                args_value.as_mut_slice(),
                                                "unop"),
                    false))
            },
//< ch-2 ch-3 ir-expression


//< ir-binary
            &parser::BinaryExpr(ref name, ref lhs, ref rhs) => {
//> ch-2 ch-3 ir-expression ir-binary

                if name.as_str() == "=" {
                    let var_name = match **lhs {
                        parser::VariableExpr(ref nm) => nm,
                        _ => return error("destination of '=' must be a variable")
                    };

                    let (value, _) = try!(rhs.codegen(context, module_provider));

                    let variable = match context.named_values.get(var_name) {
                        Some(vl) => *vl,
                        None => return error("unknown variable name")
                    };

                    context.builder.build_store(value, variable);

                    return Ok((value, false))
                }

//< ch-2 ch-3 ir-expression ir-binary
                let (lhs_value, _) = try!(lhs.codegen(context, module_provider));
                let (rhs_value, _) = try!(rhs.codegen(context, module_provider));

                match name.as_str() {
                    "+" => Ok((context.builder.build_fadd(lhs_value,
                                                          rhs_value,
                                                          "addtmp"),
                               false)),
                    "-" => Ok((context.builder.build_fsub(lhs_value,
                                                          rhs_value,
                                                          "subtmp"),
                               false)),
                    "*" => Ok((context.builder.build_fmul(lhs_value,
                                                          rhs_value,
                                                          "multmp"),
                               false)),
                    "<" => {
                        let cmp = context.builder.build_fcmp(LLVMRealOLT,
                                                             lhs_value,
                                                             rhs_value,
                                                             "cmptmp");

                        // convert boolean to double 0.0 or 1.0
                        Ok((context.builder.build_ui_to_fp(cmp,
                                                           context.ty.to_ref(),
                                                           "booltmp"),
                            false))
                    },
//> ch-2 ch-3 ir-expression ir-binary
/*
//< ch-2 ch-3 ir-expression ir-binary
                    _ => error("invalid binary operator")
//> ch-2 ch-3 ir-expression ir-binary
*/
                    op => {
                        let name = "binary".to_string() + op;

                        let (function, _) = match module_provider.get_function(&name) {
                            Some(function) => function,
                            None => return error("binary operator not found")
                        };

                        let mut args_value = vec![lhs_value, rhs_value];

                        Ok((context.builder.build_call(function.to_ref(),
                                                       args_value.as_mut_slice(),
                                                       "binop"),
                            false))
                    }
//< ch-2 ch-3 ir-expression ir-binary
                }
            },
//> ch-2 ch-3 ir-binary


//< ch-2 ch-3 ir-call
            &parser::CallExpr(ref name, ref args) => {
                let (function, _) = match module_provider.get_function(name) {
                    Some(function) => function,
                    None => return error("unknown function referenced")
                };

                if function.count_params() as usize != args.len() {
                    return error("incorrect number of arguments passed")
                }

                let mut args_value = Vec::new();
                for arg in args.iter() {
                    let (arg_value, _) = try!(arg.codegen(context, module_provider));
                    args_value.push(arg_value);
                }

                Ok((context.builder.build_call(function.to_ref(),
                                               args_value.as_mut_slice(),
                                               "calltmp"),
                    false))
            }
//> ch-2 ch-3 ir-expression ir-call
            ,


            &parser::ConditionalExpr{ref cond_expr, ref then_expr, ref else_expr} => {
                let (cond_value, _) = try!(cond_expr.codegen(context, module_provider));
                let zero = RealConstRef::get(&context.ty, 0.0);
                let ifcond = context.builder.build_fcmp(LLVMRealONE, cond_value, zero.to_ref(), "ifcond");

                let block = context.builder.get_insert_block();
                let mut function = block.get_parent();
                let mut then_block = function.append_basic_block_in_context(&mut context.context, "then");
                let mut else_block = function.append_basic_block_in_context(&mut context.context, "else");
                let mut merge_block = function.append_basic_block_in_context(&mut context.context, "ifcont");
                context.builder.build_cond_br(ifcond, &then_block, &else_block);

                context.builder.position_at_end(&mut then_block);
                let (then_value, _) = try!(then_expr.codegen(context, module_provider));
                context.builder.build_br(&merge_block);
                let then_end_block = context.builder.get_insert_block();

                context.builder.position_at_end(&mut else_block);
                let (else_value, _) = try!(else_expr.codegen(context, module_provider));
                context.builder.build_br(&merge_block);
                let else_end_block = context.builder.get_insert_block();

                context.builder.position_at_end(&mut merge_block);
                let mut phi = unsafe {
                    PHINodeRef::from_ref(context.builder.build_phi(context.ty.to_ref(), "ifphi"))
                };
                phi.add_incoming(vec![then_value].as_mut_slice(), vec![then_end_block].as_mut_slice());
                phi.add_incoming(vec![else_value].as_mut_slice(), vec![else_end_block].as_mut_slice());

                Ok((phi.to_ref(), false))
            },


            &parser::LoopExpr{ref var_name, ref start_expr, ref end_expr, ref step_expr, ref body_expr} => {
                let (start_value, _) = try!(start_expr.codegen(context, module_provider));

                let preheader_block = context.builder.get_insert_block();
                let mut function = preheader_block.get_parent();

                let variable = create_entry_block_alloca(context, &function, var_name);
                context.builder.build_store(start_value, variable);
                let old_value = context.named_values.remove(var_name);
                context.named_values.insert(var_name.clone(), variable);

                let mut preloop_block = function.append_basic_block_in_context(&mut context.context, "preloop");
                context.builder.build_br(&preloop_block);
                context.builder.position_at_end(&mut preloop_block);
                let (end_value, _) = try!(end_expr.codegen(context, module_provider));
                let zero = RealConstRef::get(&context.ty, 0.0);
                let end_cond = context.builder.build_fcmp(LLVMRealONE, end_value, zero.to_ref(), "loopcond");

                let mut after_block = function.append_basic_block_in_context(&mut context.context, "afterloop");
                let mut loop_block = function.append_basic_block_in_context(&mut context.context, "loop");

                context.builder.build_cond_br(end_cond, &loop_block, &after_block);

                context.builder.position_at_end(&mut loop_block);
                try!(body_expr.codegen(context, module_provider));

                let (step_value, _) = try!(step_expr.codegen(context, module_provider));
                let cur_value = context.builder.build_load(variable, var_name);
                let next_value = context.builder.build_fadd(cur_value, step_value, "nextvar");
                context.builder.build_store(next_value, variable);

                context.builder.build_br(&preloop_block);

                context.builder.position_at_end(&mut after_block);

                context.named_values.remove(var_name);
                match old_value {
                    Some(value) => {context.named_values.insert(var_name.clone(), value);},
                    None => ()
                };

                Ok((zero.to_ref(), false))
            },


            &parser::VarExpr{ref vars, ref body_expr} => {
                let mut old_bindings = Vec::new();
                let function = context.builder.get_insert_block().get_parent();
                for var in vars.iter() {
                    let (ref name, ref init_expr) = *var;
                    let (init_value, _) = try!(init_expr.codegen(context, module_provider));
                    let variable = create_entry_block_alloca(context, &function, name);
                    context.builder.build_store(init_value, variable);
                    old_bindings.push(context.named_values.remove(name));
                    context.named_values.insert(name.clone(), variable);
                }

                let (body_value, _) = try!(body_expr.codegen(context, module_provider));

                let mut old_iter = old_bindings.iter();
                for var in vars.iter() {
                    let (ref name, _) = *var;
                    context.named_values.remove(name);

                    match old_iter.next() {
                        Some(&Some(value)) => {context.named_values.insert(name.clone(), value);},
                        _ => ()
                    };
                }

                Ok((body_value, false))
            }
//< ch-2 ch-3 ir-expression
        }
    }
}
//> ch-2 ch-3 ir-expression
