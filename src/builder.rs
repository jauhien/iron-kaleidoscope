use std::collections::HashMap;
use std::iter;

use llvm_sys::LLVMRealPredicate::{LLVMRealOLT, LLVMRealONE};
use llvm_sys::core::LLVMDeleteFunction;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::basic_block::{BasicBlock};
use iron_llvm::core::instruction::{PHINode, PHINodeRef};
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value, RealConstRef, RealConstCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

use jitter;
use parser;

pub struct Context {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: RealTypeRef
}

pub type IRBuildingResult = Result<(LLVMValueRef, bool), String>;

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult;
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

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

impl IRBuilder for parser::ParsingResult {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context, mcjitter),
            &Err(ref message) => Err(message.clone())
        }
    }
}

impl IRBuilder for Vec<parser::ASTNode> {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context, mcjitter)));
        }

        result
    }
}

impl IRBuilder for parser::ASTNode {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        match self {
            &parser::ExternNode(ref prototype) => prototype.codegen(context, mcjitter),
            &parser::FunctionNode(ref function) => function.codegen(context, mcjitter)
        }
    }
}

impl IRBuilder for parser::Prototype {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        // check if declaration with this name was already done
        let function = match mcjitter.get_current_module().get_function_by_name(&self.name) {
            Some(prev_definition) => {
                // we do not allow to redeclare functions with
                // other signatures
                if prev_definition.count_params() as usize != self.args.len() {
                    return error("redefinition of function with different number of args")
                }

                // we do not allow to redefine/redeclare already
                // defined functions (those that have the body)
                if prev_definition.count_basic_blocks() != 0 {
                    return error("redefinition of function");
                }

                prev_definition
            },
            None => {
                // function type is defined by number and types of
                // the arguments
                let mut param_types = iter::repeat(context.ty.to_ref()).take(self.args.len()).collect::<Vec<_>>();
                let fty = FunctionTypeRef::get(&context.ty, param_types.as_mut_slice(), false);
                FunctionRef::new(&mut mcjitter.get_current_module(), &self.name, &fty)
            }
        };

        // set correct parameters names
        for (param, arg) in function.params_iter().zip(&self.args) {
            param.set_name(arg);
        }

        Ok((function.to_ref(), false))
    }
}

impl IRBuilder for parser::Function {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        // we have no global variables, so we can clear all the
        // previously defined named values as they come from other functions
        context.named_values.clear();

        let (function, _) = try!(self.prototype.codegen(context, mcjitter));
        let mut function = unsafe {FunctionRef::from_ref(function)};

        // basic block that will contain generated instructions
        let mut bb = function.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);

        // set function parameters
        for (param, arg) in function.params_iter().zip(&self.prototype.args) {
            let arg_alloca = create_entry_block_alloca(context, &function, arg);
            context.builder.build_store(param.to_ref(), arg_alloca);
            context.named_values.insert(arg.clone(), arg_alloca);
        }

        // emit function body
        // if error occured, remove the function, so user can
        // redefine it
        let body = match self.body.codegen(context, mcjitter) {
            Ok((value, _)) => value,
            Err(message) => {
                unsafe {LLVMDeleteFunction(function.to_ref())};
                return Err(message);
            }
        };

        // the last instruction should be return
        context.builder.build_ret(&body);

        mcjitter.get_pass_manager().run(&mut function);

        // clear local variables
        context.named_values.clear();
        Ok((function.to_ref(), self.prototype.name.as_str() == ""))
    }
}

fn create_entry_block_alloca(context: &mut Context, function: &FunctionRef, var_name: &str) -> LLVMValueRef {
    let mut builder = core::Builder::new();
    let mut bb = function.get_entry();
    let fi = bb.get_first_instruction();
    builder.position(&mut bb, &fi);
    builder.build_alloca(context.ty.to_ref(), var_name)
}


impl IRBuilder for parser::Expression {
    fn codegen(&self, context: &mut Context, mcjitter: &mut jitter::MCJITter) -> IRBuildingResult {
        match self {


            &parser::LiteralExpr(ref value) => {
                Ok((RealConstRef::get(&context.ty, *value).to_ref(), false))
            },


            &parser::VariableExpr(ref name) => {
                match context.named_values.get(name) {
                    Some(value) => {
                        let var = context.builder.build_load(*value, name);
                        Ok((var, false))
                    },
                    None => error("unknown variable name")
                }
            },


            &parser::UnaryExpr(ref operator, ref operand) => {
                let (operand, _) = try!(operand.codegen(context, mcjitter));

                let name = "unary".to_string() + operator;
                let function = try!(mcjitter.get_function(name.as_str()));

                let mut args_value = vec![operand];

                Ok((context.builder.build_call(function.to_ref(),
                                                args_value.as_mut_slice(),
                                                "unop"),
                    false))
            },


            &parser::BinaryExpr(ref name, ref lhs, ref rhs) => {

                if name.as_str() == "=" {
                    let var_name = match **lhs {
                        parser::VariableExpr(ref nm) => nm,
                        _ => return error("destination of '=' must be a variable")
                    };

                    let (value, _) = try!(rhs.codegen(context, mcjitter));

                    let variable = match context.named_values.get(var_name) {
                        Some(vl) => *vl,
                        None => return error("unknown variable name")
                    };

                    context.builder.build_store(value, variable);

                    return Ok((value, false))
                }

                let (lhs_value, _) = try!(lhs.codegen(context, mcjitter));
                let (rhs_value, _) = try!(rhs.codegen(context, mcjitter));

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
                    op => {
                        let name = "binary".to_string() + op;

                        let function = try!(mcjitter.get_function(name.as_str()));

                        let mut args_value = vec![lhs_value, rhs_value];

                        Ok((context.builder.build_call(function.to_ref(),
                                                       args_value.as_mut_slice(),
                                                       "binop"),
                            false))
                    }
                }
            },


            &parser::CallExpr(ref name, ref args) => {
                let function = try!(mcjitter.get_function(name.as_str()));

                if function.count_params() as usize != args.len() {
                    return error("incorrect number of arguments passed")
                }

                let mut args_value = Vec::new();
                for arg in args.iter() {
                    let (arg_value, _) = try!(arg.codegen(context, mcjitter));
                    args_value.push(arg_value);
                }

                Ok((context.builder.build_call(function.to_ref(),
                                               args_value.as_mut_slice(),
                                               "calltmp"),
                    false))
            },


            &parser::ConditionalExpr{ref cond_expr, ref then_expr, ref else_expr} => {
                let (cond_value, _) = try!(cond_expr.codegen(context, mcjitter));
                let zero = RealConstRef::get(&context.ty, 0.0);
                let ifcond = context.builder.build_fcmp(LLVMRealONE, cond_value, zero.to_ref(), "ifcond");

                let block = context.builder.get_insert_block();
                let function = block.get_parent();
                let mut then_block = function.append_basic_block_in_context(&mut context.context, "then");
                let mut else_block = function.append_basic_block_in_context(&mut context.context, "else");
                let mut merge_block = function.append_basic_block_in_context(&mut context.context, "ifcont");
                context.builder.build_cond_br(ifcond, &then_block, &else_block);

                context.builder.position_at_end(&mut then_block);
                let (then_value, _) = try!(then_expr.codegen(context, mcjitter));
                context.builder.build_br(&merge_block);
                let then_end_block = context.builder.get_insert_block();

                context.builder.position_at_end(&mut else_block);
                let (else_value, _) = try!(else_expr.codegen(context, mcjitter));
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
                let (start_value, _) = try!(start_expr.codegen(context, mcjitter));

                let preheader_block = context.builder.get_insert_block();
                let function = preheader_block.get_parent();

                let variable = create_entry_block_alloca(context, &function, var_name);
                context.builder.build_store(start_value, variable);
                let old_value = context.named_values.remove(var_name);
                context.named_values.insert(var_name.clone(), variable);

                let mut preloop_block = function.append_basic_block_in_context(&mut context.context, "preloop");
                context.builder.build_br(&preloop_block);
                context.builder.position_at_end(&mut preloop_block);
                let (end_value, _) = try!(end_expr.codegen(context, mcjitter));
                let zero = RealConstRef::get(&context.ty, 0.0);
                let end_cond = context.builder.build_fcmp(LLVMRealONE, end_value, zero.to_ref(), "loopcond");

                let mut after_block = function.append_basic_block_in_context(&mut context.context, "afterloop");
                let mut loop_block = function.append_basic_block_in_context(&mut context.context, "loop");

                context.builder.build_cond_br(end_cond, &loop_block, &after_block);

                context.builder.position_at_end(&mut loop_block);
                try!(body_expr.codegen(context, mcjitter));

                let (step_value, _) = try!(step_expr.codegen(context, mcjitter));
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
                    let (init_value, _) = try!(init_expr.codegen(context, mcjitter));
                    let variable = create_entry_block_alloca(context, &function, name);
                    context.builder.build_store(init_value, variable);
                    old_bindings.push(context.named_values.remove(name));
                    context.named_values.insert(name.clone(), variable);
                }

                let (body_value, _) = try!(body_expr.codegen(context, mcjitter));

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
        }
    }
}
