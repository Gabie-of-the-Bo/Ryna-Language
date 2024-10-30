use std::sync::Arc;
use std::time::Instant;

use colored::Colorize;
use rustc_hash::FxHashMap;
use serde::Serialize;
use malachite::Integer;
use malachite::num::conversion::traits::{RoundingFrom, SaturatingInto};
use malachite::rounding_modes::RoundingMode;

use crate::config::{precompile_ryna_module_with_config, read_compiled_cache, save_compiled_cache, compute_project_hash};
use crate::debug::DebugInfo;
use crate::functions::FunctionOverload;
use crate::integer_ext::{is_valid_index, to_usize, ONE};
use crate::ryna_warning;
use crate::types::Type;
use crate::object::{RynaArray, RynaLambda, RynaTuple, Object, TypeInstance};
use crate::context::RynaContext;
use crate::operations::Operator;
use crate::compilation::{CompiledRynaExpr, RynaError};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl RynaContext {
    pub fn parse_and_execute_ryna_module(&mut self, code: &String) -> Result<ExecutionInfo, RynaError> {
        let mut compiled_code = self.parse_and_compile(code)?;

        if self.optimize {
            self.optimize_instructions(&mut compiled_code);
        }

        for (idx, i) in compiled_code.iter().enumerate() {
            println!("{:<3} {}", idx, i.to_string(self));
        }
        
        self.execute_compiled_code::<false>(&compiled_code.into_iter().map(|i| i.instruction).collect::<Vec<_>>(), &[])
    }

    pub fn parse_and_execute_ryna_project_inner<const DEBUG: bool>(path: String, macro_code: Option<String>, force_recompile: bool, optimize: bool, test: bool, program_input: &[String]) -> Result<ExecutionInfo, RynaError> {
        let combined_hash;
        let all_modules;
        let file_cache;

        let no_macro = macro_code.is_none();

        match compute_project_hash(&path, macro_code, optimize, test) {
            Ok((hash, all_mods, files)) => {
                combined_hash = hash.clone();
                all_modules = all_mods;
                file_cache = files;

                if !force_recompile {
                    if let Some(mut code) = read_compiled_cache(&path) {
                        if hash == code.hash {
                            if DEBUG {
                                ryna_warning!(
                                    "Function timings will not be dumped when executing cached code (use {} to force recompilation)",
                                    "--recompile".green()
                                );
                            }

                            return code.execute::<DEBUG>(program_input); 
                        }    
                    }
                }
            }

            Err(err) => err.emit()
        }

        match precompile_ryna_module_with_config(&path, all_modules, file_cache, optimize, test, force_recompile) {
            Ok((mut ctx, code)) => match ctx.compiled_form(&code) {
                Ok(mut instr) => {
                    if optimize {
                        ctx.optimize_instructions(&mut instr);
                    }
               
                    if no_macro {
                        let ser_module = ctx.get_serializable_module(combined_hash, &instr);

                        if let Err(err) = save_compiled_cache(&path, &ser_module) {
                            err.emit();
                        }
                    }

                    ctx.program_input = program_input.to_vec();

                    let mut instructions = Vec::with_capacity(instr.len());
                    let mut debug_info = Vec::with_capacity(instr.len());

                    for i in instr {
                        instructions.push(i.instruction);
                        debug_info.push(i.debug_info);
                    }

                    ctx.execute_compiled_code::<DEBUG>(&instructions, &debug_info)
                },

                Err(err) => err.emit(),
            },

            Err(err) => err.emit(),
        }
    }

    pub fn parse_and_execute_ryna_project<const DEBUG: bool>(path: String, force_recompile: bool, optimize: bool, test: bool, program_input: &[String]) -> Result<ExecutionInfo, RynaError> {        
        Self::parse_and_execute_ryna_project_inner::<DEBUG>(path, None, force_recompile, optimize, test, program_input)
    }
}

#[derive(Serialize)]
pub struct ProfilingInfo {
    pub instr_count: FxHashMap<&'static str, usize>,
    pub instr_time: FxHashMap<&'static str, u128>,
    pub loc_time: FxHashMap<Arc<String>, FxHashMap<usize, u128>>,
    pub total_time: u128
}

pub struct ExecutionInfo {
    pub profiling_info: Option<ProfilingInfo>,
    pub captured_output: String
}

impl RynaContext {
    pub fn execute_compiled_code<const DEBUG: bool>(&mut self, program: &[CompiledRynaExpr], debug_info: &[DebugInfo]) -> Result<ExecutionInfo, RynaError> {
        use CompiledRynaExpr::*;

        const MAX_STACK_FRAMES: usize = 100000;

        let mut ip: i32 = 0;
        let mut offset: usize = 0;
        
        let mut call_stack: Vec<(i32, usize, i32)> = Vec::with_capacity(1000);
        let mut stack: Vec<Object> = Vec::with_capacity(1000);

        let mut instr_count = FxHashMap::<&str, usize>::default();
        let mut instr_time = FxHashMap::<&str, u128>::default();
        let mut loc_time = FxHashMap::<Arc<String>, FxHashMap<usize, u128>>::default();
        let mut total_time = 0;

        macro_rules! tos {
            () => {
                stack.pop().unwrap()
            }
        }

        macro_rules! fetch_opcode {
            () => {
                unsafe { program.get_unchecked(ip as usize) }
            }
        }

        macro_rules! unary_op {
            ($name: expr, $a: ident, $get_a: ident, $t: ty, $op: expr) => {
                ryna_instruction!($name, {
                    let _a = tos!();
                    let $a = &*_a.$get_a::<$t>();
    
                    stack.push(Object::new($op));
                    ip += 1;
                })
            };
        }

        macro_rules! bin_op {
            ($name: expr, $a: ident, $b: ident, $get_a: ident, $get_b: ident, $t: ty, $op: expr) => {
                ryna_instruction!($name, {
                    let _a = tos!();
                    let _b = tos!();
                    let $a = &*_a.$get_a::<$t>();
                    let $b = &*_b.$get_b::<$t>();
                        
                    stack.push(Object::new($op));
                    ip += 1;
                })
            };
        }

        macro_rules! ryna_instruction {
            ($name: expr, $expr: expr) => {
                if DEBUG {                    
                    let now = Instant::now();

                    $expr

                    let elapsed = now.elapsed().as_nanos();

                    *instr_time.entry($name).or_default() += elapsed;
                    *instr_count.entry($name).or_default() += 1;

                    let lines_to_check = call_stack[1..].iter()
                                                        .flat_map(|i| &debug_info[i.0 as usize].lines)
                                                        .chain(&debug_info[ip as usize].lines)
                                                        .collect::<rustc_hash::FxHashSet<_>>();
                    
                    // For each frame in the stack (excluding the first)
                    for j in lines_to_check {
                        *loc_time.entry(j.0.clone()).or_default().entry(j.1).or_default() += elapsed;    
                    }

                    total_time += elapsed;

                } else {
                    $expr
                }
            };
        }

        macro_rules! idx_op {
            ($deref_arr: ident, $ref_method: ident) => {
                let arr = tos!();
                let first = tos!();

                let arr = &*arr.$deref_arr::<RynaArray>();
                let idx = &*first.get::<Integer>();

                if !is_valid_index(idx) {
                    return Err(RynaError::execution_error(format!("{} is not a valid index", idx)));
                
                } else {
                    let native_idx = to_usize(idx);
                    
                    if arr.elements.len() <= native_idx {
                        return Err(RynaError::execution_error(format!("{} is higher than the length of the array ({})", idx, arr.elements.len())));
    
                    } else {
                        stack.push(arr.elements[native_idx].$ref_method());
                    }
                } 

                ip += 1;
            };
        }

        macro_rules! check_call_stack_limit {
            () => {
                if call_stack.len() > MAX_STACK_FRAMES {
                    return Err(RynaError::execution_error(format!("Too many stack frames (max. of {})", MAX_STACK_FRAMES)));
                }
            }
        }

        macro_rules! store_variable {
            ($id: expr, $value: expr) => {
                if $id >= self.variables.len() {
                    self.variables.resize($id + 1, Object::no_value());
                }

                // SAFETY: this is safe because the previous check makes sure that the index
                // exists inside self.variables, making this unsafe access an optimization
                unsafe { *self.variables.get_unchecked_mut($id) = $value; }
            }
        }

        macro_rules! get_variable {
            ($id: expr) => {
                // SAFETY: this is safe as long as an storage is made before the access, which
                // is guaranteed by the Ryna compiler
                unsafe { self.variables.get_unchecked($id) }
            }
        }

        macro_rules! update_max_var {
            ($id: expr) => {
                // SAFETY: this is safe because call_stack will never be empty
                let idx = call_stack.len() - 1;
                let l = unsafe { &mut call_stack.get_unchecked_mut(idx).2 };
                *l = (*l).max($id as i32);
            }
        }

        macro_rules! add_stack_frame {
            ($new_ip: expr) => {
                // SAFETY: this is safe because call_stack will never be empty
                call_stack.push((ip + 1, offset, -1));
                ip = $new_ip;
                unsafe { offset += (call_stack.get_unchecked(call_stack.len() - 2).2 + 1) as usize };

                check_call_stack_limit!();
            }
        }

        macro_rules! lambda_call {
            ($lambda_ref: ident) => {
                let arg = tos!();
                let f = &arg.$lambda_ref::<RynaLambda>();

                stack.extend(f.captures.iter().rev().cloned());
                
                add_stack_frame!(f.loc as i32);
            };
        }

        call_stack.push((0, 0, -1));

        loop {
            match fetch_opcode!() {
                Empty => ryna_instruction!("Empty", {
                    stack.push(Object::empty());
                    ip += 1;
                }),

                Bool(obj) => ryna_instruction!("Bool", {
                    stack.push(Object::new(*obj));
                    ip += 1;
                }),

                Float(obj) => ryna_instruction!("Float", {
                    stack.push(Object::new(*obj));
                    ip += 1;
                }),

                Int(obj) => ryna_instruction!("Int", {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                }),

                Str(obj) => ryna_instruction!("Str", {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                }),

                Array(length, t) => ryna_instruction!("Array", {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::arr(args, t.clone()));

                    ip += 1;
                }),

                Lambda(pos, cap, args, ret) => ryna_instruction!("Lambda", {
                    let start_idx = stack.len() - cap;
                    let captures = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::lambda(*pos, captures, args.clone(), ret.clone()));
                    ip += 1;
                }),

                Construct(id, length, ts) => ryna_instruction!("Construct", {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::new(TypeInstance {
                        id: *id,
                        params: ts.clone(),
                        attributes: args,
                    }));

                    ip += 1;
                }),

                AttributeAssign(attr_idx) => ryna_instruction!("AttributeAssign", {
                    let a = tos!();
                    let b = tos!();

                    b.deref::<TypeInstance>().attributes[*attr_idx] = a;

                    ip += 1;
                }),

                AttributeMove(idx) => ryna_instruction!("AttributeMove", {
                    let elem = tos!();
                    stack.push(elem.get::<TypeInstance>().attributes[*idx].move_contents_if_ref());
                    ip += 1;
                }),

                AttributeRef(idx) => ryna_instruction!("AttributeRef", {
                    let elem = tos!();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_ref());
                    ip += 1;
                }),

                AttributeMut(idx) => ryna_instruction!("AttributeMut", {
                    let elem = tos!();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_mut());
                    ip += 1;
                }),

                AttributeCopy(idx) => ryna_instruction!("AttributeCopy", {
                    let elem = tos!();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].deref_deep_clone());
                    ip += 1;
                }),

                AttributeDeref(idx) => ryna_instruction!("AttributeDeref", {
                    let elem = tos!();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].deref_if_ref());
                    ip += 1;
                }),

                Tuple(length) => ryna_instruction!("Tuple", {     
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect::<Vec<_>>();
                    let types = args.iter().map(|i| i.get_type()).collect::<Vec<_>>();

                    stack.push(Object::tuple(args, types));

                    ip += 1;
                }),

                TupleElemMove(idx) => ryna_instruction!("TupleElemMove", {
                    let elem = tos!();
                    stack.push(elem.get::<RynaTuple>().elements[*idx].move_contents_if_ref());
                    ip += 1;
                }),

                TupleElemRef(idx) => ryna_instruction!("TupleElemRef", {
                    let elem = tos!();
                    stack.push(elem.deref::<RynaTuple>().elements[*idx].get_ref());
                    ip += 1;
                }),

                TupleElemMut(idx) => ryna_instruction!("TupleElemMut", {
                    let elem = tos!();
                    stack.push(elem.deref::<RynaTuple>().elements[*idx].get_mut());
                    ip += 1;
                }),

                TupleElemCopy(idx) => ryna_instruction!("TupleElemCopy", {
                    let elem = tos!();
                    stack.push(elem.deref::<RynaTuple>().elements[*idx].deref_deep_clone());
                    ip += 1;
                }),

                TupleElemDeref(idx) => ryna_instruction!("TupleElemDeref", {
                    let elem = tos!();
                    stack.push(elem.deref::<RynaTuple>().elements[*idx].deref_if_ref());
                    ip += 1;
                }),

                IdxMove => ryna_instruction!("IdxMove", { idx_op!(get, move_contents_if_ref); }),
                IdxRef => ryna_instruction!("IdxRef", { idx_op!(deref, get_ref_nostack); }),
                IdxMut => ryna_instruction!("IdxMut", { idx_op!(deref, get_mut_nostack); }),
                IdxMoveRef => ryna_instruction!("IdxMoveRef", { idx_op!(deref, move_contents_if_ref); }),

                StoreIntVariable(id, obj) => ryna_instruction!("StoreIntVariable", {
                    update_max_var!(*id);
                    store_variable!(*id + offset, Object::new(obj.clone()));
                    ip += 1;
                }),

                StoreStringVariable(id, obj) => ryna_instruction!("StoreStringVariable", {
                    update_max_var!(*id);
                    store_variable!(*id + offset, Object::new(obj.clone()));
                    ip += 1;
                }),

                StoreBoolVariable(id, obj) => ryna_instruction!("StoreBoolVariable", {
                    update_max_var!(*id);
                    store_variable!(*id + offset, Object::new(*obj));
                    ip += 1;
                }),

                StoreFloatVariable(id, obj) => ryna_instruction!("StoreFloatVariable", {
                    update_max_var!(*id);
                    store_variable!(*id + offset, Object::new(*obj));
                    ip += 1;
                }),
                
                StoreVariable(id) => ryna_instruction!("StoreVariable", {
                    update_max_var!(*id);
                    store_variable!(*id + offset, tos!());
                    ip += 1;
                }),

                GetVariable(id) => ryna_instruction!("GetVariable", {
                    stack.push(get_variable!(*id + offset).get_mut());
                    ip += 1;
                }),

                CloneVariable(id) => ryna_instruction!("CloneVariable", {
                    stack.push(get_variable!(*id + offset).clone());
                    ip += 1;
                }),

                RefVariable(id) => ryna_instruction!("RefVariable", {
                    stack.push(get_variable!(*id + offset).get_ref());
                    ip += 1;
                }),

                DerefVariable(id) => ryna_instruction!("DerefVariable", {
                    stack.push(get_variable!(*id + offset).deref_if_ref());
                    ip += 1;
                }),

                CopyVariable(id) => ryna_instruction!("CopyVariable", {
                    stack.push(get_variable!(*id + offset).deref_deep_clone());
                    ip += 1;
                }),

                MoveVariable(id) => ryna_instruction!("MoveVariable", {
                    stack.push(get_variable!(*id + offset).move_contents_if_ref());
                    ip += 1;
                }),

                Assign => ryna_instruction!("Assign", {
                    let a = tos!();
                    let b = tos!();

                    if let Err(msg) = a.assign(b, self) {
                        return Err(RynaError::execution_error(msg));
                    }

                    ip += 1;
                }),

                AssignToVar(id) => ryna_instruction!("AssignToVar", {
                    let var = &get_variable!(*id + offset);
                    let value = tos!();

                    if let Err(msg) = var.assign_direct(value, self) {
                        return Err(RynaError::execution_error(msg));
                    }

                    ip += 1;
                }),

                AssignToVarDirect(id) => ryna_instruction!("AssignToVarDirect", {
                    let var = &get_variable!(*id + offset);
                    let value = tos!();

                    if let Err(msg) = var.assign(value, self) {
                        return Err(RynaError::execution_error(msg));
                    }

                    ip += 1;
                }),

                Drop => ryna_instruction!("Drop", {
                    tos!();
                    ip += 1;
                }),

                Jump(to) => ip = *to as i32,
                RelativeJump(to) => ip += *to,
                RelativeJumpIfFalse(to, false) => ryna_instruction!("RelativeJumpIfFalse", {
                    if !*tos!().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),

                RelativeJumpIfTrue(to, false) => ryna_instruction!("RelativeJumpIfTrue", {
                    if *tos!().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),

                RelativeJumpIfFalse(to, true) => ryna_instruction!("RelativeJumpIfFalse", {
                    if !*stack.last().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),

                RelativeJumpIfTrue(to, true) => ryna_instruction!("RelativeJumpIfTrue", {
                    if *stack.last().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),

                Call(to) => ryna_instruction!("Call", { add_stack_frame!(*to as i32); }),
                CallDestructor(to) => ryna_instruction!("CallDestructor", { 
                    if stack.last().unwrap().deref_ref_count() <= 2 {
                        add_stack_frame!(*to as i32); 
                    
                    } else {
                        ip += 1;
                    }
                }),
                LambdaCall => ryna_instruction!("LambdaCall", { lambda_call!(get); }),
                LambdaCallRef => ryna_instruction!("LambdaCallRef", { lambda_call!(deref); }),

                DeleteVar(var_idx) => ryna_instruction!("DeleteVar", { 
                    self.variables[*var_idx + offset] = Object::no_value();
                    ip += 1;                    
                }),

                Return => ryna_instruction!("Return", {
                    let (prev_ip, prev_offset, _) = call_stack.pop().unwrap();
                    let idx = call_stack.len() - 1;
                    let l = call_stack[idx].2.max(0) as usize;

                    // Clear context variables
                    self.variables[offset..(offset + l)].fill(Object::no_value());

                    ip = prev_ip;
                    offset = prev_offset;
                }), 

                NativeFunctionCall(func_id, ov_id, type_args) => ryna_instruction!("NativeFunctionCall", {
                    if let FunctionOverload { args: Type::And(v), ret: r, function: Some(f), .. } = &self.functions[*func_id].overloads[*ov_id] {
                        let mut args = Vec::with_capacity(v.len());

                        for _ in v {
                            args.push(tos!());
                        }

                        match f(type_args, r, args, self) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(RynaError::execution_error(msg))
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                NativeFunctionCallNoRet(func_id, ov_id, type_args) => ryna_instruction!("NativeFunctionCallNoRet", {
                    if let FunctionOverload { args: Type::And(v), ret: r, function: Some(f), .. } = &self.functions[*func_id].overloads[*ov_id] {
                        let mut args = Vec::with_capacity(v.len());

                        for _ in v {
                            args.push(tos!());
                        }

                        if let Err(msg) = f(type_args, r, args, self) {
                            return Err(RynaError::execution_error(msg));
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                UnaryOperatorCall(op_id, ov_id, type_args) => ryna_instruction!("UnaryOperatorCall", {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = tos!();

                        let ov = &operations[*ov_id];

                        match ov.operation.unwrap()(type_args, &ov.ret, obj) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(RynaError::execution_error(msg))
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                UnaryOperatorCallNoRet(op_id, ov_id, type_args) => ryna_instruction!("UnaryOperatorCallNoRet", {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = tos!();

                        let ov = &operations[*ov_id];

                        if let Err(msg) = ov.operation.unwrap()(type_args, &ov.ret, obj) {
                            return Err(RynaError::execution_error(msg));
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                BinaryOperatorCall(op_id, ov_id, type_args) => ryna_instruction!("BinaryOperatorCall", {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = tos!();
                        let b = tos!();

                        let ov = &operations[*ov_id];

                        match ov.operation.unwrap()(type_args, &ov.ret, a, b, self) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(RynaError::execution_error(msg))
                        };
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                BinaryOperatorCallNoRet(op_id, ov_id, type_args) => ryna_instruction!("BinaryOperatorCallNoRet", {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = tos!();
                        let b = tos!();

                        let ov = &operations[*ov_id];

                        if let Err(msg) = ov.operation.unwrap()(type_args, &ov.ret, a, b, self) {
                            return Err(RynaError::execution_error(msg));
                        };
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                NaryOperatorCall(op_id, ov_id, type_args) => ryna_instruction!("NaryOperatorCall", {
                    if let Operator::Nary{operations, ..} = &self.nary_ops[*op_id] {
                        let op_ov = &operations[*ov_id];
                        let res = op_ov.operation.unwrap()((&mut stack, &mut offset, &mut call_stack, &mut ip), type_args, &op_ov.ret);

                        if let Err(msg) = res {
                            return Err(RynaError::execution_error(msg));
                        }

                    } else {
                        unreachable!();
                    }
                }),

                ToFloat => unary_op!("ToFloat", a, get, Integer, f64::rounding_from(a, RoundingMode::Exact).0),

                Ref => ryna_instruction!("Ref", {
                    let a = tos!();
                    stack.push(a.get_ref_nostack());
                    ip += 1;
                }),

                Mut => ryna_instruction!("Mut", {
                    let a = tos!();
                    stack.push(a.get_mut_nostack());
                    ip += 1;
                }),

                Copy => ryna_instruction!("Copy", {
                    let a = tos!();
                    stack.push(a.deref_obj().deep_clone());
                    ip += 1;
                }),

                Deref => ryna_instruction!("Deref", {
                    let a = tos!();
                    stack.push(a.deref_obj());
                    ip += 1;
                }),

                Demut => ryna_instruction!("Demut", {
                    let a = tos!();
                    stack.push(a.get_ref());
                    ip += 1;
                }),

                Move => ryna_instruction!("Move", {
                    let a = tos!();
                    stack.push(a.move_contents());
                    ip += 1;
                }),

                Inc => ryna_instruction!("Inc", {
                    let a = tos!();
                    *a.deref::<Integer>() += &*ONE;
                    ip += 1;
                }),

                Dec => ryna_instruction!("Dec", {
                    let a = tos!();
                    *a.deref::<Integer>() -= &*ONE;
                    ip += 1;
                }),

                Addi => bin_op!("Addi", a, b, get, get, Integer, a + b),
                Subi => bin_op!("Subi", a, b, get, get, Integer, a - b),
                Muli => bin_op!("Muli", a, b, get, get, Integer, a * b),
                Divi => bin_op!("Divi", a, b, get, get, Integer, a / b),
                Modi => bin_op!("Modi", a, b, get, get, Integer, a % b),
                Negi => unary_op!("Negi", a, get, Integer, -a),
                Addf => bin_op!("Addf", a, b, get, get, f64, a + b),
                Subf => bin_op!("Subf", a, b, get, get, f64, a - b),
                Mulf => bin_op!("Mulf", a, b, get, get, f64, a * b),
                Divf => bin_op!("Divf", a, b, get, get, f64, a / b),
                Modf => bin_op!("Modf", a, b, get, get, f64, a % b),
                Negf => unary_op!("Negf", a, get, f64, -a),

                AddStr => bin_op!("AddStr", a, b, get, get, String, format!("{}{}", a, b)),

                NotB => unary_op!("NotB", a, get, Integer, !a),
                AndB => bin_op!("AndB", a, b, get, get, Integer, a & b),
                OrB => bin_op!("OrB", a, b, get, get, Integer, a | b),
                XorB => bin_op!("XorB", a, b, get, get, Integer, a ^ b),
                Shl => bin_op!("Shl", a, b, get, get, Integer, a << SaturatingInto::<i64>::saturating_into(b)),
                Shr => bin_op!("Shr", a, b, get, get, Integer, a >> SaturatingInto::<i64>::saturating_into(b)),

                Lti => bin_op!("Lti", a, b, get, get, Integer, a < b),
                Gti => bin_op!("Gti", a, b, get, get, Integer, a > b),
                Lteqi => bin_op!("Lteqi", a, b, get, get, Integer, a <= b),
                Gteqi => bin_op!("Gteqi", a, b, get, get, Integer, a >= b),
                Eqi => bin_op!("Eqi", a, b, get, get, Integer, a == b),
                Neqi => bin_op!("Neqi", a, b, get, get, Integer, a != b),
                Ltf => bin_op!("Ltf", a, b, get, get, f64, a < b),
                Gtf => bin_op!("Gtf", a, b, get, get, f64, a > b),
                Lteqf => bin_op!("Lteqf", a, b, get, get, f64, a <= b),
                Gteqf => bin_op!("Gteqf", a, b, get, get, f64, a >= b),
                Eqf => bin_op!("Eqf", a, b, get, get, f64, a == b),
                Neqf => bin_op!("Neqf", a, b, get, get, f64, a != b),

                EqBool => bin_op!("EqBool", a, b, get, get, bool, a == b),
                NeqBool => bin_op!("NeqBool", a, b, get, get, bool, a != b),

                EqStr => bin_op!("EqStr", a, b, get, get, String, a == b),
                NeqStr => bin_op!("NeqStr", a, b, get, get, String, a != b),

                Not => unary_op!("Not", a, get, bool, !a),
                Or => bin_op!("Or", a, b, get, get, bool, *a || *b),
                And => bin_op!("And", a, b, get, get, bool, *a && *b),
                Xor => bin_op!("Xor", a, b, get, get, bool, *a ^ *b),
                Nor => bin_op!("Nor", a, b, get, get, bool, !(*a || *b)),
                Nand => bin_op!("Nand", a, b, get, get, bool, !(*a && *b)),

                Placeholder(_) => unreachable!(),

                Halt => break,
            }
        }

        Ok(ExecutionInfo {
            profiling_info: if DEBUG {
                Some(ProfilingInfo { 
                    instr_count, instr_time, loc_time, total_time
                })

            } else {
                None
            },

            captured_output: self.captured_output.borrow().clone()
        })
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use malachite::Integer;

    use crate::object::*;
    use crate::context::*;
    use crate::types::*;

    #[test]
    fn compilation_and_execution() {
        let mut ctx = standard_ctx();

        let code_str = "
            fn test(a: Int) -> Int {
                if 0 < a {
                    return test(a - 1) + a;
                }

                print(*a);
                print(0 < a);

                return 0;
            }

            let a = test(100);
        ";

        ctx.parse_and_execute_ryna_module(&code_str.into()).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(5050)));
    }

    #[test]
    fn variable_definition() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let v_0 = 0;
            let v_1: Bool = true;
            let v_2: String = \"test\";
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(0)));
        assert_eq!(ctx.variables[1], Object::new(true));
        assert_eq!(ctx.variables[2], Object::new("test".to_string()));
    }

    #[test]
    fn operations_and_functions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let v_0 = !true;
            let v_1 = 3 + 4;
            let v_2: Int = 2;

            inc(v_2);
            inc(v_2);
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(false));
        assert_eq!(ctx.variables[1], Object::new(Integer::from(7)));
        assert_eq!(ctx.variables[2], Object::new(Integer::from(4)));
    }

    #[test]
    fn flow_control() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let array: Array<Int> = arr<Int>();
            array.push<Int>(5);

            let iter: ArrayIterator<Int, @Int> = array.iterator<Int>();
            let ended_1: Bool = iter.is_consumed();
            
            let elem: @Int = iter.next<Int>();
            let ended_2: Bool = iter.is_consumed();

            let array_2: Array<Int> = arr<Int>();
            array_2.push<Int>(0);
            array_2.push<Int>(2);
            array_2.push<Int>(4);
            array_2.push<Int>(6);
            array_2.push<Int>(8);

            let sum: Int = 0;

            for i in array_2 {
                sum = sum + i;
            }

            let array_3: Array<Int> = arr<Int>();
            array_3.push<Int>(0);
            array_3.push<Int>(1);
            array_3.push<Int>(2);
            array_3.push<Int>(3);
            array_3.push<Int>(4);
            array_3.push<Int>(5);
            array_3.push<Int>(6);
            array_3.push<Int>(7);
            array_3.push<Int>(8);

            let sum_2: Int = 0;

            for i in array_3 {
                if i % 2 != 0 {
                    sum_2 = sum_2 + i;
                }
            }
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::arr(vec!(Object::new(Integer::from(5))), INT));
        assert_eq!(ctx.variables[1], Object::arr_it(INT, Type::MutRef(Box::new(INT)), ctx.variables[0].inner.clone(), 1));
        assert_eq!(ctx.variables[2], Object::new(false));
        assert_eq!(ctx.variables[3], Object::new(Integer::from(5)).get_mut());
        assert_eq!(ctx.variables[4], Object::new(true));
        assert_eq!(ctx.variables[5], Object::arr(vec!(
            Object::new(Integer::from(0)),
            Object::new(Integer::from(2)),
            Object::new(Integer::from(4)),
            Object::new(Integer::from(6)),
            Object::new(Integer::from(8)),
        ), INT));
        assert_eq!(ctx.variables[6], Object::new(Integer::from(20)));

        if let Type::Template(..) = ctx.variables[8].get_type() {
            assert_eq!(ctx.variables[8], Object::arr(vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(1)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(3)),
                Object::new(Integer::from(4)),
                Object::new(Integer::from(5)),
                Object::new(Integer::from(6)),
                Object::new(Integer::from(7)),
                Object::new(Integer::from(8)),
            ), INT));
            assert_eq!(ctx.variables[9], Object::new(Integer::from(16)));

        } else {
            assert_eq!(ctx.variables[9], Object::arr(vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(1)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(3)),
                Object::new(Integer::from(4)),
                Object::new(Integer::from(5)),
                Object::new(Integer::from(6)),
                Object::new(Integer::from(7)),
                Object::new(Integer::from(8)),
            ), INT));
            assert_eq!(ctx.variables[8], Object::new(Integer::from(16)));
        }

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn is_prime(n: @Int) -> Bool {
                if n <= 1 {
                    return false;
                }
                
                let i: Int = 1;

                while i < n - 1 {
                    i = i + 1;

                    if n % i == 0 {
                        return false;
                    }
                }

                return true;
            }

            let array: Array<Int> = arr<Int>();
            let it: Int = 0;

            while it < 50 {
                if is_prime(it) {
                    array.push<Int>(it.deref<Int>());
                }

                it = it + 1;
            }
        ".to_string();
        
        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::arr(vec!(
            Object::new(Integer::from(2)),
            Object::new(Integer::from(3)),
            Object::new(Integer::from(5)),
            Object::new(Integer::from(7)),
            Object::new(Integer::from(11)),
            Object::new(Integer::from(13)),
            Object::new(Integer::from(17)),
            Object::new(Integer::from(19)),
            Object::new(Integer::from(23)),
            Object::new(Integer::from(29)),
            Object::new(Integer::from(31)),
            Object::new(Integer::from(37)),
            Object::new(Integer::from(41)),
            Object::new(Integer::from(43)),
            Object::new(Integer::from(47)),
        ), INT));
    }

    #[test]
    fn operator_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary prefix op \"~\" (201);

            op ~(arg: @Int) -> @Int {
                return arg;
            }

            unary postfix op \"¡\" (301);

            op (arg: @Int)¡ -> @Int {
                return arg;
            }

            let a: Int = 3;
            let b: @Int = ~a¡;

            binary op \"·\" (401);
            binary op \"$\" (501);
            binary op \"@\" (601);

            op (a: @Int) · (b: @Int) -> Int {
                return a + b;
            }

            let c: Int = a · b;

            nary op from \"`\" to \"´\" (701);

            op (a: @Int)`b: @Int, c: @Int´ -> Int {
                return a + b + ~c;
            }

            let d = a`b, c´;
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();
    }

    #[test]
    fn function_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test_1() -> Int {
                return 5;
            }
        
            fn test_2() -> @Int {
                let res: Int = 0;

                return res;
            }
        
            fn test_3() -> @String {
                let res: String = \"\";

                res = \"Hello\";

                return res;
            }
        
            fn test_4() -> @Int {
                let res: Int = test_1() + test_1();

                return res;
            }
        
            fn test_5(a: Int, b: Int) -> @Int {
                let res: Int = a + b;

                return res;
            }
        
            fn test_6(a: Int) -> Int | @Int {
                if true {
                    return a;

                } else {
                    return 0;
                }
            }
        
            fn test_7(a: Int) -> Int {
                if 0 < a {
                    return test_7(a - 1) + a;
                }

                return 0;
            }
        
            fn test_8(a: Int) -> Int {
                if a == 1 {
                    return 1;

                } else if a % 2 == 0 {
                    return test_8(a / 2) + 1;

                } else {
                    return test_8(a * 3 + 1) + 1;
                }
            }

            let v_1 = test_1();
            let v_2 = test_2();
            let v_3 = test_3();
            let v_4 = test_4();
            let v_5 = test_5(2, 4);
            let v_6 = test_6(9);
            let v_7 = test_7(10);
            let v_8 = test_8(100);
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(5)));
        assert_eq!(ctx.variables[1], Object::new(Integer::from(0)).get_mut());
        assert_eq!(ctx.variables[2], Object::new("Hello".to_string()).get_mut());
        assert_eq!(ctx.variables[3], Object::new(Integer::from(10)).get_mut());
        assert_eq!(ctx.variables[4], Object::new(Integer::from(6)).get_mut());
        assert_eq!(ctx.variables[5], Object::new(Integer::from(9)).get_mut());
        assert_eq!(ctx.variables[6], Object::new(Integer::from(55)));
        assert_eq!(ctx.variables[7], Object::new(Integer::from(26)));
    }

    #[test]
    fn templated_functions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn<T> test() -> Array<'T> {
                return arr<'T>();
            }

            let a = test<Int>();
            let b = test<String>();
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::arr(vec!(), INT));
        assert_eq!(ctx.variables[1], Object::arr(vec!(), STR));

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn<T> sum(a: 'T, b: 'T) -> 'T {
                return a + b;
            }

            let a = sum<Int>(5, 6);
            let b = sum<String>(\"test\", \"tset\");
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(11)));
        assert_eq!(ctx.variables[1], Object::new("testtset".to_string()));
    }

    #[test]
    fn templated_operations() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary prefix op \"~\" (151);

            op<T> ~(a: 'T) -> 'T {
                return a + a;
            }
            
            let a = ~<Int>7;
            let b = ~<String>\"test\";
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(14)));
        assert_eq!(ctx.variables[1], Object::new("testtest".to_string()));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary postfix op \"~\" (151);

            op<T> (a: 'T)~ -> 'T {
                return a + a;
            }
            
            let a = 7<Int>~;
            let b = \"test\"<String>~;
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(14)));
        assert_eq!(ctx.variables[1], Object::new("testtest".to_string()));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            binary op \"@\" (151);

            op<T> (a: 'T) @ (b: 'T) -> 'T {
                return a + b;
            }

            let a = 5 <Int>@ 8;
            let b = \"test\" <String>@ \"tset\";
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(13)));
        assert_eq!(ctx.variables[1], Object::new("testtset".to_string()));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            nary op from \"`\" to \"´\" (151);

            op<T, G> (a: 'T)`b: 'G´ -> 'T {
                return a + b;
            }

            let a = 2<Int, Int>`9´;
            let b = \"test\"<String, String>`\"tttt\"´;
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(11)));
        assert_eq!(ctx.variables[1], Object::new("testtttt".to_string()));
    }

    #[test]
    fn custom_iterators() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        fn iterator(it: Int) -> Int {
            return it.deref<Int>();
        }

        fn next(it: @Int) -> Int {
            it.inc();
            return it.deref<Int>();
        }

        fn is_consumed(it: @Int) -> Bool {
            return it >= 10;
        }

        implement Iterable<Int, Int> for Int;

        let sum: Int = 0;

        for i in 0 {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(55)));

        let mut ctx = standard_ctx();
        
        let code_str = "
        class Range {
            start: Int;
            current: Int;
            end: Int;
        }

        fn iterator(it: Range) -> Range {
            return it.deref<Range>();
        }

        fn next(it: @Range) -> Int {
            let curr: @Int = it.current;
            curr.inc();

            return curr.deref<Int>();
        }

        fn is_consumed(it: @Range) -> Bool {
            return it.current >= it.end;
        }

        implement Iterable<Range, Int> for Range;

        let sum: Int = 0;

        for i in Range(0, 0, 10) {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::new(Integer::from(55)));
    }

    #[test]
    fn class_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        class Range {
            start: Int;
            current: Int;
            end: Int;
        }

        let r: Range = Range(0, 2, 10);

        let a = r.start;
        let b = r.current;
        let c = r.end;
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        let id = ctx.get_type_id("Range".into()).unwrap();

        assert_eq!(ctx.variables[0], Object::new(TypeInstance {
            id,
            params: vec!(),
            attributes: vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(10))
            )
        }));

        assert_eq!(ctx.variables[1], Object::new(Integer::from(0)).get_mut());
        assert_eq!(ctx.variables[2], Object::new(Integer::from(2)).get_mut());
        assert_eq!(ctx.variables[3], Object::new(Integer::from(10)).get_mut());

        let mut ctx = standard_ctx();
        
        let code_str = "
        class Option<T> {
            present: Bool;
            obj: 'T;
        }

        let a: Option<Int> = Option<Int>(true, 5);

        let b = a.present;
        let c = a.obj;
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        let id = ctx.get_type_id("Option".into()).unwrap();

        assert_eq!(ctx.variables[0], Object::new(TypeInstance {
            id,
            params: vec!(INT),
            attributes: vec!(
                Object::new(true),
                Object::new(Integer::from(5))
            )
        }));

        assert_eq!(ctx.variables[1], Object::new(true).get_mut());
        assert_eq!(ctx.variables[2], Object::new(Integer::from(5)).get_mut());
    }

    #[test]
    fn lambda_definition() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        let a: (Int) => Int = (n: Int) -> Int n * 2;

        let b = a<Int, Int>(4);

        let c: (Int, Bool) => Int = (n: Int, b: Bool) -> Int {
            if *<Bool>b {
                return n + 2;
            }

            return n + 1;
        };

        let d = c<Int, Bool, Int>(5, true);
        let e = c<Int, Bool, Int>(5, false);
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[1], Object::new(Integer::from(8)));
        assert_eq!(ctx.variables[3], Object::new(Integer::from(7)));
        assert_eq!(ctx.variables[4], Object::new(Integer::from(6)));

        let mut ctx = standard_ctx();
        
        let code_str = "
        let apply: (Int, @(Int => Int)) => Int = (n: Int, f: @(Int => Int)) -> Int f<Int, Int>(*<Int>n);
        let f: (Int) => Int = (n: Int) -> Int n * n;

        let a = apply<Int, @(Int => Int), Int>(5, f);
        ".to_string();

        ctx.parse_and_execute_ryna_module(&code_str).unwrap();

        assert_eq!(ctx.variables[2], Object::new(Integer::from(25)));
    }
}