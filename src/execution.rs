use std::collections::HashMap;
use std::time::Instant;

use colored::Colorize;
use rustc_hash::FxHashMap;
use serde::Serialize;

use crate::config::{precompile_nessa_module_with_config, read_compiled_cache, save_compiled_cache, compute_project_hash};
use crate::nessa_warning;
use crate::number::{Integer, ONE};
use crate::types::Type;
use crate::object::{NessaTuple, Object, TypeInstance};
use crate::context::NessaContext;
use crate::operations::Operator;
use crate::compilation::{CompiledNessaExpr, NessaError};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    pub fn parse_and_execute_nessa_module(&mut self, code: &String) -> Result<ExecutionInfo, NessaError> {
        let mut compiled_code = self.parse_and_compile(code)?;

        if self.optimize {
            self.optimize_instructions(&mut compiled_code);
        }

        for (idx, i) in compiled_code.iter().enumerate() {
            println!("{:<3} {}", idx, i.to_string(self));
        }
        
        self.execute_compiled_code::<false>(&compiled_code.into_iter().map(|i| i.instruction).collect::<Vec<_>>())
    }

    pub fn parse_and_execute_nessa_project_inner<const DEBUG: bool>(path: String, macro_code: Option<String>, force_recompile: bool, optimize: bool, program_input: &[String]) -> Result<ExecutionInfo, NessaError> {
        let combined_hash;
        let all_modules;
        let file_cache;

        let no_macro = macro_code.is_none();

        match compute_project_hash(&path, macro_code) {
            Ok((hash, all_mods, files)) => {
                combined_hash = hash.clone();
                all_modules = all_mods;
                file_cache = files;

                if !force_recompile {
                    if let Some(mut code) = read_compiled_cache(&path) {
                        if hash == code.hash {
                            if DEBUG {
                                nessa_warning!(
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

        match precompile_nessa_module_with_config(&path, all_modules, file_cache, optimize) {
            Ok((mut ctx, code)) => match ctx.compiled_form(&code) {
                Ok(mut instr) => {                    
                    if no_macro {
                        let ser_module = ctx.get_serializable_module(combined_hash, &instr);

                        if let Err(err) = save_compiled_cache(&path, &ser_module) {
                            err.emit();
                        }
                    }

                    ctx.program_input = program_input.to_vec();

                    if optimize {
                        ctx.optimize_instructions(&mut instr);
                    }

                    ctx.execute_compiled_code::<DEBUG>(&instr.into_iter().map(|i| i.instruction).collect::<Vec<_>>())
                },

                Err(err) => err.emit(),
            },

            Err(err) => err.emit(),
        }
    }

    pub fn parse_and_execute_nessa_project<const DEBUG: bool>(path: String, force_recompile: bool, optimize: bool, program_input: &[String]) -> Result<ExecutionInfo, NessaError> {
        Self::parse_and_execute_nessa_project_inner::<DEBUG>(path, None, force_recompile, optimize, program_input)
    }
}

#[derive(Serialize)]
pub struct ProfilingInfo {
    pub instr_count: HashMap<&'static str, usize>,
    pub instr_time: HashMap<&'static str, u128>,
    pub loc_count: HashMap<i32, usize>,
    pub loc_time: HashMap<i32, u128>,
    pub ranges: FxHashMap<String, (usize, usize)>,
    pub fn_count: HashMap<usize, usize>,
    pub fn_time: HashMap<String, (u128, u128, f64)>,
    pub total_time: u128
}

pub struct ExecutionInfo {
    pub profiling_info: Option<ProfilingInfo>,
    pub captured_output: String
}


impl ProfilingInfo {
    pub fn process(&mut self) {
        self.total_time = self.loc_time.values().copied().sum();

        self.fn_time = self.ranges.iter().map(|(n, (f, t))| {
            let time = (*f..*t).map(|j| *self.loc_time.get(&(j as i32)).unwrap_or(&0)).sum();
            let count = *self.fn_count.get(f).unwrap_or(&1);
            (n.clone(), (time, (time as f64 / count as f64) as u128, time as f64 / self.total_time as f64))
        }).collect();
    }
}

impl NessaContext {
    pub fn execute_compiled_code<const DEBUG: bool>(&mut self, program: &[CompiledNessaExpr]) -> Result<ExecutionInfo, NessaError> {
        use CompiledNessaExpr::*;

        let mut ip: i32 = 0;
        let mut offset: usize = 0;
        
        let mut call_stack: Vec<(i32, usize, i32)> = Vec::with_capacity(1000);
        let mut stack: Vec<Object> = Vec::with_capacity(1000);

        let mut instr_count = HashMap::<&str, usize>::new();
        let mut instr_time = HashMap::<&str, u128>::new();
        let mut loc_count = HashMap::<i32, usize>::new();
        let mut loc_time = HashMap::<i32, u128>::new();
        let mut fn_count = HashMap::new();

        macro_rules! unary_op {
            ($name: expr, $a: ident, $get_a: ident, $t: ty, $op: expr) => {
                nessa_instruction!($name, {
                    let _a = stack.pop().unwrap();
                    let $a = &*_a.$get_a::<$t>();
    
                    stack.push(Object::new($op));
                    ip += 1;
                })
            };
        }

        macro_rules! bin_op {
            ($name: expr, $a: ident, $b: ident, $get_a: ident, $get_b: ident, $t: ty, $op: expr) => {
                nessa_instruction!($name, {
                    let _a = stack.pop().unwrap();
                    let _b = stack.pop().unwrap();
                    let $a = &*_a.$get_a::<$t>();
                    let $b = &*_b.$get_b::<$t>();
                        
                    stack.push(Object::new($op));
                    ip += 1;
                })
            };
        }

        macro_rules! nessa_instruction {
            ($name: expr, $expr: expr) => {
                if DEBUG {                    
                    let now = Instant::now();

                    $expr

                    let elapsed = now.elapsed().as_nanos();

                    *instr_time.entry($name).or_default() += elapsed;
                    *loc_time.entry(ip).or_default() += elapsed;

                    *instr_count.entry($name).or_default() += 1;
                    *loc_count.entry(ip).or_default() += 1;

                } else {
                    $expr
                }
            };
        }

        call_stack.push((0, 0, -1));

        loop {
            match &program[ip as usize] {
                Empty => nessa_instruction!("Empty", {
                    stack.push(Object::empty());
                    ip += 1;
                }),

                Bool(obj) => nessa_instruction!("Bool", {
                    stack.push(Object::new(*obj));
                    ip += 1;
                }),

                Float(obj) => nessa_instruction!("Float", {
                    stack.push(Object::new(*obj));
                    ip += 1;
                }),

                Int(obj) => nessa_instruction!("Int", {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                }),

                Str(obj) => nessa_instruction!("Str", {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                }),

                Array(length, t) => nessa_instruction!("Array", {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::arr(args, t.clone()));

                    ip += 1;
                }),

                Lambda(pos, args, ret) => nessa_instruction!("Lambda", {
                    stack.push(Object::lambda(*pos, args.clone(), ret.clone()));
                    ip += 1;
                }),

                Construct(id, length, ts) => nessa_instruction!("Construct", {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::new(TypeInstance {
                        id: *id,
                        params: ts.clone(),
                        attributes: args,
                    }));

                    ip += 1;
                }),

                AttributeMove(idx) => nessa_instruction!("AttributeMove", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.get::<TypeInstance>().attributes[*idx].move_contents_if_ref());
                    ip += 1;
                }),

                AttributeRef(idx) => nessa_instruction!("AttributeRef", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_ref());
                    ip += 1;
                }),

                AttributeMut(idx) => nessa_instruction!("AttributeMut", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_mut());
                    ip += 1;
                }),

                AttributeCopy(idx) => nessa_instruction!("AttributeCopy", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].deref_deep_clone());
                    ip += 1;
                }),

                AttributeDeref(idx) => nessa_instruction!("AttributeDeref", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].deref_if_ref());
                    ip += 1;
                }),

                Tuple(length) => nessa_instruction!("Tuple", {     
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect::<Vec<_>>();
                    let types = args.iter().map(|i| i.get_type()).collect::<Vec<_>>();

                    stack.push(Object::tuple(args, types));

                    ip += 1;
                }),

                TupleElemMove(idx) => nessa_instruction!("TupleElemMove", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.get::<NessaTuple>().elements[*idx].move_contents_if_ref());
                    ip += 1;
                }),

                TupleElemRef(idx) => nessa_instruction!("TupleElemRef", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<NessaTuple>().elements[*idx].get_ref());
                    ip += 1;
                }),

                TupleElemMut(idx) => nessa_instruction!("TupleElemMut", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<NessaTuple>().elements[*idx].get_mut());
                    ip += 1;
                }),

                TupleElemCopy(idx) => nessa_instruction!("TupleElemCopy", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<NessaTuple>().elements[*idx].deref_deep_clone());
                    ip += 1;
                }),

                TupleElemDeref(idx) => nessa_instruction!("TupleElemDeref", {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<NessaTuple>().elements[*idx].deref_if_ref());
                    ip += 1;
                }),

                StoreVariable(id) => nessa_instruction!("StoreVariable", {
                    let idx = call_stack.len() - 1;
                    let l = &mut call_stack[idx].2;
                    *l = (*l).max(*id as i32);
                    
                    self.variables[*id + offset] = stack.pop().unwrap();
                    ip += 1;
                }),

                GetVariable(id) => nessa_instruction!("GetVariable", {
                    stack.push(self.variables[*id + offset].get_mut());
                    ip += 1;
                }),

                RefVariable(id) => nessa_instruction!("RefVariable", {
                    stack.push(self.variables[*id + offset].get_ref());
                    ip += 1;
                }),

                DerefVariable(id) => nessa_instruction!("DerefVariable", {
                    stack.push(self.variables[*id + offset].deref_if_ref());
                    ip += 1;
                }),

                CopyVariable(id) => nessa_instruction!("CopyVariable", {
                    stack.push(self.variables[*id + offset].deref_deep_clone());
                    ip += 1;
                }),

                MoveVariable(id) => nessa_instruction!("MoveVariable", {
                    stack.push(self.variables[*id + offset].move_contents_if_ref());
                    ip += 1;
                }),

                Assign => nessa_instruction!("Assign", {
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();

                    if let Err(msg) = a.assign(b, self) {
                        return Err(NessaError::execution_error(msg));
                    }

                    ip += 1;
                }),

                Drop => nessa_instruction!("Drop", {
                    stack.pop().unwrap();
                    ip += 1;
                }),

                Jump(to) => ip = *to as i32,
                RelativeJump(to) => ip += *to,
                RelativeJumpIfFalse(to, false) => nessa_instruction!("RelativeJumpIfFalse", {
                    if !*stack.pop().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),
                RelativeJumpIfTrue(to, false) => nessa_instruction!("RelativeJumpIfTrue", {
                    if *stack.pop().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),
                RelativeJumpIfFalse(to, true) => nessa_instruction!("RelativeJumpIfFalse", {
                    if !*stack.last().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),
                RelativeJumpIfTrue(to, true) => nessa_instruction!("RelativeJumpIfTrue", {
                    if *stack.last().unwrap().get::<bool>() {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                }),
                Call(to) => nessa_instruction!("Call", {
                    call_stack.push((ip + 1, offset, -1));
                    ip = *to as i32;
                    offset += (call_stack[call_stack.len() - 2].2 + 1) as usize;

                    if DEBUG {
                        *fn_count.entry(*to).or_default() += 1;
                    }
                }),
                Return => nessa_instruction!("Return", {
                    let (prev_ip, prev_offset, _) = call_stack.pop().unwrap();

                    ip = prev_ip;
                    offset = prev_offset;
                }), 

                NativeFunctionCall(func_id, ov_id, type_args) => nessa_instruction!("NativeFunctionCall", {
                    if let (_, Type::And(v), r, Some(f)) = &self.functions[*func_id].overloads[*ov_id] {
                        let mut args = Vec::with_capacity(v.len());

                        for _ in v {
                            args.push(stack.pop().unwrap());
                        }

                        match f(type_args, r, args, self) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(NessaError::execution_error(msg))
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                NativeFunctionCallNoRet(func_id, ov_id, type_args) => nessa_instruction!("NativeFunctionCallNoRet", {
                    if let (_, Type::And(v), r, Some(f)) = &self.functions[*func_id].overloads[*ov_id] {
                        let mut args = Vec::with_capacity(v.len());

                        for _ in v {
                            args.push(stack.pop().unwrap());
                        }

                        if let Err(msg) = f(type_args, r, args, self) {
                            return Err(NessaError::execution_error(msg));
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                UnaryOperatorCall(op_id, ov_id, type_args) => nessa_instruction!("UnaryOperatorCall", {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        match ov.3.unwrap()(type_args, &ov.2, obj) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(NessaError::execution_error(msg))
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                UnaryOperatorCallNoRet(op_id, ov_id, type_args) => nessa_instruction!("UnaryOperatorCallNoRet", {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        if let Err(msg) = ov.3.unwrap()(type_args, &ov.2, obj) {
                            return Err(NessaError::execution_error(msg));
                        };

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                BinaryOperatorCall(op_id, ov_id, type_args) => nessa_instruction!("BinaryOperatorCall", {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = stack.pop().unwrap();
                        let b = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        match ov.3.unwrap()(type_args, &ov.2, a, b, self) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(NessaError::execution_error(msg))
                        };
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                BinaryOperatorCallNoRet(op_id, ov_id, type_args) => nessa_instruction!("BinaryOperatorCallNoRet", {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = stack.pop().unwrap();
                        let b = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        if let Err(msg) = ov.3.unwrap()(type_args, &ov.2, a, b, self) {
                            return Err(NessaError::execution_error(msg));
                        };
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }),

                NaryOperatorCall(op_id, ov_id, type_args) => nessa_instruction!("NaryOperatorCall", {
                    if let Operator::Nary{operations, ..} = &self.nary_ops[*op_id] {
                        if let (_, _, r, Some(f)) = &operations[*ov_id] {
                            let res = f((&mut stack, &mut offset, &mut call_stack, &mut ip), type_args, r);

                            if let Err(msg) = res {
                                return Err(NessaError::execution_error(msg));
                            }

                        } else {
                            unreachable!();
                        }

                    } else {
                        unreachable!();
                    }
                }),

                ToFloat => unary_op!("ToFloat", a, get, Integer, a.to_f64()),

                Ref => nessa_instruction!("Ref", {
                    let a = stack.pop().unwrap();
                    stack.push(a.get_ref_nostack());
                    ip += 1;
                }),

                Mut => nessa_instruction!("Mut", {
                    let a = stack.pop().unwrap();
                    stack.push(a.get_mut_nostack());
                    ip += 1;
                }),

                Copy => nessa_instruction!("Copy", {
                    let a = stack.pop().unwrap();
                    stack.push(a.deref_obj().deep_clone());
                    ip += 1;
                }),

                Deref => nessa_instruction!("Deref", {
                    let a = stack.pop().unwrap();
                    stack.push(a.deref_obj());
                    ip += 1;
                }),

                Demut => nessa_instruction!("Demut", {
                    let a = stack.pop().unwrap();
                    stack.push(a.get_ref());
                    ip += 1;
                }),

                Move => nessa_instruction!("Move", {
                    let a = stack.pop().unwrap();
                    stack.push(a.move_contents());
                    ip += 1;
                }),

                Inc => nessa_instruction!("Inc", {
                    let a = stack.pop().unwrap();
                    *a.deref::<Integer>() += &*ONE;
                    ip += 1;
                }),

                Dec => nessa_instruction!("Dec", {
                    let a = stack.pop().unwrap();
                    *a.deref::<Integer>() -= &*ONE;
                    ip += 1;
                }),

                Addi => bin_op!("Addi", a, b, get, get, Integer, a + b),
                Subi => bin_op!("Subi", a, b, get, get, Integer, a - b),
                Muli => bin_op!("Muli", a, b, get, get, Integer, a * b),
                Divi => bin_op!("Divi", a, b, get, get, Integer, a / b),
                Modi => bin_op!("Modi", a, b, get, get, Integer, a % b),
                Negi => unary_op!("Negi", a, get, Integer, Integer::new(!a.negative, a.limbs.clone())),
                Addf => bin_op!("Addf", a, b, get, get, f64, a + b),
                Subf => bin_op!("Subf", a, b, get, get, f64, a - b),
                Mulf => bin_op!("Mulf", a, b, get, get, f64, a * b),
                Divf => bin_op!("Divf", a, b, get, get, f64, a / b),
                Modf => bin_op!("Modf", a, b, get, get, f64, a % b),
                Negf => unary_op!("Negf", a, get, f64, -a),

                NotB => unary_op!("NotB", a, get, Integer, !a),
                AndB => bin_op!("AndB", a, b, get, get, Integer, a & b),
                OrB => bin_op!("OrB", a, b, get, get, Integer, a | b),
                XorB => bin_op!("XorB", a, b, get, get, Integer, a ^ b),
                Shl => bin_op!("Shl", a, b, get, get, Integer, if b.negative { a >> b.limbs[0] } else { a << b.limbs[0] }),
                Shr => bin_op!("Shr", a, b, get, get, Integer, if b.negative { a << b.limbs[0] } else { a >> b.limbs[0] }),

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

                Not => unary_op!("Not", a, get, bool, !a),
                Or => bin_op!("Or", a, b, get, get, bool, *a || *b),
                And => bin_op!("And", a, b, get, get, bool, *a && *b),
                Xor => bin_op!("Xor", a, b, get, get, bool, *a ^ *b),
                Nor => bin_op!("Nor", a, b, get, get, bool, !(*a || *b)),
                Nand => bin_op!("Nand", a, b, get, get, bool, !(*a && *b)),

                Halt => break,
            }
        }

        Ok(ExecutionInfo {
            profiling_info: if DEBUG {
                let mut ex = ProfilingInfo { 
                    instr_count, instr_time, loc_count, loc_time, fn_count,
                    ranges: self.cache.ranges.inner_clone(),
                    fn_time: HashMap::new(),
                    total_time: 0
                };

                ex.process();

                Some(ex)

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
    use crate::number::*;
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

        ctx.parse_and_execute_nessa_module(&code_str.into()).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

            let iter: ArrayIterator<@Int> = array.iterator<Int>();
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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Object::arr(vec!(Object::new(Integer::from(5))), INT));
        assert_eq!(ctx.variables[1], Object::arr_it(Type::MutRef(Box::new(INT)), ctx.variables[0].inner.clone(), 1));
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

        if let Type::Template(..) = ctx.variables[7].get_type() {
            assert_eq!(ctx.variables[7], Object::arr(vec!(
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

        } else {
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
            assert_eq!(ctx.variables[7], Object::new(Integer::from(16)));
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
        
        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();
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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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
            let curr: @Int = it.current();
            curr.inc();

            return curr.deref<Int>();
        }

        fn is_consumed(it: @Range) -> Bool {
            return it.current() >= it.end();
        }

        implement Iterable<Range, Int> for Range;

        let sum: Int = 0;

        for i in Range(0, 0, 10) {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        let a = r.start();
        let b = r.current();
        let c = r.end();
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        let b = a.present<Int>();
        let c = a.obj<Int>();
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[1], Object::new(Integer::from(8)));
        assert_eq!(ctx.variables[3], Object::new(Integer::from(7)));
        assert_eq!(ctx.variables[4], Object::new(Integer::from(6)));

        let mut ctx = standard_ctx();
        
        let code_str = "
        let apply: (Int, @(Int => Int)) => Int = (n: Int, f: @(Int => Int)) -> Int f<Int, Int>(*<Int>n);
        let f: (Int) => Int = (n: Int) -> Int n * n;

        let a = apply<Int, @(Int => Int), Int>(5, f);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[2], Object::new(Integer::from(25)));
    }
}