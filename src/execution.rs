use std::collections::HashMap;

use crate::config::{precompile_nessa_module_with_config, read_compiled_cache, save_compiled_cache, compute_project_hash};
use crate::number::Integer;
use crate::types::{Type, BOOL_ID, TypeInstance};
use crate::object::Object;
use crate::context::NessaContext;
use crate::operations::Operator;
use crate::compilation::{CompiledNessaExpr, NessaError};

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    pub fn parse_and_execute_nessa_module(&mut self, code: &String) -> Result<(), NessaError> {
        let compiled_code = self.parse_and_compile(code)?;

        for (idx, i) in compiled_code.iter().enumerate() {
            println!("{:<3} {}", idx, i.to_string(self));
        }
        
        return self.execute_compiled_code(&compiled_code.into_iter().map(|i| i.instruction).collect());
    }

    pub fn parse_and_execute_nessa_project(&mut self, path: String, force_recompile: bool) -> Result<(), NessaError> {
        let mut combined_hash = "".into();
        let mut all_modules = HashMap::new();
        let mut file_cache = HashMap::new();

        match compute_project_hash(&path) {
            Ok((hash, all_mods, files)) => {
                combined_hash = hash.clone();
                all_modules = all_mods;
                file_cache = files;

                if !force_recompile {
                    if let Some(mut code) = read_compiled_cache(&path) {
                        if hash == code.hash {
                            code.execute();
        
                            return Ok(());    
                        }    
                    }
                }
            }

            Err(err) => err.emit()
        }

        match precompile_nessa_module_with_config(&path, all_modules, file_cache) {
            Ok((mut ctx, code)) => match ctx.compiled_form(&code) {
                Ok(instr) => {
                    let ser_module = ctx.get_serializable_module(combined_hash, &instr);
                    
                    if let Err(err) = save_compiled_cache(&path, &ser_module) {
                        err.emit();
                    }

                    match self.execute_compiled_code(&instr.into_iter().map(|i| i.instruction).collect()) {
                        Ok(_) => {},
                        Err(err) => err.emit(),
                    }
                },

                Err(err) => err.emit(),
            },

            Err(err) => err.emit(),
        }
        
        return Ok(());
    }
}

impl NessaContext {
    pub fn execute_compiled_code(&mut self, program: &Vec<CompiledNessaExpr>) -> Result<(), NessaError> {
        use CompiledNessaExpr::*;

        fn check_bool_obj(obj: Object) -> bool {
            if let Type::Basic(BOOL_ID) = obj.get_type() {
                return *obj.get::<bool>();
            }

            unreachable!();
        }

        let mut ip: i32 = 0;
        let mut offset: usize = 0;
        
        let mut call_stack: Vec<(i32, usize, i32)> = Vec::with_capacity(1000);
        let mut stack: Vec<Object> = Vec::with_capacity(1000);

        macro_rules! unary_op {
            ($a: ident, $get_a: ident, $t: ty, $op: expr) => {
                {
                    let _a = stack.pop().unwrap();
                    let $a = &*_a.$get_a::<$t>();
    
                    stack.push(Object::new($op));
                    ip += 1;
                }
            };
        }

        macro_rules! bin_op {
            ($a: ident, $b: ident, $get_a: ident, $get_b: ident, $t: ty, $op: expr) => {
                {
                    let _a = stack.pop().unwrap();
                    let _b = stack.pop().unwrap();
                    let $a = &*_a.$get_a::<$t>();
                    let $b = &*_b.$get_b::<$t>();
                        
                    stack.push(Object::new($op));
                    ip += 1;
                }
            };
        }

        call_stack.push((0, 0, -1));

        loop {
            match &program[ip as usize] {
                Empty => {
                    stack.push(Object::empty());
                    ip += 1;
                },

                Bool(obj) => {
                    stack.push(Object::new(*obj));
                    ip += 1;
                },

                Float(obj) => {
                    stack.push(Object::new(*obj));
                    ip += 1;
                },

                Int(obj) => {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                },

                Str(obj) => {
                    stack.push(Object::new(obj.clone()));
                    ip += 1;
                },

                Array(length, t) => {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::new((t.clone(), args)));

                    ip += 1;
                },

                Lambda(pos, args, ret) => {
                    stack.push(Object::new((*pos, args.clone(), ret.clone())));
                    ip += 1;
                },

                Construct(id, length, ts) => {
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();

                    stack.push(Object::new(TypeInstance {
                        id: *id,
                        params: ts.clone(),
                        attributes: args,
                    }));

                    ip += 1;
                }

                Attribute(idx) => {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.get::<TypeInstance>().attributes[*idx].clone());
                    ip += 1;
                },

                AttributeRef(idx) => {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_ref_obj());
                    ip += 1;
                },

                AttributeMut(idx) => {
                    let elem = stack.pop().unwrap();
                    stack.push(elem.deref::<TypeInstance>().attributes[*idx].get_ref_mut_obj());
                    ip += 1;
                },

                Tuple(length) => {     
                    let start_idx = stack.len() - length;
                    let args = stack.drain(start_idx..).rev().collect();
                    
                    stack.push(Object::new(crate::types::Tuple::new(args)));

                    ip += 1;
                },

                StoreVariable(id) => {
                    let idx = call_stack.len() - 1;
                    let l = &mut call_stack[idx].2;
                    *l = (*l).max(*id as i32);
                    
                    self.variables[*id + offset] = Some(stack.pop().unwrap());
                    ip += 1;
                },

                GetVariable(id) => {
                    stack.push(self.variables[*id + offset].as_ref().unwrap().get_ref_mut_obj());
                    ip += 1;
                },

                Drop => {
                    stack.pop().unwrap();
                    ip += 1;
                },

                Jump(to) => ip = *to as i32,
                RelativeJump(to) => ip += *to,
                RelativeJumpIfFalse(to) => {
                    if !check_bool_obj(stack.pop().unwrap()) {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                },
                RelativeJumpIfTrue(to) => {
                    if check_bool_obj(stack.pop().unwrap()) {
                        ip += *to as i32;

                    } else {
                        ip += 1;
                    }
                },
                Call(to) => {
                    call_stack.push((ip + 1, offset, -1));
                    ip = *to as i32;
                    offset += (call_stack[call_stack.len() - 2].2 + 1) as usize;
                },
                Return => {
                    let (prev_ip, prev_offset, _) = call_stack.pop().unwrap();

                    ip = prev_ip;
                    offset = prev_offset;
                }, 

                NativeFunctionCall(func_id, ov_id, type_args) => {
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
                }

                UnaryOperatorCall(op_id, ov_id, type_args) => {
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
                }

                BinaryOperatorCall(op_id, ov_id, type_args) => {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = stack.pop().unwrap();
                        let b = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        match ov.3.unwrap()(type_args, &ov.2, a, b) {
                            Ok(obj) => stack.push(obj),
                            Err(msg) => return Err(NessaError::execution_error(msg))
                        };
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                NaryOperatorCall(op_id, ov_id, type_args) => {
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
                },

                ToFloat => unary_op!(a, get, Integer, a.to_f64()),

                Copy => {
                    let a = stack.pop().unwrap();
                    stack.push(a.deref_obj().deep_clone());
                    ip += 1;
                },

                Deref => {
                    let a = stack.pop().unwrap();
                    stack.push(a.deref_obj());
                    ip += 1;
                },

                Addi => bin_op!(a, b, get, get, Integer, a + b),
                Subi => bin_op!(a, b, get, get, Integer, a - b),
                Muli => bin_op!(a, b, get, get, Integer, a * b),
                Divi => bin_op!(a, b, get, get, Integer, a / b),
                Modi => bin_op!(a, b, get, get, Integer, a % b),
                Negi => unary_op!(a, get, Integer, Integer::new(!a.negative, a.limbs.clone())),
                Addf => bin_op!(a, b, get, get, f64, a + b),
                Subf => bin_op!(a, b, get, get, f64, a - b),
                Mulf => bin_op!(a, b, get, get, f64, a * b),
                Divf => bin_op!(a, b, get, get, f64, a / b),
                Modf => bin_op!(a, b, get, get, f64, a % b),
                Negf => unary_op!(a, get, f64, -a),

                Lti => bin_op!(a, b, get, get, Integer, a < b),
                Gti => bin_op!(a, b, get, get, Integer, a > b),
                Lteqi => bin_op!(a, b, get, get, Integer, a <= b),
                Gteqi => bin_op!(a, b, get, get, Integer, a >= b),
                Eqi => bin_op!(a, b, get, get, Integer, a == b),
                Neqi => bin_op!(a, b, get, get, Integer, a != b),
                Ltf => bin_op!(a, b, get, get, f64, a < b),
                Gtf => bin_op!(a, b, get, get, f64, a > b),
                Lteqf => bin_op!(a, b, get, get, f64, a <= b),
                Gteqf => bin_op!(a, b, get, get, f64, a >= b),
                Eqf => bin_op!(a, b, get, get, f64, a == b),
                Neqf => bin_op!(a, b, get, get, f64, a != b),

                Not => unary_op!(a, get, bool, !a),
                Or => bin_op!(a, b, get, get, bool, *a || *b),
                And => bin_op!(a, b, get, get, bool, *a && *b),

                Halt => break,

                // _ => todo!()
            }
        }

        return Ok(());
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

                println(a);
                println(0 < a);

                return 0;
            }

            let a = test(100);
        ";

        ctx.parse_and_execute_nessa_module(&code_str.into()).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(5050))));
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(0))));
        assert_eq!(ctx.variables[1], Some(Object::new(true)));
        assert_eq!(ctx.variables[2], Some(Object::new("test".to_string())));
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

        assert_eq!(ctx.variables[0], Some(Object::new(false)));
        assert_eq!(ctx.variables[1], Some(Object::new(Integer::from(7))));
        assert_eq!(ctx.variables[2], Some(Object::new(Integer::from(4))));
    }

    #[test]
    fn flow_control() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let array: Array<Int> = arr<Int>();
            array.push<Int>(5);

            let iter: ArrayIterator<&&Int> = array.iterator<Int>();
            let ended_1: Bool = iter.is_consumed();
            
            let elem: Int = iter.next<Int>();
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

        assert_eq!(ctx.variables[0], Some(Object::new((INT, vec!(Object::new(Integer::from(5)))))));
        assert_eq!(ctx.variables[1], Some(Object::new((Type::MutRef(Box::new(INT)), ctx.variables[0].as_ref().unwrap().get_ref_mut(), 1))));
        assert_eq!(ctx.variables[2], Some(Object::new(false)));
        assert_eq!(ctx.variables[3], Some(Object::new(Integer::from(5))));
        assert_eq!(ctx.variables[4], Some(Object::new(true)));
        assert_eq!(ctx.variables[5], Some(Object::new((INT, vec!(
            Object::new(Integer::from(0)),
            Object::new(Integer::from(2)),
            Object::new(Integer::from(4)),
            Object::new(Integer::from(6)),
            Object::new(Integer::from(8)),
        )))));
        assert_eq!(ctx.variables[6], Some(Object::new(Integer::from(20))));

        if let Type::Template(..) = ctx.variables[7].as_ref().unwrap().get_type() {
            assert_eq!(ctx.variables[7], Some(Object::new((INT, vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(1)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(3)),
                Object::new(Integer::from(4)),
                Object::new(Integer::from(5)),
                Object::new(Integer::from(6)),
                Object::new(Integer::from(7)),
                Object::new(Integer::from(8)),
            )))));
            assert_eq!(ctx.variables[8], Some(Object::new(Integer::from(16))));

        } else {
            assert_eq!(ctx.variables[8], Some(Object::new((INT, vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(1)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(3)),
                Object::new(Integer::from(4)),
                Object::new(Integer::from(5)),
                Object::new(Integer::from(6)),
                Object::new(Integer::from(7)),
                Object::new(Integer::from(8)),
            )))));
            assert_eq!(ctx.variables[7], Some(Object::new(Integer::from(16))));
        }

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn is_prime(n: &&Int) -> Bool {
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

        assert_eq!(ctx.variables[0], Some(Object::new((INT, vec!(
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
        )))));
    }

    #[test]
    fn operator_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary prefix op \"~\" (201);

            op ~(arg: &&Int) -> &&Int {
                return arg;
            }

            unary postfix op \"¡\" (301);

            op (arg: &&Int)¡ -> &&Int {
                return arg;
            }

            let a: Int = 3;
            let b: &&Int = ~a¡;

            binary op \"·\" (401);
            binary op \"$\" (501);
            binary op \"@\" (601);

            op (a: &&Int) · (b: &&Int) -> Int {
                return a + b;
            }

            let c: Int = a · b;

            nary op from \"`\" to \"´\" (701);

            op (a: &&Int)`b: &&Int, c: &&Int´ -> Int {
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
        
            fn test_2() -> &&Int {
                let res: Int = 0;

                return res;
            }
        
            fn test_3() -> &&String {
                let res: String = \"\";

                res = \"Hello\";

                return res;
            }
        
            fn test_4() -> &&Int {
                let res: Int = test_1() + test_1();

                return res;
            }
        
            fn test_5(a: Int, b: Int) -> &&Int {
                let res: Int = a + b;

                return res;
            }
        
            fn test_6(a: Int) -> Int | &&Int {
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(5))));
        assert_eq!(ctx.variables[1], Some(Object::new(Integer::from(0)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new("Hello".to_string()).get_ref_mut_obj()));
        assert_eq!(ctx.variables[3], Some(Object::new(Integer::from(10)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[4], Some(Object::new(Integer::from(6)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[5], Some(Object::new(Integer::from(9)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[6], Some(Object::new(Integer::from(55))));
        assert_eq!(ctx.variables[7], Some(Object::new(Integer::from(26))));
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

        assert_eq!(ctx.variables[0], Some(Object::new((INT, vec!()))));
        assert_eq!(ctx.variables[1], Some(Object::new((STR, vec!()))));

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn<T> sum(a: 'T, b: 'T) -> 'T {
                return a + b;
            }

            let a = sum<Int>(5, 6);
            let b = sum<String>(\"test\", \"tset\");
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(11))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtset".to_string())));
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(14))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtest".to_string())));
        
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(14))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtest".to_string())));
        
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(13))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtset".to_string())));
        
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

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(11))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtttt".to_string())));
    }

    #[test]
    fn custom_iterators() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        fn iterator(it: Int) -> Int {
            return it.deref<Int>();
        }

        fn next(it: &&Int) -> Int {
            it.inc();
            return it.deref<Int>();
        }

        fn is_consumed(it: &&Int) -> Bool {
            return it >= 10;
        }

        implement Iterable<Int, Int> for Int;

        let sum: Int = 0;

        for i in 0 {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(55))));

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

        fn next(it: &&Range) -> Int {
            let curr: &&Int = it.current();
            curr.inc();

            return curr.deref<Int>();
        }

        fn is_consumed(it: &&Range) -> Bool {
            return it.current() >= it.end();
        }

        implement Iterable<Range, Int> for Range;

        let sum: Int = 0;

        for i in Range(0, 0, 10) {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Integer::from(55))));
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

        assert_eq!(ctx.variables[0], Some(Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Integer::from(0)),
                Object::new(Integer::from(2)),
                Object::new(Integer::from(10))
            )
        })));

        assert_eq!(ctx.variables[1], Some(Object::new(Integer::from(0)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new(Integer::from(2)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[3], Some(Object::new(Integer::from(10)).get_ref_mut_obj()));

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

        assert_eq!(ctx.variables[0], Some(Object::new(TypeInstance {
            id: id,
            params: vec!(INT),
            attributes: vec!(
                Object::new(true),
                Object::new(Integer::from(5))
            )
        })));

        assert_eq!(ctx.variables[1], Some(Object::new(true).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new(Integer::from(5)).get_ref_mut_obj()));
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

        assert_eq!(ctx.variables[1], Some(Object::new(Integer::from(8))));
        assert_eq!(ctx.variables[3], Some(Object::new(Integer::from(7))));
        assert_eq!(ctx.variables[4], Some(Object::new(Integer::from(6))));

        let mut ctx = standard_ctx();
        
        let code_str = "
        let apply: (Int, &&(Int => Int)) => Int = (n: Int, f: &&(Int => Int)) -> Int f<Int, Int>(*<Int>n);
        let f: (Int) => Int = (n: Int) -> Int n * n;

        let a = apply<Int, &&(Int => Int), Int>(5, f);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[2], Some(Object::new(Integer::from(25))));
    }
}