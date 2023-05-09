use crate::types::Type;
use crate::object::Object;
use crate::context::NessaContext;
use crate::operations::Operator;
use crate::compilation::CompiledNessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    pub fn parse_and_execute_nessa_module(&mut self, code: &String) -> Result<(), String> {
        let compiled_code = self.parse_and_compile(code)?;

        for (idx, i) in compiled_code.iter().enumerate() {
            println!("{:<3} {}", idx, i.to_string());
        }
        
        return self.execute_compiled_code(&compiled_code.into_iter().map(|i| i.instruction).collect());
    }
}

impl NessaContext {
    pub fn execute_compiled_code(&mut self, program: &Vec<CompiledNessaExpr>) -> Result<(), String> {
        use CompiledNessaExpr::*;

        fn check_bool_obj(obj: Object) -> bool {
            if obj.is_ref() {
                let ref_obj = obj.deref_obj();

                if let Type::Basic(2) = ref_obj.get_type() {
                    return *ref_obj.get::<bool>();
                }
            }

            if let Type::Basic(2) = obj.get_type() {
                return *obj.get::<bool>();
            }

            return false;
        }

        let mut ip: i32 = 0;
        let mut offset: usize = 0;
        
        let mut call_stack: Vec<(i32, usize, i32)> = Vec::with_capacity(1000);
        let mut stack: Vec<Object> = Vec::with_capacity(1000);

        call_stack.push((0, 0, -1));

        loop {
            match &program[ip as usize] {
                Literal(obj) => {
                    stack.push(obj.deep_clone());
                    ip += 1;
                },

                Tuple(types) => {     
                    let start_idx = stack.len() - types.len();
                    let args = stack.drain(start_idx..).rev().collect();
                    
                    stack.push(Object::new(crate::types::Tuple {
                        types: types.clone(),
                        exprs: args
                    }));

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

                        stack.push(f(type_args, r, args)?);

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                UnaryOperatorCall(op_id, ov_id, type_args) => {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = stack.pop().unwrap();

                        let ov = &operations[*ov_id];

                        stack.push(ov.3.unwrap()(type_args, &ov.2, obj)?);

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

                        stack.push(ov.3.unwrap()(type_args, &ov.2, a, b)?);
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                NaryOperatorCall(op_id, ov_id, type_args) => {
                    if let Operator::Nary{operations, ..} = &self.nary_ops[*op_id] {
                        if let (_, _, r, Some(f)) = &operations[*ov_id] {
                            f((&mut stack, &mut offset, &mut call_stack, &mut ip), type_args, r)?;

                        } else {
                            unreachable!();
                        }

                    } else {
                        unreachable!();
                    }
                }

                Halt => break
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
            fn test(a: Number) -> Number {
                if 0 < a {
                    return test(a - 1) + a;
                }

                return 0;
            }

            let a = test(100);
        ";

        ctx.parse_and_execute_nessa_module(&code_str.into()).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(5050))));
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

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(0))));
        assert_eq!(ctx.variables[1], Some(Object::new(true)));
        assert_eq!(ctx.variables[2], Some(Object::new("test".to_string())));
    }

    #[test]
    fn operations_and_functions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let v_0 = !true;
            let v_1 = 3 + 4;
            let v_2: Number = 2;

            inc(v_2);
            inc(v_2);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(false)));
        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(7))));
        assert_eq!(ctx.variables[2], Some(Object::new(Number::from(4))));
    }

    #[test]
    fn flow_control() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let array: Array<Number> = arr<Number>();
            array.push<Number>(5);

            let iter: ArrayIterator<&&Number> = array.iterator<Number>();
            let ended_1: Bool = iter.is_consumed();
            
            let elem: Number = iter.next<&&Number>();
            let ended_2: Bool = iter.is_consumed();

            let array_2: Array<Number> = arr<Number>();
            array_2.push<Number>(0);
            array_2.push<Number>(2);
            array_2.push<Number>(4);
            array_2.push<Number>(6);
            array_2.push<Number>(8);

            let sum: Number = 0;

            for i in array_2 {
                sum = sum + i;
            }

            let array_3: Array<Number> = arr<Number>();
            array_3.push<Number>(0);
            array_3.push<Number>(1);
            array_3.push<Number>(2);
            array_3.push<Number>(3);
            array_3.push<Number>(4);
            array_3.push<Number>(5);
            array_3.push<Number>(6);
            array_3.push<Number>(7);
            array_3.push<Number>(8);

            let sum_2: Number = 0;

            for i in array_3 {
                if i % 2 != 0 {
                    sum_2 = sum_2 + i;
                }
            }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new((Type::Basic(0), vec!(Object::new(Number::from(5)))))));
        assert_eq!(ctx.variables[1], Some(Object::new((Type::MutRef(Box::new(Type::Basic(0))), ctx.variables[0].as_ref().unwrap().get_ref_mut(), 1))));
        assert_eq!(ctx.variables[2], Some(Object::new(false)));
        assert_eq!(ctx.variables[3], Some(Object::new(Number::from(5)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[4], Some(Object::new(true)));
        assert_eq!(ctx.variables[5], Some(Object::new((Type::Basic(0), vec!(
            Object::new(Number::from(0)),
            Object::new(Number::from(2)),
            Object::new(Number::from(4)),
            Object::new(Number::from(6)),
            Object::new(Number::from(8)),
        )))));
        assert_eq!(ctx.variables[6], Some(Object::new(Number::from(20))));

        if let Type::Template(..) = ctx.variables[7].as_ref().unwrap().get_type() {
            assert_eq!(ctx.variables[7], Some(Object::new((Type::Basic(0), vec!(
                Object::new(Number::from(0)),
                Object::new(Number::from(1)),
                Object::new(Number::from(2)),
                Object::new(Number::from(3)),
                Object::new(Number::from(4)),
                Object::new(Number::from(5)),
                Object::new(Number::from(6)),
                Object::new(Number::from(7)),
                Object::new(Number::from(8)),
            )))));
            assert_eq!(ctx.variables[8], Some(Object::new(Number::from(16))));

        } else {
            assert_eq!(ctx.variables[8], Some(Object::new((Type::Basic(0), vec!(
                Object::new(Number::from(0)),
                Object::new(Number::from(1)),
                Object::new(Number::from(2)),
                Object::new(Number::from(3)),
                Object::new(Number::from(4)),
                Object::new(Number::from(5)),
                Object::new(Number::from(6)),
                Object::new(Number::from(7)),
                Object::new(Number::from(8)),
            )))));
            assert_eq!(ctx.variables[7], Some(Object::new(Number::from(16))));
        }

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn is_prime(n: &&Number) -> Bool {
                if n <= 1 {
                    return false;
                }
                
                let i: Number = 1;

                while i < n - 1 {
                    i = i + 1;

                    if n % i == 0 {
                        return false;
                    }
                }

                return true;
            }

            let array: Array<Number> = arr<Number>();
            let it: Number = 0;

            while it < 50 {
                if is_prime(it) {
                    array.push<Number>(it.deref<Number>());
                }

                it = it + 1;
            }
        ".to_string();
        
        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new((Type::Basic(0), vec!(
            Object::new(Number::from(2)),
            Object::new(Number::from(3)),
            Object::new(Number::from(5)),
            Object::new(Number::from(7)),
            Object::new(Number::from(11)),
            Object::new(Number::from(13)),
            Object::new(Number::from(17)),
            Object::new(Number::from(19)),
            Object::new(Number::from(23)),
            Object::new(Number::from(29)),
            Object::new(Number::from(31)),
            Object::new(Number::from(37)),
            Object::new(Number::from(41)),
            Object::new(Number::from(43)),
            Object::new(Number::from(47)),
        )))));
    }

    #[test]
    fn operator_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary prefix op \"~\" (201);

            op ~(arg: &&Number) -> &&Number {
                return arg;
            }

            unary postfix op \"¡\" (301);

            op (arg: &&Number)¡ -> &&Number {
                return arg;
            }

            let a: Number = 3;
            let b: &&Number = ~a¡;

            binary op \"·\" (401);
            binary op \"$\" (501);
            binary op \"@\" (601);

            op (a: &&Number) · (b: &&Number) -> Number {
                return a + b;
            }

            let c: Number = a · b;

            nary op from \"`\" to \"´\" (701);

            op (a: &&Number)`b: &&Number, c: &&Number´ -> Number {
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
            fn test_1() -> Number {
                return 5;
            }
        
            fn test_2() -> &&Number {
                let res: Number = 0;

                return res;
            }
        
            fn test_3() -> &&String {
                let res: String = \"\";

                res = \"Hello\";

                return res;
            }
        
            fn test_4() -> &&Number {
                let res: Number = test_1() + test_1();

                return res;
            }
        
            fn test_5(a: Number, b: Number) -> &&Number {
                let res: Number = a + b;

                return res;
            }
        
            fn test_6(a: Number) -> Number | &&Number {
                if true {
                    return a;

                } else {
                    return 0;
                }
            }
        
            fn test_7(a: Number) -> Number {
                if 0 < a {
                    return test_7(a - 1) + a;
                }

                return 0;
            }
        
            fn test_8(a: Number) -> Number {
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

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(5))));
        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(0)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new("Hello".to_string()).get_ref_mut_obj()));
        assert_eq!(ctx.variables[3], Some(Object::new(Number::from(10)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[4], Some(Object::new(Number::from(6)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[5], Some(Object::new(Number::from(9)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[6], Some(Object::new(Number::from(55))));
        assert_eq!(ctx.variables[7], Some(Object::new(Number::from(26))));
    }

    #[test]
    fn templated_functions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn<T> test() -> Array<'T> {
                return arr<'T>();
            }

            let a = test<Number>();
            let b = test<String>();
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new((Type::Basic(0), vec!()))));
        assert_eq!(ctx.variables[1], Some(Object::new((Type::Basic(1), vec!()))));

        let mut ctx = standard_ctx();
        
        let code_str = "
            fn<T> sum(a: 'T, b: 'T) -> 'T {
                return a + b;
            }

            let a = sum<Number>(5, 6);
            let b = sum<String>(\"test\", \"tset\");
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(11))));
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
            
            let a = ~<Number>7;
            let b = ~<String>\"test\";
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(14))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtest".to_string())));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary postfix op \"~\" (151);

            op<T> (a: 'T)~ -> 'T {
                return a + a;
            }
            
            let a = 7<Number>~;
            let b = \"test\"<String>~;
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(14))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtest".to_string())));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            binary op \"@\" (151);

            op<T> (a: 'T) @ (b: 'T) -> 'T {
                return a + b;
            }

            let a = 5 <Number>@ 8;
            let b = \"test\" <String>@ \"tset\";
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(13))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtset".to_string())));
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            nary op from \"`\" to \"´\" (151);

            op<T, G> (a: 'T)`b: 'G´ -> 'T {
                return a + b;
            }

            let a = 2<Number, Number>`9´;
            let b = \"test\"<String, String>`\"tttt\"´;
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(11))));
        assert_eq!(ctx.variables[1], Some(Object::new("testtttt".to_string())));
    }

    #[test]
    fn custom_iterators() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        fn iterator(it: Number) -> Number {
            return it.deref<Number>();
        }

        fn next(it: &&Number) -> Number {
            it.inc();
            return it.deref<Number>();
        }

        fn is_consumed(it: &&Number) -> Bool {
            return it >= 10;
        }

        let sum: Number = 0;

        for i in 0 {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(55))));

        let mut ctx = standard_ctx();
        
        let code_str = "
        class Range {
            start: Number;
            current: Number;
            end: Number;
        }

        fn iterator(it: Range) -> Range {
            return it.deref<Range>();
        }

        fn next(it: &&Range) -> Number {
            let curr: &&Number = it.current();
            curr.inc();

            return curr.deref<Number>();
        }

        fn is_consumed(it: &&Range) -> Bool {
            return it.current() >= it.end();
        }

        let sum: Number = 0;

        for i in Range(0, 0, 10) {
            sum = sum + i;
        }
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(55))));
    }

    #[test]
    fn class_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        class Range {
            start: Number;
            current: Number;
            end: Number;
        }

        let r: Range = Range(0, 2, 10);

        let a = r.start();
        let b = r.current();
        let c = r.end();
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        let id = ctx.type_templates.iter().filter(|i| i.name == "Range").next().unwrap().id;

        assert_eq!(ctx.variables[0], Some(Object::new(TypeInstance {
            id: id,
            params: vec!(),
            attributes: vec!(
                Object::new(Number::from(0)),
                Object::new(Number::from(2)),
                Object::new(Number::from(10))
            )
        })));

        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(0)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new(Number::from(2)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[3], Some(Object::new(Number::from(10)).get_ref_mut_obj()));

        let mut ctx = standard_ctx();
        
        let code_str = "
        class Option<T> {
            present: Bool;
            obj: 'T;
        }

        let a: Option<Number> = Option<Number>(true, 5);

        let b = a.present<Number>();
        let c = a.obj<Number>();
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        let id = ctx.type_templates.iter().filter(|i| i.name == "Option").next().unwrap().id;

        assert_eq!(ctx.variables[0], Some(Object::new(TypeInstance {
            id: id,
            params: vec!(Type::Basic(0)),
            attributes: vec!(
                Object::new(true),
                Object::new(Number::from(5))
            )
        })));

        assert_eq!(ctx.variables[1], Some(Object::new(true).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new(Number::from(5)).get_ref_mut_obj()));
    }

    #[test]
    fn lambda_definition() {
        let mut ctx = standard_ctx();
        
        let code_str = "
        let a: (Number) => Number = (n: Number) -> Number n * 2;

        let b = a<Number, Number>(4);

        let c: (Number, Bool) => Number = (n: Number, b: Bool) -> Number {
            if *<Bool>b {
                return n + 2;
            }

            return n + 1;
        };

        let d = c<Number, Bool, Number>(5, true);
        let e = c<Number, Bool, Number>(5, false);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(8))));
        assert_eq!(ctx.variables[3], Some(Object::new(Number::from(7))));
        assert_eq!(ctx.variables[4], Some(Object::new(Number::from(6))));

        let mut ctx = standard_ctx();
        
        let code_str = "
        let apply: (Number, &&(Number => Number)) => Number = (n: Number, f: &&(Number => Number)) -> Number f<Number, Number>(*<Number>n);
        let f: (Number) => Number = (n: Number) -> Number n * n;

        let a = apply<Number, &&(Number => Number), Number>(5, f);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[2], Some(Object::new(Number::from(25))));
    }
}