use crate::types::Type;
use crate::object::{Object, Reference};
use crate::parser::NessaExpr;
use crate::context::NessaContext;
use crate::operations::Operator;
use crate::functions::FunctionOverload;
use crate::compilation::CompiledNessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    fn parse_and_execute_nessa_module(&mut self, code: &String) -> Result<(), String> {
        let compiled_code = self.parse_and_compile(code)?;
        
        return self.execute_compiled_code(&compiled_code.into_iter().map(|i| i.instruction).collect());
    }
}

impl NessaContext {
    pub fn execute_compiled_code(&mut self, program: &Vec<CompiledNessaExpr>) -> Result<(), String> {
        use CompiledNessaExpr::*;

        fn check_bool_obj(mut obj: Object) -> bool {
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

        let mut ip: usize = 0;
        let mut offset: usize = 0;
        
        let mut call_stack: Vec<(usize, usize)> = Vec::with_capacity(1000);
        let mut stack: Vec<Object> = Vec::with_capacity(1000);

        loop {
            match &program[ip] {
                Literal(obj) => {
                    stack.push(obj.clone());
                    ip += 1;
                },

                StoreVariable(id) => {
                    self.variables[*id + offset] = Some(stack.pop().unwrap());
                    ip += 1;                    
                },

                GetVariable(id) => {
                    stack.push(self.variables[*id + offset].as_ref().unwrap().get_ref_mut_obj());
                    ip += 1;
                },

                Jump(to) => ip = *to,
                RelativeJump(to) => ip += *to,
                ConditionalRelativeJump(to) => {
                    if !check_bool_obj(stack.pop().unwrap()) {
                        ip += *to;

                    } else {
                        ip += 1;
                    }
                },
                Call(to, off) => {
                    call_stack.push((ip + 1, offset));
                    ip = *to;
                    offset += off;
                },
                Return => {
                    let (prev_ip, prev_offset) = call_stack.pop().unwrap();

                    ip = prev_ip;
                    offset = prev_offset;
                }, 

                NativeFunctionCall(func_id, ov_id) => {
                    if let (Type::And(v), _, Some(f)) = &self.functions[*func_id].overloads[*ov_id] {
                        let mut args = Vec::with_capacity(v.len());

                        for _ in v {
                            args.push(stack.pop().unwrap());
                        }

                        stack.push(f(vec!(), args));

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                UnaryOperatorCall(op_id, ov_id) => {
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*op_id] {
                        let obj = stack.pop().unwrap();

                        // stack.push(operations[*ov_id].2(&obj));

                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                BinaryOperatorCall(op_id, ov_id) => {
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*op_id] {
                        let a = stack.pop().unwrap();
                        let b = stack.pop().unwrap();

                        stack.push(operations[*ov_id].2.unwrap()(&a, &b));
                        
                        ip += 1;
                    
                    } else {
                        unreachable!();
                    }
                }

                NaryOperatorCall(op_id, ov_id) => {
                    if let Operator::Nary{operations, ..} = &self.nary_ops[*op_id] {
                        /*
                        let a = stack.pop().unwrap();
                        let b = stack.pop().unwrap();

                        stack.push(operations[*ov_id].2(&a, &b));

                        ip += 1;
                        */
                    
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
            let v_0 = 5;
            let v_1 = 3 + 4;
            let v_2 = inc(inc(2));
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(5))));
        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(7))));
        assert_eq!(ctx.variables[2], Some(Object::new(Number::from(4))));
    }

    #[test]
    fn flow_control() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let a = 0;
            let b = 0;

            if true {
                a = 1;
            }

            if false {
                b = 1;
            }

            let array: Array<Number> = arr<Number>();

            push<Number>(array, 5);
            array.push<Number>(10);

            let array_2: Array<&Number> = arr<&Number>();
            
            for e in array {
                array_2.e.push<&Number>();
            }
        ".to_string();

        /*
        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(1))));
        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(0))));
        assert_eq!(ctx.variables[2], Some(Object::new((
            Type::Basic(0), 
            vec!(
                Object::new(Number::from(5)),
                Object::new(Number::from(10))
            )
        ))));

        assert_eq!(ctx.variables[3], Some(Object::new((
            Type::Ref(Box::new(Type::Basic(0))), 
            vec!(
                Object::new(Number::from(5)).get_ref_obj(),
                Object::new(Number::from(10)).get_ref_obj()
            )
        ))));
        */
    }

    #[test]
    fn operator_definitions() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            unary prefix op \"~\" (200);

            op ~(arg: &&Number) -> &&Number {
                return arg;
            }

            unary postfix op \"¡\" (300);

            op (arg: &&Number)¡ -> &&Number {
                return arg;
            }

            let a: Number = 3;
            let b: &&Number = ~a¡;

            binary op \"·\" (300);
            binary op \"$\" (300);
            binary op \"@\" (300);

            op (a: &&Number) · (b: &&Number) -> &&Number {
                return a + b;
            }

            let c: &&Number = a · b;

            nary op from \"`\" to \"´\" (500);

            op (a: &&Number)`b: &&Number, c: &&Number´ -> &&Number {
                return a · b · ~c;
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
                let res = 0;

                return res;
            }
        
            fn test_3() -> &&String {
                let res = 0;

                res = \"Hello\";

                return res;
            }
        
            fn test_4() -> &&Number {
                let res = test_1() + test_1();

                return res;
            }
        
            fn test_5(a: Number, b: Number) -> &&Number {
                let res = a + b;

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

            let v_1 = test_1();
            let v_2 = test_2();
            let v_3 = test_3();
            let v_4 = test_4();
            let v_5 = test_5(2, 4);
            let v_6 = test_6(9);
            let v_7 = test_7(10);
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(ctx.variables[0], Some(Object::new(Number::from(5))));
        assert_eq!(ctx.variables[1], Some(Object::new(Number::from(0)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[2], Some(Object::new("Hello".to_string()).get_ref_mut_obj()));
        assert_eq!(ctx.variables[3], Some(Object::new(Number::from(10)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[4], Some(Object::new(Number::from(6)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[5], Some(Object::new(Number::from(9)).get_ref_mut_obj()));
        assert_eq!(ctx.variables[6], Some(Object::new(Number::from(55))));
    }
}