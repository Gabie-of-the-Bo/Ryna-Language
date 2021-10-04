use crate::types::Type;
use crate::object::Object;
use crate::parser::NessaExpr;
use crate::context::NessaContext;
use crate::functions::FunctionOverload;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl FunctionOverload {
    pub fn call(&self, ctx: &NessaContext, templates: &[Type], args: &[&Object], var_offset: usize) -> Object {
        return match self {
            FunctionOverload::Native(f) => f(templates, args),
            FunctionOverload::Nessa(b, a, v) => {
                for (i, (n, t)) in a.iter().enumerate() {
                    ctx.define_variable(var_offset + i, n.clone(), t.clone()).unwrap();
                    ctx.assign_variable(var_offset + i, args[i].clone()).unwrap();
                }

                let res = ctx.execute_nessa_module(b, var_offset, *v).unwrap().0.unwrap();

                for i in var_offset..(var_offset + v) {
                    ctx.delete_variable(i).unwrap();
                }
                
                res
            }
        };
    }
}

impl NessaExpr {
    fn execute(&self, ctx: &NessaContext, var_offset: usize, used_registers: usize) -> Result<(Option<Object>, bool), String> {
        match self {
            NessaExpr::Literal(obj) => return Ok((Some(obj.clone()), false)),
            NessaExpr::Variable(id, n) => {
                let var = ctx.get_variable(var_offset + *id)?;

                if let Some(obj) = &var.value {
                    if let Type::Ref(_) = &var.var_type {
                        return Ok((Some(obj.get_ref_obj()), false));
                    
                    } else {
                        return Ok((Some(obj.get_ref_mut_obj()), false));
                    }

                } else{
                    return Err(format!("Value not present in register with id={} ({})", id, n))
                }
            },

            NessaExpr::CompiledVariableDefinition(id, n, t, e) => {
                let ex_e = e.execute(ctx, var_offset, used_registers)?;

                if let (Some(obj), _) = ex_e {
                    ctx.define_variable(var_offset + *id, n.clone(), t.clone())?;
                    ctx.assign_variable(var_offset + *id, obj)?;

                } else{
                    return Err("Cannot assign a non-existent value to a variable".into());
                }
            }

            NessaExpr::CompiledVariableAssignment(id, _, e) => {
                let ex_e = e.execute(ctx, var_offset, used_registers)?;

                if let (Some(obj), _) = ex_e {
                    ctx.assign_variable(var_offset + *id, obj)?;

                } else{
                    return Err("Cannot assign a non-existent value to a variable".into());
                }
            }

            NessaExpr::UnaryOperation(id, e) => {
                let ex_e = e.execute(ctx, var_offset, used_registers)?;

                if let (Some(obj), _) = ex_e {
                    return Ok((Some(Object::apply_unary_operation(&obj, *id, ctx)?), false));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::BinaryOperation(id, a, b) => {
                let ex_a = a.execute(ctx, var_offset, used_registers)?;
                let ex_b = b.execute(ctx, var_offset, used_registers)?;

                if let ((Some(obj_a), _), (Some(obj_b), _)) = (ex_a, ex_b) {
                    return Ok((Some(Object::apply_binary_operation(&obj_a, &obj_b, *id, ctx)?), false));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::NaryOperation(id, _, a, b) => {
                let ex_a = a.execute(ctx, var_offset, used_registers)?;
                let ex_b = b.into_iter().map(|i| i.execute(ctx, var_offset, used_registers))
                                        .collect::<Result<Vec<_>, _>>()?
                                        .into_iter()
                                        .map(|(a, _)| a)
                                        .collect::<Option<Vec<_>>>()
                                        .expect("Cannot apply an operation to a non-existent value");

                if let ((Some(obj_a), _), obj_b) = (ex_a, ex_b) {
                    let ref_vec = obj_b.iter().collect::<Vec<_>>();

                    return Ok((Some(Object::apply_nary_operation(&obj_a, &ref_vec, *id, ctx)?), false));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::FunctionCall(id, t, a) => {
                let args = a.into_iter().map(|i| i.execute(ctx, var_offset, used_registers))
                                        .collect::<Result<Vec<_>, _>>()?
                                        .into_iter()
                                        .map(|(a, _)| a)
                                        .collect::<Option<Vec<_>>>()
                                        .expect("Cannot apply an operation to a non-existent value");

                return Ok((Some(Object::apply_function(&args.iter().collect::<Vec<_>>(), &t, *id, ctx, var_offset + used_registers)?), false));
            }

            NessaExpr::If(h, b, ei, eb) => {
                let mut else_execution = true;

                let ex_h = h.execute(ctx, var_offset, used_registers)?.0.expect("Cannot evaluate non-existent expression");
                let h_type = ex_h.get_type();

                // If execution
                if let Type::Basic(2) = h_type {
                    if *ex_h.get::<bool>() {
                        let (obj, ret) = ctx.execute_nessa_module(b, var_offset, used_registers)?;
                        else_execution = false;

                        if ret {
                            return Ok((obj, ret));
                        }
                    }
                
                } else {
                    return Err(format!("Cannot evaluate value of type {} as Bool", h_type.get_name(ctx)))
                }

                // Else ifs execution
                for (ei_h, ei_b) in ei {
                    let ex_h = ei_h.execute(ctx, var_offset, used_registers)?.0.expect("Cannot evaluate non-existent expression");
                    let h_type = ex_h.get_type();

                    if let Type::Basic(2) = h_type {
                        if *ex_h.get::<bool>() {
                            let (obj, ret) = ctx.execute_nessa_module(ei_b, var_offset, used_registers)?;
                            else_execution = false;

                            if ret {
                                return Ok((obj, ret));
                            }
                        }
                    
                    } else {
                        return Err(format!("Cannot evaluate value of type {} as Bool", h_type.get_name(ctx)))
                    }
                }

                // Else execution
                if else_execution && eb.is_some() {
                    let (obj, ret) = ctx.execute_nessa_module(eb.as_ref().unwrap(), var_offset, used_registers)?;

                    if ret {
                        return Ok((obj, ret));
                    }
                }
            }

            NessaExpr::CompiledFor(id, n, c, b) => {
                let mut ex_c = c.execute(ctx, var_offset, used_registers)?.0.expect("Cannot evaluate non-existent expression");

                if ex_c.is_ref() {
                    ex_c = ex_c.deref_obj();
                }

                let c_type = ex_c.get_type();

                if let Type::Template(3, _) = c_type {
                    let arr = &*ex_c.get::<(Type, Vec<Object>)>();

                    for el in &arr.1 {
                        ctx.define_variable(var_offset + *id, n.clone(), Type::Ref(Box::new(arr.0.clone())))?;
                        ctx.assign_variable(*id, el.get_ref_obj())?;

                        let (obj, ret) = ctx.execute_nessa_module(b, var_offset, used_registers)?;
                        
                        ctx.delete_variable(*id)?;

                        if ret {
                            return Ok((obj, ret));
                        }
                    }
                
                } else {
                    return Err(format!("Value of type {} is not iterable", c_type.get_name(ctx)))
                }
            }

            NessaExpr::Return(e) => {
                let inner = e.execute(ctx, var_offset, used_registers)?.0.expect("Cannot return non-existent value");

                return Ok((Some(inner), true));
            }

            _ => {}
        }

        return Ok((None, false));
    }
}

impl NessaContext {
    fn define_module_operators(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_operators_parser(code).unwrap().1;

        for i in ops {
            match i {
                NessaExpr::PrefixOperatorDefinition(n, p) => self.define_unary_operator(n.clone(), true, p)?,
                NessaExpr::PostfixOperatorDefinition(n, p) => self.define_unary_operator(n.clone(), false, p)?,
                NessaExpr::BinaryOperatorDefinition(n, p) => self.define_binary_operator(n.clone(), p)?,
                NessaExpr::NaryOperatorDefinition(o, c, p) => self.define_nary_operator(o.clone(), c.clone(), p)?,

                _ => unreachable!()
            }
        }

        return Ok(());
    }
    
    fn define_module_functions(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_function_headers_parser(code).unwrap().1;

        for i in ops {
            self.define_function(i.0, i.1.unwrap_or_default())?;
        }

        return Ok(());
    }
    
    fn define_module_operations(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_operations_parser(code).unwrap().1;

        for i in ops {
            // TODO: create functions from bodies
            match i {
                NessaExpr::PrefixOperationDefinition(id, _a, t, r, _) => self.define_unary_operation(id, t, r, |a| a.clone())?,
                NessaExpr::PostfixOperationDefinition(id, _a, t, r, _) => self.define_unary_operation(id, t, r, |a| a.clone())?,
                NessaExpr::BinaryOperationDefinition(id, (_a, ta), (_b, tb), r, _) => self.define_binary_operation(id, ta, tb, r, |a, _| a.clone())?,
                NessaExpr::NaryOperationDefinition(id, (_a, ta), v, r, _) => self.define_nary_operation(id, ta, &v.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(), r, |a, _| a.clone())?,

                _ => unreachable!()
            }
        }

        return Ok(());
    }

    fn define_module_function_overloads(&mut self, lines: &Vec<NessaExpr>) -> Result<(), String> {
        for i in lines {
            match i {
                NessaExpr::CompiledFunctionDefinition(id, _t, a, r, b, v) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    self.define_function_overload(*id, &arg_types, r.clone(), FunctionOverload::Nessa(b.clone(), a.clone(), *v))?
                },

                _ => {}
            }
        }

        return Ok(());
    }

    fn parse_nessa_module(&mut self, code: &String) -> Vec<NessaExpr> {
        return self.nessa_parser(code).unwrap().1;
    }
    
    fn execute_nessa_module(&self, program: &Vec<NessaExpr>, var_offset: usize, used_registers: usize) -> Result<(Option<Object>, bool), String> {
        for expr in program {
            let (obj, ret) = expr.execute(self, var_offset, used_registers)?;

            if ret {
                return Ok((obj, true));
            }
        }

        return Ok((None, false));
    }

    fn parse_and_execute_nessa_module(&mut self, code: &String) -> Result<(), String> {
        self.define_module_operators(&code)?;
        self.define_module_functions(&code)?;
        self.define_module_operations(&code)?;

        let mut lines = self.parse_nessa_module(&code);
        let used_registers = self.compile(&mut lines, &vec!())?;

        self.define_module_function_overloads(&lines)?;
        
        return self.execute_nessa_module(&lines, 0, used_registers).map(|_| ());
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
    use crate::types::*;
    use crate::variables::*;
    use crate::context::*;
    
    #[test]
    fn variable_definition() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let v_0 = 0;
            let v_1: Bool = true;
            let v_2: String = \"test\";
        ".to_string();

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(*ctx.get_variable(0).unwrap(), Variable {
            id: 0,
            name: "v_0".into(),
            value: Some(Object::new(Number::from(0))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(1).unwrap(), Variable {
            id: 1,
            name: "v_1".into(),
            value: Some(Object::new(true)),
            var_type: Type::Basic(2) 
        });

        assert_eq!(*ctx.get_variable(2).unwrap(), Variable {
            id: 2,
            name: "v_2".into(),
            value: Some(Object::new("test".to_string())),
            var_type: Type::Basic(1) 
        });
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

        assert_eq!(*ctx.get_variable(0).unwrap(), Variable {
            id: 0,
            name: "v_0".into(),
            value: Some(Object::new(Number::from(5))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(1).unwrap(), Variable {
            id: 1,
            name: "v_1".into(),
            value: Some(Object::new(Number::from(7))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(2).unwrap(), Variable {
            id: 2,
            name: "v_2".into(),
            value: Some(Object::new(Number::from(4))),
            var_type: Type::Wildcard 
        });
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

        ctx.parse_and_execute_nessa_module(&code_str).unwrap();

        assert_eq!(*ctx.get_variable(0).unwrap(), Variable {
            id: 0,
            name: "a".into(),
            value: Some(Object::new(Number::from(1))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(1).unwrap(), Variable {
            id: 1,
            name: "b".into(),
            value: Some(Object::new(Number::from(0))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(2).unwrap(), Variable {
            id: 2,
            name: "array".into(),
            value: Some(Object::new((
                Type::Basic(0), 
                vec!(
                    Object::new(Number::from(5)),
                    Object::new(Number::from(10))
                )
            ))),
            var_type: Type::Template(3, vec!(Type::Basic(0))) 
        });

        assert_eq!(*ctx.get_variable(3).unwrap(), Variable {
            id: 3,
            name: "array_2".into(),
            value: Some(Object::new((
                Type::Ref(Box::new(Type::Basic(0))), 
                vec!(
                    Object::new(Number::from(5)).get_ref_obj(),
                    Object::new(Number::from(10)).get_ref_obj()
                )
            ))),
            var_type: Type::Template(3, vec!(Type::Ref(Box::new(Type::Basic(0))))) 
        });
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

            let a = 3;
            let b = ~a¡;

            binary op \"·\" (300);
            binary op \"$\" (300);
            binary op \"@\" (300);

            op (a: &&Number) · (b: &&Number) -> &&Number {
                return a + b;
            }

            let c = a · b;

            nary op from \"`\" to \"´\" (500);

            op (a: &&Number)`b: &&Number, c: &&Number´ -> &&Number {
                return a · b + ~c;
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

        assert_eq!(*ctx.get_variable(0).unwrap(), Variable {
            id: 0,
            name: "v_1".into(),
            value: Some(Object::new(Number::from(5))),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(1).unwrap(), Variable {
            id: 1,
            name: "v_2".into(),
            value: Some(Object::new(Number::from(0)).get_ref_mut_obj()),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(2).unwrap(), Variable {
            id: 2,
            name: "v_3".into(),
            value: Some(Object::new("Hello".to_string()).get_ref_mut_obj()),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(3).unwrap(), Variable {
            id: 3,
            name: "v_4".into(),
            value: Some(Object::new(Number::from(10)).get_ref_mut_obj()),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(4).unwrap(), Variable {
            id: 4,
            name: "v_5".into(),
            value: Some(Object::new(Number::from(6)).get_ref_mut_obj()),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(5).unwrap(), Variable {
            id: 5,
            name: "v_6".into(),
            value: Some(Object::new(Number::from(9)).get_ref_mut_obj()),
            var_type: Type::Wildcard 
        });

        assert_eq!(*ctx.get_variable(6).unwrap(), Variable {
            id: 6,
            name: "v_7".into(),
            value: Some(Object::new(Number::from(55))),
            var_type: Type::Wildcard 
        });
    }
}