use crate::types::Type;
use crate::object::Object;
use crate::parser::NessaExpr;
use crate::context::NessaContext;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaExpr {
    fn execute(&self, ctx: &mut NessaContext) -> Result<Option<Object>, String> {
        match self {
            NessaExpr::Literal(obj) => return Ok(Some(obj.clone())),
            NessaExpr::Variable(id, n) => {
                let var = ctx.get_variable(*id)?;

                if let Some(obj) = &var.value {
                    return Ok(Some(obj.get_ref_mut_obj()));

                } else{
                    return Err(format!("Value not present in register with id={} ({})", id, n))
                }
            },

            NessaExpr::CompiledVariableDefinition(id, n, t, e) => {
                let ex_e = e.execute(ctx)?;

                if let Some(obj) = ex_e {
                    ctx.define_variable(*id, n.clone(), t.clone())?;
                    ctx.assign_variable(*id, obj)?;

                } else{
                    return Err("Cannot assign a non-existent value to a variable".into());
                }
            }

            NessaExpr::CompiledVariableAssignment(id, _, e) => {
                let ex_e = e.execute(ctx)?;

                if let Some(obj) = ex_e {
                    ctx.assign_variable(*id, obj)?;

                } else{
                    return Err("Cannot assign a non-existent value to a variable".into());
                }
            }

            NessaExpr::UnaryOperation(id, e) => {
                let ex_e = e.execute(ctx)?;

                if let Some(obj) = ex_e {
                    return Ok(Some(Object::apply_unary_operation(&obj, *id, ctx)?));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::BinaryOperation(id, a, b) => {
                let ex_a = a.execute(ctx)?;
                let ex_b = b.execute(ctx)?;

                if let (Some(obj_a), Some(obj_b)) = (ex_a, ex_b) {
                    return Ok(Some(Object::apply_binary_operation(&obj_a, &obj_b, *id, ctx)?));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::NaryOperation(id, _, a, b) => {
                let ex_a = a.execute(ctx)?;
                let ex_b = b.into_iter().map(|i| i.execute(ctx))
                                        .collect::<Result<Vec<_>, _>>()?
                                        .into_iter()
                                        .collect::<Option<Vec<_>>>()
                                        .expect("Cannot apply an operation to a non-existent value");

                if let (Some(obj_a), obj_b) = (ex_a, ex_b) {
                    let ref_vec = obj_b.iter().collect::<Vec<_>>();

                    return Ok(Some(Object::apply_nary_operation(&obj_a, &ref_vec, *id, ctx)?));

                } else{
                    return Err("Cannot apply an operation to a non-existent value".into());
                }                
            }

            NessaExpr::FunctionCall(id, _, a) => {
                let args = a.into_iter().map(|i| i.execute(ctx))
                                        .collect::<Result<Vec<_>, _>>()?
                                        .into_iter()
                                        .collect::<Option<Vec<_>>>()
                                        .expect("Cannot apply an operation to a non-existent value");

                return Ok(Some(Object::apply_function(&args.iter().collect::<Vec<_>>(), *id, ctx)?));
            }

            NessaExpr::If(h, b, ei, eb) => {
                let mut else_execution = true;

                let ex_h = h.execute(ctx)?.expect("Cannot evaluate non-existent expression");
                let h_type = ex_h.get_type();

                // If execution
                if let Type::Basic(2) = h_type {
                    if *ex_h.get::<bool>() {
                        ctx.execute_nessa_program(b)?;
                        else_execution = false;
                    }
                
                } else {
                    return Err(format!("Cannot evaluate value of type {} as Bool", h_type.get_name(ctx)))
                }

                // Else ifs execution
                for (ei_h, ei_b) in ei {
                    let ex_h = ei_h.execute(ctx)?.expect("Cannot evaluate non-existent expression");
                    let h_type = ex_h.get_type();

                    if let Type::Basic(2) = h_type {
                        if *ex_h.get::<bool>() {
                            ctx.execute_nessa_program(ei_b)?;
                            else_execution = false;
                        }
                    
                    } else {
                        return Err(format!("Cannot evaluate value of type {} as Bool", h_type.get_name(ctx)))
                    }
                }

                // Else execution
                if else_execution && eb.is_some() {
                    ctx.execute_nessa_program(eb.as_ref().unwrap())?;
                }
            }

            NessaExpr::CompiledFor(id, n, c, b) => {
                let ex_c = c.execute(ctx)?.expect("Cannot evaluate non-existent expression");
                let c_type = ex_c.get_type();

                if let Type::Template(3, _) = c_type {
                    let arr = &*ex_c.get::<Vec<Object>>();

                    for el in arr {
                        ctx.define_variable(*id, n.clone(), Type::Wildcard)?;
                        ctx.assign_variable(*id, el.get_ref_obj())?;

                        ctx.execute_nessa_program(b)?;
                        
                        ctx.delete_variable(*id)?;
                    }
                
                } else {
                    return Err(format!("Value of type {} is not iterable", c_type.get_name(ctx)))
                }
            }

            _ => {}
        }

        return Ok(None);
    }
}

impl NessaContext {
    fn execute_nessa_program(&mut self, program: &Vec<NessaExpr>) -> Result<(), String> {
        for expr in program {
            expr.execute(self)?;
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
        ".chars().collect::<Vec<_>>();

        let mut code;

        {
            let parser = ctx.nessa_parser();
            code = parser.parse(&code_str).unwrap();

            ctx.compile(&mut code).unwrap();
        }

        ctx.execute_nessa_program(&code).unwrap();

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
        ".chars().collect::<Vec<_>>();

        let mut code;

        {
            let parser = ctx.nessa_parser();
            code = parser.parse(&code_str).unwrap();

            ctx.compile(&mut code).unwrap();
        }

        ctx.execute_nessa_program(&code).unwrap();

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
        ".chars().collect::<Vec<_>>();

        let mut code;

        {
            let parser = ctx.nessa_parser();
            code = parser.parse(&code_str).unwrap();

            ctx.compile(&mut code).unwrap();
        }

        ctx.execute_nessa_program(&code).unwrap();

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
    }
}