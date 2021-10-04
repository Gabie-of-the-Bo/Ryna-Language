use std::collections::HashMap;

use crate::functions::Function;
use crate::context::NessaContext;
use crate::parser::NessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {

    /*
        ╒══════════════════════╕
        │ Variable compilation │
        ╘══════════════════════╛
    */

    fn compile_expr_variables(expr: &mut NessaExpr, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, usize>, curr_ctx: &mut HashMap<String, usize>) -> Result<(), String> {
        match expr {
            // Compile variable references
            NessaExpr::NameReference(n) if ctx_idx.contains_key(n) => {
                *expr = NessaExpr::Variable(*ctx_idx.get(n).unwrap(), n.clone());
            },

            NessaExpr::VariableAssignment(n, e) if ctx_idx.contains_key(n) => {
                if ctx_idx.contains_key(n) {
                    *expr = NessaExpr::CompiledVariableAssignment(*ctx_idx.get(n).unwrap(), n.clone(), e.clone());
                
                } else {
                    return Err(format!("Variable with name {} is not defined", n));
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(n, t, e) => {
                let idx = registers.pop().unwrap();
                ctx_idx.entry(n.clone()).or_insert(idx);
                curr_ctx.entry(n.clone()).or_insert(idx);

                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                *expr = NessaExpr::CompiledVariableDefinition(idx, n.clone(), t.clone(), e.clone());
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, e) => {
                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            NessaExpr::BinaryOperation(_, a, b) => {
                NessaContext::compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;
                NessaContext::compile_expr_variables(b, registers, ctx_idx, curr_ctx)?;
            }
            
            NessaExpr::NaryOperation(_, _, a, b) => {
                NessaContext::compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;

                for i in b {
                    NessaContext::compile_expr_variables(i, registers, ctx_idx, curr_ctx)?
                }
            }

            // Compile flow control
            NessaExpr::If(h, ib, ei, eb) => {
                NessaContext::compile_expr_variables(h, registers, ctx_idx, curr_ctx)?;
                NessaContext::compile_variables_ctx(ib, registers, ctx_idx, &vec!())?;

                for (ei_h, ei_b) in ei {
                    NessaContext::compile_expr_variables(ei_h, registers, ctx_idx, curr_ctx)?;
                    NessaContext::compile_variables_ctx(ei_b, registers, ctx_idx, &vec!())?;
                }

                if let Some(eb_inner) = eb {
                    NessaContext::compile_variables_ctx(eb_inner, registers, ctx_idx, &vec!())?;
                }
            }

            NessaExpr::For(i, c, b) => {
                let idx = registers.pop().unwrap();
                ctx_idx.entry(i.clone()).or_insert(idx);
                curr_ctx.entry(i.clone()).or_insert(idx);

                NessaContext::compile_expr_variables(c, registers, ctx_idx, curr_ctx)?;
                NessaContext::compile_variables_ctx(b, registers, ctx_idx, &vec!())?;

                *expr = NessaExpr::CompiledFor(idx, i.clone(), c.clone(), b.clone());
            }

            NessaExpr::Return(e) => {
                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            _ => {}
        }

        return Ok(());
    }
    
    fn compile_variables_ctx(body: &mut Vec<NessaExpr>, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, usize>, args: &Vec<String>) -> Result<usize, String> {
        let mut curr_ctx = HashMap::new();

        for n in args {
            let idx = registers.pop().unwrap();
            ctx_idx.entry(n.clone()).or_insert(idx);
            curr_ctx.entry(n.clone()).or_insert(idx);
        }

        // Compile each expression sequentially
        for e in body {
            NessaContext::compile_expr_variables(e, registers, ctx_idx, &mut curr_ctx)?;
        }

        let mut max_var = 0;

        // Free the registers inside the context
        curr_ctx.into_iter().for_each(|(n, i)| {
            ctx_idx.remove(&n);
            registers.push(i);

            max_var = max_var.max(i + 1); // Maximum register
        });

        return Ok(max_var);
    }

    pub fn compile_variables(&self, body: &mut Vec<NessaExpr>, args: &Vec<String>) -> Result<usize, String> {
        return NessaContext::compile_variables_ctx(body, &mut (0..self.variables.borrow().len()).rev().collect(), &mut HashMap::new(), args);
    }

    /*
        ╒══════════════════════╕
        │ Function compilation │
        ╘══════════════════════╛
    */

    fn get_func_name(&self, name: &String) -> Option<&Function>{
        return self.functions.iter().filter(|i| i.name == *name).next();
    }

    fn compile_expr_function_names(&self, expr: &mut NessaExpr) {
        match expr {
            // Compile function name references
            NessaExpr::NameReference(n) => {
                if let Some(f) = self.get_func_name(n) {
                    *expr = NessaExpr::FunctionName(f.id);
                }
            },

            // Compile variable definitions
            NessaExpr::CompiledVariableDefinition(_, _, _, e) => {
                self.compile_expr_function_names(e);
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, e) => {
                self.compile_expr_function_names(e);
            }

            NessaExpr::BinaryOperation(_, a, b) => {
                self.compile_expr_function_names(a);
                self.compile_expr_function_names(b);
            }
            
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_expr_function_names(a);
                b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
            }

            // Compile flow control
            NessaExpr::If(h, ib, ei, eb) => {
                self.compile_expr_function_names(h);
                ib.iter_mut().for_each(|i| self.compile_expr_function_names(i));

                ei.iter_mut().for_each(|(ei_h, ei_b)| {
                    self.compile_expr_function_names(ei_h);
                    ei_b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
                });

                if let Some(eb_inner) = eb {
                    eb_inner.iter_mut().for_each(|i| self.compile_expr_function_names(i));
                }
            }

            NessaExpr::CompiledFor(_, _, c, b) => {
                self.compile_expr_function_names(c);
                b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
            }

            NessaExpr::Return(e) => {
                self.compile_expr_function_names(e);
            }

            _ => {}
        }
    }

    fn compile_expr_function_calls(&self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            // Compile variable definitions
            NessaExpr::CompiledVariableDefinition(_, _, _, e) => {
                self.compile_expr_function_calls(e)?;
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, e) => {
                self.compile_expr_function_calls(e)?;
            }

            NessaExpr::BinaryOperation(id, a, b) => {
                self.compile_expr_function_calls(a)?;
                self.compile_expr_function_calls(b)?;

                // Member function calls
                if *id == 3 {
                    if let NessaExpr::FunctionCall(f_id, t, args) = b.as_ref() {
                        // Append first operand to the function's arguments 
                        let mut new_args = vec!(a.as_ref().clone());
                        new_args.extend(args.iter().cloned());

                        *expr = NessaExpr::FunctionCall(*f_id, t.clone(), new_args);
                    }
                }
            }
            
            // Function call
            NessaExpr::NaryOperation(0, t, a, b) => {
                self.compile_expr_function_calls(a)?;
                b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                if let NessaExpr::FunctionName(id) = a.as_ref() {
                    let f = &self.functions[*id];
                    let f_params = &f.params;

                    if f_params.len() == t.len() {
                        *expr = NessaExpr::FunctionCall(*id, t.clone(), b.clone());
                    
                    } else{
                        return Err(format!(
                            "Unable to match type parameters for {}: <{}> <-> <{}> (lengths differ)", 
                            f.name, 
                            t.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                            f.params.join(", ")
                        ))
                    }

                } else if !t.is_empty() {
                    return Err("Invalid type parameters on n-ary call operation".into());
                }
            }

            // Compile flow control
            NessaExpr::If(h, ib, ei, eb) => {
                self.compile_expr_function_calls(h)?;
                ib.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                ei.iter_mut().map(|(ei_h, ei_b)| -> Result<(), String> {
                    self.compile_expr_function_calls(ei_h)?;
                    ei_b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                    return Ok(());
                }).collect::<Result<_, _>>()?;

                if let Some(eb_inner) = eb {
                    eb_inner.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;
                }
            }

            NessaExpr::CompiledFor(_, _, c, b) => {
                self.compile_expr_function_calls(c)?;
                b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?
            }

            NessaExpr::Return(e) => {
                self.compile_expr_function_calls(e)?;
            }

            _ => {}
        }

        return Ok(());
    }

    fn compile_expr_function_bodies(&self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            // Compile variable definitions
            NessaExpr::FunctionDefinition(id, t, a, r, b) => {
                let args = a.iter().map(|(n, _)| n).cloned().collect();
                let max_var = self.compile(b, &args)?;

                *expr = NessaExpr::CompiledFunctionDefinition(*id, t.clone(), a.clone(), r.clone(), b.clone(), max_var);
            }

            _ => {}
        }

        return Ok(());
    }

    pub fn compile_functions(&self, body: &mut Vec<NessaExpr>) -> Result<(), String> {
        body.iter_mut().for_each(|i| self.compile_expr_function_names(i));    
        body.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;   
        body.iter_mut().map(|i| self.compile_expr_function_bodies(i)).collect::<Result<_, _>>()?;   

        return Ok(());
    }

    /*
        ╒══════════════════╕
        │ Full compilation │
        ╘══════════════════╛
    */

    pub fn compile(&self, body: &mut Vec<NessaExpr>, args: &Vec<String>) -> Result<usize, String> {
        let max_register = self.compile_variables(body, args)?;
        self.compile_functions(body)?;

        return Ok(max_register);
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
    use crate::parser::*;
    use crate::context::*;
    
    #[test]
    fn variable_register_allocation() {
        let ctx = standard_ctx();
        
        let code_str = "
            let v_0 = 0;
            let v_1 = !v_0;

            if v_1 {
                let v_2 = v_0;
                let v_3 = v_2 + v_1;
            }

            let v_4 = v_0[v_1, v_0];
        ";

        let (_, mut code) = ctx.nessa_parser(code_str).unwrap();
        ctx.compile_variables(&mut code, &vec!()).unwrap();

        if let NessaExpr::CompiledVariableDefinition(idx, _, _, _) = code[0] {
            assert_eq!(idx, 0);
            
        } else {
            panic!("Invalid expr type");
        }

        if let NessaExpr::CompiledVariableDefinition(idx, _, _, e) = &code[1] {
            assert_eq!(*idx, 1);

            if let NessaExpr::UnaryOperation(_, e2) = e.as_ref() {
                if let NessaExpr::Variable(idx, _) = e2.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
            }
            
        } else {
            panic!("Invalid expr type");
        }

        if let NessaExpr::If(_, b, _, _) = &code[2] {
            if let NessaExpr::CompiledVariableDefinition(idx, _, _, e) = &b[0] {
                assert_eq!(*idx, 2);

                if let NessaExpr::Variable(idx, _) = e.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
            } else {
                panic!("Invalid expr type");
            }

            if let NessaExpr::CompiledVariableDefinition(idx, _, _, e) = &b[1] {
                assert_eq!(*idx, 3);

                if let NessaExpr::BinaryOperation(_, a, b) = e.as_ref() {
                    if let NessaExpr::Variable(idx, _) = a.as_ref() {
                        assert_eq!(*idx, 2);
                        
                    } else {
                        panic!("Invalid expr type");
                    }

                    if let NessaExpr::Variable(idx, _) = b.as_ref() {
                        assert_eq!(*idx, 1);
                        
                    } else {
                        panic!("Invalid expr type");
                    }
                    
                } else {
                    panic!("Invalid expr type");
                }
                
            } else {
                panic!("Invalid expr type");
            }
            
        } else {
            panic!("Invalid expr type");
        }

        if let NessaExpr::CompiledVariableDefinition(idx, _, _, e) = &code[3] {
            assert!(*idx == 2 || *idx == 3);

            if let NessaExpr::NaryOperation(_, _, a, b) = e.as_ref() {
                if let NessaExpr::Variable(idx, _) = a.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx, _) = b[0] {
                    assert_eq!(idx, 1);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx, _) = b[1] {
                    assert_eq!(idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
            }
            
        } else {
            panic!("Invalid expr type");
        }
    }

    #[test]
    fn function_names_and_calls() {
        let ctx = standard_ctx();
        
        let code_1_str = "
        inc(5);
        ";
        
        let code_2_str = "
        inc<Number>(5);
        ";
        
        let code_3_str = "
        wea<Number>(5);
        ";

        let (_, mut code) = ctx.nessa_parser(code_1_str).unwrap();
        ctx.compile_functions(&mut code).unwrap();

        assert_eq!(code, vec!(
            NessaExpr::FunctionCall(0, vec!(), vec!(
                NessaExpr::Literal(Object::new(Number::from(5)))
            ))
        ));
        
        let (_, mut code) = ctx.nessa_parser(code_2_str).unwrap();

        assert!(ctx.compile_functions(&mut code).is_err());
        
        let (_, mut code) = ctx.nessa_parser(code_3_str).unwrap();

        assert!(ctx.compile_functions(&mut code).is_err());
    }
}