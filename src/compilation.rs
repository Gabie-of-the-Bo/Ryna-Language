use std::collections::HashMap;

use crate::context::NessaContext;
use crate::parser::NessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    fn compile_expr_variables(expr: &mut NessaExpr, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, usize>, curr_ctx: &mut HashMap<String, usize>) {
        match expr {
            // Compile variable references
            NessaExpr::NameReference(n) if ctx_idx.contains_key(n) => {
                *expr = NessaExpr::Variable(*ctx_idx.get(n).unwrap());
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(n, t, e) => {
                let idx = registers.pop().unwrap();
                ctx_idx.entry(n.clone()).or_insert(idx);
                curr_ctx.entry(n.clone()).or_insert(idx);

                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx);

                *expr = NessaExpr::CompiledVariableDefinition(idx, t.clone(), e.clone());
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, e) => {
                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx);
            }

            NessaExpr::BinaryOperation(_, a, b) => {
                NessaContext::compile_expr_variables(a, registers, ctx_idx, curr_ctx);
                NessaContext::compile_expr_variables(b, registers, ctx_idx, curr_ctx);
            }
            
            NessaExpr::NaryOperation(_, _, a, b) => {
                NessaContext::compile_expr_variables(a, registers, ctx_idx, curr_ctx);
                b.iter_mut().for_each(|i| NessaContext::compile_expr_variables(i, registers, ctx_idx, curr_ctx));
            }

            // Compile flow control
            NessaExpr::If(h, ib, ei, eb) => {
                NessaContext::compile_expr_variables(h, registers, ctx_idx, curr_ctx);
                NessaContext::compile_variables_ctx(ib, registers, ctx_idx);

                ei.iter_mut().for_each(|(ei_h, ei_b)| {
                    NessaContext::compile_expr_variables(ei_h, registers, ctx_idx, curr_ctx);
                    NessaContext::compile_variables_ctx(ei_b, registers, ctx_idx);
                });

                if let Some(eb_inner) = eb {
                    NessaContext::compile_variables_ctx(eb_inner, registers, ctx_idx);
                }
            }

            NessaExpr::For(_, c, b) => {
                NessaContext::compile_expr_variables(c, registers, ctx_idx, curr_ctx);
                NessaContext::compile_variables_ctx(b, registers, ctx_idx);
            }

            NessaExpr::Return(e) => {
                NessaContext::compile_expr_variables(e, registers, ctx_idx, curr_ctx);
            }

            _ => {}
        }
    }
    
    fn compile_variables_ctx(body: &mut Vec<NessaExpr>, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, usize>) {
        let mut curr_ctx = HashMap::new();

        // Compile each expression sequentially
        body.iter_mut().for_each(|e| NessaContext::compile_expr_variables(e, registers, ctx_idx, &mut curr_ctx));

        // Free the registers inside the context
        curr_ctx.into_iter().for_each(|(n, i)| {
            ctx_idx.remove(&n);
            registers.push(i);
        });
    }

    pub fn compile_variables(&self, body: &mut Vec<NessaExpr>) {
        NessaContext::compile_variables_ctx(body, &mut (0..self.variables.len()).rev().collect(), &mut HashMap::new());
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
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
        ".chars().collect::<Vec<_>>();

        let parser = ctx.nessa_parser();
        let mut code = parser.parse(&code_str).unwrap();
        ctx.compile_variables(&mut code);

        if let NessaExpr::CompiledVariableDefinition(idx, _, _) = code[0] {
            assert_eq!(idx, 0);
            
        } else {
            panic!("Invalid expr type");
        }

        if let NessaExpr::CompiledVariableDefinition(idx, _, e) = &code[1] {
            assert_eq!(*idx, 1);

            if let NessaExpr::UnaryOperation(_, e2) = e.as_ref() {
                if let NessaExpr::Variable(idx) = e2.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
            }
            
        } else {
            panic!("Invalid expr type");
        }

        if let NessaExpr::If(_, b, _, _) = &code[2] {
            if let NessaExpr::CompiledVariableDefinition(idx, _, e) = &b[0] {
                assert_eq!(*idx, 2);

                if let NessaExpr::Variable(idx) = e.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
            } else {
                panic!("Invalid expr type");
            }

            if let NessaExpr::CompiledVariableDefinition(idx, _, e) = &b[1] {
                assert_eq!(*idx, 3);

                if let NessaExpr::BinaryOperation(_, a, b) = e.as_ref() {
                    if let NessaExpr::Variable(idx) = a.as_ref() {
                        assert_eq!(*idx, 2);
                        
                    } else {
                        panic!("Invalid expr type");
                    }

                    if let NessaExpr::Variable(idx) = b.as_ref() {
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

        if let NessaExpr::CompiledVariableDefinition(idx, _, e) = &code[3] {
            assert!(*idx == 2 || *idx == 3);

            if let NessaExpr::NaryOperation(_, _, a, b) = e.as_ref() {
                if let NessaExpr::Variable(idx) = a.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx) = b[0] {
                    assert_eq!(idx, 1);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx) = b[1] {
                    assert_eq!(idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
            }
            
        } else {
            panic!("Invalid expr type");
        }
    }
}