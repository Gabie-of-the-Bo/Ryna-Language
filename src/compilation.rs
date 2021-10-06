use std::collections::HashMap;

use crate::functions::Function;
use crate::context::NessaContext;
use crate::parser::NessaExpr;
use crate::types::Type;
use crate::object::Object;
use crate::functions::FunctionOverload;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

/*
    ╒═════════════════════════════════╕
    │ Expression tree transformations │
    ╘═════════════════════════════════╛
*/

impl NessaContext {

    /*
        ╒══════════════════════╕
        │ Variable compilation │
        ╘══════════════════════╛
    */

    fn compile_expr_variables(expr: &mut NessaExpr, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, curr_ctx: &mut HashMap<String, usize>) -> Result<(), String> {
        match expr {
            // Compile variable references
            NessaExpr::NameReference(n) if ctx_idx.contains_key(n) => {
                let (idx, t) = ctx_idx.get(n).unwrap();
                *expr = NessaExpr::Variable(*idx, n.clone(), t.clone());
            },

            NessaExpr::VariableAssignment(n, e) if ctx_idx.contains_key(n) => {
                if ctx_idx.contains_key(n) {
                    let (idx, _) = ctx_idx.get(n).unwrap();
                    *expr = NessaExpr::CompiledVariableAssignment(*idx, n.clone(), e.clone());
                
                } else {
                    return Err(format!("Variable with name {} is not defined", n));
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(n, t, e) => {
                let idx = registers.pop().unwrap();
                ctx_idx.entry(n.clone()).or_insert((idx, t.clone()));
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
                ctx_idx.entry(i.clone()).or_insert((idx, Type::Wildcard));
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
    
    fn compile_variables_ctx(body: &mut Vec<NessaExpr>, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, args: &Vec<(String, Type)>) -> Result<usize, String> {
        let mut curr_ctx = HashMap::new();

        for (n, t) in args {
            let idx = registers.pop().unwrap();
            ctx_idx.entry(n.clone()).or_insert((idx, t.clone()));
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

    pub fn compile_variables(&self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<usize, String> {
        return NessaContext::compile_variables_ctx(body, &mut (0..self.variables.len()).rev().collect(), &mut HashMap::new(), args);
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
                let max_var = self.compile(b, &a)?;

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

    pub fn compile(&self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<usize, String> {
        let max_register = self.compile_variables(body, args)?;
        self.compile_functions(body)?;

        return Ok(max_register);
    }
}

/*
    ╒═══════════════════════╕
    │ Compiled Nessa struct │
    ╘═══════════════════════╛
*/

#[derive(Debug)]
pub enum CompiledNessaExpr {
    Literal(Object),

    StoreVariable(usize),
    GetVariable(usize),

    Jump(usize),
    RelativeJump(usize),
    ConditionalRelativeJump(usize),
    Call(usize, usize),
    Return,

    NativeFunctionCall(usize, usize),
    UnaryOperatorCall(usize, usize),
    BinaryOperatorCall(usize, usize),
    NaryOperatorCall(usize, usize),

    Halt
}

impl NessaContext{
    pub fn compiled_form(&self, lines: &Vec<NessaExpr>, max_register: usize) -> Result<Vec<CompiledNessaExpr>, String> {
        let mut program_size = 1;
        let mut functions: HashMap<(usize, usize), usize> = HashMap::new();
        let mut functions_registers: HashMap<usize, usize> = HashMap::new();

        // Define function indexes
        for (j, expr) in lines.iter().enumerate() {
            if let NessaExpr::CompiledFunctionDefinition(id, _, a, _, b, v) = expr {
                let and = Type::And(a.iter().map(|(_, t)| t).cloned().collect());

                // Find function overload id
                for (i, (i_t, _, _)) in self.functions[*id].overloads.iter().enumerate() {
                    if *i_t == and {
                        functions.entry((*id, i)).or_insert(program_size);
                        functions_registers.insert(j, *v);
                        break;
                    }
                }

                program_size += self.compiled_form_body_size(b) + a.len();
            }
        }

        let mut res = vec!(CompiledNessaExpr::Jump(program_size));

        // Define functions
        for (j, expr) in lines.iter().enumerate() {
            if let NessaExpr::CompiledFunctionDefinition(_, _, a, _, b, _) = expr {
                // Store parameters
                for i in 0..a.len(){
                    res.push(CompiledNessaExpr::StoreVariable(i));
                }

                res.extend(self.compiled_form_body(b, &functions, *functions_registers.get(&j).unwrap())?);
            }
        }

        // Define everything else
        for expr in lines {
            match expr {
                NessaExpr::CompiledFunctionDefinition(..) => {},
                _ => res.extend(self.compiled_form_expr(expr, &functions, max_register)?)
            }
        }

        res.push(CompiledNessaExpr::Halt);

        return Ok(res);
    }

    pub fn compiled_form_size(&self, expr: &NessaExpr) -> usize {
        use NessaExpr::*;

        return match expr {
            Literal(_) | Variable(..) => 1, 
            BinaryOperation(_, a, b) => self.compiled_form_size(a) + self.compiled_form_size(b) + 1,
            Return(e) | CompiledVariableDefinition(_, _, _, e) | CompiledVariableAssignment(_, _, e) | UnaryOperation(_, e) => self.compiled_form_size(e) + 1,
            If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_size(ih) + self.compiled_form_body_size(ib) + 1;

                for (h, b) in ei {
                    res += self.compiled_form_size(h) + self.compiled_form_body_size(b) + 1
                }

                if let Some(b) = e {
                    res += self.compiled_form_body_size(b);
                }
                
                res
            },
            FunctionCall(_, _, a) => self.compiled_form_body_size(a) + 1, 
            _ => unreachable!()
        }
    }

    pub fn compiled_form_body_size(&self, lines: &Vec<NessaExpr>) -> usize {
        return lines.iter().map(|i| self.compiled_form_size(i)).sum();
    }

    pub fn compiled_form_expr(&self, expr: &NessaExpr, functions: &HashMap<(usize, usize), usize>, max_register: usize) -> Result<Vec<CompiledNessaExpr>, String> {
        return match expr {
            NessaExpr::Literal(obj) => Ok(vec!(CompiledNessaExpr::Literal(obj.clone()))),
            NessaExpr::Variable(id, _, _) => Ok(vec!(CompiledNessaExpr::GetVariable(*id))), 
            NessaExpr::CompiledVariableDefinition(id, _, _, e) | NessaExpr::CompiledVariableAssignment(id, _, e) => {
                let mut res = self.compiled_form_expr(e, functions, max_register)?;
                res.push(CompiledNessaExpr::StoreVariable(*id));

                Ok(res)
            },
            NessaExpr::UnaryOperation(id, e) => {
                let mut res = self.compiled_form_expr(e, functions, max_register)?;

                let t = self.infer_type(e).unwrap();
                let (ov_id, _) = self.get_first_unary_op(*id, t).unwrap();

                res.push(CompiledNessaExpr::UnaryOperatorCall(*id, ov_id));

                Ok(res)
            },
            NessaExpr::BinaryOperation(id, a, b) => {
                let mut res = self.compiled_form_expr(b, functions, max_register)?;
                res.extend(self.compiled_form_expr(a, functions, max_register)?);

                let a_t = self.infer_type(a).unwrap();
                let b_t = self.infer_type(b).unwrap();

                let (ov_id, _) = self.get_first_binary_op(*id, a_t, b_t).unwrap();

                res.push(CompiledNessaExpr::BinaryOperatorCall(*id, ov_id));

                Ok(res)
            },
            NessaExpr::If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_expr(ih, functions, max_register)?;
                let if_body = self.compiled_form_body(ib, functions, max_register)?;

                res.push(CompiledNessaExpr::ConditionalRelativeJump(if_body.len() + 1));
                res.extend(if_body);

                for (h, b) in ei {
                    res.extend(self.compiled_form_expr(h, functions, max_register)?);

                    let elif_body = self.compiled_form_body(b, functions, max_register)?;

                    res.push(CompiledNessaExpr::ConditionalRelativeJump(elif_body.len() + 1));
                    res.extend(elif_body);
                }

                if let Some(b) = e {
                    res.extend(self.compiled_form_body(b, functions, max_register)?);
                }

                Ok(res)
            },
            NessaExpr::Return(e) => {
                let mut res = self.compiled_form_expr(e, functions, max_register)?;
                res.push(CompiledNessaExpr::Return);

                Ok(res)
            },
            NessaExpr::FunctionCall(id, _, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, max_register)?);
                }

                let args_types = a.iter().map(|i| self.infer_type(i).unwrap()).collect();
                let (ov_id, _, native) = self.get_first_function_overload(*id, args_types).unwrap();

                if native {
                    res.push(CompiledNessaExpr::NativeFunctionCall(*id, ov_id));

                } else {
                    let pos = functions.get(&(*id, ov_id)).unwrap();
                    res.push(CompiledNessaExpr::Call(*pos, max_register));
                }

                Ok(res)
            }
            _ => { Ok(vec!()) }
        };
    }

    pub fn compiled_form_body(&self, lines: &Vec<NessaExpr>, functions: &HashMap<(usize, usize), usize>, max_register: usize) -> Result<Vec<CompiledNessaExpr>, String> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, functions, max_register)).flat_map(|i| i.unwrap()).collect());
    }

    
    pub fn define_module_operators(&mut self, code: &String) -> Result<(), String> {
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
    
    pub fn define_module_functions(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_function_headers_parser(code).unwrap().1;

        for i in ops {
            self.define_function(i.0, i.1.unwrap_or_default())?;
        }

        return Ok(());
    }
    
    pub fn define_module_operations(&mut self, code: &String) -> Result<(), String> {
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

    pub fn define_module_function_overloads(&mut self, lines: &Vec<NessaExpr>) -> Result<(), String> {
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

    pub fn parse_nessa_module(&mut self, code: &String) -> Vec<NessaExpr> {
        return self.nessa_parser(code).unwrap().1;
    }

    pub fn parse_and_compile(&mut self, code: &String) -> Result<Vec<CompiledNessaExpr>, String> {
        self.define_module_operators(code)?;
        self.define_module_functions(code)?;
        self.define_module_operations(code)?;

        let mut lines = self.parse_nessa_module(code);
        let max_register = self.compile(&mut lines, &vec!())?;

        self.define_module_function_overloads(&lines)?;

        return self.compiled_form(&lines, max_register);
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
                if let NessaExpr::Variable(idx, _, _) = e2.as_ref() {
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

                if let NessaExpr::Variable(idx, _, _) = e.as_ref() {
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
                    if let NessaExpr::Variable(idx, _, _) = a.as_ref() {
                        assert_eq!(*idx, 2);
                        
                    } else {
                        panic!("Invalid expr type");
                    }

                    if let NessaExpr::Variable(idx, _, _) = b.as_ref() {
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
                if let NessaExpr::Variable(idx, _, _) = a.as_ref() {
                    assert_eq!(*idx, 0);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx, _, _) = b[0] {
                    assert_eq!(idx, 1);
                    
                } else {
                    panic!("Invalid expr type");
                }
                
                if let NessaExpr::Variable(idx, _, _) = b[1] {
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

    #[test]
    fn compiled_form() {
        let mut ctx = standard_ctx();

        let code_str = "
            fn test(a: Number) -> Number {
                if 0 < a {
                    return test(a - 1) + a;
                }

                return 0;
            }

            let a = test(10);
        ";

        let compiled_code = ctx.parse_and_compile(&code_str.into());

        assert!(compiled_code.is_ok());
    }
}