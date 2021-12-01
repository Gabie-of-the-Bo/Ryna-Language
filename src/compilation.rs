use std::collections::HashMap;

use seq_macro::seq;

use crate::functions::Function;
use crate::context::NessaContext;
use crate::parser::NessaExpr;
use crate::types::*;
use crate::object::Object;
use crate::functions::*;
use crate::operations::*;

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

    fn compile_expr_variables(&self, expr: &mut NessaExpr, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, curr_ctx: &mut HashMap<String, usize>) -> Result<(), String> {
        match expr {
            // Compile variable references
            NessaExpr::NameReference(n) if ctx_idx.contains_key(n) => {
                let (idx, t) = ctx_idx.get(n).unwrap();
                *expr = NessaExpr::Variable(*idx, n.clone(), t.clone());
            },

            NessaExpr::VariableAssignment(n, e) if ctx_idx.contains_key(n) => {
                if ctx_idx.contains_key(n) {
                    self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                    let (idx, t) = ctx_idx.get(n).unwrap();

                    *expr = NessaExpr::CompiledVariableAssignment(*idx, n.clone(), t.clone(), e.clone());
                
                } else {
                    return Err(format!("Variable with name {} is not defined", n));
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(n, t, e) => {
                let idx = registers.pop().unwrap();
                ctx_idx.entry(n.clone()).or_insert((idx, t.clone()));
                curr_ctx.entry(n.clone()).or_insert(idx);

                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                *expr = NessaExpr::CompiledVariableDefinition(idx, n.clone(), t.clone(), e.clone());
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, _, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            NessaExpr::BinaryOperation(_, _, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;
                self.compile_expr_variables(b, registers, ctx_idx, curr_ctx)?;
            }
            
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;

                for i in b {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;
                }
            }

            NessaExpr::Tuple(a) |
            NessaExpr::FunctionCall(_, _, a) => {
                for i in a {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;                    
                }
            }

            // Compile flow control
            NessaExpr::If(h, ib, ei, eb) => {
                self.compile_expr_variables(h, registers, ctx_idx, curr_ctx)?;
                self.compile_variables_ctx(ib, registers, ctx_idx, &vec!())?;

                for (ei_h, ei_b) in ei {
                    self.compile_expr_variables(ei_h, registers, ctx_idx, curr_ctx)?;
                    self.compile_variables_ctx(ei_b, registers, ctx_idx, &vec!())?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_variables_ctx(eb_inner, registers, ctx_idx, &vec!())?;
                }
            }

            NessaExpr::While(c, b) => {
                self.compile_expr_variables(c, registers, ctx_idx, curr_ctx)?;
                self.compile_variables_ctx(b, registers, ctx_idx, &vec!())?;
            }

            NessaExpr::For(i, c, b) => {
                self.compile_expr_variables(c, registers, ctx_idx, curr_ctx)?;

                let container_type = self.infer_type(c).unwrap();
                let iterator_type = self.get_iterator_type(&container_type)?;
                let element_type = self.get_iterator_output_type(&iterator_type)?;

                let iterator_idx = *registers.last().unwrap();
                let element_idx = *registers.get(registers.len() - 2).unwrap();

                self.compile_variables_ctx(b, registers, ctx_idx, &vec!(("__iterator__".into(), iterator_type), (i.clone(), element_type)))?;

                *expr = NessaExpr::CompiledFor(iterator_idx, element_idx, i.clone(), c.clone(), b.clone());
            }

            NessaExpr::Return(e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            _ => {}
        }

        return Ok(());
    }
    
    fn compile_variables_ctx(&self, body: &mut Vec<NessaExpr>, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, args: &Vec<(String, Type)>) -> Result<usize, String> {
        let mut curr_ctx = HashMap::new();

        for (n, t) in args {
            let idx = registers.pop().unwrap();
            ctx_idx.entry(n.clone()).or_insert((idx, t.clone()));
            curr_ctx.entry(n.clone()).or_insert(idx);
        }

        // Compile each expression sequentially
        for e in body {
            self.compile_expr_variables(e, registers, ctx_idx, &mut curr_ctx)?;
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
        return self.compile_variables_ctx(body, &mut (0..self.variables.len()).rev().collect(), &mut HashMap::new(), args);
    }

    /*
        ╒══════════════════════╕
        │ Function compilation │
        ╘══════════════════════╛
    */

    fn get_func_name(&self, name: &String) -> Option<&Function>{
        return self.functions.iter().filter(|i| i.name == *name).next();
    }

    fn compile_expr_function_names(&mut self, expr: &mut NessaExpr) {
        match expr {
            // Compile function name references
            NessaExpr::NameReference(n) => {
                if let Some(f) = self.get_func_name(n) {
                    *expr = NessaExpr::FunctionName(f.id);
                }
            },

            NessaExpr::Tuple(e) => e.iter_mut().for_each(|i| self.compile_expr_function_names(i)),

            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, e) |
            NessaExpr::VariableAssignment(_, e) => {
                self.compile_expr_function_names(e);
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, _, e) => {
                self.compile_expr_function_names(e);
            }

            NessaExpr::BinaryOperation(_, _, a, b) => {
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

            NessaExpr::For(_, c, b) |
            NessaExpr::While(c, b) => {
                self.compile_expr_function_names(c);
                b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
            }

            NessaExpr::Return(e) => {
                self.compile_expr_function_names(e);
            }

            _ => {}
        }
    }

    fn compile_expr_function_calls(&mut self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            // Compile tuples
            NessaExpr::Tuple(e) => {
                for i in e {
                    self.compile_expr_function_calls(i)?;
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, e) |
            NessaExpr::VariableAssignment(_, e) => {
                self.compile_expr_function_calls(e)?;
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, _, e) => {
                self.compile_expr_function_calls(e)?;
            }

            NessaExpr::BinaryOperation(id, _, a, b) => {
                self.compile_expr_function_calls(a)?;
                self.compile_expr_function_calls(b)?;

                // Member function calls
                if *id == DOT_BINOP_ID {
                    if let NessaExpr::FunctionCall(f_id, t, args) = b.as_ref() {
                        // Append first operand to the function's arguments 
                        let mut new_args = vec!(a.as_ref().clone());
                        new_args.extend(args.iter().cloned());

                        *expr = NessaExpr::FunctionCall(*f_id, t.clone(), new_args);
                    }
                }
            }
            
            // Function call
            NessaExpr::NaryOperation(CALL_OP, t, a, b) => {
                self.compile_expr_function_calls(a)?;
                b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                if let NessaExpr::FunctionName(id) = a.as_ref() {                    
                    *expr = NessaExpr::FunctionCall(*id, t.clone(), b.clone());

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

            NessaExpr::For(_, c, b) |
            NessaExpr::While(c, b) => {
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

    fn compile_expr_function_bodies(&mut self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) |
            NessaExpr::ClassDefinition(..) |
            NessaExpr::NameReference(..) |
            NessaExpr::Literal(..) => {},

            NessaExpr::VariableAssignment(_, e) |
            NessaExpr::VariableDefinition(_, _, e) |
            NessaExpr::UnaryOperation(_, _, e) |
            NessaExpr::Return(e) => {
                self.compile_expr_function_bodies(e)?;
            },

            NessaExpr::FunctionCall(_, _, args) |
            NessaExpr::Tuple(args) => {
                for e in args {
                    self.compile_expr_function_bodies(e)?;
                }
            }

            NessaExpr::BinaryOperation(_, _, a, b) => {
                self.compile_expr_function_bodies(a)?;
                self.compile_expr_function_bodies(b)?;
            }

            NessaExpr::For(_, a, b) |
            NessaExpr::While(a, b) |
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_expr_function_bodies(a)?;

                for e in b {
                    self.compile_expr_function_bodies(e)?;
                }
            }

            NessaExpr::If(ih, ib, ei, eb) => {
                self.compile_expr_function_bodies(ih)?;

                for e in ib {
                    self.compile_expr_function_bodies(e)?;
                }

                for (ei_h, ei_b) in ei {
                    self.compile_expr_function_bodies(ei_h)?;

                    for e in ei_b {
                        self.compile_expr_function_bodies(e)?;
                    }   
                }

                if let Some(eb_inner) = eb {
                    for e in eb_inner {
                        self.compile_expr_function_bodies(e)?;
                    }   
                }
            }

            NessaExpr::Lambda(a, r, b) => {
                let max_var = self.compile(b, &a)?;

                *expr = NessaExpr::CompiledLambda(self.lambdas, a.clone(), r.clone(), b.clone(), max_var);
                self.lambdas += 1;
            },

            NessaExpr::FunctionDefinition(id, t, a, r, b) => {
                let max_var = self.compile(b, &a)?;

                *expr = NessaExpr::CompiledFunctionDefinition(*id, t.clone(), a.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::PrefixOperationDefinition(id, tm, n, t, r, b) => {
                let max_var = self.compile(b, &vec!((n.clone(), t.clone())))?;

                *expr = NessaExpr::CompiledPrefixOperationDefinition(*id, tm.clone(), n.clone(), t.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::PostfixOperationDefinition(id, tm, n, t, r, b) => {
                let max_var = self.compile(b, &vec!((n.clone(), t.clone())))?;

                *expr = NessaExpr::CompiledPostfixOperationDefinition(*id, tm.clone(), n.clone(), t.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::BinaryOperationDefinition(id, tm, a1, a2, r, b) => {
                let max_var = self.compile(b, &vec!(a1.clone(), a2.clone()))?;

                *expr = NessaExpr::CompiledBinaryOperationDefinition(*id, tm.clone(), a1.clone(), a2.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::NaryOperationDefinition(id, tm, a, args, r, b) => {
                let mut all_args = vec!(a.clone());
                all_args.extend(args.iter().cloned());

                let max_var = self.compile(b, &all_args)?;

                *expr = NessaExpr::CompiledNaryOperationDefinition(*id, tm.clone(), a.clone(), args.clone(), r.clone(), b.clone(), max_var);
            }

            _ => unimplemented!("{:?}", expr)
        }

        return Ok(());
    }

    pub fn compile_functions(&mut self, body: &mut Vec<NessaExpr>) -> Result<(), String> {
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

    pub fn compile(&mut self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<usize, String> {
        self.compile_functions(body)?;
        let max_register = self.compile_variables(body, args)?;

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
    Tuple(Vec<Type>),

    StoreVariable(usize),
    GetVariable(usize),

    Jump(usize),
    RelativeJump(i32),
    RelativeJumpIfFalse(usize),
    RelativeJumpIfTrue(usize),
    Call(usize, usize),
    Return,

    NativeFunctionCall(usize, usize, Vec<Type>),
    UnaryOperatorCall(usize, usize, Vec<Type>),
    BinaryOperatorCall(usize, usize, Vec<Type>),
    NaryOperatorCall(usize, usize, Vec<Type>),

    Halt
}

#[derive(Debug)]
pub struct NessaInstruction {
    pub instruction: CompiledNessaExpr,
    pub comment: String
}

impl NessaInstruction {
    pub fn to_string(&self) -> String {
        return format!("{:<30}{}{}", format!("{:?}", self.instruction), if self.comment.is_empty() { "" } else { "# " }, format!("{}", self.comment));
    }
}

impl From<CompiledNessaExpr> for NessaInstruction {
    fn from(obj: CompiledNessaExpr) -> NessaInstruction {
        return NessaInstruction {
            instruction: obj,
            comment: String::new()
        };
    }
}

impl NessaInstruction {
    pub fn new(instruction: CompiledNessaExpr, comment: String) -> NessaInstruction {
        return NessaInstruction {
            instruction: instruction,
            comment: comment
        }
    }
}

impl NessaContext{
    fn add_template_instance(map: &mut HashMap<usize, Vec<Vec<Type>>>, id: usize, args: &Vec<Type>){
        let curr = map.entry(id).or_default();

        for i in curr.iter() {
            if i == args {
                return;
            }
        }

        curr.push(args.clone());
    }

    pub fn get_template_calls(
        &self, 
        expr: &NessaExpr, 
        functions: &mut HashMap<usize, Vec<Vec<Type>>>, 
        unary: &mut HashMap<usize, Vec<Vec<Type>>>,
        binary: &mut HashMap<usize, Vec<Vec<Type>>>,
        nary: &mut HashMap<usize, Vec<Vec<Type>>>
    ) {
        return match expr {
            NessaExpr::CompiledVariableDefinition(_, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) | 
            NessaExpr::Return(e) => self.get_template_calls(e, functions, unary, binary, nary), 

            NessaExpr::Tuple(e) => self.get_template_calls_body(e, functions, unary, binary, nary),

            NessaExpr::UnaryOperation(id, t, e) => {
                NessaContext::add_template_instance(unary, *id, t);

                self.get_template_calls(e, functions, unary, binary, nary);
            }

            NessaExpr::BinaryOperation(id, t, a, b) => {
                NessaContext::add_template_instance(binary, *id, t);

                self.get_template_calls(a, functions, unary, binary, nary);
                self.get_template_calls(b, functions, unary, binary, nary);
            },

            NessaExpr::NaryOperation(id, t, a, b) => {
                NessaContext::add_template_instance(nary, *id, t);

                self.get_template_calls(a, functions, unary, binary, nary);
                self.get_template_calls_body(b, functions, unary, binary, nary);
            }

            NessaExpr::If(i, ib, ei, eb) => {
                self.get_template_calls(i, functions, unary, binary, nary);
                self.get_template_calls_body(ib, functions, unary, binary, nary);

                for (ei_h, ei_b) in ei {
                    self.get_template_calls(ei_h, functions, unary, binary, nary);
                    self.get_template_calls_body(ei_b, functions, unary, binary, nary);
                }

                if let Some(b) = eb {
                    self.get_template_calls_body(b, functions, unary, binary, nary);
                }
            }

            NessaExpr::CompiledFor(_, _, _, c, b) => {
                self.get_template_calls(c, functions, unary, binary, nary);

                if let Some(ct) = self.infer_type(c) {
                    if let Some((_, it_type, _, it_args)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(ct.clone()), true) {
                        let it_mut = Type::MutRef(Box::new(it_type.clone()));

                        // Implicitly call "iterator", "next" and "is_consumed"
                        if let Some((_, _, _, next_args)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), true) {
                            if let Some((_, _, _, consumed_args)) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), true) {
                                NessaContext::add_template_instance(functions, ITERATOR_FUNC_ID, &it_args);
                                NessaContext::add_template_instance(functions, NEXT_FUNC_ID, &next_args);
                                NessaContext::add_template_instance(functions, IS_CONSUMED_FUNC_ID, &consumed_args);
                            }
                        }
                    }
                }

                for line in b {
                    self.get_template_calls(line, functions, unary, binary, nary);
                }
            }

            NessaExpr::While(c, b) => {
                self.get_template_calls(c, functions, unary, binary, nary);
                self.get_template_calls_body(b, functions, unary, binary, nary);
            }

            NessaExpr::FunctionCall(id, t, args) => {
                NessaContext::add_template_instance(functions, *id, t);

                self.get_template_calls_body(args, functions, unary, binary, nary);
            }

            _ => {}
        }
    }

    pub fn get_template_calls_body(
        &self, 
        lines: &Vec<NessaExpr>, 
        functions: &mut HashMap<usize, Vec<Vec<Type>>>, 
        unary: &mut HashMap<usize, Vec<Vec<Type>>>,
        binary: &mut HashMap<usize, Vec<Vec<Type>>>,
        nary: &mut HashMap<usize, Vec<Vec<Type>>>
    ) {
        for line in lines {
            self.get_template_calls(line, functions, unary, binary, nary);
        }
    }

    pub fn subtitute_type_params_expr(&self, expr: &mut NessaExpr, templates: &HashMap<usize, Type>) {
        match expr {
            NessaExpr::Literal(_) => {},

            NessaExpr::Tuple(e) => e.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates)),

            NessaExpr::Variable(_, _, t) => *t = t.sub_templates(templates),

            NessaExpr::Return(e) => self.subtitute_type_params_expr(e, templates),

            NessaExpr::CompiledVariableAssignment(_, _, t, e) |
            NessaExpr::CompiledVariableDefinition(_, _, t, e) => {
                *t = t.sub_templates(templates);

                self.subtitute_type_params_expr(e, templates);
            },
            
            NessaExpr::UnaryOperation(_, t, a) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(a, templates);
            },

            NessaExpr::BinaryOperation(_, t, a, b) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(a, templates);
                self.subtitute_type_params_expr(b, templates);
            }

            NessaExpr::NaryOperation(_, t, first, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(first, templates);
                args.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },
            
            NessaExpr::FunctionCall(_, t, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                args.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },
            
            NessaExpr::CompiledFor(_, _, _, container, body) |
            NessaExpr::While(container, body) => {
                self.subtitute_type_params_expr(container, templates);
                body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },

            NessaExpr::If(ih, ib, ei, eb) => {
                self.subtitute_type_params_expr(ih, templates);
                ib.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));

                for (ei_h, ei_b) in ei {
                    self.subtitute_type_params_expr(ei_h, templates);
                    ei_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
                }

                if let Some(b) = eb {
                    b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
                }
            }
            
            _ => unimplemented!("{:?}", expr)
        };
    }

    pub fn compile_lambda_expr(
        &self, 
        line: &NessaExpr, 
        lambdas: &mut Vec<NessaInstruction>,
        lambda_positions: &mut HashMap<usize, usize>,
        functions: &mut HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        binary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        nary: &mut HashMap<(usize, usize, Vec<Type>), usize>
    ) -> Result<(), String> {
        return match line {
            NessaExpr::Literal(..) |
            NessaExpr::Variable(..) |
            NessaExpr::ClassDefinition(..) |
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) => Ok(()),

            NessaExpr::CompiledLambda(i, a, _, b, m) => {
                self.compile_lambda(b, lambdas, lambda_positions, functions, unary, binary, nary)?;

                lambda_positions.entry(*i).or_insert(lambdas.len() + 1);

                for i in (0..a.len()).rev() {
                    lambdas.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(i)));
                }

                lambdas.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions, *m)?);

                Ok(())
            }

            NessaExpr::CompiledVariableDefinition(_, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) |
            NessaExpr::Return(e) |
            NessaExpr::UnaryOperation(_, _, e) => self.compile_lambda_expr(e, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::BinaryOperation(_, _, a, b) => {
                self.compile_lambda_expr(a, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda_expr(b, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::CompiledFor(_, _, _, a, b) |
            NessaExpr::While(a, b) |
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_lambda_expr(a, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda(b, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::If(ih, ib, ei, eb) => {
                self.compile_lambda_expr(ih, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda(ib, lambdas, lambda_positions, functions, unary, binary, nary)?;

                for (ei_h, ei_b) in ei {
                    self.compile_lambda_expr(ei_h, lambdas, lambda_positions, functions, unary, binary, nary)?;
                    self.compile_lambda(ei_b, lambdas, lambda_positions, functions, unary, binary, nary)?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_lambda(eb_inner, lambdas, lambda_positions, functions, unary, binary, nary)?;                    
                }

                Ok(())
            }

            NessaExpr::Tuple(args) |
            NessaExpr::FunctionCall(_, _, args) => self.compile_lambda(args, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::CompiledFunctionDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledPrefixOperationDefinition(_, _, _, _, _, b, _) |
            NessaExpr::CompiledPostfixOperationDefinition(_, _, _, _, _, b, _) |
            NessaExpr::CompiledBinaryOperationDefinition(_, _, _, _, _, b, _) |
            NessaExpr::CompiledNaryOperationDefinition(_, _, _, _, _, b, _) => self.compile_lambda(b, lambdas, lambda_positions, functions, unary, binary, nary),

            _ => unimplemented!("{:?}", line)
        };
    }

    pub fn compile_lambda(
        &self, 
        lines: &Vec<NessaExpr>, 
        lambdas: &mut Vec<NessaInstruction>,
        lambda_positions: &mut HashMap<usize, usize>,
        functions: &mut HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        binary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        nary: &mut HashMap<(usize, usize, Vec<Type>), usize>
    ) -> Result<(), String> {
        for line in lines {
            self.compile_lambda_expr(line, lambdas, lambda_positions, functions, unary, binary, nary)?;
        }

        return Ok(());
    }

    pub fn compiled_form(&self, lines: &Vec<NessaExpr>, max_register: usize) -> Result<Vec<NessaInstruction>, String> {
        let mut program_size = 1;
        let mut functions: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut unary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut binary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut nary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();

        let mut registers: HashMap<usize, usize> = HashMap::new();

        let mut function_instances = HashMap::new();
        let mut unary_instances = HashMap::new();
        let mut binary_instances = HashMap::new();
        let mut nary_instances = HashMap::new();

        self.get_template_calls_body(lines, &mut function_instances, &mut unary_instances, &mut binary_instances, &mut nary_instances);

        // Define function indexes
        for (j, expr) in lines.iter().enumerate() {
            match expr {
                NessaExpr::CompiledFunctionDefinition(id, _, a, _, b, v) => {
                    let and = Type::And(a.iter().map(|(_, t)| t).cloned().collect());
                    
                    if let Some(usages) = function_instances.get(id) {
                        for ov in usages {
                            // Find function overload id
                            for (i, (_, i_t, _, _)) in self.functions[*id].overloads.iter().enumerate() {
                                if *i_t == and {
                                    functions.entry((*id, i, ov.clone())).or_insert(program_size);
                                    registers.insert(j, *v);
                                    break;
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b) + a.len();
                        }
                    }
                },
                
                NessaExpr::CompiledPrefixOperationDefinition(id, _, _, t, _, b, v) |
                NessaExpr::CompiledPostfixOperationDefinition(id, _, _, t, _, b, v) => {
                    if let Some(usages) = unary_instances.get(id) {
                        for ov in usages {
                            // Find unary operator overload id
                            if let Operator::Unary{operations, ..} = &self.unary_ops[*id] {
                                for (i, (_, i_t, _, _)) in operations.iter().enumerate() {
                                    if i_t == t {
                                        unary.entry((*id, i, ov.clone())).or_insert(program_size);
                                        registers.insert(j, *v);
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b) + 1;
                        }
                    }
                }
                
                NessaExpr::CompiledBinaryOperationDefinition(id, _, (_, a_t), (_, b_t), _, b, v) => {
                    let and = Type::And(vec!(a_t.clone(), b_t.clone()));

                    if let Some(usages) = binary_instances.get(id) {
                        for ov in usages {
                            // Find binary operator overload id
                            if let Operator::Binary{operations, ..} = &self.binary_ops[*id] {
                                for (i, (_, i_t, _, _)) in operations.iter().enumerate() {
                                    if *i_t == and {
                                        binary.entry((*id, i, ov.clone())).or_insert(program_size);
                                        registers.insert(j, *v);
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b) + 2;
                        }
                    }
                }
                
                NessaExpr::CompiledNaryOperationDefinition(id, _, (_, a_t), a, _, b, v) => {
                    let mut arg_types = vec!(a_t.clone());
                    arg_types.extend(a.iter().map(|(_, t)| t).cloned());

                    let and = Type::And(arg_types);

                    if let Some(usages) = nary_instances.get(id) {
                        for ov in usages {
                            // Find n-ary operator overload id
                            if let Operator::Nary{operations, ..} = &self.nary_ops[*id] {
                                for (i, (_, i_t, _, _)) in operations.iter().enumerate() {
                                    if *i_t == and {
                                        nary.entry((*id, i, ov.clone())).or_insert(program_size);
                                        registers.insert(j, *v);
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b) + a.len() + 1;
                        }
                    }
                }

                _ => {}
            }
        }


        let mut lambda_positions: HashMap<usize, usize> = HashMap::new();
        let mut lambdas = vec!();
        self.compile_lambda(lines, &mut lambdas, &mut lambda_positions, &mut functions, &mut unary, &mut binary, &mut nary)?;

        let mut res = vec!(NessaInstruction::from(CompiledNessaExpr::Jump(program_size + lambdas.len())));

        // Define functions
        for (j, expr) in lines.iter().enumerate() {
            match expr {
                NessaExpr::CompiledFunctionDefinition(id, _, a, r, b, _) => {
                    if let Some(usages) = function_instances.get(id) {
                        for ov in usages {
                            // Store parameters
                            for i in 0..a.len(){
                                if i == 0 {
                                    let comment = format!(
                                        "fn {}{}({}) -> {}",
                                        self.functions[*id].name,
                                        if ov.is_empty() { "".into() } else { format!("<{}>", ov.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")) },
                                        a.iter().map(|(_, t)| t.get_name(self)).collect::<Vec<_>>().join(", "),
                                        r.get_name(self)
                                    );

                                    res.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(i), comment));

                                } else {
                                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(i)));
                                }
                            }

                            // Substitute type parameters if it is necessary
                            if ov.is_empty() {
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);                                
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);                                
                            }
                        }
                    }
                },

                NessaExpr::CompiledPrefixOperationDefinition(id, _, _, t, r, b, _) |
                NessaExpr::CompiledPostfixOperationDefinition(id, _, _, t, r, b, _) => {
                    if let Some(usages) = unary_instances.get(id) {
                        for ov in usages {
                            let mut rep = String::new();
                            let mut is_prefix = false;
        
                            if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*id] {
                                rep = representation.clone();
                                is_prefix = *prefix;
                            }
        
                            // Store parameter
                            let comment;
        
                            if is_prefix {
                                comment = format!("op {}({}) -> {}", rep, t.get_name(self), r.get_name(self));
        
                            } else {
                                comment = format!("op ({}){} -> {}", t.get_name(self), rep, r.get_name(self));
                            }
                            
                            res.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(0), comment));

                            // Substitute type parameters if it is necessary
                            if ov.is_empty() {
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);                                
                            }
                        }
                    }
                },

                NessaExpr::CompiledBinaryOperationDefinition(id, _, (_, t1), (_, t2), r, b, _) => {
                    if let Some(usages) = binary_instances.get(id) {
                        for ov in usages {
                            // Store parameter
                            let mut rep = String::new();

                            if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                                rep = representation.clone();
                            }

                            let comment = format!("op ({}) {} ({}) -> {}", t1.get_name(self), rep, t2.get_name(self), r.get_name(self));

                            res.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(0), comment));
                            res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(1)));

                            // Substitute type parameters if it is necessary
                            if ov.is_empty() {
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);                                
                            }
                        }
                    }
                },

                NessaExpr::CompiledNaryOperationDefinition(id, _, (_, t), a, r, b, _) => {
                    if let Some(usages) = nary_instances.get(id) {
                        for ov in usages {
                            // Store parameters
                            let mut o_rep = String::new();
                            let mut c_rep = String::new();

                            if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                                o_rep = open_rep.clone();
                                c_rep = close_rep.clone();
                            }

                            for i in 0..=a.len(){
                                if i == 0 {
                                    let comment = format!(
                                        "op ({}){}{}{} -> {}", 
                                        t.get_name(self),
                                        o_rep, 
                                        a.iter().map(|(_, t)| t.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        c_rep,
                                        r.get_name(self)
                                    );

                                    res.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(i), comment));

                                } else {
                                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(i)));
                                }
                            }

                            // Substitute type parameters if it is necessary
                            if ov.is_empty() {
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions, *registers.get(&j).unwrap())?);                                
                            }
                        }
                    }
                },

                _ => {}
            }
        }

        res.extend(lambdas);

        // Define everything else
        for expr in lines {
            match expr {
                NessaExpr::CompiledFunctionDefinition(..) | 
                NessaExpr::CompiledPrefixOperationDefinition(..) |
                NessaExpr::CompiledPostfixOperationDefinition(..) |
                NessaExpr::CompiledBinaryOperationDefinition(..) |
                NessaExpr::CompiledNaryOperationDefinition(..) => {},

                _ => res.extend(self.compiled_form_expr(expr, &functions, &unary, &binary, &nary, &lambda_positions, max_register)?)
            }
        }

        res.push(NessaInstruction::new(CompiledNessaExpr::Halt, "End of the program".into()));

        return Ok(res);
    }

    pub fn compiled_form_size(&self, expr: &NessaExpr) -> usize {
        use NessaExpr::*;

        return match expr {
            Literal(_) | Variable(..) | CompiledLambda(..) => 1, 
            BinaryOperation(_, _, a, b) => self.compiled_form_size(a) + self.compiled_form_size(b) + 1,
            NaryOperation(_, _, a, b) => self.compiled_form_size(a) + self.compiled_form_body_size(b) + 1,
            Return(e) | CompiledVariableDefinition(_, _, _, e) | CompiledVariableAssignment(_, _, _, e) | UnaryOperation(_, _, e) => self.compiled_form_size(e) + 1,
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
            CompiledFor(_, _, _, c, b) => self.compiled_form_size(c) + self.compiled_form_body_size(b) + 9,
            While(c, b) => self.compiled_form_size(c) + self.compiled_form_body_size(b) + 2,
            FunctionCall(_, _, a) => self.compiled_form_body_size(a) + 1, 
            _ => unreachable!()
        }
    }

    pub fn compiled_form_body_size(&self, lines: &Vec<NessaExpr>) -> usize {
        return lines.iter().map(|i| self.compiled_form_size(i)).sum();
    }

    pub fn compiled_form_expr(
        &self, expr: &NessaExpr, 
        functions: &HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        binary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        nary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        lambda_positions: &HashMap<usize, usize>, 
        max_register: usize
    ) -> Result<Vec<NessaInstruction>, String> {
        return match expr {
            NessaExpr::Literal(obj) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(obj.clone())))),

            NessaExpr::CompiledLambda(i, a, r, _, m) => {
                Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(Object::new((
                    *lambda_positions.get(i).unwrap(),
                    *m,
                    Type::And(a.iter().map(|(_, t)| t).cloned().collect()),
                    r.clone()
                ))))))
            },

            NessaExpr::Tuple(e) => {
                let mut types = Vec::with_capacity(e.len());
                let mut res = vec!();

                for i in e.iter().rev() {
                    types.push(self.infer_type(i).unwrap());
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, max_register)?);
                }

                res.push(NessaInstruction::from(CompiledNessaExpr::Tuple(types)));

                Ok(res)
            }

            NessaExpr::Variable(id, _, _) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::GetVariable(*id)))), 
            NessaExpr::CompiledVariableDefinition(id, _, _, e) | NessaExpr::CompiledVariableAssignment(id, _, _, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, max_register)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*id)));

                Ok(res)
            },

            NessaExpr::UnaryOperation(id, t, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, max_register)?;

                let i_t = self.infer_type(e).unwrap();
                let (ov_id, _, native, _) = self.get_first_unary_op(*id, i_t, false).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::UnaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = unary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },

            NessaExpr::BinaryOperation(id, t, a, b) => {
                let mut res = self.compiled_form_expr(b, functions, unary, binary, nary, lambda_positions, max_register)?;
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, max_register)?);
                
                let a_t = self.infer_type(a).unwrap();
                let b_t = self.infer_type(b).unwrap();

                let (ov_id, _, native, _) = self.get_first_binary_op(*id, a_t, b_t, false).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::BinaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = binary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },

            NessaExpr::NaryOperation(id, tm, a, b) => {
                let mut res = vec!();

                for i in b.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, max_register)?);
                }
                
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, max_register)?);

                let a_t = self.infer_type(a).unwrap();
                let b_t = b.iter().map(|i| self.infer_type(i).unwrap()).collect();

                let (ov_id, _, native, _) = self.get_first_nary_op(*id, a_t, b_t, false).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NaryOperatorCall(*id, ov_id, tm.clone())));

                } else {
                    let pos = nary.get(&(*id, ov_id, tm.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },

            NessaExpr::If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_expr(ih, functions, unary, binary, nary, lambda_positions, max_register)?;
                let if_body = self.compiled_form_body(ib, functions, unary, binary, nary, lambda_positions, max_register)?;

                res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(if_body.len() + 1)));
                res.extend(if_body);

                for (h, b) in ei {
                    res.extend(self.compiled_form_expr(h, functions, unary, binary, nary, lambda_positions, max_register)?);

                    let elif_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions, max_register)?;

                    res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(elif_body.len() + 1)));
                    res.extend(elif_body);
                }

                if let Some(b) = e {
                    res.extend(self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions, max_register)?);
                }

                Ok(res)
            },
            NessaExpr::While(c, b) => {
                // Start with the condition
                let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, lambda_positions, max_register)?;
                let while_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions, max_register)?;

                // Add while body
                let beginning_jmp = CompiledNessaExpr::RelativeJump(-(while_body.len() as i32 + res.len() as i32 + 1));

                res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(while_body.len() + 2)));
                res.extend(while_body);

                // Jump to the beginning of the loop
                res.push(NessaInstruction::from(beginning_jmp));

                Ok(res)
            },
            NessaExpr::CompiledFor(it_var_id, elem_var_id, _, c, b) => {
                if let Some(t) = self.infer_type(c) {
                    let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, lambda_positions, max_register)?;

                    // Get "iterator", "next" and "is_consumed" function overloads and check them
                    if let Some((it_ov_id, it_type, it_native, it_args)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(t.clone()), true) {
                        let it_mut = Type::MutRef(Box::new(it_type.clone()));

                        if let Some((next_ov_id, _, next_native, next_args)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), true) {
                            if let Some((consumed_ov_id, consumed_res, consumed_native, consumed_args)) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), true) {
                                if let Type::Basic(2) = consumed_res {
                                    let for_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions, max_register)?;

                                    // Convert the iterable into an iterator
                                    if it_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(ITERATOR_FUNC_ID, it_ov_id, it_args)));
    
                                    } else {
                                        let pos = functions.get(&(ITERATOR_FUNC_ID, it_ov_id, it_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                                    }

                                    // Store the iterator
                                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*it_var_id)));

                                    // Check end of iterator
                                    res.push(NessaInstruction::from(CompiledNessaExpr::GetVariable(*it_var_id)));

                                    if consumed_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(IS_CONSUMED_FUNC_ID, consumed_ov_id, consumed_args)));
    
                                    } else {
                                        let pos = functions.get(&(7, consumed_ov_id, consumed_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                                    }                                    

                                    // Jump to end of loop
                                    res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfTrue(for_body.len() + 5)));

                                    // Get next value
                                    res.push(NessaInstruction::from(CompiledNessaExpr::GetVariable(*it_var_id)));

                                    if next_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(NEXT_FUNC_ID, next_ov_id, next_args)));
    
                                    } else {
                                        let pos = functions.get(&(6, next_ov_id, next_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                                    }

                                    // Store next value
                                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*elem_var_id)));

                                    // Add for body
                                    let beginning_jmp = CompiledNessaExpr::RelativeJump(-(for_body.len() as i32 + 6));

                                    res.extend(for_body);

                                    // Jump to the beginning of the loop
                                    res.push(NessaInstruction::from(beginning_jmp));
    
                                    Ok(res)

                                } else {
                                    Err(format!("Funtion overload is_consumed({}) returns {} (expected Bool)", it_mut.get_name(self), consumed_res.get_name(self)))
                                }

                            } else {
                                Err(format!("Funtion overload for is_consumed({}) is not defined", it_mut.get_name(self)))
                            }

                        } else {
                            Err(format!("Funtion overload for next({}) is not defined", it_mut.get_name(self)))
                        }

                    } else {
                        Err(format!("Funtion overload for iterator({}) is not defined", t.get_name(self)))
                    }
                    
                } else {
                    Err("Container expression does not return a valid value".into())
                }
            },
            NessaExpr::Return(e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, max_register)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::Return));

                Ok(res)
            },
            NessaExpr::FunctionCall(id, t, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, max_register)?);
                }
                
                let args_types = a.iter().map(|i| self.infer_type(i).unwrap()).collect();
                let (ov_id, _, native, _) = self.get_first_function_overload(*id, args_types, false).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(*id, ov_id, t.clone())));

                } else {
                    let pos = functions.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            }
            _ => { Ok(vec!()) }
        };
    }

    pub fn compiled_form_body(
        &self, lines: &Vec<NessaExpr>, 
        functions: &HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        binary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        nary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        lambda_positions: &HashMap<usize, usize>,
        max_register: usize
    ) -> Result<Vec<NessaInstruction>, String> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, max_register)).flat_map(|i| i.unwrap()).collect());
    }

    pub fn define_module_classes(&mut self, code: &String) -> Result<(), String> {
        let mut needed = true;

        while needed {
            needed = false;

            for i in self.nessa_class_parser(code).unwrap().1 {
                match i {
                    NessaExpr::ClassDefinition(n, _, _, _) if self.type_templates.iter().filter(|t| t.name == n).next().is_some() => {},
                    NessaExpr::ClassDefinition(n, t, a, p) => {
                        needed = true; // Repeat class parsing after creating a new one

                        self.implicit_syntax_check(&n, &t, &a, &p)?;

                        let n_templates = t.len();
                        let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                        
                        self.define_type(n.clone(), t, a.clone(), p, Some(
                            |ctx, c_type, s| {
                                if let Ok((_, o)) = ctx.parse_literal_type(c_type, s.as_str()) {
                                    return Ok(o);
                                }
    
                                return Err(format!("Unable to parse {} from {}", c_type.name, s));
                            }
                        ))?;
    
                        self.define_function(n.clone()).unwrap_or_default(); // Define constructor function
    
                        let func_id = self.functions.iter().filter(|i| i.name == n).next().unwrap().id;
                        let class_id = self.type_templates.last().unwrap().id;
    
                        if n_templates == 0 {
                            // Define constructor instance
                            self.define_native_function_overload(func_id, 0, &arg_types, Type::Basic(class_id), |_, r, a| {
                                if let Type::Basic(id) = r {
                                    return Ok(Object::new(TypeInstance {
                                        id: *id,
                                        params: vec!(),
                                        attributes: a
                                    }))
                                }
    
                                unreachable!();
                            })?;
                            
                            // Define constructor meber access
                            for (i, (att_name, att_type)) in a.into_iter().enumerate() {
                                self.define_function(att_name.clone()).unwrap_or_default(); // Define accesor function
                                let att_func_id = self.functions.iter().filter(|i| i.name == *att_name).next().unwrap().id;
    
                                let ref_type = match &att_type {
                                    Type::MutRef(t) => Type::Ref(t.clone()),
                                    Type::Ref(t) => Type::Ref(t.clone()),
                                    t => Type::Ref(Box::new(t.clone()))
                                };
    
                                let mut_type = match &att_type {
                                    Type::MutRef(t) => Type::MutRef(t.clone()),
                                    Type::Ref(t) => Type::Ref(t.clone()),
                                    t => Type::MutRef(Box::new(t.clone()))
                                };
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, 0, &[Type::Basic(class_id)], att_type.clone(), match i {
                                        #( N => |_, _, a| Ok(a[0].get::<TypeInstance>().attributes[N].clone()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, 0, &[Type::Ref(Box::new(Type::Basic(class_id)))], ref_type, match i {
                                        #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_obj()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, 0, &[Type::MutRef(Box::new(Type::Basic(class_id)))], mut_type, match i {
                                        #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_mut_obj()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
                            }
    
                        } else {
                            let templ = (0..n_templates).into_iter().map(|i| Type::TemplateParam(i)).collect::<Vec<_>>();
    
                            // Define constructor instance
                            self.define_native_function_overload(func_id, n_templates, &arg_types, Type::Template(class_id, templ.clone()), |t, r, a| {
                                if let Type::Template(id, _) = r {
                                    return Ok(Object::new(TypeInstance {
                                        id: *id,
                                        params: t.clone(),
                                        attributes: a
                                    }))
                                }
    
                                unreachable!();
                            })?;
    
                            for (i, (att_name, att_type)) in a.into_iter().enumerate() {
                                self.define_function(att_name.clone()).unwrap_or_default(); // Define accesor function
                                let att_func_id = self.functions.iter().filter(|i| i.name == *att_name).next().unwrap().id;
    
                                let ref_type = match &att_type {
                                    Type::MutRef(t) => Type::Ref(t.clone()),
                                    Type::Ref(t) => Type::Ref(t.clone()),
                                    t => Type::Ref(Box::new(t.clone()))
                                };
    
                                let mut_type = match &att_type {
                                    Type::MutRef(t) => Type::MutRef(t.clone()),
                                    Type::Ref(t) => Type::Ref(t.clone()),
                                    t => Type::MutRef(Box::new(t.clone()))
                                };
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, n_templates, &[Type::Template(class_id, templ.clone())], att_type.clone(), match i {
                                        #( N => |_, _, a| Ok(a[0].get::<TypeInstance>().attributes[N].clone()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, n_templates, &[Type::Ref(Box::new(Type::Template(class_id, templ.clone())))], ref_type.clone(), match i {
                                        #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_obj()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
    
                                seq!(N in 0..100 {
                                    self.define_native_function_overload(att_func_id, n_templates, &[Type::MutRef(Box::new(Type::Template(class_id, templ.clone())))], mut_type.clone(), match i {
                                        #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_mut_obj()), )*
                                        _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                                    })?;
                                });
                            }
                        }
                    },
    
                    _ => unreachable!()
                }
            }
        }

        return Ok(());
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
            self.define_function(i.0).unwrap_or_default();
        }

        return Ok(());
    }
    
    pub fn define_module_operations(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_operations_parser(code).unwrap().1;

        for i in ops {
            match i {
                NessaExpr::PrefixOperationDefinition(id, tm, _a, t, r, _) => self.define_unary_operation(id, tm.len(), t, r, None)?,
                NessaExpr::PostfixOperationDefinition(id, tm, _a, t, r, _) => self.define_unary_operation(id, tm.len(), t, r, None)?,
                NessaExpr::BinaryOperationDefinition(id, tm, (_a, ta), (_b, tb), r, _) => self.define_binary_operation(id, tm.len(), ta, tb, r, None)?,
                NessaExpr::NaryOperationDefinition(id, tm, (_a, ta), v, r, _) => self.define_nary_operation(id, tm.len(), ta, &v.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(), r, None)?,

                _ => unreachable!()
            }
        }

        return Ok(());
    }

    pub fn define_module_function_overloads(&mut self, lines: &Vec<NessaExpr>) -> Result<(), String> {
        for i in lines {
            match i {
                NessaExpr::FunctionDefinition(id, t, a, r, _) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    self.define_function_overload(*id, t.len(), &arg_types, r.clone(), None)?
                },

                _ => {}
            }
        }

        return Ok(());
    }

    pub fn parse_nessa_module(&mut self, code: &String) -> Vec<NessaExpr> {
        return self.nessa_parser(code).unwrap().1;
    }

    pub fn parse_and_compile(&mut self, code: &String) -> Result<Vec<NessaInstruction>, String> {
        self.define_module_classes(code)?;
        self.define_module_operators(code)?;
        self.define_module_functions(code)?;
        self.define_module_operations(code)?;

        let mut lines = self.parse_nessa_module(code);
        self.define_module_function_overloads(&lines)?;

        let max_register = self.compile(&mut lines, &vec!())?;

        for expr in &lines {
            self.static_check(expr)?;
        }

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

            if let NessaExpr::UnaryOperation(_, _, e2) = e.as_ref() {
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

                if let NessaExpr::BinaryOperation(_, _, a, b) = e.as_ref() {
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
        let mut ctx = standard_ctx();
        
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

        assert!(ctx.compile_functions(&mut code).is_ok());
        
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