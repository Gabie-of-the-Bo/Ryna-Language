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
            NessaExpr::UnaryOperation(_, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            NessaExpr::BinaryOperation(_, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;
                self.compile_expr_variables(b, registers, ctx_idx, curr_ctx)?;
            }
            
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;

                for i in b {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;
                }
            }

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

    fn compile_expr_function_names(&self, expr: &mut NessaExpr) {
        match expr {
            // Compile function name references
            NessaExpr::NameReference(n) => {
                if let Some(f) = self.get_func_name(n) {
                    *expr = NessaExpr::FunctionName(f.id);
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, e) |
            NessaExpr::VariableAssignment(_, e) => {
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

    fn compile_expr_function_calls(&self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, e) |
            NessaExpr::VariableAssignment(_, e) => {
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
            NessaExpr::NaryOperation(0, t, a, b) => {
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

    fn compile_expr_function_bodies(&self, expr: &mut NessaExpr) -> Result<(), String> {
        match expr {
            NessaExpr::FunctionDefinition(id, t, a, r, b) => {
                let max_var = self.compile(b, &a)?;

                *expr = NessaExpr::CompiledFunctionDefinition(*id, t.clone(), a.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::PrefixOperationDefinition(id, n, t, r, b) => {
                let max_var = self.compile(b, &vec!((n.clone(), t.clone())))?;

                *expr = NessaExpr::CompiledPrefixOperationDefinition(*id, n.clone(), t.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::PostfixOperationDefinition(id, n, t, r, b) => {
                let max_var = self.compile(b, &vec!((n.clone(), t.clone())))?;

                *expr = NessaExpr::CompiledPostfixOperationDefinition(*id, n.clone(), t.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::BinaryOperationDefinition(id, a1, a2, r, b) => {
                let max_var = self.compile(b, &vec!(a1.clone(), a2.clone()))?;

                *expr = NessaExpr::CompiledBinaryOperationDefinition(*id, a1.clone(), a2.clone(), r.clone(), b.clone(), max_var);
            },

            NessaExpr::NaryOperationDefinition(id, a, args, r, b) => {
                let mut all_args = vec!(a.clone());
                all_args.extend(args.iter().cloned());

                let max_var = self.compile(b, &all_args)?;

                *expr = NessaExpr::CompiledNaryOperationDefinition(*id, a.clone(), args.clone(), r.clone(), b.clone(), max_var);
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

    StoreVariable(usize),
    GetVariable(usize),

    Jump(usize),
    RelativeJump(i32),
    RelativeJumpIfFalse(usize),
    RelativeJumpIfTrue(usize),
    Call(usize, usize),
    Return,

    NativeFunctionCall(usize, usize, Vec<Type>),
    UnaryOperatorCall(usize, usize),
    BinaryOperatorCall(usize, usize),
    NaryOperatorCall(usize, usize),

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
    fn add_function_instance(map: &mut HashMap<usize, Vec<Vec<Type>>>, id: usize, args: &Vec<Type>){
        let curr = map.entry(id).or_default();

        for i in curr.iter() {
            if i == args {
                return;
            }
        }

        curr.push(args.clone());
    }

    fn merge_function_instances(a: &mut HashMap<usize, Vec<Vec<Type>>>, b: HashMap<usize, Vec<Vec<Type>>>){
        for (id, inst) in b {
            for args in inst {
                NessaContext::add_function_instance(a, id, &args);
            }
        }
    }

    pub fn get_templated_function_instances(&self, expr: &NessaExpr) -> HashMap<usize, Vec<Vec<Type>>> {
        return match expr {
            NessaExpr::CompiledVariableDefinition(_, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) | 
            NessaExpr::UnaryOperation(_, e) |
            NessaExpr::Return(e) => self.get_templated_function_instances(e), 

            NessaExpr::BinaryOperation(_, a, b) => {
                let mut res = self.get_templated_function_instances(a);

                NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(b));

                res
            }

            NessaExpr::NaryOperation(_, _, a, b) => {
                let mut res = self.get_templated_function_instances(a);

                for arg in b {
                    NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(arg));
                }

                res
            }

            NessaExpr::If(i, ib, ei, eb) => {
                let mut res = self.get_templated_function_instances(i);

                for line in ib {
                    NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(line));
                }

                for (ei_h, ei_b) in ei {
                    NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(ei_h));

                    for line in ei_b {
                        NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(line));
                    }
                }

                if let Some(b) = eb {
                    for line in b {
                        NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(line));
                    }
                }

                res
            }

            NessaExpr::CompiledFor(_, _, _, c, b) | 
            NessaExpr::While(c, b) => {
                let mut res = self.get_templated_function_instances(c);

                for line in b {
                    NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(line));
                }

                res
            }

            NessaExpr::FunctionCall(id, t, args) => {
                let mut res = HashMap::new();

                NessaContext::add_function_instance(&mut res, *id, t);

                for arg in args {
                    NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(arg));
                }

                res
            }

            NessaExpr::CompiledFunctionDefinition(id, t, _, _, _, _) => {
                let mut res = HashMap::new();

                if t.is_empty() {
                    NessaContext::add_function_instance(&mut res, *id, &vec!());
                }

                res
            }

            _ => HashMap::new()
        }
    }

    pub fn get_templated_function_instances_module(&self, lines: &Vec<NessaExpr>) -> HashMap<usize, Vec<Vec<Type>>> {
        let mut res = HashMap::new();

        for line in lines {
            NessaContext::merge_function_instances(&mut res, self.get_templated_function_instances(line));
        }

        return res;
    }

    pub fn subtitute_type_params_expr(&self, expr: &mut NessaExpr, templates: &HashMap<usize, Type>) {
        match expr {
            NessaExpr::Variable(_, _, t) => *t = t.sub_templates(templates),

            NessaExpr::CompiledVariableAssignment(_, _, _, e) |
            NessaExpr::UnaryOperation(_, e) |
            NessaExpr::Return(e) => self.subtitute_type_params_expr(e, templates),

            NessaExpr::CompiledVariableDefinition(_, _, t, e) => {
                *t = t.sub_templates(templates);
                self.subtitute_type_params_expr(e, templates);
            },
            
            NessaExpr::FunctionCall(_, t, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));
                args.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },
            
            NessaExpr::CompiledFor(_, _, _, container, body) |
            NessaExpr::While(container, body) |
            NessaExpr::NaryOperation(_, _, container, body) => {
                self.subtitute_type_params_expr(container, templates);
                body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },

            NessaExpr::BinaryOperation(_, a, b) => {
                self.subtitute_type_params_expr(a, templates);
                self.subtitute_type_params_expr(b, templates);
            }

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
            
            _ => unimplemented!()
        };
    }

    pub fn compiled_form(&self, lines: &Vec<NessaExpr>, max_register: usize) -> Result<Vec<NessaInstruction>, String> {
        let mut program_size = 1;
        let mut functions: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut unary: HashMap<(usize, usize), usize> = HashMap::new();
        let mut binary: HashMap<(usize, usize), usize> = HashMap::new();
        let mut nary: HashMap<(usize, usize), usize> = HashMap::new();

        let mut registers: HashMap<usize, usize> = HashMap::new();

        let function_instances = self.get_templated_function_instances_module(lines);

        // Define function indexes
        for (j, expr) in lines.iter().enumerate() {
            match expr {
                NessaExpr::CompiledFunctionDefinition(id, _, a, _, b, v) => {
                    if let Some(usages) = function_instances.get(id) {
                        for ov in usages {
                            let and = Type::And(a.iter().map(|(_, t)| t).cloned().collect());

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
                
                NessaExpr::CompiledPrefixOperationDefinition(id, _, t, _, b, v) |
                NessaExpr::CompiledPostfixOperationDefinition(id, _, t, _, b, v) => {
                    // Find unary operator overload id
                    if let Operator::Unary{operations, ..} = &self.unary_ops[*id] {
                        for (i, (i_t, _, _)) in operations.iter().enumerate() {
                            if i_t == t {
                                unary.entry((*id, i)).or_insert(program_size);
                                registers.insert(j, *v);
                                break;
                            }
                        }
                    }
    
                    program_size += self.compiled_form_body_size(b) + 1;
                }
                
                NessaExpr::CompiledBinaryOperationDefinition(id, (_, a_t), (_, b_t), _, b, v) => {
                    let and = Type::And(vec!(a_t.clone(), b_t.clone()));

                    // Find binary operator overload id
                    if let Operator::Binary{operations, ..} = &self.binary_ops[*id] {
                        for (i, (i_t, _, _)) in operations.iter().enumerate() {
                            if *i_t == and {
                                binary.entry((*id, i)).or_insert(program_size);
                                registers.insert(j, *v);
                                break;
                            }
                        }
                    }
    
                    program_size += self.compiled_form_body_size(b) + 2;
                }
                
                NessaExpr::CompiledNaryOperationDefinition(id, (_, a_t), a, _, b, v) => {
                    let mut arg_types = vec!(a_t.clone());
                    arg_types.extend(a.iter().map(|(_, t)| t).cloned());

                    let and = Type::And(arg_types);

                    // Find n-ary operator overload id
                    if let Operator::Nary{operations, ..} = &self.nary_ops[*id] {
                        for (i, (i_t, _, _)) in operations.iter().enumerate() {
                            if *i_t == and {
                                nary.entry((*id, i)).or_insert(program_size);
                                registers.insert(j, *v);
                                break;
                            }
                        }
                    }
    
                    program_size += self.compiled_form_body_size(b) + a.len() + 1;
                }

                _ => {}
            }
        }

        let mut res = vec!(NessaInstruction::from(CompiledNessaExpr::Jump(program_size)));

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
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, *registers.get(&j).unwrap())?);                                
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, *registers.get(&j).unwrap())?);                                
                            }
            
                        }
                    }
                },

                NessaExpr::CompiledPrefixOperationDefinition(id, _, t, r, b, _) |
                NessaExpr::CompiledPostfixOperationDefinition(id, _, t, r, b, _) => {
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
                    res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, *registers.get(&j).unwrap())?);
                },

                NessaExpr::CompiledBinaryOperationDefinition(id, (_, t1), (_, t2), r, b, _) => {
                    // Store parameter
                    let mut rep = String::new();

                    if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                        rep = representation.clone();
                    }

                    let comment = format!("op ({}) {} ({}) -> {}", t1.get_name(self), rep, t2.get_name(self), r.get_name(self));

                    res.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(0), comment));
                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(1)));
                    res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, *registers.get(&j).unwrap())?);
                },

                NessaExpr::CompiledNaryOperationDefinition(id, (_, t), a, r, b, _) => {
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
    
                    res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, *registers.get(&j).unwrap())?);
                },

                _ => {}
            }
        }

        // Define everything else
        for expr in lines {
            match expr {
                NessaExpr::CompiledFunctionDefinition(..) | 
                NessaExpr::CompiledPrefixOperationDefinition(..) |
                NessaExpr::CompiledPostfixOperationDefinition(..) |
                NessaExpr::CompiledBinaryOperationDefinition(..) |
                NessaExpr::CompiledNaryOperationDefinition(..) => {},

                _ => res.extend(self.compiled_form_expr(expr, &functions, &unary, &binary, &nary, max_register)?)
            }
        }

        res.push(NessaInstruction::new(CompiledNessaExpr::Halt, "End of the program".into()));

        return Ok(res);
    }

    pub fn compiled_form_size(&self, expr: &NessaExpr) -> usize {
        use NessaExpr::*;

        return match expr {
            Literal(_) | Variable(..) => 1, 
            BinaryOperation(_, a, b) => self.compiled_form_size(a) + self.compiled_form_size(b) + 1,
            NaryOperation(_, _, a, b) => self.compiled_form_size(a) + self.compiled_form_body_size(b) + 1,
            Return(e) | CompiledVariableDefinition(_, _, _, e) | CompiledVariableAssignment(_, _, _, e) | UnaryOperation(_, e) => self.compiled_form_size(e) + 1,
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
        unary: &HashMap<(usize, usize), usize>, 
        binary: &HashMap<(usize, usize), usize>, 
        nary: &HashMap<(usize, usize), usize>, 
        max_register: usize
    ) -> Result<Vec<NessaInstruction>, String> {
        return match expr {
            NessaExpr::Literal(obj) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(obj.clone())))),
            NessaExpr::Variable(id, _, _) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::GetVariable(*id)))), 
            NessaExpr::CompiledVariableDefinition(id, _, _, e) | NessaExpr::CompiledVariableAssignment(id, _, _, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, max_register)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*id)));

                Ok(res)
            },
            NessaExpr::UnaryOperation(id, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, max_register)?;

                let t = self.infer_type(e).unwrap();
                let (ov_id, _, native) = self.get_first_unary_op(*id, t).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::UnaryOperatorCall(*id, ov_id)));

                } else {
                    let pos = unary.get(&(*id, ov_id)).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },
            NessaExpr::BinaryOperation(id, a, b) => {
                let mut res = self.compiled_form_expr(b, functions, unary, binary, nary, max_register)?;
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, max_register)?);
                
                let a_t = self.infer_type(a).unwrap();
                let b_t = self.infer_type(b).unwrap();

                let (ov_id, _, native) = self.get_first_binary_op(*id, a_t, b_t).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::BinaryOperatorCall(*id, ov_id)));

                } else {
                    let pos = binary.get(&(*id, ov_id)).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },
            NessaExpr::NaryOperation(id, _, a, b) => {
                let mut res = vec!();

                for i in b.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, max_register)?);
                }
                
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, max_register)?);

                let a_t = self.infer_type(a).unwrap();
                let b_t = b.iter().map(|i| self.infer_type(i).unwrap()).collect();

                let (ov_id, _, native) = self.get_first_nary_op(*id, a_t, b_t).unwrap();

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NaryOperatorCall(*id, ov_id)));

                } else {
                    let pos = nary.get(&(*id, ov_id)).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos, max_register)));
                }

                Ok(res)
            },
            NessaExpr::If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_expr(ih, functions, unary, binary, nary, max_register)?;
                let if_body = self.compiled_form_body(ib, functions, unary, binary, nary, max_register)?;

                res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(if_body.len() + 1)));
                res.extend(if_body);

                for (h, b) in ei {
                    res.extend(self.compiled_form_expr(h, functions, unary, binary, nary, max_register)?);

                    let elif_body = self.compiled_form_body(b, functions, unary, binary, nary, max_register)?;

                    res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(elif_body.len() + 1)));
                    res.extend(elif_body);
                }

                if let Some(b) = e {
                    res.extend(self.compiled_form_body(b, functions, unary, binary, nary, max_register)?);
                }

                Ok(res)
            },
            NessaExpr::While(c, b) => {
                // Start with the condition
                let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, max_register)?;
                let while_body = self.compiled_form_body(b, functions, unary, binary, nary, max_register)?;

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
                    let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, max_register)?;

                    // Get "iterator", "next" and "is_consumed" function overloads and check them
                    if let Some((it_ov_id, it_type, it_native, it_args)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(t.clone())) {
                        let it_mut = Type::MutRef(Box::new(it_type.clone()));

                        if let Some((next_ov_id, _, next_native, next_args)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone())) {
                            if let Some((consumed_ov_id, consumed_res, consumed_native, consumed_args)) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone())) {
                                if let Type::Basic(2) = consumed_res {
                                    let for_body = self.compiled_form_body(b, functions, unary, binary, nary, max_register)?;

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
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, max_register)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::Return));

                Ok(res)
            },
            NessaExpr::FunctionCall(id, t, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, max_register)?);
                }
                
                let args_types = a.iter().map(|i| self.infer_type(i).unwrap()).collect();
                let (ov_id, _, native, _) = self.get_first_function_overload(*id, args_types).unwrap();

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
        unary: &HashMap<(usize, usize), usize>, 
        binary: &HashMap<(usize, usize), usize>, 
        nary: &HashMap<(usize, usize), usize>, 
        max_register: usize
    ) -> Result<Vec<NessaInstruction>, String> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, functions, unary, binary, nary, max_register)).flat_map(|i| i.unwrap()).collect());
    }

    pub fn define_module_classes(&mut self, code: &String) -> Result<(), String> {
        let ops = self.nessa_class_parser(code).unwrap().1;

        for i in ops {
            match i {
                NessaExpr::ClassDefinition(n, t, a) => {
                    let n_templates = t.len();
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                    self.define_type(n.clone(), t, a.clone())?;
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
                        unimplemented!();
                    }
                },

                _ => unreachable!()
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
            // TODO: create functions from bodies
            match i {
                NessaExpr::PrefixOperationDefinition(id, _a, t, r, _) => self.define_unary_operation(id, t, r, None)?,
                NessaExpr::PostfixOperationDefinition(id, _a, t, r, _) => self.define_unary_operation(id, t, r, None)?,
                NessaExpr::BinaryOperationDefinition(id, (_a, ta), (_b, tb), r, _) => self.define_binary_operation(id, ta, tb, r, None)?,
                NessaExpr::NaryOperationDefinition(id, (_a, ta), v, r, _) => self.define_nary_operation(id, ta, &v.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(), r, None)?,

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