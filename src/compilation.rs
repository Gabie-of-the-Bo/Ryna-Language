use std::collections::{ HashMap, HashSet };

use seq_macro::seq;

use crate::config::ImportMap;
use crate::config::Imports;
use crate::config::InnerDepGraph;
use crate::functions::Function;
use crate::context::NessaContext;
use crate::graph::DirectedGraph;
use crate::parser::*;
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
                self.compile(b, &a)?;

                *expr = NessaExpr::CompiledLambda(self.lambdas, a.clone(), r.clone(), b.clone());
                self.lambdas += 1;
            },

            NessaExpr::FunctionDefinition(_, _, a, _, b) => {
                self.compile(b, &a)?;                    
            },

            NessaExpr::PrefixOperationDefinition(_, _, n, t, _, b) => {
                self.compile(b, &vec!((n.clone(), t.clone())))?;                    
            },

            NessaExpr::PostfixOperationDefinition(_, _, n, t, _, b) => {
                self.compile(b, &vec!((n.clone(), t.clone())))?;                    
            },

            NessaExpr::BinaryOperationDefinition(_, _, a1, a2, _, b) => {
                self.compile(b, &vec!(a1.clone(), a2.clone()))?;                    
            },

            NessaExpr::NaryOperationDefinition(_, _, a, args, _, b) => {
                let mut all_args = vec!(a.clone());
                all_args.extend(args.iter().cloned());

                self.compile(b, &all_args)?;   
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

    pub fn compile(&mut self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<(), String> {
        self.compile_functions(body)?;
        self.compile_variables(body, args)?;

        return Ok(());
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
    Drop,

    Jump(usize),
    RelativeJump(i32),
    RelativeJumpIfFalse(usize),
    RelativeJumpIfTrue(usize),
    Call(usize),
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
    fn add_template_instance(map: &mut HashMap<usize, Vec<Vec<Type>>>, id: usize, args: &Vec<Type>) -> bool {
        let curr = map.entry(id).or_default();

        for i in curr.iter() {
            if i == args {
                return false;
            }
        }

        curr.push(args.clone());

        return true;
    }

    pub fn get_inner_dep_graph(&mut self, lines: &Vec<NessaExpr>) -> DirectedGraph<(ImportType, usize), ()> {
        let mut res = DirectedGraph::new();
        let mut compiled = lines.clone();

        self.compile(&mut compiled, &vec!()).unwrap(); // TODO: this seems costly...

        self.get_inner_dep_graph_body(&compiled, &(ImportType::Outer, 0), &mut res);

        return res;
    }

    pub fn get_inner_dep_graph_body(&self, lines: &Vec<NessaExpr>, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        for line in lines {
            self.get_inner_dep_graph_expr(line, parent, deps);
        }
    }

    pub fn get_inner_dep_graph_expr(&self, expr: &NessaExpr, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        match expr {
            NessaExpr::Literal(obj) => {
                deps.connect(parent.clone(), (ImportType::Class, obj.get_type_id()), ());
            }

            NessaExpr::Return(e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, e) => {
                self.get_inner_dep_graph_expr(e, parent, deps);
            }

            NessaExpr::Tuple(b) => {
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            NessaExpr::FunctionCall(id, _, args) => {
                deps.connect(parent.clone(), (ImportType::Fn, *id), ());

                self.get_inner_dep_graph_body(args, parent, deps);
            }

            NessaExpr::UnaryOperation(id, _, a) => {
                if let Operator::Unary { id, prefix, .. } = &self.unary_ops[*id] {
                    if *prefix {
                        deps.connect(parent.clone(), (ImportType::Prefix, *id), ());
                    
                    } else {
                        deps.connect(parent.clone(), (ImportType::Postfix, *id), ());
                    }

                    self.get_inner_dep_graph_expr(a, parent, deps);
                }
            }

            NessaExpr::BinaryOperation(id, _, a, b) => {
                if let Operator::Binary { id, .. } = &self.binary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Binary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_expr(b, parent, deps);
                }
            }

            NessaExpr::NaryOperation(id, _, a, b) => {
                if let Operator::Nary { id, .. } = &self.nary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Nary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_body(b, parent, deps);
                }
            }

            NessaExpr::While(c, b) |
            NessaExpr::CompiledFor(_, _, _, c, b) => {
                self.get_inner_dep_graph_expr(c, parent, deps);
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            NessaExpr::If(ic, ib, ie, eb) => {
                self.get_inner_dep_graph_expr(ic, parent, deps);
                self.get_inner_dep_graph_body(ib, parent, deps);

                for (ie_c, ie_b) in ie {
                    self.get_inner_dep_graph_expr(ie_c, parent, deps);
                    self.get_inner_dep_graph_body(ie_b, parent, deps);
                }

                if let Some(b) = eb {
                    self.get_inner_dep_graph_body(b, parent, deps);
                }
            }

            NessaExpr::CompiledLambda(_, _, _, b) => {
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            NessaExpr::FunctionDefinition(id, _, _, _, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Fn, *id), deps);
            }

            NessaExpr::PrefixOperationDefinition(id, _, _, _, _, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Prefix, *id), deps);
            }

            NessaExpr::PostfixOperationDefinition(id, _, _, _, _, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Postfix, *id), deps);
            }

            NessaExpr::BinaryOperationDefinition(id, _, _, _, _, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Binary, *id), deps);
            }

            NessaExpr::NaryOperationDefinition(id, _, _, _, _, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Nary, *id), deps);
            }

            NessaExpr::ClassDefinition(n, _, _, _) => {
                if let Some(t) = self.type_templates.iter().find(|i| i.name == *n) {
                    deps.connect(parent.clone(), (ImportType::Class, t.id), ());

                    // Dependencies from the attributes
                    for (_, tp) in &t.attributes {
                        for t_dep in tp.type_dependencies() {
                            deps.connect((ImportType::Class, t.id), (ImportType::Class, t_dep), ());
                        }
                    }
                }
            },

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
        let mut changed = true;
        
        while changed {
            changed = false;
            self.get_template_calls_body_pass(lines, functions, unary, binary, nary, &mut changed);
        }
    }

    pub fn get_template_calls_pass(
        &self, 
        expr: &NessaExpr, 
        functions: &mut HashMap<usize, Vec<Vec<Type>>>, 
        unary: &mut HashMap<usize, Vec<Vec<Type>>>,
        binary: &mut HashMap<usize, Vec<Vec<Type>>>,
        nary: &mut HashMap<usize, Vec<Vec<Type>>>,
        changed: &mut bool
    ) {
        return match expr {
            NessaExpr::CompiledVariableDefinition(_, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) | 
            NessaExpr::Return(e) => self.get_template_calls_pass(e, functions, unary, binary, nary, changed), 

            NessaExpr::Tuple(e) => self.get_template_calls_body_pass(e, functions, unary, binary, nary, changed),

            NessaExpr::UnaryOperation(id, t, e) => {
                *changed |= NessaContext::add_template_instance(unary, *id, t);

                self.get_template_calls_pass(e, functions, unary, binary, nary, changed);
            }

            NessaExpr::BinaryOperation(id, t, a, b) => {
                *changed |= NessaContext::add_template_instance(binary, *id, t);

                self.get_template_calls_pass(a, functions, unary, binary, nary, changed);
                self.get_template_calls_pass(b, functions, unary, binary, nary, changed);
            },

            NessaExpr::NaryOperation(id, t, a, b) => {
                *changed |= NessaContext::add_template_instance(nary, *id, t);

                self.get_template_calls_pass(a, functions, unary, binary, nary, changed);
                self.get_template_calls_body_pass(b, functions, unary, binary, nary, changed);
            }

            NessaExpr::If(i, ib, ei, eb) => {
                self.get_template_calls_pass(i, functions, unary, binary, nary, changed);
                self.get_template_calls_body_pass(ib, functions, unary, binary, nary, changed);

                for (ei_h, ei_b) in ei {
                    self.get_template_calls_pass(ei_h, functions, unary, binary, nary, changed);
                    self.get_template_calls_body_pass(ei_b, functions, unary, binary, nary, changed);
                }

                if let Some(b) = eb {
                    self.get_template_calls_body_pass(b, functions, unary, binary, nary, changed);
                }
            }

            NessaExpr::CompiledFor(_, _, _, c, b) => {
                self.get_template_calls_pass(c, functions, unary, binary, nary, changed);

                if let Some(ct) = self.infer_type(c) {
                    if let Some((_, it_type, _, it_args)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(ct.clone()), true) {
                        let it_mut = Type::MutRef(Box::new(it_type.clone()));

                        // Implicitly call "iterator", "next" and "is_consumed"
                        if let Some((_, _, _, next_args)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), true) {
                            if let Some((_, _, _, consumed_args)) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), true) {
                                *changed |= NessaContext::add_template_instance(functions, ITERATOR_FUNC_ID, &it_args);
                                *changed |= NessaContext::add_template_instance(functions, NEXT_FUNC_ID, &next_args);
                                *changed |= NessaContext::add_template_instance(functions, IS_CONSUMED_FUNC_ID, &consumed_args);
                            }
                        }
                    }
                }

                for line in b {
                    self.get_template_calls_pass(line, functions, unary, binary, nary, changed);
                }
            }

            NessaExpr::While(c, b) => {
                self.get_template_calls_pass(c, functions, unary, binary, nary, changed);
                self.get_template_calls_body_pass(b, functions, unary, binary, nary, changed);
            }

            NessaExpr::FunctionCall(id, t, args) => {
                *changed |= NessaContext::add_template_instance(functions, *id, t);

                self.get_template_calls_body_pass(args, functions, unary, binary, nary, changed);
            }

            NessaExpr::FunctionDefinition(id, _, _, ret, b) => {
                if let Some(usages) = functions.get(id).cloned() {
                    for ov in usages {
                        // TODO: cache this
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                            // Statically check the newly instantiated functions
                            for line in &body {
                                self.static_check_expected(line, &Some(ret.sub_templates(&templates))).unwrap();
                            }
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::PostfixOperationDefinition(id, _, _, _, ret, b) |
            NessaExpr::PrefixOperationDefinition(id, _, _, _, ret, b) => {
                if let Some(usages) = unary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                            // Statically check the newly instantiated functions
                            for line in &body {
                                self.static_check_expected(line, &Some(ret.sub_templates(&templates))).unwrap();
                            }
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::BinaryOperationDefinition(id, _, _, _, ret, b) => {
                if let Some(usages) = binary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                            // Statically check the newly instantiated functions
                            for line in &body {
                                self.static_check_expected(line, &Some(ret.sub_templates(&templates))).unwrap();
                            }
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::NaryOperationDefinition(id, _, _, _, ret, b) => {
                if let Some(usages) = nary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                            // Statically check the newly instantiated functions
                            for line in &body {
                                self.static_check_expected(line, &Some(ret.sub_templates(&templates))).unwrap();
                            }
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            _ => {}
        }
    }

    pub fn get_template_calls_body_pass(
        &self, 
        lines: &Vec<NessaExpr>, 
        functions: &mut HashMap<usize, Vec<Vec<Type>>>, 
        unary: &mut HashMap<usize, Vec<Vec<Type>>>,
        binary: &mut HashMap<usize, Vec<Vec<Type>>>,
        nary: &mut HashMap<usize, Vec<Vec<Type>>>,
        changed: &mut bool
    ) {
        for line in lines {
            self.get_template_calls_pass(line, functions, unary, binary, nary, changed);
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
        current_size: usize,
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

            NessaExpr::CompiledLambda(i, a, _, b) => {
                self.compile_lambda(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                lambda_positions.entry(*i).or_insert(lambdas.len() + current_size);

                for i in 0..a.len() {
                    if i == 0 {
                        lambdas.push(NessaInstruction::new(CompiledNessaExpr::StoreVariable(i), "Lambda expression start".into()));

                    } else {
                        lambdas.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(i)));
                    }
                }

                lambdas.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions)?);
                
                Ok(())
            }

            NessaExpr::CompiledVariableDefinition(_, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, e) |
            NessaExpr::Return(e) |
            NessaExpr::UnaryOperation(_, _, e) => self.compile_lambda_expr(e, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::BinaryOperation(_, _, a, b) => {
                self.compile_lambda_expr(a, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda_expr(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::CompiledFor(_, _, _, a, b) |
            NessaExpr::While(a, b) |
            NessaExpr::NaryOperation(_, _, a, b) => {
                self.compile_lambda_expr(a, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::If(ih, ib, ei, eb) => {
                self.compile_lambda_expr(ih, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda(ib, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                for (ei_h, ei_b) in ei {
                    self.compile_lambda_expr(ei_h, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                    self.compile_lambda(ei_b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_lambda(eb_inner, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;                    
                }

                Ok(())
            }

            NessaExpr::Tuple(args) |
            NessaExpr::FunctionCall(_, _, args) => self.compile_lambda(args, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::FunctionDefinition(_, _, _, _, b) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, b) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, b) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, b) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, b) => self.compile_lambda(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            _ => unimplemented!("{:?}", line)
        };
    }

    pub fn compile_lambda(
        &self, 
        lines: &Vec<NessaExpr>, 
        current_size: usize,
        lambdas: &mut Vec<NessaInstruction>,
        lambda_positions: &mut HashMap<usize, usize>,
        functions: &mut HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        binary: &mut HashMap<(usize, usize, Vec<Type>), usize>,
        nary: &mut HashMap<(usize, usize, Vec<Type>), usize>
    ) -> Result<(), String> {
        for line in lines {
            self.compile_lambda_expr(line, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
        }

        return Ok(());
    }

    pub fn compiled_form(&self, lines: &Vec<NessaExpr>) -> Result<Vec<NessaInstruction>, String> {
        let mut program_size = 1;
        let mut functions: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut unary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut binary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();
        let mut nary: HashMap<(usize, usize, Vec<Type>), usize> = HashMap::new();

        let mut function_instances = HashMap::new();
        let mut unary_instances = HashMap::new();
        let mut binary_instances = HashMap::new();
        let mut nary_instances = HashMap::new();

        self.get_template_calls_body(lines, &mut function_instances, &mut unary_instances, &mut binary_instances, &mut nary_instances);

        // Define function indexes
        for expr in lines {
            match expr {
                NessaExpr::FunctionDefinition(id, _, a, _, b) => {
                    let and = Type::And(a.iter().map(|(_, t)| t).cloned().collect());
                    
                    if let Some(usages) = function_instances.get(id) {
                        for ov in usages {
                            // Find function overload id
                            for (i, (_, i_t, _, _)) in self.functions[*id].overloads.iter().enumerate() {
                                if *i_t == and {
                                    functions.entry((*id, i, ov.clone())).or_insert(program_size);
                                    break;
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b, true) + a.len();
                        }
                    }
                },
                
                NessaExpr::PrefixOperationDefinition(id, _, _, t, _, b) |
                NessaExpr::PostfixOperationDefinition(id, _, _, t, _, b) => {
                    if let Some(usages) = unary_instances.get(id) {
                        for ov in usages {
                            // Find unary operator overload id
                            if let Operator::Unary{operations, ..} = &self.unary_ops[*id] {
                                for (i, (_, i_t, _, _)) in operations.iter().enumerate() {
                                    if i_t == t {
                                        unary.entry((*id, i, ov.clone())).or_insert(program_size);
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b, true) + 1;
                        }
                    }
                }
                
                NessaExpr::BinaryOperationDefinition(id, _, (_, a_t), (_, b_t), _, b) => {
                    let and = Type::And(vec!(a_t.clone(), b_t.clone()));

                    if let Some(usages) = binary_instances.get(id) {
                        for ov in usages {
                            // Find binary operator overload id
                            if let Operator::Binary{operations, ..} = &self.binary_ops[*id] {
                                for (i, (_, i_t, _, _)) in operations.iter().enumerate() {
                                    if *i_t == and {
                                        binary.entry((*id, i, ov.clone())).or_insert(program_size);
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b, true) + 2;
                        }
                    }
                }
                
                NessaExpr::NaryOperationDefinition(id, _, (_, a_t), a, _, b) => {
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
                                        break;
                                    }
                                }
                            }
            
                            program_size += self.compiled_form_body_size(b, true) + a.len() + 1;
                        }
                    }
                }

                _ => {}
            }
        }


        let mut lambda_positions: HashMap<usize, usize> = HashMap::new();
        let mut lambdas = vec!();
        self.compile_lambda(lines, program_size, &mut lambdas, &mut lambda_positions, &mut functions, &mut unary, &mut binary, &mut nary)?;

        let mut res = vec!(NessaInstruction::from(CompiledNessaExpr::Jump(program_size + lambdas.len())));

        // Define functions
        for expr in lines {
            match expr {
                NessaExpr::FunctionDefinition(id, _, a, r, b) => {
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
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::PrefixOperationDefinition(id, _, _, t, r, b) |
                NessaExpr::PostfixOperationDefinition(id, _, _, t, r, b) => {
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
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions)?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::BinaryOperationDefinition(id, _, (_, t1), (_, t2), r, b) => {
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
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions)?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::NaryOperationDefinition(id, _, (_, t), a, r, b) => {
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
                                res.extend(self.compiled_form_body(b, &functions, &unary, &binary, &nary, &lambda_positions)?);
                            
                            } else {
                                let mut sub_b = b.clone();
                                let templates = ov.iter().cloned().enumerate().collect();

                                sub_b.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
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
                NessaExpr::FunctionDefinition(..) | 
                NessaExpr::PrefixOperationDefinition(..) |
                NessaExpr::PostfixOperationDefinition(..) |
                NessaExpr::BinaryOperationDefinition(..) |
                NessaExpr::NaryOperationDefinition(..) => {},

                _ => res.extend(self.compiled_form_expr(expr, &functions, &unary, &binary, &nary, &lambda_positions, true)?)
            }
        }

        res.push(NessaInstruction::new(CompiledNessaExpr::Halt, "End of the program".into()));

        return Ok(res);
    }

    pub fn compiled_form_size(&self, expr: &NessaExpr, root: bool, root_counter: &mut usize) -> usize {
        use NessaExpr::*;

        return match expr {
            Literal(_) | Variable(..) | CompiledLambda(..) => 1, 
            BinaryOperation(_, _, a, b) => self.compiled_form_size(a, false, root_counter) + self.compiled_form_size(b, false, root_counter) + 1,
            NaryOperation(_, _, a, b) => self.compiled_form_size(a, false, root_counter) + self.compiled_form_body_size(b, false) + 1,
            Return(e) | CompiledVariableDefinition(_, _, _, e) | CompiledVariableAssignment(_, _, _, e) | UnaryOperation(_, _, e) => self.compiled_form_size(e, false, root_counter) + 1,
            If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_size(ih, false, root_counter) + self.compiled_form_body_size(ib, true) + 1;

                for (h, b) in ei {
                    res += self.compiled_form_size(h, false, root_counter) + self.compiled_form_body_size(b, true) + 1
                }

                if let Some(b) = e {
                    res += self.compiled_form_body_size(b, true);
                }
                
                res
            },
            CompiledFor(_, _, _, c, b) => self.compiled_form_size(c, false, root_counter) + self.compiled_form_body_size(b, true) + 9,
            While(c, b) => self.compiled_form_size(c, false, root_counter) + self.compiled_form_body_size(b, true) + 2,
            FunctionCall(_, _, a) => {
                *root_counter += root as usize; // Add drop instruction
                self.compiled_form_body_size(a, false) + 1
            }, 
            _ => unreachable!()
        }
    }

    pub fn compiled_form_body_size(&self, lines: &Vec<NessaExpr>, root: bool) -> usize {
        let mut counter = 0;
        let res: usize = lines.iter().map(|i| self.compiled_form_size(i, root, &mut counter)).sum();

        return res + counter;
    }

    pub fn compiled_form_expr(
        &self, expr: &NessaExpr, 
        functions: &HashMap<(usize, usize, Vec<Type>), usize>, 
        unary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        binary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        nary: &HashMap<(usize, usize, Vec<Type>), usize>, 
        lambda_positions: &HashMap<usize, usize>,
        root: bool
    ) -> Result<Vec<NessaInstruction>, String> {
        return match expr {
            NessaExpr::Literal(obj) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(obj.clone())))),

            NessaExpr::CompiledLambda(i, a, r, _) => {
                Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(Object::new((
                    *lambda_positions.get(i).unwrap(),
                    Type::And(a.iter().map(|(_, t)| t).cloned().collect()),
                    r.clone()
                ))))))
            },

            NessaExpr::Tuple(e) => {
                let mut types = Vec::with_capacity(e.len());
                let mut res = vec!();

                for i in e.iter().rev() {
                    types.push(self.infer_type(i).unwrap());
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }

                res.push(NessaInstruction::from(CompiledNessaExpr::Tuple(types)));

                Ok(res)
            }

            NessaExpr::Variable(id, _, _) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::GetVariable(*id)))), 
            NessaExpr::CompiledVariableDefinition(id, _, _, e) | NessaExpr::CompiledVariableAssignment(id, _, _, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*id)));

                Ok(res)
            },

            NessaExpr::UnaryOperation(id, t, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;

                let i_t = self.infer_type(e).unwrap();
                let (ov_id, _, native, t_args) = self.get_first_unary_op(*id, i_t, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(format!("Unary operation expected {} type arguments and got {}", t_args.len(), t.len()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::UnaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = unary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::BinaryOperation(id, t, a, b) => {
                let mut res = self.compiled_form_expr(b, functions, unary, binary, nary, lambda_positions, false)?;
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, false)?);
                
                let a_t = self.infer_type(a).unwrap();
                let b_t = self.infer_type(b).unwrap();

                let (ov_id, _, native, t_args) = self.get_first_binary_op(*id, a_t, b_t, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(format!("Binary operation expected {} type arguments and got {}", t_args.len(), t.len()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::BinaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = binary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::NaryOperation(id, tm, a, b) => {
                let mut res = vec!();

                for i in b.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }
                
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, false)?);

                let a_t = self.infer_type(a).unwrap();
                let b_t = b.iter().map(|i| self.infer_type(i).unwrap()).collect();

                let (ov_id, _, native, t_args) = self.get_first_nary_op(*id, a_t, b_t, false).unwrap();

                if tm.len() != t_args.len() {
                    return Err(format!("N-ary operation expected {} type arguments and got {}", t_args.len(), tm.len()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NaryOperatorCall(*id, ov_id, tm.clone())));

                } else {
                    let pos = nary.get(&(*id, ov_id, tm.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::If(ih, ib, ei, e) => {
                let mut res = self.compiled_form_expr(ih, functions, unary, binary, nary, lambda_positions, false)?;
                let if_body = self.compiled_form_body(ib, functions, unary, binary, nary, lambda_positions)?;

                res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(if_body.len() + 1)));
                res.extend(if_body);

                for (h, b) in ei {
                    res.extend(self.compiled_form_expr(h, functions, unary, binary, nary, lambda_positions, false)?);

                    let elif_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions)?;

                    res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(elif_body.len() + 1)));
                    res.extend(elif_body);
                }

                if let Some(b) = e {
                    res.extend(self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions)?);
                }

                Ok(res)
            },
            NessaExpr::While(c, b) => {
                // Start with the condition
                let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, lambda_positions, false)?;
                let while_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions)?;

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
                    let mut res = self.compiled_form_expr(c, functions, unary, binary, nary, lambda_positions, false)?;

                    // Get "iterator", "next" and "is_consumed" function overloads and check them
                    if let Some((it_ov_id, it_type, it_native, it_args)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(t.clone()), true) {
                        let it_mut = Type::MutRef(Box::new(it_type.clone()));

                        if let Some((next_ov_id, _, next_native, next_args)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), true) {
                            if let Some((consumed_ov_id, consumed_res, consumed_native, consumed_args)) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), true) {
                                if let Type::Basic(2) = consumed_res {
                                    let for_body = self.compiled_form_body(b, functions, unary, binary, nary, lambda_positions)?;

                                    // Convert the iterable into an iterator
                                    if it_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(ITERATOR_FUNC_ID, it_ov_id, it_args)));
    
                                    } else {
                                        let pos = functions.get(&(ITERATOR_FUNC_ID, it_ov_id, it_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                                    }

                                    // Store the iterator
                                    res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*it_var_id)));

                                    // Check end of iterator
                                    res.push(NessaInstruction::from(CompiledNessaExpr::GetVariable(*it_var_id)));

                                    if consumed_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(IS_CONSUMED_FUNC_ID, consumed_ov_id, consumed_args)));
    
                                    } else {
                                        let pos = functions.get(&(7, consumed_ov_id, consumed_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                                    }                                    

                                    // Jump to end of loop
                                    res.push(NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfTrue(for_body.len() + 5)));

                                    // Get next value
                                    res.push(NessaInstruction::from(CompiledNessaExpr::GetVariable(*it_var_id)));

                                    if next_native {
                                        res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(NEXT_FUNC_ID, next_ov_id, next_args)));
    
                                    } else {
                                        let pos = functions.get(&(6, next_ov_id, next_args)).unwrap();
                                        res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
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
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::Return));

                Ok(res)
            },
            NessaExpr::FunctionCall(id, t, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }
                
                let args_types = a.iter().map(|i| self.infer_type(i).unwrap()).collect();
                let (ov_id, _, native, t_args) = self.get_first_function_overload(*id, args_types, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(format!("Function call {} expected {} type arguments and got {}", self.functions[*id].name, t_args.len(), t.len()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NativeFunctionCall(*id, ov_id, t.clone())));

                } else {
                    let pos = functions.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                if root { // Drop if the return value is unused
                    res.push(NessaInstruction::from(CompiledNessaExpr::Drop));
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
        lambda_positions: &HashMap<usize, usize>
    ) -> Result<Vec<NessaInstruction>, String> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, true)).flat_map(|i| i.unwrap()).collect());
    }

    pub fn define_module_class(&mut self, definition: NessaExpr, needed: &mut bool) -> Result<(), String> {
        match definition {
            NessaExpr::ClassDefinition(n, _, _, _) if self.type_templates.iter().filter(|t| t.name == n).next().is_some() => {},
            NessaExpr::ClassDefinition(n, t, a, p) => {
                *needed = true; // Repeat class parsing after creating a new one

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
                let class_id = self.type_templates.iter().filter(|t| t.name == n).next().unwrap().id;

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

        return Ok(());
    }

    pub fn define_module_classes(&mut self, code: &String) -> Result<(), String> {
        let mut needed = true;

        while needed {
            needed = false;

            for i in self.nessa_class_parser(code).unwrap().1 {
                self.define_module_class(i, &mut needed)?;
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

    fn map_nessa_class(&mut self, other: &NessaContext, id: usize, classes: &mut HashMap<usize, usize>) -> Result<usize, String> {
        let other_cl = &other.type_templates[id];
        let c_name = &other_cl.name;

        if !classes.contains_key(&id) {
            let class_id;

            // If the function has another id in the target context
            if let Some(f) = self.type_templates.iter().filter(|f| f.name == *c_name).next() {
                class_id = f.id;

            } else { // Else the function needs to be defined
                class_id = self.type_templates.len();
                self.define_type(c_name.clone(), other_cl.params.clone(), other_cl.attributes.clone(), other_cl.patterns.clone(), other_cl.parser)?;
            }

            return Ok(*classes.entry(id).or_insert(class_id));
        }

        return Ok(classes[&id]);
    }

    fn map_nessa_function(&mut self, other: &NessaContext, id: usize, functions: &mut HashMap<usize, usize>) -> Result<usize, String> {
        let f_name = &other.functions[id].name;

        if !functions.contains_key(&id) {
            let fn_id;

            // If the function has another id in the target context
            if let Some(f) = self.functions.iter().filter(|f| f.name == *f_name).next() {
                fn_id = f.id;

            } else { // Else the function needs to be defined
                fn_id = self.functions.len();
                self.define_function(f_name.clone())?;
            }

            return Ok(*functions.entry(id).or_insert(fn_id));
        }

        return Ok(functions[&id]);
    }

    fn map_nessa_unary_operator(&mut self, other: &NessaContext, id: usize, unary_operators: &mut HashMap<usize, usize>) -> Result<usize, String> {
        if let Operator::Unary{representation: r, prefix, precedence, ..} = &other.unary_ops[id] {
            if !unary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _)) = self.unary_ops.iter()
                                     .map(|op| if let Operator::Unary{id: op_id, representation: op_rep, ..} = op { (op_id, op_rep) } else { unreachable!() })
                                     .filter(|(_, op_rep)| *op_rep == r)
                                     .next() {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.unary_ops.len();
                    self.define_unary_operator(r.clone(), *prefix, *precedence)?;
                }
    
                return Ok(*unary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(format!("Unable to find unary operator with id = {}", id));
        }

        return Ok(unary_operators[&id]);
    }

    fn map_nessa_binary_operator(&mut self, other: &NessaContext, id: usize, binary_operators: &mut HashMap<usize, usize>) -> Result<usize, String> {
        if let Operator::Binary{representation: r, precedence, ..} = &other.binary_ops[id] {
            if !binary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _)) = self.binary_ops.iter()
                                     .map(|op| if let Operator::Binary{id: op_id, representation: op_rep, ..} = op { (op_id, op_rep) } else { unreachable!() })
                                     .filter(|(_, op_rep)| *op_rep == r)
                                     .next() {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.binary_ops.len();
                    self.define_binary_operator(r.clone(), *precedence)?;
                }
    
                return Ok(*binary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(format!("Unable to find binary operator with id = {}", id));
        }

        return Ok(binary_operators[&id]);
    }

    fn map_nessa_nary_operator(&mut self, other: &NessaContext, id: usize, nary_operators: &mut HashMap<usize, usize>) -> Result<usize, String> {
        if let Operator::Nary{open_rep: or, close_rep: cr, precedence, ..} = &other.nary_ops[id] {
            if !nary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _, _)) = self.nary_ops.iter()
                                     .map(|op| if let Operator::Nary{id: op_id, open_rep: op_or, close_rep: op_cr, ..} = op { (op_id, op_or, op_cr) } else { unreachable!() })
                                     .filter(|(_, op_or, op_cr)| *op_or == or && *op_cr == cr)
                                     .next() {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.binary_ops.len();
                    self.define_nary_operator(or.clone(), cr.clone(), *precedence)?;
                }
    
                return Ok(*nary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(format!("Unable to find binary operator with id = {}", id));
        }

        return Ok(nary_operators[&id]);
    }

    pub fn map_nessa_expression(
        &mut self, expr: &mut NessaExpr, ctx: &NessaContext,
        functions: &mut HashMap<usize, usize>,
        unary_operators: &mut HashMap<usize, usize>,
        binary_operators: &mut HashMap<usize, usize>,
        nary_operators: &mut HashMap<usize, usize>,
        classes: &mut HashMap<usize, usize>
    ) -> Result<(), String> {
        match expr {
            NessaExpr::Literal(..) |
            NessaExpr::NameReference(..) => {}

            NessaExpr::VariableDefinition(_, t, e) => {
                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.map_basic_types(&mut mapping);

                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::VariableAssignment(_, e) => {
                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::UnaryOperation(id, t, a) => {
                *id = self.map_nessa_unary_operator(ctx, *id, unary_operators)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::BinaryOperation(id, t, a, b) => {
                *id = self.map_nessa_binary_operator(ctx, *id, unary_operators)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                self.map_nessa_expression(b, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::NaryOperation(id, t, a, b) => {
                *id = self.map_nessa_nary_operator(ctx, *id, nary_operators)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;

                for arg in b {
                    self.map_nessa_expression(arg, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::FunctionCall(id, t, args) => {
                *id = self.map_nessa_function(ctx, *id, functions)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                for arg in args {
                    self.map_nessa_expression(arg, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::If(ih, ib, ei, eb) => {
                self.map_nessa_expression(ih, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;

                for line in ib {
                    self.map_nessa_expression(line, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }

                for (ei_h, ei_b) in ei {
                    self.map_nessa_expression(ei_h, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;

                    for line in ei_b {
                        self.map_nessa_expression(line, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.map_nessa_expression(line, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                    }
                }
            }

            NessaExpr::For(_, c, lines) => {
                self.map_nessa_expression(c, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                
                for line in lines {
                    self.map_nessa_expression(line, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::Return(e) => {
                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            _ => unimplemented!("{:?}", expr)
        }

        return Ok(());
    }

    pub fn import_code(&mut self, code: &Vec<NessaExpr>, ctx: &NessaContext, imports: &HashMap<ImportType, HashSet<String>>) -> Result<Vec<NessaExpr>, String> {
        let mut res = vec!();
        let mut functions: HashMap<usize, usize> = HashMap::new();
        let mut unary_operators: HashMap<usize, usize> = HashMap::new();
        let mut binary_operators: HashMap<usize, usize> = HashMap::new();
        let mut nary_operators: HashMap<usize, usize> = HashMap::new();
        let mut classes: HashMap<usize, usize> = HashMap::new();

        for line in code {
            match line {
                NessaExpr::ClassDefinition(n, t, atts, p) => {
                    if imports.contains_key(&ImportType::Class) && imports[&ImportType::Class].contains(n) {
                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_atts = atts.iter().map(|(n, t)| (n.clone(), t.map_basic_types(&mut mapping))).collect();

                        let mut needed = false;
                        let mapped_expr = NessaExpr::ClassDefinition(n.clone(), t.clone(), mapped_atts, p.clone());

                        self.define_module_class(mapped_expr.clone(), &mut needed)?;
    
                        res.push(mapped_expr);
                    }
                }

                NessaExpr::FunctionDefinition(id, t, a, r, b) => {
                    let f_name = &ctx.functions[*id].name;

                    // If the function needs to be imported
                    if imports.contains_key(&ImportType::Fn) && imports[&ImportType::Fn].contains(f_name) {
                        let fn_id = self.map_nessa_function(&ctx, *id, &mut functions)?;

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_args = a.iter().map(|(n, t)| (n.clone(), t.map_basic_types(&mut mapping))).collect::<Vec<_>>();
                        let mapped_return = r.map_basic_types(&mut mapping);

                        let mut mapped_body = b.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_nessa_expression(line, ctx, &mut functions, &mut unary_operators, &mut binary_operators, &mut nary_operators, &mut classes)?;
                        }

                        let arg_types = mapped_args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                        self.define_function_overload(fn_id, t.len(), &arg_types, mapped_return.clone(), None)?;

                        // Add the mapped function to the list of new expressions
                        res.push(NessaExpr::FunctionDefinition(fn_id, t.clone(), mapped_args.clone(), mapped_return, mapped_body));
                    }
                }

                NessaExpr::PrefixOperationDefinition(..) => unimplemented!(),
                NessaExpr::PostfixOperationDefinition(..) => unimplemented!(),
                NessaExpr::BinaryOperationDefinition(..) => unimplemented!(),
                NessaExpr::NaryOperationDefinition(..) => unimplemented!(),
                _ => {}
            }
        }

        return Ok(res);
    }

    // BFS on imports
    fn cascade_imports(
        imports: &mut ImportMap,
        modules: &HashMap<String, (NessaContext, Vec<NessaExpr>, ImportMap, InnerDepGraph)>
    )
    {
        let mut res = HashMap::new();
        
        while res != *imports {
            res = imports.clone();

            for (name, _) in imports.iter() {
                for (d_name, d_deps) in &modules.get(name).unwrap().2 {
                    for (t, n) in d_deps {
                        res.entry(d_name.clone()).or_default().entry(t.clone()).or_default().extend(n.iter().cloned());
                    }
                }
            }
    
            *imports = res.clone();
        }
    }

    fn map_import(&self, import: &ImportType, name: &String) -> usize {
        return match import {
            ImportType::Class => self.type_templates.iter().find(|i| i.name == *name).unwrap().id,
            ImportType::Fn => self.functions.iter().find(|i| i.name == *name).unwrap().id,

            ImportType::Prefix |
            ImportType::Postfix => self.unary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),
            
            ImportType::Binary => self.binary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),
            ImportType::Nary => self.nary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),

            _ => unimplemented!()
        };
    }

    fn rev_map_import(&self, import: &ImportType, id: usize) -> String {
        return match import {
            ImportType::Class => self.type_templates[id].name.clone(),
            ImportType::Fn => self.functions[id].name.clone(),

            ImportType::Prefix |
            ImportType::Postfix => self.unary_ops[id].get_repr(),
            
            ImportType::Binary => self.binary_ops[id].get_repr(),
            ImportType::Nary => self.nary_ops[id].get_repr(),

            _ => unimplemented!()
        };
    }

    // BFS on imports (inner dependencies)
    fn cascade_imports_inner(
        imports: &mut ImportMap,
        modules: &HashMap<String, (NessaContext, Vec<NessaExpr>, ImportMap, InnerDepGraph)>
    )
    {
        for (m, imps) in imports {
            let mut new_imports = Imports::new();
            let (ctx, _, _, graph) = &modules.get(m).unwrap();

            for (t, names) in imps.iter() {
                for name in names.iter() {
                    let id = ctx.map_import(t, name);

                    graph.dfs(&(t.clone(), id), |(tp, id)| {
                        let mapped_name = ctx.rev_map_import(tp, *id);
                        new_imports.entry(tp.clone()).or_default().insert(mapped_name);
                    });
                }
            }

            for (t, names) in new_imports {
                imps.entry(t.clone()).or_default().extend(names);
            }
        }
    }

    pub fn parse_and_precompile_with_dependencies(
        &mut self, 
        code: &String, 
        modules: &HashMap<String, (NessaContext, Vec<NessaExpr>, ImportMap, InnerDepGraph)>
    ) -> Result<Vec<NessaExpr>, String> {
        let mut res = vec!();
        let mut imports = nessa_module_imports_parser(&code).unwrap().1; // TODO: should cache this

        Self::cascade_imports(&mut imports, modules);
        Self::cascade_imports_inner(&mut imports, modules);

        // Import code from dependencies
        for (m, i) in imports {
            let (other_ctx, other_code, _, _) = modules.get(&m).as_ref().unwrap();

            let mut new_code = self.import_code(&other_code, &other_ctx, &i)?;
            res.append(&mut new_code);
        }

        let mut main_code = self.parse_without_precompiling(code)?;
        res.append(&mut main_code);

        return Ok(res);
    }

    pub fn parse_without_precompiling(&mut self, code: &String) -> Result<Vec<NessaExpr>, String> {
        self.define_module_classes(code)?;
        self.define_module_operators(code)?;
        self.define_module_functions(code)?;
        self.define_module_operations(code)?;

        let lines = self.parse_nessa_module(code);
        self.define_module_function_overloads(&lines)?;

        return Ok(lines);
    }

    pub fn precompile_module(&mut self, lines: &mut Vec<NessaExpr>) -> Result<(), String> {        
        self.compile(lines, &vec!())?;

        for expr in lines {
            self.static_check(expr)?;
        }

        return Ok(());
    }

    pub fn parse_and_precompile(&mut self, code: &String) -> Result<Vec<NessaExpr>, String> {
        let mut lines = self.parse_without_precompiling(code)?;

        self.precompile_module(&mut lines)?;

        return Ok(lines);
    }

    pub fn parse_and_compile(&mut self, code: &String) -> Result<Vec<NessaInstruction>, String> {
        let lines = self.parse_and_precompile(&code)?;

        return self.compiled_form(&lines);
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

        let (_, mut code) = ctx.nessa_parser(code_1_str).unwrap();
        ctx.compile_functions(&mut code).unwrap();

        assert_eq!(code, vec!(
            NessaExpr::FunctionCall(0, vec!(), vec!(
                NessaExpr::Literal(Object::new(Number::from(5)))
            ))
        ));
        
        let (_, mut code) = ctx.nessa_parser(code_2_str).unwrap();

        assert!(ctx.compile_functions(&mut code).is_ok());
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