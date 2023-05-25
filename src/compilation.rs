use std::collections::{ HashMap };

use colored::Colorize;
use levenshtein::levenshtein;
use nom::error::{VerboseErrorKind, VerboseError};
use seq_macro::seq;

use crate::config::ImportMap;
use crate::config::Imports;
use crate::config::NessaModule;
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
    ╒══════════════════════════════╕
    │ Compilation error management │
    ╘══════════════════════════════╛
*/

#[derive(Debug)]
pub struct NessaError {
    pub err_type: String,
    pub message: String,

    pub has_location: bool,
    pub line: usize,
    pub column: usize,
    pub fragment: String,

    pub suggestions: Vec<String>
}

impl NessaError {
    pub fn syntax_error(message: String, line: usize, column: usize, fragment: String, suggestions: Vec<String>) -> Self {
        return NessaError { err_type: "Syntax error".into(), has_location: true, message, line, column, fragment, suggestions };
    }

    pub fn compiler_error(message: String, location: &Location, suggestions: Vec<String>) -> Self {
        return NessaError { 
            err_type: "Compilation error".into(), 
            has_location: true,
            message, 
            line: location.line, 
            column: location.column, 
            fragment: location.span.clone(), 
            suggestions 
        };
    }

    pub fn execution_error(message: String) -> Self {
        return NessaError { 
            err_type: "Execution error".into(), 
            has_location: false,
            message, 
            line: 0, 
            column: 0, 
            fragment: "".into(), 
            suggestions: vec!()
        };
    }

    pub fn module_error(message: String) -> Self {
        return NessaError { 
            err_type: "Module error".into(), 
            has_location: false,
            message, 
            line: 0, 
            column: 0, 
            fragment: "".into(), 
            suggestions: vec!()
        };
    }

    pub fn emit(&self) {
        if self.has_location {
            let mut frag = self.fragment.as_str();
            
            if let Some(pos) = frag.find('\n') {
                frag = &frag[..pos];
            }
    
            if frag.len() > 50 {
                frag = &frag[..50];
            }
    
            frag = frag.trim();
    
            eprintln!(
                "\n[{} at line {}, column {}]\n\n • {}:\n\n\t[...] {} [...]\n\t      {}\n", 
                self.err_type.red().bold(), 
                self.line.to_string().yellow(), self.column.to_string().yellow(), 
                self.message, frag,
                "^".repeat(frag.len()).red()
            );

            if self.suggestions.len() > 0 {
                eprintln!("[{}]\n", "Suggestions".blue().bold());

                for s in &self.suggestions {
                    eprintln!(" • {}", s);                    
                }

                eprintln!();
            }

        } else {
            eprintln!(
                "\n[{}] {}\n", 
                self.err_type.red().bold(),
                self.message
            );
        }
        
        panic!();
    }
}

impl<'a> From<VerboseError<Span<'a>>> for NessaError {
    fn from(error: VerboseError<Span<'a>>) -> Self {
        let err = error.errors.last().unwrap();

        let fragment = err.0;
        let error_msg = match &err.1 {
            VerboseErrorKind::Context(ctx) => ctx,
            _ => "Unable to parse"
        };

        return NessaError::syntax_error(
            error_msg.into(), 
            fragment.location_line() as usize, fragment.get_column(), 
            fragment.to_string(), 
            vec!()
        );
    }
}

impl<'a> From<nom::Err<VerboseError<Span<'a>>>> for NessaError {
    fn from(error: nom::Err<VerboseError<Span<'a>>>) -> Self {
        return match error {
            nom::Err::Error(err) |
            nom::Err::Failure(err) => NessaError::from(err),

            _ => unreachable!()
        }
    }
}

/*
    ╒═════════════════════════════════╕
    │ Expression tree transformations │
    ╘═════════════════════════════════╛
*/

impl NessaContext {

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
            NessaExpr::NameReference(l, n) => {
                if let Some(f) = self.get_func_name(n) {
                    *expr = NessaExpr::FunctionName(l.clone(), f.id);
                }
            },

            NessaExpr::Tuple(_, e) => e.iter_mut().for_each(|i| self.compile_expr_function_names(i)),

            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, _, e) |
            NessaExpr::VariableAssignment(_, _, e) => {
                self.compile_expr_function_names(e);
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, _, _, e) => {
                self.compile_expr_function_names(e);
            }

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.compile_expr_function_names(a);
                self.compile_expr_function_names(b);
            }
            
            NessaExpr::NaryOperation(_, _, _, a, b) => {
                self.compile_expr_function_names(a);
                b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
            }

            // Compile flow control
            NessaExpr::If(_, h, ib, ei, eb) => {
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

            NessaExpr::For(_, _, c, b) |
            NessaExpr::While(_, c, b) => {
                self.compile_expr_function_names(c);
                b.iter_mut().for_each(|i| self.compile_expr_function_names(i));
            }

            NessaExpr::Return(_, e) => {
                self.compile_expr_function_names(e);
            }

            _ => {}
        }
    }

    fn compile_expr_function_calls(&mut self, expr: &mut NessaExpr) -> Result<(), NessaError> {
        match expr {
            // Compile tuples
            NessaExpr::Tuple(_, e) => {
                for i in e {
                    self.compile_expr_function_calls(i)?;
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(_, _, _, e) |
            NessaExpr::VariableAssignment(_, _, e) => {
                self.compile_expr_function_calls(e)?;
            },

            // Compile operations
            NessaExpr::UnaryOperation(_, _, _, e) => {
                self.compile_expr_function_calls(e)?;
            }

            NessaExpr::BinaryOperation(l, id, _, a, b) => {
                self.compile_expr_function_calls(a)?;
                self.compile_expr_function_calls(b)?;

                // Member function calls
                if *id == DOT_BINOP_ID {
                    if let NessaExpr::FunctionCall(_, f_id, t, args) = b.as_ref() {
                        // Append first operand to the function's arguments 
                        let mut new_args = vec!(a.as_ref().clone());
                        new_args.extend(args.iter().cloned());

                        *expr = NessaExpr::FunctionCall(l.clone(), *f_id, t.clone(), new_args);
                    }
                }
            }
            
            // Function call
            NessaExpr::NaryOperation(l, CALL_OP, t, a, b) => {
                self.compile_expr_function_calls(a)?;
                b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                if let NessaExpr::FunctionName(_, id) = a.as_ref() {                    
                    *expr = NessaExpr::FunctionCall(l.clone(), *id, t.clone(), b.clone());
                }
            }

            // Compile flow control
            NessaExpr::If(_, h, ib, ei, eb) => {
                self.compile_expr_function_calls(h)?;
                ib.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                ei.iter_mut().map(|(ei_h, ei_b)| -> Result<(), NessaError> {
                    self.compile_expr_function_calls(ei_h)?;
                    ei_b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;

                    return Ok(());
                }).collect::<Result<_, _>>()?;

                if let Some(eb_inner) = eb {
                    eb_inner.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?;
                }
            }

            NessaExpr::For(_, _, c, b) |
            NessaExpr::While(_, c, b) => {
                self.compile_expr_function_calls(c)?;
                b.iter_mut().map(|i| self.compile_expr_function_calls(i)).collect::<Result<_, _>>()?
            }

            NessaExpr::Return(_, e) => {
                self.compile_expr_function_calls(e)?;
            }

            _ => {}
        }

        return Ok(());
    }

    fn infer_lambda_return_type(&mut self, lines: &mut Vec<NessaExpr>) -> Option<Type> {
        let merge_types = |a: Option<Type>, b: Option<Type>| -> Option<Type> {
            if b.is_none() {
                return a;
            }
            
            return match &a {
                Some(na) => {
                    if na.bindable_to(b.as_ref().unwrap()) {
                        return b;
                    
                    } else if b.as_ref().unwrap().bindable_to(&na) {
                        return a;

                    } else {
                        return Some(Type::Or(vec!(na.clone(), b.unwrap())));
                    }
                },

                None => b,
            };
        };

        let mut res = None;

        for expr in lines {
            match expr {
                NessaExpr::While(_, _, b) |
                NessaExpr::CompiledFor(_, _, _, _, _, b) => res = merge_types(res, self.infer_lambda_return_type(b)),

                NessaExpr::If(_, _, ib, ei, eb) => {
                    res = merge_types(res, self.infer_lambda_return_type(ib));

                    for (_, eib) in ei {
                        res = merge_types(res, self.infer_lambda_return_type(eib));
                    }

                    if let Some(eb_inner) = eb {
                        res = merge_types(res, self.infer_lambda_return_type(eb_inner));
                    }
                },

                NessaExpr::Return(_, expr) => res = merge_types(res, self.infer_type(expr)),

                _ => {}
            }
        }

        return res;
    }

    fn compile_expr_function_bodies(&mut self, expr: &mut NessaExpr) -> Result<(), NessaError> {
        match expr {
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) |
            NessaExpr::ClassDefinition(..) |
            NessaExpr::NameReference(..) |
            NessaExpr::Literal(..) => {},

            NessaExpr::VariableAssignment(_, _, e) |
            NessaExpr::VariableDefinition(_, _, _, e) |
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) => {
                self.compile_expr_function_bodies(e)?;
            },

            NessaExpr::FunctionCall(_, _, _, args) |
            NessaExpr::Tuple(_, args) => {
                for e in args {
                    self.compile_expr_function_bodies(e)?;
                }
            }

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.compile_expr_function_bodies(a)?;
                self.compile_expr_function_bodies(b)?;
            }

            NessaExpr::For(_, _, a, b) |
            NessaExpr::While(_, a, b) |
            NessaExpr::NaryOperation(_, _, _, a, b) => {
                self.compile_expr_function_bodies(a)?;

                for e in b {
                    self.compile_expr_function_bodies(e)?;
                }
            }

            NessaExpr::If(_, ih, ib, ei, eb) => {
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

            NessaExpr::Lambda(l, a, r, b) => {
                self.compile(b, &a)?;

                // Infer further
                if *r == Type::Wildcard {
                    *r = self.infer_lambda_return_type(b).unwrap();
                }

                *expr = NessaExpr::CompiledLambda(l.clone(), self.lambdas, a.clone(), r.clone(), b.clone());
                self.lambdas += 1;
            },

            NessaExpr::FunctionDefinition(l, _, _, a, r, b) => {
                self.compile(b, &a)?;

                if let Type::Empty = r {
                    if self.ensured_return_check_body(b, l).is_err() {
                        b.push(NessaExpr::Return(l.clone(), Box::new(NessaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            NessaExpr::PrefixOperationDefinition(l, _, _, n, t, r, b) => {
                self.compile(b, &vec!((n.clone(), t.clone())))?;

                if let Type::Empty = r {
                    if self.ensured_return_check_body(b, l).is_err() {
                        b.push(NessaExpr::Return(l.clone(), Box::new(NessaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            NessaExpr::PostfixOperationDefinition(l, _, _, n, t, r, b) => {
                self.compile(b, &vec!((n.clone(), t.clone())))?;

                if let Type::Empty = r {
                    if self.ensured_return_check_body(b, l).is_err() {
                        b.push(NessaExpr::Return(l.clone(), Box::new(NessaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            NessaExpr::BinaryOperationDefinition(l, _, _, a1, a2, r, b) => {
                self.compile(b, &vec!(a1.clone(), a2.clone()))?;

                if let Type::Empty = r {
                    if self.ensured_return_check_body(b, l).is_err() {
                        b.push(NessaExpr::Return(l.clone(), Box::new(NessaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            NessaExpr::NaryOperationDefinition(l, _, _, a, args, r, b) => {
                let mut all_args = vec!(a.clone());
                all_args.extend(args.iter().cloned());

                self.compile(b, &all_args)?;

                if let Type::Empty = r {
                    if self.ensured_return_check_body(b, l).is_err() {
                        b.push(NessaExpr::Return(l.clone(), Box::new(NessaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            }

            _ => {}
        }

        return Ok(());
    }

    pub fn compile_functions(&mut self, body: &mut Vec<NessaExpr>) -> Result<(), NessaError> {
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

    fn compile_expr_variables(&self, expr: &mut NessaExpr, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, curr_ctx: &mut HashMap<String, usize>) -> Result<(), NessaError> {
        match expr {
            // Compile variable references
            NessaExpr::NameReference(l, n) if ctx_idx.contains_key(n) => {
                let (idx, t) = ctx_idx.get(n).unwrap();
                *expr = NessaExpr::Variable(l.clone(), *idx, n.clone(), t.clone());
            },

            NessaExpr::NameReference(l, n) => {
                let similar_vars = ctx_idx.keys().filter(|name| levenshtein(n, name) < 3)
                                                 .map(|i| format!("{} (Variable)", i.cyan()))
                                                 .collect::<Vec<_>>();

                let similar_func = self.functions.iter().map(|f| &f.name)
                                                        .filter(|name| levenshtein(n, name) < 3)
                                                        .map(|i| format!("{} (Function)", i.green()))
                                                        .collect::<Vec<_>>();

                return Err(NessaError::compiler_error(
                    format!("Identifier with name {} is not defined", n), l, 
                    similar_vars.into_iter().chain(similar_func).map(|i| format!("Similar: {}", i)).collect()
                ));
            }

            NessaExpr::VariableAssignment(l, n, e) if ctx_idx.contains_key(n) => {
                if ctx_idx.contains_key(n) {
                    self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                    let (idx, t) = ctx_idx.get(n).unwrap();

                    *expr = NessaExpr::CompiledVariableAssignment(l.clone(), *idx, n.clone(), t.clone(), e.clone());
                
                } else {
                    return Err(NessaError::compiler_error(format!("Variable with name {} is not defined", n), l, vec!()));
                }
            },

            // Compile variable definitions
            NessaExpr::VariableDefinition(l, n, t, e) => {
                let idx = registers.pop().unwrap();

                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                if let Type::InferenceMarker = t {
                    *t = self.infer_type(e).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of definition right handside".into(), l, vec!()
                    ))?;
                }

                ctx_idx.entry(n.clone()).or_insert((idx, t.clone()));
                curr_ctx.entry(n.clone()).or_insert(idx);

                *expr = NessaExpr::CompiledVariableDefinition(l.clone(), idx, n.clone(), t.clone(), e.clone());
            },

            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            NessaExpr::CompiledVariableDefinition(_, id, n, t, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                ctx_idx.entry(n.clone()).or_insert((*id, t.clone()));
                curr_ctx.entry(n.clone()).or_insert(*id);
            }

            // Compile operations
            NessaExpr::UnaryOperation(l, id, t, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;

                if t.len() == 0 {
                    let arg_type = self.infer_type(e).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of unary operation argument".into(), l, vec!()
                    ))?;

                    if self.is_unary_op_ambiguous(*id, arg_type.clone()).is_none() {
                        if let Some((_, _, _, it_args)) = self.get_first_unary_op(*id, arg_type.clone(), true) {
                            if it_args.len() > 0 {
                                *t = it_args;
                            }
                        }
                    }
                }
            }

            NessaExpr::BinaryOperation(l, id, t, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;
                self.compile_expr_variables(b, registers, ctx_idx, curr_ctx)?;

                if t.len() == 0 {
                    let arg_type_1 = self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of binary operation argument".into(), &l, vec!()
                    ))?;

                    let arg_type_2 = self.infer_type(b).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of binary operation argument".into(), &l, vec!()
                    ))?;

                    if self.is_binary_op_ambiguous(*id, arg_type_1.clone(), arg_type_2.clone()).is_none() {
                        if let Some((_, _, _, it_args)) = self.get_first_binary_op(*id, arg_type_1.clone(), arg_type_2.clone(), true) {
                            if it_args.len() > 0 {
                                *t = it_args;
                            }
                        }
                    }
                }
            }
            
            NessaExpr::NaryOperation(l, id, t, a, b) => {
                self.compile_expr_variables(a, registers, ctx_idx, curr_ctx)?;

                for i in b.iter_mut() {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;
                }

                if t.len() == 0 {
                    let arg_type = self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of n-ary operation argument".into(), &l, vec!()
                    ))?;

                    let arg_types: Vec<_> = b.iter().map(|a| self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of unary operation argument".into(), &l, vec!()
                    ))).collect::<Result<_, _>>()?;
                    
                    if self.is_nary_op_ambiguous(*id, arg_type.clone(), arg_types.clone()).is_none() {
                        if let Some((_, _, _, it_args)) = self.get_first_nary_op(*id, arg_type.clone(), arg_types, true) {
                            if it_args.len() > 0 {
                                *t = it_args;
                            }
                        }
                    }
                }
            }

            NessaExpr::Tuple(_, args) => {
                for i in args {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;                    
                }
            }

            NessaExpr::FunctionCall(l, id, t, args) => {
                for i in args.iter_mut() {
                    self.compile_expr_variables(i, registers, ctx_idx, curr_ctx)?;                    
                }

                if t.len() == 0 {
                    let arg_types: Vec<_> = args.iter().map(|a| self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer type of function argument".into(), &l, vec!()
                    ))).collect::<Result<_, _>>()?;

                    if self.is_function_overload_ambiguous(*id, arg_types.clone()).is_none() {
                        if let Some((_, _, _, it_args)) = self.get_first_function_overload(*id, arg_types.clone(), true) {
                            if it_args.len() > 0 {
                                *t = it_args;
                            }
                        }
                    }
                }
            }

            // Compile flow control
            NessaExpr::If(_, h, ib, ei, eb) => {
                self.compile_expr_variables(h, registers, ctx_idx, curr_ctx)?;
                self.compile_vars_and_infer_ctx(ib, registers, ctx_idx, &vec!())?;

                for (ei_h, ei_b) in ei {
                    self.compile_expr_variables(ei_h, registers, ctx_idx, curr_ctx)?;
                    self.compile_vars_and_infer_ctx(ei_b, registers, ctx_idx, &vec!())?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_vars_and_infer_ctx(eb_inner, registers, ctx_idx, &vec!())?;
                }
            }

            NessaExpr::While(_, c, b) => {
                self.compile_expr_variables(c, registers, ctx_idx, curr_ctx)?;
                self.compile_vars_and_infer_ctx(b, registers, ctx_idx, &vec!())?;
            }

            NessaExpr::For(l, i, c, b) => {
                self.compile_expr_variables(c, registers, ctx_idx, curr_ctx)?;

                let container_type = self.infer_type(c).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of for loop container".into(), &l, vec!()
                ))?;

                let iterator_type = self.get_iterator_type(&container_type);

                if let Err(msg) = &iterator_type {
                    return Err(NessaError::compiler_error(msg.clone(), &l, vec!()));
                }

                let element_type = self.get_iterator_output_type(iterator_type.as_ref().unwrap());

                if let Err(msg) = element_type {
                    return Err(NessaError::compiler_error(msg, &l, vec!()));
                }

                let iterator_idx = *registers.last().unwrap();
                let element_idx = *registers.get(registers.len() - 2).unwrap();

                self.compile_vars_and_infer_ctx(b, registers, ctx_idx, &vec!(("__iterator__".into(), iterator_type.unwrap()), (i.clone(), element_type.unwrap())))?;

                *expr = NessaExpr::CompiledFor(l.clone(), iterator_idx, element_idx, i.clone(), c.clone(), b.clone());
            }

            NessaExpr::Return(_, e) => {
                self.compile_expr_variables(e, registers, ctx_idx, curr_ctx)?;
            }

            _ => {}
        }

        return Ok(());
    }
    
    fn compile_vars_and_infer_ctx(&self, body: &mut Vec<NessaExpr>, registers: &mut Vec<usize>, ctx_idx: &mut HashMap<String, (usize, Type)>, args: &Vec<(String, Type)>) -> Result<usize, NessaError> {
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

    pub fn compile_vars_and_infer(&self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<usize, NessaError> {
        return self.compile_vars_and_infer_ctx(body, &mut (0..self.variables.len()).rev().collect(), &mut HashMap::new(), args);
    }

    pub fn compile(&mut self, body: &mut Vec<NessaExpr>, args: &Vec<(String, Type)>) -> Result<(), NessaError> {
        self.compile_functions(body)?;
        self.compile_vars_and_infer(body, args)?;

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

    pub fn get_inner_dep_graph(&mut self, lines: &Vec<NessaExpr>) -> Result<DirectedGraph<(ImportType, usize), ()>, NessaError> {
        let mut res = DirectedGraph::new();
        let mut compiled = lines.clone();

        self.compile(&mut compiled, &vec!())?; // TODO: this seems costly...

        self.get_inner_dep_graph_body(&compiled, &(ImportType::Outer, 0), &mut res);

        return Ok(res);
    }

    pub fn get_inner_dep_graph_body(&self, lines: &Vec<NessaExpr>, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        for line in lines {
            self.get_inner_dep_graph_expr(line, parent, deps);
        }
    }

    pub fn get_inner_dep_graph_expr(&self, expr: &NessaExpr, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        match expr {
            NessaExpr::Literal(_, obj) => {
                deps.connect(parent.clone(), (ImportType::Class, obj.get_type_id()), ());
            }

            NessaExpr::Return(_, e) => self.get_inner_dep_graph_expr(e, parent, deps),

            NessaExpr::CompiledVariableAssignment(_, _, _, t, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, t, e) => {
                self.get_inner_dep_graph_expr(e, parent, deps);

                for td in t.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::Tuple(_, b) => {
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            NessaExpr::FunctionCall(_, id, ts, args) => {
                deps.connect(parent.clone(), (ImportType::Fn, *id), ());

                self.get_inner_dep_graph_body(args, parent, deps);

                for t in ts {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }
                }
            }

            NessaExpr::UnaryOperation(_, id, ts, a) => {
                if let Operator::Unary { id, prefix, .. } = &self.unary_ops[*id] {
                    if *prefix {
                        deps.connect(parent.clone(), (ImportType::Prefix, *id), ());
                    
                    } else {
                        deps.connect(parent.clone(), (ImportType::Postfix, *id), ());
                    }

                    self.get_inner_dep_graph_expr(a, parent, deps);

                    for t in ts {
                        for td in t.type_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Class, td), ());
                        }
                    }
                }
            }

            NessaExpr::BinaryOperation(_, id, ts, a, b) => {
                if let Operator::Binary { id, .. } = &self.binary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Binary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_expr(b, parent, deps);

                    for t in ts {
                        for td in t.type_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Class, td), ());
                        }
                    }
                }
            }

            NessaExpr::NaryOperation(_, id, ts, a, b) => {
                if let Operator::Nary { id, .. } = &self.nary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Nary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_body(b, parent, deps);

                    for t in ts {
                        for td in t.type_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Class, td), ());
                        }
                    }
                }
            }

            NessaExpr::While(_, c, b) => {
                self.get_inner_dep_graph_expr(c, parent, deps);
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            NessaExpr::CompiledFor(_, _, _, _, c, b) => {
                self.get_inner_dep_graph_expr(c, parent, deps);
                self.get_inner_dep_graph_body(b, parent, deps);

                // Iteration functions, since they are used implicitly
                deps.connect(parent.clone(), (ImportType::Fn, ITERATOR_FUNC_ID), ());
                deps.connect(parent.clone(), (ImportType::Fn, NEXT_FUNC_ID), ());
                deps.connect(parent.clone(), (ImportType::Fn, IS_CONSUMED_FUNC_ID), ());
            }

            NessaExpr::If(_, ic, ib, ie, eb) => {
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

            NessaExpr::CompiledLambda(_, _, args, ret, b) => {
                self.get_inner_dep_graph_body(b, parent, deps);

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::FunctionDefinition(_, id, _, args, ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Fn, *id), deps);

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::PrefixOperationDefinition(_, id, _, _, t1, t2, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Prefix, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::PostfixOperationDefinition(_, id, _, _, t1, t2, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Postfix, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::BinaryOperationDefinition(_, id, _, (_, t1), (_, t2), ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Binary, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::NaryOperationDefinition(_, id, _, (_, t1), args, ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Nary, *id), deps);

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }
                }

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }
            }

            NessaExpr::ClassDefinition(_, n, _, _, _) => {
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
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) | 
            NessaExpr::Return(_, e) => self.get_template_calls_pass(e, functions, unary, binary, nary, changed), 

            NessaExpr::Tuple(_, e) => self.get_template_calls_body_pass(e, functions, unary, binary, nary, changed),

            NessaExpr::UnaryOperation(_, id, t, e) => {
                *changed |= NessaContext::add_template_instance(unary, *id, t);

                self.get_template_calls_pass(e, functions, unary, binary, nary, changed);
            }

            NessaExpr::BinaryOperation(_, id, t, a, b) => {
                *changed |= NessaContext::add_template_instance(binary, *id, t);

                self.get_template_calls_pass(a, functions, unary, binary, nary, changed);
                self.get_template_calls_pass(b, functions, unary, binary, nary, changed);
            },

            NessaExpr::NaryOperation(_, id, t, a, b) => {
                *changed |= NessaContext::add_template_instance(nary, *id, t);

                self.get_template_calls_pass(a, functions, unary, binary, nary, changed);
                self.get_template_calls_body_pass(b, functions, unary, binary, nary, changed);
            }

            NessaExpr::If(_, i, ib, ei, eb) => {
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

            NessaExpr::CompiledFor(_, _, _, _, c, b) => {
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

            NessaExpr::While(_, c, b) => {
                self.get_template_calls_pass(c, functions, unary, binary, nary, changed);
                self.get_template_calls_body_pass(b, functions, unary, binary, nary, changed);
            }

            NessaExpr::FunctionCall(_, id, t, args) => {
                *changed |= NessaContext::add_template_instance(functions, *id, t);

                self.get_template_calls_body_pass(args, functions, unary, binary, nary, changed);
            }

            NessaExpr::FunctionDefinition(_, id, _, _, _, b) => {
                if let Some(usages) = functions.get(id).cloned() {
                    for ov in usages {
                        // TODO: cache this
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::PostfixOperationDefinition(_, id, _, _, _, _, b) |
            NessaExpr::PrefixOperationDefinition(_, id, _, _, _, _, b) => {
                if let Some(usages) = unary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::BinaryOperationDefinition(_, id, _, _, _, _, b) => {
                if let Some(usages) = binary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));
                        }

                        self.get_template_calls_body_pass(&body, functions, unary, binary, nary, changed);
                    }
                }
            }

            NessaExpr::NaryOperationDefinition(_, id, _, _, _, _, b) => {
                if let Some(usages) = nary.get(id).cloned() {
                    for ov in usages {
                        let mut body = b.clone();

                        if !ov.is_empty() {
                            let templates = ov.iter().cloned().enumerate().collect();
                            body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, &templates));
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
            NessaExpr::Literal(_, _) => {},

            NessaExpr::Tuple(_, e) => e.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates)),

            NessaExpr::Variable(_, _, _, t) => *t = t.sub_templates(templates),

            NessaExpr::Return(_, e) => self.subtitute_type_params_expr(e, templates),

            NessaExpr::CompiledVariableAssignment(_, _, _, t, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, t, e) => {
                *t = t.sub_templates(templates);

                self.subtitute_type_params_expr(e, templates);
            },
            
            NessaExpr::UnaryOperation(_, _, t, a) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(a, templates);
            },

            NessaExpr::BinaryOperation(_, _, t, a, b) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(a, templates);
                self.subtitute_type_params_expr(b, templates);
            }

            NessaExpr::NaryOperation(_, _, t, first, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                self.subtitute_type_params_expr(first, templates);
                args.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },
            
            NessaExpr::FunctionCall(_, _, t, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                args.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },
            
            NessaExpr::CompiledFor(_, _, _, _, container, body) |
            NessaExpr::While(_, container, body) => {
                self.subtitute_type_params_expr(container, templates);
                body.iter_mut().for_each(|i| self.subtitute_type_params_expr(i, templates));
            },

            NessaExpr::If(_, ih, ib, ei, eb) => {
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
    ) -> Result<(), NessaError> {
        return match line {
            NessaExpr::Literal(..) |
            NessaExpr::Variable(..) |
            NessaExpr::ClassDefinition(..) |
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) => Ok(()),

            NessaExpr::CompiledLambda(_, i, a, _, b) => {
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

            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            NessaExpr::Return(_, e) |
            NessaExpr::UnaryOperation(_, _, _, e) => self.compile_lambda_expr(e, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.compile_lambda_expr(a, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda_expr(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::CompiledFor(_, _, _, _, a, b) |
            NessaExpr::While(_, a, b) |
            NessaExpr::NaryOperation(_, _, _, a, b) => {
                self.compile_lambda_expr(a, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
                self.compile_lambda(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;

                Ok(())
            }

            NessaExpr::If(_, ih, ib, ei, eb) => {
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

            NessaExpr::Tuple(_, args) |
            NessaExpr::FunctionCall(_, _, _, args) => self.compile_lambda(args, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::FunctionDefinition(_, _, _, _, _, b) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, b) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, b) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, b) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, b) => self.compile_lambda(b, current_size, lambdas, lambda_positions, functions, unary, binary, nary),

            NessaExpr::Macro(..) => { Ok(()) },

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
    ) -> Result<(), NessaError> {
        for line in lines {
            self.compile_lambda_expr(line, current_size, lambdas, lambda_positions, functions, unary, binary, nary)?;
        }

        return Ok(());
    }

    pub fn compiled_form(&mut self, lines: &Vec<NessaExpr>) -> Result<Vec<NessaInstruction>, NessaError> {
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
                NessaExpr::FunctionDefinition(_, id, _, a, _, b) => {
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
                
                NessaExpr::PrefixOperationDefinition(_, id, _, _, t, _, b) |
                NessaExpr::PostfixOperationDefinition(_, id, _, _, t, _, b) => {
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
                
                NessaExpr::BinaryOperationDefinition(_, id, _, (_, a_t), (_, b_t), _, b) => {
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
                
                NessaExpr::NaryOperationDefinition(_, id, _, (_, a_t), a, _, b) => {
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
                NessaExpr::FunctionDefinition(_, id, _, a, r, b) => {
                    if let Some(usages) = function_instances.get(id) {
                        for ov in usages {
                            // Store parameters
                            for i in 0..a.len(){
                                if i == 0 {
                                    let comment = format!(
                                        "fn {}{}({}) -> {}",
                                        self.functions[*id].name.green(),
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
                                self.compile(&mut sub_b, &vec!())?;    

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::PrefixOperationDefinition(_, id, _, _, t, r, b) |
                NessaExpr::PostfixOperationDefinition(_, id, _, _, t, r, b) => {
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
                                self.compile(&mut sub_b, &vec!())?;    

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::BinaryOperationDefinition(_, id, _, (_, t1), (_, t2), r, b) => {
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
                                self.compile(&mut sub_b, &vec!())?;    

                                // Statically check the newly instantiated functions
                                for line in &sub_b {
                                    self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                }

                                res.extend(self.compiled_form_body(&sub_b, &functions, &unary, &binary, &nary, &lambda_positions)?);                                
                            }
                        }
                    }
                },

                NessaExpr::NaryOperationDefinition(_, id, _, (_, t), a, r, b) => {
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
                                self.compile(&mut sub_b, &vec!())?;    

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
            Literal(..) | Variable(..) | CompiledLambda(..) => 1, 
            BinaryOperation(_, _, _, a, b) => self.compiled_form_size(a, false, root_counter) + self.compiled_form_size(b, false, root_counter) + 1,
            NaryOperation(_, _, _, a, b) => self.compiled_form_size(a, false, root_counter) + self.compiled_form_body_size(b, false) + 1,
            Return(_, e) | CompiledVariableDefinition(_, _, _, _, e) | CompiledVariableAssignment(_, _, _, _, e) | UnaryOperation(_, _, _, e) => self.compiled_form_size(e, false, root_counter) + 1,
            If(_, ih, ib, ei, e) => {
                let mut res = self.compiled_form_size(ih, false, root_counter) + self.compiled_form_body_size(ib, true) + 1;

                for (h, b) in ei {
                    res += self.compiled_form_size(h, false, root_counter) + self.compiled_form_body_size(b, true) + 1
                }

                if let Some(b) = e {
                    res += self.compiled_form_body_size(b, true);
                }
                
                res
            },
            CompiledFor(_, _, _, _, c, b) => self.compiled_form_size(c, false, root_counter) + self.compiled_form_body_size(b, true) + 9,
            While(_, c, b) => self.compiled_form_size(c, false, root_counter) + self.compiled_form_body_size(b, true) + 2,
            FunctionCall(_, _, _, a) => {
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
    ) -> Result<Vec<NessaInstruction>, NessaError> {
        return match expr {
            NessaExpr::Literal(_, obj) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(obj.clone())))),

            NessaExpr::CompiledLambda(_, i, a, r, _) => {
                Ok(vec!(NessaInstruction::from(CompiledNessaExpr::Literal(Object::new((
                    *lambda_positions.get(i).unwrap(),
                    Type::And(a.iter().map(|(_, t)| t).cloned().collect()),
                    r.clone()
                ))))))
            },

            NessaExpr::Tuple(l, e) => {
                let mut types = Vec::with_capacity(e.len());
                let mut res = vec!();

                for i in e.iter().rev() {
                    types.push(self.infer_type(i).ok_or_else(|| NessaError::compiler_error(
                        "Unable to infer tuple argument type".into(), l, vec!()
                    ))?);

                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }

                res.push(NessaInstruction::from(CompiledNessaExpr::Tuple(types)));

                Ok(res)
            }

            NessaExpr::Variable(_, id, _, _) => Ok(vec!(NessaInstruction::from(CompiledNessaExpr::GetVariable(*id)))), 
            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) | NessaExpr::CompiledVariableAssignment(_, id, _, _, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::StoreVariable(*id)));

                Ok(res)
            },

            NessaExpr::UnaryOperation(l, id, t, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;

                let i_t = self.infer_type(e).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of unary operation argument".into(), l, vec!()
                ))?;

                let (ov_id, _, native, t_args) = self.get_first_unary_op(*id, i_t, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(NessaError::compiler_error(format!("Unary operation expected {} type arguments and got {}", t_args.len(), t.len()), l, vec!()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::UnaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = unary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::BinaryOperation(l, id, t, a, b) => {
                let mut res = self.compiled_form_expr(b, functions, unary, binary, nary, lambda_positions, false)?;
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, false)?);
                
                let a_t = self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of binary operation argument".into(), l, vec!()
                ))?;

                let b_t = self.infer_type(b).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of binary operation argument".into(), l, vec!()
                ))?;

                let (ov_id, _, native, t_args) = self.get_first_binary_op(*id, a_t, b_t, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(NessaError::compiler_error(format!("Binary operation expected {} type arguments and got {}", t_args.len(), t.len()), l, vec!()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::BinaryOperatorCall(*id, ov_id, t.clone())));

                } else {
                    let pos = binary.get(&(*id, ov_id, t.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::NaryOperation(l, id, tm, a, b) => {
                let mut res = vec!();

                for i in b.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }
                
                res.extend(self.compiled_form_expr(a, functions, unary, binary, nary, lambda_positions, false)?);

                let a_t = self.infer_type(a).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of n-ary operation argument".into(), l, vec!()
                ))?;

                let b_t = b.iter().map(|i| self.infer_type(i).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of n-ary operation argument".into(), l, vec!()
                ))).collect::<Result<_, _>>()?;

                let (ov_id, _, native, t_args) = self.get_first_nary_op(*id, a_t, b_t, false).unwrap();

                if tm.len() != t_args.len() {
                    return Err(NessaError::compiler_error(format!("N-ary operation expected {} type arguments and got {}", t_args.len(), tm.len()), l, vec!()));
                }

                if native {
                    res.push(NessaInstruction::from(CompiledNessaExpr::NaryOperatorCall(*id, ov_id, tm.clone())));

                } else {
                    let pos = nary.get(&(*id, ov_id, tm.clone())).unwrap();
                    res.push(NessaInstruction::from(CompiledNessaExpr::Call(*pos)));
                }

                Ok(res)
            },

            NessaExpr::If(_, ih, ib, ei, e) => {
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
            NessaExpr::While(_, c, b) => {
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
            NessaExpr::CompiledFor(l, it_var_id, elem_var_id, _, c, b) => {
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
                                    Err(NessaError::compiler_error(format!("Funtion overload is_consumed({}) returns {} (expected Bool)", it_mut.get_name(self), consumed_res.get_name(self)), l, vec!()))
                                }

                            } else {
                                Err(NessaError::compiler_error(format!("Funtion overload for is_consumed({}) is not defined", it_mut.get_name(self)), l, vec!()))
                            }

                        } else {
                            Err(NessaError::compiler_error(format!("Funtion overload for next({}) is not defined", it_mut.get_name(self)), l, vec!()))
                        }

                    } else {
                        Err(NessaError::compiler_error(format!("Funtion overload for iterator({}) is not defined", t.get_name(self)), l, vec!()))
                    }
                    
                } else {
                    Err(NessaError::compiler_error("Container expression does not return a valid value".into(), l, vec!()))
                }
            },
            NessaExpr::Return(_, e) => {
                let mut res = self.compiled_form_expr(e, functions, unary, binary, nary, lambda_positions, false)?;
                res.push(NessaInstruction::from(CompiledNessaExpr::Return));

                Ok(res)
            },
            NessaExpr::FunctionCall(l, id, t, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, false)?);
                }
                
                let args_types = a.iter().map(|i| self.infer_type(i).ok_or_else(|| NessaError::compiler_error(
                    "Unable to infer type of function argument".into(), l, vec!()
                ))).collect::<Result<_, _>>()?;

                let (ov_id, _, native, t_args) = self.get_first_function_overload(*id, args_types, false).unwrap();

                if t.len() != t_args.len() {
                    return Err(NessaError::compiler_error(format!("Function call {} expected {} type arguments and got {}", self.functions[*id].name.green(), t_args.len(), t.len()), l, vec!()));
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
    ) -> Result<Vec<NessaInstruction>, NessaError> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, functions, unary, binary, nary, lambda_positions, true)).flat_map(|i| i.unwrap()).collect());
    }

    pub fn define_module_macro(&mut self, definition: NessaExpr) -> Result<(), NessaError> {
        match definition {
            NessaExpr::Macro(l, n, p, m) => {
                if self.macros.iter().any(|i| i.0 == n) {
                    return Err(NessaError::compiler_error(format!("Syntax with name '{n}' is already defined"), &l, vec!()));
                }

                self.macros.push((n, p, m));
            }

            _ => {}
        }

        return Ok(());
    }

    pub fn define_module_class(&mut self, definition: NessaExpr, needed: &mut bool) -> Result<(), NessaError> {
        match definition {
            NessaExpr::ClassDefinition(_, n, _, _, _) if self.type_templates.iter().filter(|t| t.name == n).next().is_some() => {},

            NessaExpr::ClassDefinition(l, n, t, a, p) => {
                *needed = true; // Repeat class parsing after creating a new one

                let err = self.implicit_syntax_check(&n, &t, &a, &p);

                if let Err(msg) = err {
                    return Err(NessaError::compiler_error(msg, &l, vec!()));
                }

                let n_templates = t.len();
                let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                
                let err = self.define_type(n.clone(), t, a.clone(), p, Some(
                    |ctx, c_type, s| {
                        if let Ok((_, o)) = ctx.parse_literal_type(c_type, Span::new(s.as_str())) {
                            return Ok(o);
                        }

                        return Err(format!("Unable to parse {} from {}", c_type.name, s));
                    }
                ));

                if let Err(msg) = err {
                    return Err(NessaError::compiler_error(msg, &l, vec!()));
                }

                self.define_function(n.clone()).unwrap_or_default(); // Define constructor function

                let func_id = self.functions.iter().filter(|i| i.name == n).next().unwrap().id;
                let class_id = self.type_templates.iter().filter(|t| t.name == n).next().unwrap().id;

                if n_templates == 0 {
                    // Define constructor instance
                    let err = self.define_native_function_overload(func_id, 0, &arg_types, Type::Basic(class_id), |_, r, a| {
                        if let Type::Basic(id) = r {
                            return Ok(Object::new(TypeInstance {
                                id: *id,
                                params: vec!(),
                                attributes: a
                            }))
                        }

                        unreachable!();
                    });

                    if let Err(msg) = err {
                        return Err(NessaError::compiler_error(msg, &l, vec!()));
                    }
                    
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
                            let err = self.define_native_function_overload(att_func_id, 0, &[Type::Basic(class_id)], att_type.clone(), match i {
                                #( N => |_, _, a| Ok(a[0].get::<TypeInstance>().attributes[N].clone()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });

                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });

                        seq!(N in 0..100 {
                            let err = self.define_native_function_overload(att_func_id, 0, &[Type::Ref(Box::new(Type::Basic(class_id)))], ref_type, match i {
                                #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_obj()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });

                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });

                        seq!(N in 0..100 {
                            let err = self.define_native_function_overload(att_func_id, 0, &[Type::MutRef(Box::new(Type::Basic(class_id)))], mut_type, match i {
                                #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_mut_obj()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });
                            
                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });
                    }

                } else {
                    let templ = (0..n_templates).into_iter().map(|i| Type::TemplateParam(i)).collect::<Vec<_>>();

                    // Define constructor instance
                    let err = self.define_native_function_overload(func_id, n_templates, &arg_types, Type::Template(class_id, templ.clone()), |t, r, a| {
                        if let Type::Template(id, _) = r {
                            return Ok(Object::new(TypeInstance {
                                id: *id,
                                params: t.clone(),
                                attributes: a
                            }))
                        }

                        unreachable!();
                    });

                    if let Err(msg) = err {
                        return Err(NessaError::compiler_error(msg, &l, vec!()));
                    }

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
                            let err = self.define_native_function_overload(att_func_id, n_templates, &[Type::Template(class_id, templ.clone())], att_type.clone(), match i {
                                #( N => |_, _, a| Ok(a[0].get::<TypeInstance>().attributes[N].clone()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });
                            
                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });

                        seq!(N in 0..100 {
                            let err = self.define_native_function_overload(att_func_id, n_templates, &[Type::Ref(Box::new(Type::Template(class_id, templ.clone())))], ref_type.clone(), match i {
                                #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_obj()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });
                            
                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });

                        seq!(N in 0..100 {
                            let err = self.define_native_function_overload(att_func_id, n_templates, &[Type::MutRef(Box::new(Type::Template(class_id, templ.clone())))], mut_type.clone(), match i {
                                #( N => |_, _, a| Ok(a[0].deref::<TypeInstance>().attributes[N].get_ref_mut_obj()), )*
                                _ => unimplemented!("Unable to define attribute with index {} (max is 100)", i)
                            });
                            
                            if let Err(msg) = err {
                                return Err(NessaError::compiler_error(msg, &l, vec!()));
                            }
                        });
                    }
                }
            },

            _ => unreachable!()
        }

        return Ok(());
    }

    pub fn define_module_macros(&mut self, code: &String) -> Result<(), NessaError> {
        let ops = self.nessa_macros_parser(Span::new(code));
        
        if let Err(err) = ops {
            return Err(NessaError::from(err));
        }
        
        for i in ops.unwrap().1 {
            self.define_module_macro(i)?;
        }

        return Ok(());
    }

    pub fn define_module_classes(&mut self, code: &String) -> Result<(), NessaError> {
        let mut needed = true;

        while needed {
            needed = false;
            let ops = self.nessa_class_parser(Span::new(code));
            
            if let Err(err) = ops {
                return Err(NessaError::from(err));
            }
            
            for i in ops.unwrap().1 {
                self.define_module_class(i, &mut needed)?;
            }
        }

        return Ok(());
    }
    
    pub fn define_module_operators(&mut self, code: &String) -> Result<(), NessaError> {
        let ops = self.nessa_operators_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(NessaError::from(err));
        }

        for i in ops.unwrap().1 {
            let (l, err) = match &i {
                NessaExpr::PrefixOperatorDefinition(l, n, p) => (l, self.define_unary_operator(n.clone(), true, *p)),
                NessaExpr::PostfixOperatorDefinition(l, n, p) => (l, self.define_unary_operator(n.clone(), false, *p)),
                NessaExpr::BinaryOperatorDefinition(l, n, p) => (l, self.define_binary_operator(n.clone(), *p)),
                NessaExpr::NaryOperatorDefinition(l, o, c, p) => (l, self.define_nary_operator(o.clone(), c.clone(), *p)),

                _ => unreachable!()
            };

            if let Err(msg) = err {
                return Err(NessaError::compiler_error(msg, &l, vec!()));
            }
        }

        return Ok(());
    }
    
    pub fn define_module_functions(&mut self, code: &String) -> Result<(), NessaError> {
        let ops = self.nessa_function_headers_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(NessaError::from(err));
        }

        for i in ops.unwrap().1 {
            self.define_function(i.0).unwrap_or_default();
        }

        return Ok(());
    }
    
    pub fn define_module_operations(&mut self, code: &String) -> Result<(), NessaError> {
        let ops = self.nessa_operations_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(NessaError::from(err));
        }

        for i in ops.unwrap().1 {
            let (l, err) = match &i {
                NessaExpr::PrefixOperationDefinition(l, id, tm, _a, t, r, _) |
                NessaExpr::PostfixOperationDefinition(l, id, tm, _a, t, r, _) => (l, self.define_unary_operation(*id, tm.len(), t.clone(), r.clone(), None)),
                NessaExpr::BinaryOperationDefinition(l, id, tm, (_a, ta), (_b, tb), r, _) => (l, self.define_binary_operation(*id, tm.len(), ta.clone(), tb.clone(), r.clone(), None)),
                NessaExpr::NaryOperationDefinition(l, id, tm, (_a, ta), v, r, _) => (l, self.define_nary_operation(*id, tm.len(), ta.clone(), &v.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(), r.clone(), None)),

                _ => unreachable!()
            };

            if let Err(msg) = err {
                return Err(NessaError::compiler_error(msg, &l, vec!()));
            }
        }

        return Ok(());
    }

    pub fn define_module_function_overloads(&mut self, lines: &Vec<NessaExpr>) -> Result<(), NessaError> {
        for i in lines {
            match i {
                NessaExpr::FunctionDefinition(l, id, t, a, r, _) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    let err = self.define_function_overload(*id, t.len(), &arg_types, r.clone(), None);

                    if let Err(msg) = err {
                        return Err(NessaError::compiler_error(msg, &l, vec!()));
                    }
                },

                _ => {}
            }
        }

        return Ok(());
    }

    pub fn parse_nessa_module(&mut self, code: &String) -> Result<Vec<NessaExpr>, NessaError> {
        return match self.nessa_parser(Span::new(code)) {
            Ok((_, lines)) => Ok(lines),

            Err(nom::Err::Error(error)) |
            Err(nom::Err::Failure(error)) => Err(NessaError::from(error)),

            _ => unreachable!()
        };
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

    fn map_nessa_function(&mut self, other: &NessaContext, id: usize, functions: &mut HashMap<usize, usize>, l: &Location) -> Result<usize, NessaError> {
        let f_name = &other.functions[id].name;

        if !functions.contains_key(&id) {
            let fn_id;

            // If the function has another id in the target context
            if let Some(f) = self.functions.iter().filter(|f| f.name == *f_name).next() {
                fn_id = f.id;

            } else { // Else the function needs to be defined
                fn_id = self.functions.len();

                if let Err(err) = self.define_function(f_name.clone()) {
                    return Err(NessaError::compiler_error(err, l, vec!()));
                }
            }

            return Ok(*functions.entry(id).or_insert(fn_id));
        }

        return Ok(functions[&id]);
    }

    fn map_nessa_unary_operator(&mut self, other: &NessaContext, id: usize, unary_operators: &mut HashMap<usize, usize>, l: &Location) -> Result<usize, NessaError> {
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

                    if let Err(err) = self.define_unary_operator(r.clone(), *prefix, *precedence) {
                        return Err(NessaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*unary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(NessaError::compiler_error(format!("Unable to find unary operator with id = {}", id), l, vec!()));
        }

        return Ok(unary_operators[&id]);
    }

    fn map_nessa_binary_operator(&mut self, other: &NessaContext, id: usize, binary_operators: &mut HashMap<usize, usize>, l: &Location) -> Result<usize, NessaError> {
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

                    if let Err(err) = self.define_binary_operator(r.clone(), *precedence) {
                        return Err(NessaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*binary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(NessaError::compiler_error(format!("Unable to find binary operator with id = {}", id), l, vec!()));
        }

        return Ok(binary_operators[&id]);
    }

    fn map_nessa_nary_operator(&mut self, other: &NessaContext, id: usize, nary_operators: &mut HashMap<usize, usize>, l: &Location) -> Result<usize, NessaError> {
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

                    if let Err(err) = self.define_nary_operator(or.clone(), cr.clone(), *precedence) {
                        return Err(NessaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*nary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(NessaError::compiler_error(format!("Unable to find binary operator with id = {}", id), l, vec!()));
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
    ) -> Result<(), NessaError> {
        match expr {
            NessaExpr::Literal(..) |
            NessaExpr::NameReference(..) => {}

            NessaExpr::VariableDefinition(_, _, t, e) => {
                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.map_basic_types(&mut mapping);

                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::VariableAssignment(_, _, e) => {
                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::UnaryOperation(l, id, t, a) => {
                *id = self.map_nessa_unary_operator(ctx, *id, unary_operators, l)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::BinaryOperation(l, id, t, a, b) => {
                *id = self.map_nessa_binary_operator(ctx, *id, unary_operators, l)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                self.map_nessa_expression(b, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            NessaExpr::NaryOperation(l, id, t, a, b) => {
                *id = self.map_nessa_nary_operator(ctx, *id, nary_operators, l)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                self.map_nessa_expression(a, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;

                for arg in b {
                    self.map_nessa_expression(arg, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::FunctionCall(l, id, t, args) => {
                *id = self.map_nessa_function(ctx, *id, functions, l)?;

                let mut mapping = |id| self.map_nessa_class(ctx, id, classes);
                *t = t.iter().map(|t| t.map_basic_types(&mut mapping)).collect();

                for arg in args {
                    self.map_nessa_expression(arg, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::If(_, ih, ib, ei, eb) => {
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

            NessaExpr::While(_, c, lines) |
            NessaExpr::For(_, _, c, lines) => {
                self.map_nessa_expression(c, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                
                for line in lines {
                    self.map_nessa_expression(line, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
                }
            }

            NessaExpr::Return(_, e) => {
                self.map_nessa_expression(e, ctx, functions, unary_operators, binary_operators, nary_operators, classes)?;
            }

            _ => unimplemented!("{:?}", expr)
        }

        return Ok(());
    }

    pub fn import_code(
        &mut self, 
        module_name: &String,
        code: &Vec<NessaExpr>, 
        source: &Vec<String>, 
        ctx: &NessaContext, 
        imports: &Imports,
        current_imports: &mut ImportMap
    ) -> Result<(Vec<NessaExpr>, Vec<String>), NessaError> {
        let mut res = vec!();
        let mut new_source = vec!();
        let mut functions: HashMap<usize, usize> = HashMap::new();
        let mut unary_operators: HashMap<usize, usize> = HashMap::new();
        let mut binary_operators: HashMap<usize, usize> = HashMap::new();
        let mut nary_operators: HashMap<usize, usize> = HashMap::new();
        let mut classes: HashMap<usize, usize> = HashMap::new();

        for (line, module) in code.iter().zip(source) {
            let foreign = module != module_name;
            let curr_mod_imports = current_imports.entry(module.clone()).or_default();

            match line {
                NessaExpr::Macro(_, n, _, _) => {
                    if imports.contains_key(&ImportType::Syntax) && imports[&ImportType::Syntax].contains(n) && 
                    (!foreign || !curr_mod_imports.contains_key(&ImportType::Syntax) || !curr_mod_imports[&ImportType::Syntax].contains(n)) {
                        self.define_module_macro(line.clone())?
                    }
                }

                NessaExpr::ClassDefinition(l, n, t, atts, p) => {
                    if imports.contains_key(&ImportType::Class) && imports[&ImportType::Class].contains(n) && 
                    (!foreign || !curr_mod_imports.contains_key(&ImportType::Class) || !curr_mod_imports[&ImportType::Class].contains(n)) {

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_atts = atts.iter().map(|(n, t)| (n.clone(), t.map_basic_types(&mut mapping))).collect();

                        let mut needed = false;
                        let mapped_expr = NessaExpr::ClassDefinition(l.clone(), n.clone(), t.clone(), mapped_atts, p.clone());

                        self.define_module_class(mapped_expr.clone(), &mut needed)?;
                        
                        res.push(mapped_expr);
                        new_source.push(module.clone());

                        // Add to current imports
                        current_imports.entry(module.clone()).or_default().entry(ImportType::Class).or_default().insert(n.clone());
                    }
                }

                NessaExpr::FunctionDefinition(l, id, t, a, r, b) => {
                    let f_name = &ctx.functions[*id].name;

                    // If the function needs to be imported
                    if imports.contains_key(&ImportType::Fn) && imports[&ImportType::Fn].contains(f_name) && 
                    (!foreign || !curr_mod_imports.contains_key(&ImportType::Fn) || !curr_mod_imports[&ImportType::Fn].contains(f_name)) {

                        let fn_id = self.map_nessa_function(&ctx, *id, &mut functions, l)?;

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_args = a.iter().map(|(n, t)| (n.clone(), t.map_basic_types(&mut mapping))).collect::<Vec<_>>();
                        let mapped_return = r.map_basic_types(&mut mapping);

                        let mut mapped_body = b.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_nessa_expression(line, ctx, &mut functions, &mut unary_operators, &mut binary_operators, &mut nary_operators, &mut classes)?;
                        }

                        let arg_types = mapped_args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                        if let Err(err) = self.define_function_overload(fn_id, t.len(), &arg_types, mapped_return.clone(), None) {
                            return Err(NessaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(NessaExpr::FunctionDefinition(l.clone(), fn_id, t.clone(), mapped_args.clone(), mapped_return, mapped_body));
                        new_source.push(module.clone());

                        // Add to current imports
                        current_imports.entry(module.clone()).or_default().entry(ImportType::Fn).or_default().insert(f_name.clone());
                    }
                }

                NessaExpr::PrefixOperationDefinition(l, id, t, arg, arg_t, ret, body) |
                NessaExpr::PostfixOperationDefinition(l, id, t, arg, arg_t, ret, body) => {
                    let rep;
                    let op_prefix;
                    let op_import_type;

                    if let Operator::Unary { representation, prefix, .. } = &ctx.unary_ops[*id] {
                        rep = representation;
                        op_prefix = prefix;
                        op_import_type = if *prefix { ImportType::Prefix } else { ImportType::Postfix };
                    
                    } else {
                        unreachable!();
                    }

                    if imports.contains_key(&op_import_type) && imports[&op_import_type].contains(rep) && 
                    (!foreign || !curr_mod_imports.contains_key(&op_import_type) || !curr_mod_imports[&op_import_type].contains(rep)) {
                    
                        let op_id = self.map_nessa_unary_operator(&ctx, *id, &mut unary_operators, l)?;

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_arg_t = arg_t.map_basic_types(&mut mapping);
                        let mapped_return = ret.map_basic_types(&mut mapping);

                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_nessa_expression(line, ctx, &mut functions, &mut unary_operators, &mut binary_operators, &mut nary_operators, &mut classes)?;
                        }

                        if let Err(err) = self.define_unary_operation(*id, t.len(), mapped_arg_t.clone(), mapped_return.clone(), None) {
                            return Err(NessaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        if *op_prefix {
                            res.push(NessaExpr::PrefixOperationDefinition(l.clone(), op_id, t.clone(), arg.clone(), mapped_arg_t, mapped_return, mapped_body));
                            current_imports.entry(module.clone()).or_default().entry(ImportType::Prefix).or_default().insert(rep.clone());

                        } else {
                            res.push(NessaExpr::PostfixOperationDefinition(l.clone(), op_id, t.clone(), arg.clone(), mapped_arg_t, mapped_return, mapped_body));
                            current_imports.entry(module.clone()).or_default().entry(ImportType::Postfix).or_default().insert(rep.clone());
                        }

                        new_source.push(module.clone());
                    }
                },

                NessaExpr::BinaryOperationDefinition(l, id, t, a, b, ret, body) => {
                    let rep = ctx.binary_ops[*id].get_repr();

                    if imports.contains_key(&ImportType::Binary) && imports[&ImportType::Binary].contains(&rep) && 
                    (!foreign || !curr_mod_imports.contains_key(&ImportType::Binary) || !curr_mod_imports[&ImportType::Binary].contains(&rep)) {

                        let op_id = self.map_nessa_binary_operator(&ctx, *id, &mut binary_operators, l)?;

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_arg1 = (a.0.clone(), a.1.map_basic_types(&mut mapping));
                        let mapped_arg2 = (b.0.clone(), b.1.map_basic_types(&mut mapping));
                        let mapped_return = ret.map_basic_types(&mut mapping);

                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_nessa_expression(line, ctx, &mut functions, &mut unary_operators, &mut binary_operators, &mut nary_operators, &mut classes)?;
                        }

                        if let Err(err) = self.define_binary_operation(*id, t.len(), mapped_arg1.1.clone(), mapped_arg2.1.clone(), mapped_return.clone(), None) {
                            return Err(NessaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(NessaExpr::BinaryOperationDefinition(l.clone(), op_id, t.clone(), mapped_arg1, mapped_arg2, mapped_return, mapped_body));
                        new_source.push(module.clone());
                        
                        // Add to current imports
                        current_imports.entry(module.clone()).or_default().entry(ImportType::Binary).or_default().insert(rep.clone());
                    }
                },

                NessaExpr::NaryOperationDefinition(l, id, t, arg, args, ret, body) => {
                    let rep = ctx.nary_ops[*id].get_repr();

                    if imports.contains_key(&ImportType::Nary) && imports[&ImportType::Nary].contains(&rep) && 
                    (!foreign || !curr_mod_imports.contains_key(&ImportType::Nary) || !curr_mod_imports[&ImportType::Nary].contains(&rep)) {

                        let op_id = self.map_nessa_nary_operator(&ctx, *id, &mut nary_operators, l)?;

                        let mut mapping = |id| self.map_nessa_class(ctx, id, &mut classes);
                        let mapped_arg = (arg.0.clone(), arg.1.map_basic_types(&mut mapping));
                        let mapped_args = args.iter().map(|(n, t)| (n.clone(), t.map_basic_types(&mut mapping))).collect::<Vec<_>>();
                        let mapped_return = ret.map_basic_types(&mut mapping);

                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_nessa_expression(line, ctx, &mut functions, &mut unary_operators, &mut binary_operators, &mut nary_operators, &mut classes)?;
                        }

                        let arg_types = mapped_args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                        if let Err(err) = self.define_nary_operation(*id, t.len(), mapped_arg.1.clone(), &arg_types, mapped_return.clone(), None) {
                            return Err(NessaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(NessaExpr::NaryOperationDefinition(l.clone(), op_id, t.clone(), mapped_arg, mapped_args, mapped_return, mapped_body));
                        new_source.push(module.clone());
                        
                        // Add to current imports
                        current_imports.entry(module.clone()).or_default().entry(ImportType::Nary).or_default().insert(rep.clone());
                    }
                },

                _ => {}
            }
        }

        return Ok((res, new_source));
    }

    // BFS on imports
    fn cascade_imports(
        imports: &mut ImportMap,
        modules: &HashMap<String, NessaModule>
    )
    {
        let mut res = HashMap::new();
        
        while res != *imports {
            res = imports.clone();

            for (name, _) in imports.iter() {
                for (d_name, d_deps) in &modules.get(name).unwrap().imports {
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
        modules: &HashMap<String, NessaModule>
    )
    {
        for (m, imps) in imports {
            let mut new_imports = Imports::new();
            let module = &modules.get(m).unwrap();

            for (t, names) in imps.iter() {
                if let ImportType::Syntax = t { // Syntaxes do not have to be mapped
                    new_imports.entry(t.clone()).or_default().extend(names.iter().cloned());                    

                } else {
                    for name in names.iter() {
                        let id = module.ctx.map_import(t, name);
    
                        module.inner_dependencies.dfs(&(t.clone(), id), |(tp, id)| {
                            let mapped_name = module.ctx.rev_map_import(tp, *id);
                            new_imports.entry(tp.clone()).or_default().insert(mapped_name);
                        });
                    }
                }
            }

            for (t, names) in new_imports {
                imps.entry(t.clone()).or_default().extend(names);
            }
        }
    }

    pub fn parse_and_precompile_with_dependencies(
        &mut self, 
        name: &String,
        code: &String, 
        modules: &HashMap<String, NessaModule>
    ) -> Result<(Vec<NessaExpr>, Vec<String>), NessaError> {
        let mut res = vec!();
        let mut source = vec!();
        let mut imports = nessa_module_imports_parser(Span::new(&code)).unwrap().1; // TODO: should cache this
        let mut current_imports = ImportMap::new();

        Self::cascade_imports(&mut imports, modules);
        Self::cascade_imports_inner(&mut imports, modules);

        // Import code from dependencies
        for (m, mut i) in imports {
            // Delete thing that are already imported
            for (t, ii) in current_imports.entry(m.clone()).or_default() {
                i.entry(t.clone()).or_default().retain(|v| !ii.contains(v));
            }

            let other = modules.get(&m).unwrap();

            let (mut new_code, mut new_source) = self.import_code(&m, &other.code, &other.source, &other.ctx, &i, &mut current_imports)?;
            source.append(&mut new_source);
            res.append(&mut new_code);
        }

        let mut main_code = self.parse_without_precompiling(code)?;
        source.extend(std::iter::repeat(name.clone()).take(main_code.len()));
        res.append(&mut main_code);

        return Ok((res, source));
    }

    pub fn parse_without_precompiling(&mut self, code: &String) -> Result<Vec<NessaExpr>, NessaError> {
        self.define_module_classes(code)?;
        self.define_module_operators(code)?;
        self.define_module_functions(code)?;
        self.define_module_operations(code)?;
        self.define_module_macros(code)?;

        let lines = self.parse_nessa_module(code)?;

        self.define_module_function_overloads(&lines)?;

        return Ok(lines);
    }

    pub fn precompile_module(&mut self, lines: &mut Vec<NessaExpr>) -> Result<(), NessaError> {        
        self.compile(lines, &vec!())?;

        for expr in lines.iter_mut() {
            self.static_check(expr)?;
        }

        return Ok(());
    }

    pub fn parse_and_precompile(&mut self, code: &String) -> Result<Vec<NessaExpr>, NessaError> {
        let mut lines = self.parse_without_precompiling(code)?;
        self.precompile_module(&mut lines)?;

        return Ok(lines);
    }

    pub fn parse_and_compile(&mut self, code: &String) -> Result<Vec<NessaInstruction>, NessaError> {
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
    fn function_names_and_calls() {
        let mut ctx = standard_ctx();
        
        let code_1_str = "
        inc(5);
        ";
        
        let code_str = "
        inc<Number>(5);
        ";

        let (_, mut code) = ctx.nessa_parser(Span::new(code_1_str)).unwrap();
        ctx.compile_functions(&mut code).unwrap();

        assert_eq!(code, vec!(
            NessaExpr::FunctionCall(Location::none(), 0, vec!(), vec!(
                NessaExpr::Literal(Location::none(), Object::new(Number::from(5)))
            ))
        ));
        
        let (_, mut code) = ctx.nessa_parser(Span::new(code_str)).unwrap();

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