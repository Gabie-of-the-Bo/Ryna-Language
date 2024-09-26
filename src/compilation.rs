use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

use colored::Colorize;
use levenshtein::levenshtein;
use nom::error::{VerboseErrorKind, VerboseError};
use rustc_hash::FxHashSet;
use serde::{Serialize, Deserialize};
use malachite::Integer;

use crate::cache::needs_import;
use crate::cache::needs_line_import;
use crate::config::ImportMap;
use crate::config::Imports;
use crate::config::RynaModule;
use crate::context::RynaContext;
use crate::debug::DebugInfo;
use crate::debug::DebugInfoBuilder;
use crate::graph::DirectedGraph;
use crate::id_mapper::IdMapper;
use crate::interfaces::ITERABLE_ID;
use crate::macros::RynaMacro;
use crate::object::TypeInstance;
use crate::parser::*;
use crate::object::RynaArray;
use crate::types::*;
use crate::object::Object;
use crate::functions::*;
use crate::operations::*;
use crate::variable_map::VariableMap;

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

#[derive(Debug, Clone)]
pub struct RynaError {
    pub err_type: String,
    pub message: String,

    pub has_location: bool,
    pub line: usize,
    pub column: usize,
    pub module: Arc<String>,
    pub fragment: String,

    pub suggestions: Vec<String>
}

impl RynaError {
    pub fn in_module(mut self, module: Arc<String>) -> Self {
        self.module = module;
        self
    } 

    pub fn syntax_error(message: String, line: usize, column: usize, module: Arc<String>, fragment: String, suggestions: Vec<String>) -> Self {
        RynaError { err_type: "Syntax error".into(), has_location: true, message, line, column, module, fragment, suggestions }
    }

    #[cold]
    pub fn compiler_error(message: String, location: &Location, suggestions: Vec<String>) -> Self {
        RynaError { 
            err_type: "Compilation error".into(), 
            has_location: true,
            message, 
            line: location.line, 
            column: location.column, 
            module: location.module.clone(),
            fragment: location.span.clone(), 
            suggestions 
        }
    }

    #[cold]
    pub fn execution_error(message: String) -> Self {
        RynaError { 
            err_type: "Execution error".into(), 
            has_location: false,
            message, 
            line: 0, 
            column: 0, 
            module: Arc::default(),
            fragment: "".into(), 
            suggestions: vec!()
        }
    }

    #[cold]
    pub fn module_error(message: String) -> Self {
        RynaError { 
            err_type: "Module error".into(), 
            has_location: false,
            message, 
            line: 0, 
            column: 0, 
            module: Arc::default(),
            fragment: "".into(), 
            suggestions: vec!()
        }
    }

    #[cold]
    pub fn emit(&self) -> ! {
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
                "\n[{} in module {}, line {}, column {}]\n\n • {}:\n\n\t[...] {} [...]\n\t      {}\n", 
                self.err_type.red().bold(), 
                self.module.green(),
                self.line.to_string().yellow(), self.column.to_string().yellow(), 
                self.message, frag,
                "^".repeat(frag.len()).red()
            );

            if !self.suggestions.is_empty() {
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
        
        exit_process();
    }
}

impl<'a> From<VerboseError<Span<'a>>> for RynaError {
    fn from(error: VerboseError<Span<'a>>) -> Self {
        let err = error.errors.last().unwrap();

        let fragment = err.0;
        let error_msg = match &err.1 {
            VerboseErrorKind::Context(ctx) => ctx,
            _ => "Unable to parse"
        };

        RynaError::syntax_error(
            error_msg.into(), 
            fragment.location_line() as usize, fragment.get_column(), Arc::default(),
            fragment.to_string(), 
            vec!()
        )
    }
}

impl<'a> From<nom::Err<VerboseError<Span<'a>>>> for RynaError {
    fn from(error: nom::Err<VerboseError<Span<'a>>>) -> Self {
        match error {
            nom::Err::Error(err) |
            nom::Err::Failure(err) => RynaError::from(err),

            _ => unreachable!()
        }
    }
}

/*
    ╒═════════════════════════════════╕
    │ Expression tree transformations │
    ╘═════════════════════════════════╛
*/

pub fn exit_process() -> ! {
    if cfg!(test) {
        panic!();

    } else {
        std::process::exit(1);
    }
}

pub fn message_and_exit(msg: String) -> ! {
    if cfg!(test) {
        panic!("{}", msg);

    } else {
        RynaError::execution_error(msg).emit();
    }
}

#[macro_export]
macro_rules! ryna_warning {
    ($pat: expr $( , $more: expr)*) => {
        println!(
            "[{}] {}",
            "Warning".yellow(),
            format!($pat, $($more,)*)
        );
    };
}

#[macro_export]
macro_rules! located_ryna_warning {
    ($l: expr, $pat: expr $( , $more: expr)*) => {
        use colored::Colorize;
        
        let loc = $l;

        println!(
            "[{} in module {}, line {}, column {}] {}",
            "Warning".yellow(),
            loc.module.green(),
            loc.line.to_string().yellow(), loc.column.to_string().yellow(),
            format!($pat, $($more,)*)
        );
    };
}

#[macro_export]
macro_rules! ryna_error {
    ($pat: expr $( , $more: expr)*) => {
        {
            use colored::Colorize;
            use $crate::compilation::exit_process;

            eprintln!(
                "[{}] {}",
                "Error".red(),
                format!($pat, $($more,)*)
            );
    
            exit_process();
        }
    };
}

impl RynaContext {
    /*
        ╒══════════════════════╕
        │ Function compilation │
        ╘══════════════════════╛
    */

    fn infer_lambda_return_type(&mut self, lines: &mut Vec<RynaExpr>) -> Result<Option<Type>, RynaError> {
        let merge_types = |a: Option<Type>, b: Option<Type>, ctx: &mut RynaContext| -> Option<Type> {
            if b.is_none() {
                return a;
            }
            
            match &a {
                Some(na) => {
                    if na.bindable_to(b.as_ref().unwrap(), ctx) {
                        b
                    
                    } else if b.as_ref().unwrap().bindable_to(na, ctx) {
                        return a;

                    } else {
                        return Some(Type::Or(vec!(na.clone(), b.unwrap())));
                    }
                },

                None => b,
            }
        };

        let mut res = None;

        for expr in lines {
            match expr {
                RynaExpr::While(_, _, b) |
                RynaExpr::CompiledFor(_, _, _, _, _, b) => res = merge_types(res, self.infer_lambda_return_type(b)?, self),

                RynaExpr::If(_, _, ib, ei, eb) => {
                    res = merge_types(res, self.infer_lambda_return_type(ib)?, self);

                    for (_, eib) in ei {
                        res = merge_types(res, self.infer_lambda_return_type(eib)?, self);
                    }

                    if let Some(eb_inner) = eb {
                        res = merge_types(res, self.infer_lambda_return_type(eb_inner)?, self);
                    }
                },

                RynaExpr::Return(_, expr) => res = merge_types(res, Some(self.infer_type(expr)?), self),

                _ => {}
            }
        }

        Ok(res)
    }

    /*
        ╒══════════════════╕
        │ Full compilation │
        ╘══════════════════╛
    */

    fn compile_expr_variables(&mut self, expr: &mut RynaExpr, registers: &mut Vec<usize>, var_map: &mut VariableMap) -> Result<(), RynaError> {
        match expr {
            // Compile variable references
            RynaExpr::NameReference(l, n) if var_map.is_var_defined(n) => {
                let (idx, t) = var_map.get_var(n).unwrap();
                *expr = RynaExpr::Variable(l.clone(), *idx, n.clone(), t.clone());
            },

            RynaExpr::NameReference(l, n) => {
                let func = self.get_function_id(n.clone()).ok();
                *expr = RynaExpr::QualifiedName(l.clone(), n.clone(), func);
            },

            RynaExpr::VariableAssignment(l, n, e) if var_map.is_var_defined(n) => {
                if var_map.is_var_defined(n) {
                    self.compile_expr_variables(e, registers, var_map)?;

                    let (idx, t) = var_map.get_var(n).unwrap();

                    *expr = RynaExpr::CompiledVariableAssignment(l.clone(), *idx, n.clone(), t.clone(), e.clone());
                
                } else {
                    return Err(RynaError::compiler_error(format!("Variable with name {} is not defined", n.green()), l, vec!()));
                }
            },

            // Compile variable definitions
            RynaExpr::VariableDefinition(l, n, t, e) => {
                if var_map.is_var_defined_in_last_ctx(n) {
                    return Err(RynaError::compiler_error(format!("Variable with name {} is already defined", n.green()), l, vec!()));
                }

                let idx = registers.pop().unwrap();

                self.compile_expr_variables(e, registers, var_map)?;

                if let Type::InferenceMarker = t {
                    *t = self.infer_type(e)?;
                }

                var_map.define_var(n.clone(), idx, t.clone());

                *expr = RynaExpr::CompiledVariableDefinition(l.clone(), idx, n.clone(), t.clone(), e.clone());
            },

            RynaExpr::CompiledVariableAssignment(_, _, _, _, e) => {
                self.compile_expr_variables(e, registers, var_map)?;
            }

            RynaExpr::CompiledVariableDefinition(l, id, n, t, e) => {
                if var_map.is_var_defined_in_last_ctx(n) {
                    return Err(RynaError::compiler_error(format!("Variable with name {} is already defined", n.green()), l, vec!()));
                }

                self.compile_expr_variables(e, registers, var_map)?;

                var_map.define_var(n.clone(), *id, t.clone());
            }

            // Compile operations
            RynaExpr::UnaryOperation(l, id, t, e) => {
                self.compile_expr_variables(e, registers, var_map)?;

                if t.is_empty() {
                    let arg_type = self.infer_type(e)?;

                    if self.is_unary_op_ambiguous(*id, arg_type.clone()).is_none() {
                        if let Ok((_, _, _, it_args)) = self.get_first_unary_op(*id, arg_type.clone(), None, true, l) {
                            if !it_args.is_empty() {
                                *t = it_args;
                            }
                        }
                    }
                }
            }

            RynaExpr::BinaryOperation(l, id, t, a, b) => {
                self.compile_expr_variables(a, registers, var_map)?;
                self.compile_expr_variables(b, registers, var_map)?;

                let is_func = matches!(b.as_ref(), RynaExpr::FunctionCall(..));
                let is_name = matches!(b.as_ref(), RynaExpr::QualifiedName(..));
                let is_attr = matches!(a.as_ref(), RynaExpr::AttributeAccess(..));
                                
                // Attribute assignments
                if *id == DEFINE_BINOP_ID && is_attr {
                    if let RynaExpr::AttributeAccess(_, e, att_idx) = a.as_ref() {
                        *expr = RynaExpr::AttributeAssignment(l.clone(), e.clone(), b.clone(), *att_idx);
                        
                        return Ok(());
                    }
                }

                // Member function calls
                if *id == DOT_BINOP_ID && is_func {
                    if let RynaExpr::FunctionCall(_, f_id, t, args) = b.as_ref() {
                        // Append first operand to the function's arguments 
                        let mut new_args = vec!(a.as_ref().clone());
                        new_args.extend(args.iter().cloned());

                        *expr = RynaExpr::FunctionCall(l.clone(), *f_id, t.clone(), new_args);
    
                        // Recompile after transformation
                        self.compile_expr_variables(expr, registers, var_map)?;
                    }

                } else if *id == DOT_BINOP_ID && is_name {
                    if let RynaExpr::QualifiedName(_, n, _) = b.as_ref() {
                        let arg_type = self.infer_type(a)?;
                        let mut changed = false;
                        let l_cpy = l.clone();
                        let n_cpy = n.clone();

                        if let Type::Basic(id) | Type::Template(id, _) = arg_type.deref_type() {
                            let attrs = &self.type_templates[*id].attributes;
    
                            for (i, att) in attrs.iter().enumerate() {
                                if &att.0 == n {
                                    *expr = RynaExpr::AttributeAccess(l.clone(), a.clone(), i);
                                    changed = true;
                                    break;
                                }
                            } 
                        
                        } else {
                            return Err(RynaError::compiler_error(
                                format!("Type {} has no attributes", arg_type.get_name(self)), 
                                &l_cpy, vec!()
                            ));
                        }

                        if !changed {
                            return Err(RynaError::compiler_error(
                                format!("Attribute with name {} was not found in class {}", n_cpy.cyan(), arg_type.deref_type().get_name(self)), 
                                &l_cpy, vec!()
                            ));
                        }                
                    }
                    
                } else if t.is_empty() {
                    let arg_type_1 = self.infer_type(a)?;
                    let arg_type_2 = self.infer_type(b)?;

                    if self.is_binary_op_ambiguous(*id, arg_type_1.clone(), arg_type_2.clone()).is_none() {
                        if let Ok((_, _, _, it_args)) = self.get_first_binary_op(*id, arg_type_1.clone(), arg_type_2.clone(), None, true, l) {
                            if !it_args.is_empty() {
                                *t = it_args;
                            }
                        }
                    }
                }
            }
            
            RynaExpr::NaryOperation(l, id, t, a, b) => {
                self.compile_expr_variables(a, registers, var_map)?;

                for i in b.iter_mut() {
                    self.compile_expr_variables(i, registers, var_map)?;
                }

                let is_func = matches!(a.as_ref(), RynaExpr::QualifiedName(_, _, Some(_)));

                if *id == CALL_OP && is_func {
                    if let RynaExpr::QualifiedName(_, _, Some(id)) = a.as_ref() {                    
                        *expr = RynaExpr::FunctionCall(l.clone(), *id, t.clone(), b.clone());
    
                        // Recompile after transformation
                        self.compile_expr_variables(expr, registers, var_map)?;
                    }
                
                } else if t.is_empty() {
                    let arg_type = self.infer_type(a)?;
                    let arg_types: Vec<_> = b.iter().map(|a| self.infer_type(a)).collect::<Result<_, _>>()?;
                    
                    if self.is_nary_op_ambiguous(*id, arg_type.clone(), arg_types.clone()).is_none() {
                        if let Ok((_, _, _, it_args)) = self.get_first_nary_op(*id, arg_type.clone(), arg_types, None, true, l) {
                            if !it_args.is_empty() {
                                *t = it_args;
                            }
                        }
                    }
                }
            }

            RynaExpr::Tuple(_, args) => {
                if args.len() == 1 {
                    *expr = args.pop().unwrap();
                    self.compile_expr_variables(expr, registers, var_map)?;                    
                    
                } else {
                    for i in args {
                        self.compile_expr_variables(i, registers, var_map)?;                    
                    }
                }
            }

            RynaExpr::FunctionCall(l, id, t, args) => {
                for i in args.iter_mut() {
                    self.compile_expr_variables(i, registers, var_map)?;                    
                }

                if t.is_empty() {
                    let arg_types: Vec<_> = args.iter().map(|a| self.infer_type(a)).collect::<Result<_, _>>()?;

                    if self.is_function_overload_ambiguous(*id, arg_types.clone()).is_none() {
                        if let Ok((_, _, _, it_args)) = self.get_first_function_overload(*id, arg_types.clone(), None, true, l) {
                            if !it_args.is_empty() {
                                *t = it_args;
                            }
                        }
                    }
                }

                
            }

            // Compile flow control
            RynaExpr::If(_, h, ib, ei, eb) => {
                self.compile_expr_variables(h, registers, var_map)?;
                self.compile_vars_and_infer_ctx(ib, registers, var_map, &vec!())?;

                for (ei_h, ei_b) in ei {
                    self.compile_expr_variables(ei_h, registers, var_map)?;
                    self.compile_vars_and_infer_ctx(ei_b, registers, var_map, &vec!())?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_vars_and_infer_ctx(eb_inner, registers, var_map, &vec!())?;
                }
            }

            RynaExpr::While(_, c, b) => {
                self.compile_expr_variables(c, registers, var_map)?;
                self.compile_vars_and_infer_ctx(b, registers, var_map, &vec!())?;
            }

            RynaExpr::For(l, i, c, b) => {
                self.compile_expr_variables(c, registers, var_map)?;

                let container_type = self.infer_type(c)?;

                if !self.implements_iterable(&container_type) {
                    return Err(RynaError::compiler_error(
                        format!("type {} does not implement {} interface", container_type.get_name(self), self.interfaces[ITERABLE_ID].name.green()), 
                        l, vec!()
                    ));
                }

                let (it_ov_id, iterator_type, _, it_args) = self.get_iterator_type(&container_type, l)?;
                let (next_ov_id, element_type, _, next_args) = self.get_iterator_output_type(&iterator_type, l)?;

                let iterator_idx = *registers.last().unwrap();
                let element_idx = *registers.get(registers.len() - 2).unwrap();

                let it_mut = iterator_type.clone().to_mut();

                let (consumed_ov_id, _, _, consumed_args) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), None, true, l)?;

                self.cache.usages.functions.add_new(ITERATOR_FUNC_ID, vec!(container_type.clone()), it_args.clone());
                self.cache.usages.functions.add_new(NEXT_FUNC_ID, vec!(it_mut.clone()), next_args.clone());
                self.cache.usages.functions.add_new(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), consumed_args.clone());

                self.cache.overloads.functions.insert((ITERATOR_FUNC_ID, vec!(container_type.clone()), it_args.clone()), it_ov_id);
                self.cache.overloads.functions.insert((NEXT_FUNC_ID, vec!(it_mut.clone()), next_args.clone()), next_ov_id);            
                self.cache.overloads.functions.insert((IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), consumed_args.clone()), consumed_ov_id);

                self.compile_vars_and_infer_ctx(b, registers, var_map, &vec!(("__iterator__".into(), iterator_type.clone()), (i.clone(), element_type.clone())))?;

                *expr = RynaExpr::CompiledFor(l.clone(), iterator_idx, element_idx, i.clone(), c.clone(), b.clone());
            }

            RynaExpr::Return(_, e) => {
                self.compile_expr_variables(e, registers, var_map)?;
            }

            RynaExpr::DoBlock(_, b, r) => {
                self.compile_vars_and_infer_ctx(b, registers, var_map, &vec!())?;

                // Infer further
                if *r == Type::InferenceMarker {
                    *r = self.infer_lambda_return_type(b)?.unwrap_or(Type::Empty);
                }
            }

            RynaExpr::Lambda(l, c, a, r, b) => {
                let mut captures = vec!();
                let mut capture_args = vec!();

                // Compile lambda captures
                for n in c {
                    if var_map.is_var_defined(n) {
                        let (idx, t) = var_map.get_var(n).unwrap();
                        captures.push((n.clone(), RynaExpr::Variable(l.clone(), *idx, n.clone(), t.clone())));
                        capture_args.push((n.clone(), t.clone()));
                    
                    } else {
                        return Err(RynaError::compiler_error(format!("Variable with name {} is not defined", n.green()), l, vec!()));
                    }
                }

                self.compile(b, &capture_args.iter().chain(a.iter()).cloned().collect())?;

                // Infer further
                if *r == Type::InferenceMarker {
                    *r = self.infer_lambda_return_type(b)?.unwrap_or(Type::Empty);
                }

                *expr = RynaExpr::CompiledLambda(l.clone(), self.lambdas, captures, a.clone(), r.clone(), b.clone());
                self.lambdas += 1;
            },

            RynaExpr::FunctionDefinition(l, _, _, tm, a, r, b) => {
                if tm.is_empty() {
                    self.compile(b, a)?;
                }
                    
                if let Type::Empty = r {
                    if RynaContext::ensured_return_check_body(b, l, "Function").is_err() {
                        b.push(RynaExpr::Return(l.clone(), Box::new(RynaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            RynaExpr::PrefixOperationDefinition(l, _, _, tm, n, t, r, b) => {
                if tm.is_empty() {
                    self.compile(b, &vec!((n.clone(), t.clone())))?;
                }
                
                if let Type::Empty = r {
                    if RynaContext::ensured_return_check_body(b, l, "Operation").is_err() {
                        b.push(RynaExpr::Return(l.clone(), Box::new(RynaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            RynaExpr::PostfixOperationDefinition(l, _, _, tm, n, t, r, b) => {
                if tm.is_empty() {
                    self.compile(b, &vec!((n.clone(), t.clone())))?;
                }

                if let Type::Empty = r {
                    if RynaContext::ensured_return_check_body(b, l, "Operation").is_err() {
                        b.push(RynaExpr::Return(l.clone(), Box::new(RynaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            RynaExpr::BinaryOperationDefinition(l, _, _, tm, a1, a2, r, b) => {
                if tm.is_empty() {
                    self.compile(b, &vec!(a1.clone(), a2.clone()))?;
                }

                if let Type::Empty = r {
                    if RynaContext::ensured_return_check_body(b, l, "Operation").is_err() {
                        b.push(RynaExpr::Return(l.clone(), Box::new(RynaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            },

            RynaExpr::NaryOperationDefinition(l, _, _, tm, a, args, r, b) => {
                let mut all_args = vec!(a.clone());
                all_args.extend(args.iter().cloned());

                if tm.is_empty() {
                    self.compile(b, &all_args)?;
                }

                if let Type::Empty = r {
                    if RynaContext::ensured_return_check_body(b, l, "Operation").is_err() {
                        b.push(RynaExpr::Return(l.clone(), Box::new(RynaExpr::Literal(l.clone(), Object::empty()))));
                    }
                }
            }

            _ => {}
        }

        Ok(())
    }
    
    fn compile_vars_and_infer_ctx(&mut self, body: &mut Vec<RynaExpr>, registers: &mut Vec<usize>, var_map: &mut VariableMap, args: &Vec<(String, Type)>) -> Result<usize, RynaError> {
        var_map.add_context();

        for (n, t) in args {
            let idx = registers.pop().unwrap();
            var_map.define_var(n.clone(), idx, t.clone());
        }

        // Compile each expression sequentially
        for e in body {
            self.compile_expr_variables(e, registers, var_map)?;
        }

        let mut max_var = 0;

        // Free the registers inside the context
        var_map.for_each_last_ctx(|i| {
            registers.push(i);
            max_var = max_var.max(i + 1); // Maximum register
        });

        var_map.remove_context();

        Ok(max_var)
    }

    pub fn transform_term(&mut self, expr: &mut RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::QualifiedName(l, _, Some(id)) => {
                let func = &self.functions[*id];

                if func.overloads.len() > 1 {
                    return Err(RynaError::compiler_error(
                        format!(
                            "Implicit lambda for function with name {} is ambiguous (found {} overloads)",
                            func.name.green(),
                            func.overloads.len()
                        ), 
                        l, vec!()
                    ));
                }

                if func.overloads[0].templates != 0 {
                    return Err(RynaError::compiler_error(
                        format!(
                            "Implicit lambda for function with name {} cannot be formed from generic overload",
                            func.name.green()
                        ), 
                        l, vec!()
                    ));
                }

                let ov = &func.overloads[0];
                let mut args = vec!();

                if let Type::And(a) = &ov.args {
                    if a.len() == 1 {
                        args.push(a[0].clone());

                    } else {
                        args.extend(a.iter().cloned());
                    }

                } else {
                    args.push(ov.args.clone());
                }

                let move_id = self.get_function_id("move".into()).unwrap();

                // Generate implicit function call
                let fn_call = RynaExpr::FunctionCall(
                    Location::none(), *id, vec!(), 
                    args.iter().enumerate()
                        .map(|(i, t)| {
                            let v = RynaExpr::Variable(Location::none(), i, format!("arg_{i}"), t.clone());

                            // Move if it is a direct value
                            if t.is_ref() {
                                v
                            
                            } else {
                                RynaExpr::FunctionCall(
                                    Location::none(), move_id, vec!(t.clone()), vec!(v) 
                                )
                            }
                        })
                        .collect()
                );

                // Generate implicit lambda
                *expr = RynaExpr::CompiledLambda(
                    l.clone(), self.lambdas, vec!(), 
                    args.into_iter().enumerate().map(|(i, t)| (format!("arg_{i}"), t)).collect(), 
                    ov.ret.clone(), 
                    vec!(RynaExpr::Return(Location::none(), Box::new(fn_call)))
                );

                self.lambdas += 1;

                Ok(())
            } 

            RynaExpr::QualifiedName(l, n, func) => {
                if func.is_none() {
                    let similar_func = self.functions.iter().map(|f| &f.name)
                                                     .filter(|name| levenshtein(n, name) < 3)
                                                     .map(|i| format!("{} (Function)", i.green()))
                                                     .collect::<Vec<_>>();

                    return Err(RynaError::compiler_error(
                        format!("Identifier with name {} is not defined", n), l, 
                        similar_func.iter().map(|i| format!("Similar: {}", i)).collect()
                    ));
                }

                Ok(())
            }

            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::Return(_, e) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.transform_term(e),

            RynaExpr::CompiledLambda(_, _, c, _, _, _) => {
                for (_, e) in c {
                    self.transform_term(e)?;
                }

                Ok(())
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.transform_term(e)?;
                }

                Ok(())
            },

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.transform_term(c)?;

                for e in exprs {
                    self.transform_term(e)?;
                }

                Ok(())
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.transform_term(c)?;

                for e in exprs {
                    self.transform_term(e)?;
                }

                Ok(())
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.transform_term(a)?;
                self.transform_term(b)?;

                Ok(())
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
                self.transform_term(ic)?;
                
                for e in ib {
                    self.transform_term(e)?;
                }

                for (ei_h, ei_b) in ei {
                    self.transform_term(ei_h)?;
                    
                    for e in ei_b {
                        self.transform_term(e)?;
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.transform_term(e)?;
                    }
                }

                Ok(())
            },
            
            RynaExpr::Variable(_, _, _, _) |
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { Ok(()) },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn compile_vars_and_infer(&mut self, body: &mut Vec<RynaExpr>, args: &Vec<(String, Type)>) -> Result<usize, RynaError> {
        self.compile_vars_and_infer_ctx(body, &mut (0..self.variables.len()).rev().collect(), &mut VariableMap::new(), args)
    }

    pub fn compile(&mut self, body: &mut Vec<RynaExpr>, args: &Vec<(String, Type)>) -> Result<(), RynaError> {
        self.compile_vars_and_infer(body, args)?;

        // Second pass to transform some terms if needed
        for expr in body.iter_mut() {
            self.transform_term(expr)?;
        }

        Ok(())
    }
}

/*
    ╒══════════════════════╕
    │ Compiled Ryna struct │
    ╘══════════════════════╛
*/

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PlaceholderType {
    Break, Continue
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CompiledRynaExpr {
    Empty,
    Bool(bool),
    Int(Integer),
    Float(f64),
    Str(String),
    Array(usize, Type),
    Lambda(usize, usize, Type, Type),

    Construct(usize, usize, Vec<Type>),
    AttributeAssign(usize),
    AttributeMove(usize),
    AttributeRef(usize),
    AttributeMut(usize),
    AttributeCopy(usize),
    AttributeDeref(usize),

    Tuple(usize),
    TupleElemMove(usize),
    TupleElemRef(usize),
    TupleElemMut(usize),
    TupleElemCopy(usize),
    TupleElemDeref(usize),

    IdxMove, IdxRef, IdxMut, IdxMoveRef,

    StoreIntVariable(usize, Integer),
    StoreBoolVariable(usize, bool),
    StoreFloatVariable(usize, f64),
    StoreStringVariable(usize, String),

    StoreVariable(usize),
    GetVariable(usize),
    CloneVariable(usize),
    RefVariable(usize),
    DerefVariable(usize),
    CopyVariable(usize),
    MoveVariable(usize),
    Assign,
    AssignToVar(usize), AssignToVarDirect(usize),
    Drop,

    Jump(usize),
    RelativeJump(i32),
    RelativeJumpIfFalse(usize, bool),
    RelativeJumpIfTrue(usize, bool),
    Call(usize),
    CallDestructor(usize),
    LambdaCall, LambdaCallRef,
    Return,

    NativeFunctionCall(usize, usize, Vec<Type>),
    UnaryOperatorCall(usize, usize, Vec<Type>),
    BinaryOperatorCall(usize, usize, Vec<Type>),
    NaryOperatorCall(usize, usize, Vec<Type>),

    NativeFunctionCallNoRet(usize, usize, Vec<Type>), // No return variants
    UnaryOperatorCallNoRet(usize, usize, Vec<Type>),
    BinaryOperatorCallNoRet(usize, usize, Vec<Type>),

    // Conversions
    Ref, Mut, Copy, Deref, Demut, Move, ToFloat,

    // Arithmetic opcodes
    Inc, Dec,
    Addi, Addf,
    Subi, Subf,
    Muli, Mulf,
    Divi, Divf,
    Modi, Modf,
    Negi, Negf,

    AddStr,

    // Bitwise opcodes
    NotB, AndB, OrB, XorB, Shr, Shl,

    // Comparison opcodes
    Lti, Ltf,
    Gti, Gtf,
    Lteqi, Lteqf,
    Gteqi, Gteqf,
    Eqi, Eqf,
    Neqi, Neqf,
    EqBool, NeqBool,
    EqStr, NeqStr,

    // Logical opcodes
    Not, Or, And, Xor,
    
    Nand, Nor, // Only via peephole optimization

    Placeholder(PlaceholderType),

    Halt
}

impl CompiledRynaExpr {
    pub fn needs_float(&self) -> bool {
        use CompiledRynaExpr::*;

        matches!(
            self,
            Addf | Subf | Mulf | Divf | Modf |
            Ltf | Gtf | Lteqf | Gteqf | Eqf | Neqf |
            Negf
        )
    }

    pub fn needs_deref(&self) -> bool {
        use CompiledRynaExpr::*;

        matches!(
            self, 
            Addf | Subf | Mulf | Divf | Modf |
            Ltf | Gtf | Lteqf | Gteqf | Eqf | Neqf | Negf |
            Addi | Subi | Muli | Divi | Modi |
            Lti | Gti | Lteqi | Gteqi | Eqi | Neqi | Negi |
            Not | Or | And | Xor |
            NotB | AndB | OrB | XorB | Shr | Shl |
            EqStr | NeqStr | EqBool | NeqBool | AddStr
        )
    }

    pub fn needs_no_drop(&self) -> bool {
        use CompiledRynaExpr::*;

        matches!(
            self, 
            Assign |
            Inc | Dec
        )
    }

    pub fn to_string(&self, ctx: &RynaContext) -> String {
        use CompiledRynaExpr::*;

        match self {
            Bool(obj) => format!("{}({})", "Bool".green(), obj.to_string().blue()),
            Int(obj) => format!("{}({})", "Int".green(), obj.to_string().blue()),
            Float(obj) => format!("{}({})", "Float".green(), obj.to_string().blue()),
            Str(obj) => format!("{}(\"{}\")", "Str".green(), obj.to_string().blue()),

            Array(length, t) => format!(
                "{}({}, {})", "Array".green(), 
                length.to_string().magenta(), 
                t.get_name(ctx)
            ),

            Lambda(pos, cap, args, ret) => format!(
                "{}({}, {}, {}, {})", "Lambda".green(), 
                pos.to_string().magenta(), 
                cap.to_string().magenta(), 
                args.get_name(ctx),
                ret.get_name(ctx)
            ),
            
            Tuple(to) => format!("{}({})", "Tuple".green(), to.to_string().blue()),

            StoreVariable(to) => format!("{}({})", "StoreVariable".green(), to.to_string().blue()),
            GetVariable(to) => format!("{}({})", "GetVariable".green(), to.to_string().blue()),
            CopyVariable(to) => format!("{}({})", "CopyVariable".green(), to.to_string().blue()),
            DerefVariable(to) => format!("{}({})", "DerefVariable".green(), to.to_string().blue()),
            MoveVariable(to) => format!("{}({})", "MoveVariable".green(), to.to_string().blue()),

            Call(to) => format!("{}({})", "Call".green(), to.to_string().blue()),

            Jump(to) => format!("{}({})", "Jump".green(), to.to_string().blue()),
            RelativeJump(to) => format!("{}({})", "RelativeJump".green(), to.to_string().blue()),
            RelativeJumpIfTrue(to, p) => format!("{}({}, {})", "RelativeJumpIfTrue".green(), to.to_string().blue(), p.to_string().blue()),
            RelativeJumpIfFalse(to, p) => format!("{}({}, {})", "RelativeJumpIfFalse".green(), to.to_string().blue(), p.to_string().blue()),

            Construct(id, length, args) => format!(
                "{}({}, {}, {{{}}})", "Construct".green(), 
                ctx.type_templates[*id].name.magenta(), 
                length.to_string().magenta(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            AttributeMove(to) => format!("{}({})", "Attribute".green(), to.to_string().blue()),
            AttributeRef(to) => format!("{}({})", "AttributeRef".green(), to.to_string().blue()),
            AttributeMut(to) => format!("{}({})", "AttributeMut".green(), to.to_string().blue()),
            AttributeCopy(to) => format!("{}({})", "AttributeCopy".green(), to.to_string().blue()),
            AttributeDeref(to) => format!("{}({})", "AttributeDeref".green(), to.to_string().blue()),

            TupleElemMove(to) => format!("{}({})", "TupleElem".green(), to.to_string().blue()),
            TupleElemRef(to) => format!("{}({})", "TupleElemRef".green(), to.to_string().blue()),
            TupleElemMut(to) => format!("{}({})", "TupleElemMut".green(), to.to_string().blue()),
            TupleElemCopy(to) => format!("{}({})", "TupleElemCopy".green(), to.to_string().blue()),
            TupleElemDeref(to) => format!("{}({})", "TupleElemDeref".green(), to.to_string().blue()),

            NativeFunctionCall(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "FunctionCall".green(), 
                ctx.functions[*id].name.magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            NativeFunctionCallNoRet(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "FunctionCallNoRet".green(), 
                ctx.functions[*id].name.magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            UnaryOperatorCall(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "UnOpCall".green(), 
                ctx.unary_ops[*id].get_repr().magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            UnaryOperatorCallNoRet(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "UnOpCallNoRet".green(), 
                ctx.unary_ops[*id].get_repr().magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            BinaryOperatorCall(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "BinOpCall".green(), 
                ctx.binary_ops[*id].get_repr().magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            BinaryOperatorCallNoRet(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "BinOpCallNoRet".green(), 
                ctx.binary_ops[*id].get_repr().magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            NaryOperatorCall(id, ov, args) => format!(
                "{}({}, {}, {{{}}})", "NaryOpCall".green(), 
                ctx.nary_ops[*id].get_repr().magenta(), 
                ov.to_string().blue(), 
                args.iter().map(|i| i.get_name(ctx)).collect::<Vec<_>>().join(", ")
            ),

            _ => format!("{:?}", self).green().to_string()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RynaInstruction {
    pub instruction: CompiledRynaExpr,
    pub debug_info: DebugInfo
}

impl RynaInstruction {
    pub fn to_string(&self, ctx: &RynaContext) -> String {
        format!("{:<30}{}{}", self.instruction.to_string(ctx), if self.debug_info.comment.is_empty() { "" } else { " # " }, self.debug_info.comment)
    }
}

impl From<CompiledRynaExpr> for RynaInstruction {
    fn from(obj: CompiledRynaExpr) -> RynaInstruction {
        RynaInstruction {
            instruction: obj,
            debug_info: DebugInfo::default()
        }
    }
}

impl RynaInstruction {
    pub fn new(instruction: CompiledRynaExpr, comment: String) -> RynaInstruction {
        RynaInstruction {
            instruction,
            debug_info: DebugInfoBuilder::default().comment(comment).build().unwrap()
        }
    }

    pub fn new_with_type(instruction: CompiledRynaExpr, comment: String, var_type: Type) -> RynaInstruction {
        RynaInstruction {
            instruction,
            debug_info: DebugInfoBuilder::default().comment(comment).var_type(Some(var_type)).build().unwrap()
        }
    }

    pub fn set_loc(mut self, loc: &Location) -> Self {
        self.debug_info.set_line(loc.module.clone(), loc.line);
        self
    }
}

impl RynaContext{
    pub fn get_inner_dep_graph(&mut self, lines: &[RynaExpr]) -> Result<DirectedGraph<(ImportType, usize), ()>, RynaError> {
        let mut res = DirectedGraph::new();
        let mut compiled = lines.to_owned();

        self.compile(&mut compiled, &vec!())?; // TODO: this seems costly...

        for (line_idx, e) in compiled.iter().enumerate() {
            self.get_inner_dep_graph_expr(e, &(ImportType::Line(line_idx), 0), &mut res);
        }

        Ok(res)
    }

    pub fn get_inner_dep_graph_body(&self, lines: &Vec<RynaExpr>, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        for line in lines {
            self.get_inner_dep_graph_expr(line, parent, deps);
        }
    }

    pub fn get_inner_dep_graph_expr(&self, expr: &RynaExpr, parent: &(ImportType, usize), deps: &mut DirectedGraph<(ImportType, usize), ()>) {
        match expr {
            RynaExpr::Literal(_, obj) => {
                deps.connect(parent.clone(), (ImportType::Class, obj.get_type_id()), ());
            }

            RynaExpr::Return(_, e) => self.get_inner_dep_graph_expr(e, parent, deps),

            RynaExpr::CompiledVariableAssignment(_, _, _, t, e) |
            RynaExpr::CompiledVariableDefinition(_, _, _, t, e) => {
                self.get_inner_dep_graph_expr(e, parent, deps);

                for td in t.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::Tuple(_, b) => {
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            RynaExpr::FunctionCall(_, id, ts, args) => {
                deps.connect(parent.clone(), (ImportType::Fn, *id), ());

                self.get_inner_dep_graph_body(args, parent, deps);

                for t in ts {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }

                    for id in t.interface_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Interface, id), ());
                    }
                }
            }

            RynaExpr::UnaryOperation(_, id, ts, a) => {
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

                        for id in t.interface_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Interface, id), ());
                        }
                    }
                }
            }

            RynaExpr::BinaryOperation(_, id, ts, a, b) => {
                if let Operator::Binary { id, .. } = &self.binary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Binary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_expr(b, parent, deps);

                    for t in ts {
                        for td in t.type_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Class, td), ());
                        }

                        for id in t.interface_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Interface, id), ());
                        }
                    }
                }
            }

            RynaExpr::NaryOperation(_, id, ts, a, b) => {
                if let Operator::Nary { id, .. } = &self.nary_ops[*id] {
                    deps.connect(parent.clone(), (ImportType::Nary, *id), ());

                    self.get_inner_dep_graph_expr(a, parent, deps);
                    self.get_inner_dep_graph_body(b, parent, deps);

                    for t in ts {
                        for td in t.type_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Class, td), ());
                        }

                        for id in t.interface_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Interface, id), ());
                        }
                    }
                }
            }

            RynaExpr::While(_, c, b) => {
                self.get_inner_dep_graph_expr(c, parent, deps);
                self.get_inner_dep_graph_body(b, parent, deps);
            }

            RynaExpr::CompiledFor(_, _, _, _, c, b) => {
                self.get_inner_dep_graph_expr(c, parent, deps);
                self.get_inner_dep_graph_body(b, parent, deps);

                // Iteration functions, since they are used implicitly
                deps.connect(parent.clone(), (ImportType::Interface, ITERABLE_ID), ());
                deps.connect(parent.clone(), (ImportType::Fn, ITERATOR_FUNC_ID), ());
                deps.connect(parent.clone(), (ImportType::Fn, NEXT_FUNC_ID), ());
                deps.connect(parent.clone(), (ImportType::Fn, IS_CONSUMED_FUNC_ID), ());
            }

            RynaExpr::If(_, ic, ib, ie, eb) => {
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

            RynaExpr::CompiledLambda(_, _, c, args, ret, b) => {
                self.get_inner_dep_graph_body(b, parent, deps);

                for (_, a) in c {
                    self.get_inner_dep_graph_expr(a, parent, deps);
                }

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }

                    for id in t.interface_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Interface, id), ());
                    }
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in ret.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::FunctionDefinition(_, _, id, _, args, ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Fn, *id), deps);

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }

                    for id in t.interface_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Interface, id), ());
                    }
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in ret.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::PrefixOperationDefinition(_, _, id, _, _, t1, t2, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Prefix, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t1.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t2.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::PostfixOperationDefinition(_, _, id, _, _, t1, t2, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Postfix, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t1.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t2.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::BinaryOperationDefinition(_, _, id, _, (_, t1), (_, t2), ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Binary, *id), deps);

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t1.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }

                for td in t2.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t2.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in ret.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::NaryOperationDefinition(_, _, id, _, (_, t1), args, ret, b) => {
                self.get_inner_dep_graph_body(b, &(ImportType::Nary, *id), deps);

                for (_, t) in args {
                    for td in t.type_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Class, td), ());
                    }

                    for id in t.interface_dependencies() {
                        deps.connect(parent.clone(), (ImportType::Interface, id), ());
                    }
                }

                for td in t1.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in t1.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }

                for td in ret.type_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Class, td), ());
                }

                for id in ret.interface_dependencies() {
                    deps.connect(parent.clone(), (ImportType::Interface, id), ());
                }
            }

            RynaExpr::InterfaceImplementation(_, _, t_i, n, a) => {
                if let Some(t) = self.get_interface(n) {
                    match t_i {
                        Type::Basic(id) | Type::Template(id, _) => {
                            deps.connect((ImportType::Class, *id), (ImportType::Interface, t.id), ());
                        }
                        
                        _ => {}
                    }

                    for tp in a {
                        for t_dep in tp.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in tp.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }
                    }
    
                    for t_dep in t_i.type_dependencies() {
                        deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                    }

                    for id in t_i.interface_dependencies() {
                        deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                    }
                }
            }

            RynaExpr::InterfaceDefinition(_, _, n, _, fns, uns, bin, nary) => {
                if let Some(t) = self.get_interface(n) {
                    for (_, f_n, _, a, r) in fns {
                        if let Some(f) = self.get_function(f_n) {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Fn, f.id), ());                        
                        }

                        for (_, tp) in a {
                            for t_dep in tp.type_dependencies() {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                            }
    
                            for id in tp.interface_dependencies() {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                            }
                        }
    
                        for t_dep in r.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in r.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }
                    }

                    for (_, op_id, _, _, at, r) in uns {
                        if let Operator::Unary { prefix, .. } = &self.unary_ops[*op_id] {
                            if *prefix {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Prefix, *op_id), ());
                                
                            } else {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Postfix, *op_id), ());
                            }
                        }

                        for t_dep in at.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in at.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }

                        for t_dep in r.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in r.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }
                    }

                    for (_, op_id, _, (_, a0t), (_, a1t), r) in bin {
                        deps.connect((ImportType::Interface, t.id), (ImportType::Binary, *op_id), ());

                        for t_dep in a0t.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in a0t.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }

                        for t_dep in a1t.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in a1t.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }

                        for t_dep in r.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in r.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }
                    }

                    for (_, op_id, _, (_, a0t), a, r) in nary {
                        deps.connect((ImportType::Interface, t.id), (ImportType::Nary, *op_id), ());

                        for t_dep in a0t.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in a0t.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }

                        for (_, tp) in a {
                            for t_dep in tp.type_dependencies() {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                            }
    
                            for id in tp.interface_dependencies() {
                                deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                            }
                        }
    
                        for t_dep in r.type_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Class, t_dep), ());
                        }
    
                        for id in r.interface_dependencies() {
                            deps.connect((ImportType::Interface, t.id), (ImportType::Interface, id), ());
                        }
                    }
                }
            }

            RynaExpr::ClassDefinition(_, _, n, _, _, _, _) => {
                if let Some(t) = self.get_type_template(n) {
                    deps.connect(parent.clone(), (ImportType::Class, t.id), ());

                    // Dependencies from the attributes
                    for (_, tp) in &t.attributes {
                        for t_dep in tp.type_dependencies() {
                            deps.connect((ImportType::Class, t.id), (ImportType::Class, t_dep), ());
                        }

                        for id in tp.interface_dependencies() {
                            deps.connect(parent.clone(), (ImportType::Interface, id), ());
                        }
                    }
                }
            },

            _ => {}
        }
    }

    pub fn get_template_calls_body(
        &mut self, 
        lines: &Vec<RynaExpr>
    ) -> Result<(), RynaError> {
        let mut changed = true;
        
        while changed {
            changed = false;
            self.get_template_calls_body_pass(lines, &mut changed)?;
        }

        Ok(())
    }

    pub fn get_template_calls_pass(
        &mut self, 
        expr: &RynaExpr,
        changed: &mut bool
    ) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::FunctionDefinition(_, _, id, _, a, r, b) => {
                let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                let and = Type::And(arg_types.clone());

                if let Some(usages) = self.cache.usages.functions.get_checked(id) {                    
                    for (args, ov) in usages {
                        if Type::And(args).bindable_to(&and, self) {
                            let mut body = b.clone();
                            let key = (*id, ov.clone(), arg_types.clone());
    
                            if !self.cache.templates.functions.contains(&key) {
                                if !ov.is_empty() {
                                    let templates = ov.iter().cloned().enumerate().collect();
                                    
                                    // Create new instance
                                    body.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, &templates));
                                    self.compile(&mut body, &a.iter().map(|(n, t)| (n.clone(), t.sub_templates(&templates))).collect())?;    
    
                                    // Statically check the newly instantiated functions
                                    for line in &body {
                                        self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                    }    
                                }

                                self.cache.templates.functions.insert(key, body.clone());

                                // Search instance recursively
                                self.get_template_calls_body_pass(&body, changed)?;

                                *changed = true;
                            }
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::PostfixOperationDefinition(_, _, id, _, n, tp, r, b) |
            RynaExpr::PrefixOperationDefinition(_, _, id, _, n, tp, r, b) => {
                if let Some(usages) = self.cache.usages.unary.get_checked(id) {
                    for (arg, ov) in usages {
                        if arg[0].bindable_to(tp, self) {
                            let mut body = b.clone();
                            let key = (*id, ov.clone(), vec!(tp.clone()));
        
                            if !self.cache.templates.unary.contains(&key) {
                                if !ov.is_empty() {
                                    let templates = ov.iter().cloned().enumerate().collect();

                                    // Create new instance
                                    body.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, &templates));
                                    self.compile(&mut body, &vec!((n.clone(), tp.sub_templates(&templates))))?;    
    
                                    // Statically check the newly instantiated functions
                                    for line in &body {
                                        self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                    }
                                }

                                self.cache.templates.unary.insert(key, body.clone());

                                // Search instance recursively
                                self.get_template_calls_body_pass(&body, changed)?;

                                *changed = true;
                            }
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::BinaryOperationDefinition(_, _, id, _, (n1, t1), (n2, t2), r, b) => {
                if let Some(usages) = self.cache.usages.binary.get_checked(id) {
                    for (arg, ov) in usages {
                        if arg[0].bindable_to(t1, self) && arg[1].bindable_to(t2, self) {
                            let mut body = b.clone();
                            let key = (*id, ov.clone(), vec!(t1.clone(), t2.clone()));
    
                            if !self.cache.templates.binary.contains(&key) {
                                if !ov.is_empty() {
                                    let templates = ov.iter().cloned().enumerate().collect();
                                    
                                    // Create new instance
                                    body.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, &templates));
                                    self.compile(&mut body, &vec!((n1.clone(), t1.sub_templates(&templates)), (n2.clone(), t2.sub_templates(&templates))))?;    
    
                                    // Statically check the newly instantiated functions
                                    for line in &body {
                                        self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                    }
                                }

                                self.cache.templates.binary.insert(key, body.clone());

                                // Search instance recursively
                                self.get_template_calls_body_pass(&body, changed)?;

                                *changed = true;
                            }
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::NaryOperationDefinition(_, _, id, _, (n, t), a, r, b) => {
                let mut all_args = vec!(t.clone());
                all_args.extend(a.iter().map(|(_, t)| t).cloned());
                let and = Type::And(all_args.clone());

                if let Some(usages) = self.cache.usages.nary.get_checked(id) {
                    for (args, ov) in usages {
                        if Type::And(args.clone()).bindable_to(&and, self) {
                            let mut body = b.clone();
                            let key = (*id, ov.clone(), all_args.clone());
    
                            if !self.cache.templates.nary.contains(&key) {
                                if !ov.is_empty() {
                                    let templates = ov.iter().cloned().enumerate().collect();
                                    
                                    // Create new instance
                                    body.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, &templates));

                                    let named_args = [(n.clone(), t.clone())].iter().chain(a).map(|(n, t)| (n.clone(), t.sub_templates(&templates))).collect();
                                    self.compile(&mut body, &named_args)?;    

                                    // Statically check the newly instantiated functions
                                    for line in &body {
                                        self.static_check_expected(line, &Some(r.sub_templates(&templates)))?;
                                    }
                                }

                                self.cache.templates.nary.insert(key, body.clone());

                                // Search instance recursively
                                self.get_template_calls_body_pass(&body, changed)?;

                                *changed = true;
                            }
                        }
                    }
                }

                Ok(())
            }

            _ => Ok(())
        }
    }

    pub fn get_template_calls_body_pass(
        &mut self, 
        lines: &Vec<RynaExpr>,
        changed: &mut bool
    ) -> Result<(), RynaError> {
        for line in lines {
            self.get_template_calls_pass(line, changed)?;
        }

        Ok(())
    }

    pub fn subtitute_type_params_expr(expr: &mut RynaExpr, templates: &HashMap<usize, Type>) {
        match expr {
            RynaExpr::Continue(..) |
            RynaExpr::Break(..) |
            RynaExpr::Literal(..) |
            RynaExpr::NameReference(..) => {},

            RynaExpr::Tuple(_, e) => e.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates)),

            RynaExpr::DoBlock(_, e, t) => {
                *t = t.sub_templates(templates);
                
                e.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates))
            },

            RynaExpr::Variable(_, _, _, t) => *t = t.sub_templates(templates),

            RynaExpr::Lambda(_, _, args, r, lines) => {
                for (_, tp) in args {
                    *tp = tp.sub_templates(templates);
                }
                
                if *r != Type::InferenceMarker {
                    *r = r.sub_templates(templates);
                }

                lines.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
            },

            RynaExpr::VariableAssignment(_, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => RynaContext::subtitute_type_params_expr(e, templates),

            RynaExpr::VariableDefinition(_, _, t, e) |
            RynaExpr::CompiledVariableAssignment(_, _, _, t, e) |
            RynaExpr::CompiledVariableDefinition(_, _, _, t, e) => {
                *t = t.sub_templates(templates);

                RynaContext::subtitute_type_params_expr(e, templates);
            },
            
            RynaExpr::UnaryOperation(_, _, t, a) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                RynaContext::subtitute_type_params_expr(a, templates);
            },

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                RynaContext::subtitute_type_params_expr(a, templates);
                RynaContext::subtitute_type_params_expr(b, templates);
            }

            RynaExpr::BinaryOperation(_, _, t, a, b) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                RynaContext::subtitute_type_params_expr(a, templates);
                RynaContext::subtitute_type_params_expr(b, templates);
            }

            RynaExpr::NaryOperation(_, _, t, first, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                RynaContext::subtitute_type_params_expr(first, templates);
                args.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
            },
            
            RynaExpr::FunctionCall(_, _, t, args) => {
                t.iter_mut().for_each(|i| *i = i.sub_templates(templates));

                args.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
            },
            
            RynaExpr::For(_, _, container, body) |
            RynaExpr::CompiledFor(_, _, _, _, container, body) |
            RynaExpr::While(_, container, body) => {
                RynaContext::subtitute_type_params_expr(container, templates);
                body.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
            },

            RynaExpr::If(_, ih, ib, ei, eb) => {
                RynaContext::subtitute_type_params_expr(ih, templates);
                ib.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));

                for (ei_h, ei_b) in ei {
                    RynaContext::subtitute_type_params_expr(ei_h, templates);
                    ei_b.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
                }

                if let Some(b) = eb {
                    b.iter_mut().for_each(|i| RynaContext::subtitute_type_params_expr(i, templates));
                }
            }
            
            _ => unimplemented!("{:?}", expr)
        };
    }

    pub fn compile_lambda_expr(
        &mut self, 
        line: &RynaExpr,
        only_length: bool
    ) -> Result<(), RynaError> {
        return match line {
            RynaExpr::Break(..) |
            RynaExpr::Continue(..) |
            RynaExpr::Literal(..) |
            RynaExpr::Variable(..) |
            RynaExpr::ClassDefinition(..) |
            RynaExpr::InterfaceDefinition(..) |
            RynaExpr::InterfaceImplementation(..) |
            RynaExpr::PrefixOperatorDefinition(..) |
            RynaExpr::PostfixOperatorDefinition(..) |
            RynaExpr::BinaryOperatorDefinition(..) |
            RynaExpr::NaryOperatorDefinition(..) => Ok(()),

            RynaExpr::CompiledLambda(_, i, c, a, _, b) => {
                self.compile_lambdas(b, only_length)?;

                if only_length {
                    self.lambda_positions.entry(*i).or_insert(1 + self.lambda_code_length);
                    
                    self.lambda_code_length += self.compiled_form_body_size(b, true)? + a.len() + c.len();

                } else {
                    for (i, e) in c.iter().enumerate() {
                        if i == 0 {
                            self.lambda_code.push(RynaInstruction::new_with_type(
                                CompiledRynaExpr::StoreVariable(i), 
                                "Lambda expression start".into(),
                                self.infer_type(&e.1).unwrap()
                            ));
    
                        } else {
                            self.lambda_code.push(RynaInstruction::new_with_type(
                                CompiledRynaExpr::StoreVariable(i), 
                                String::new(),
                                self.infer_type(&e.1).unwrap()
                            ));
                        }
                    }
    
                    for (i, arg) in a.iter().enumerate() {
                        self.lambda_code.push(RynaInstruction::new_with_type(
                            CompiledRynaExpr::StoreVariable(i + c.len()), 
                            String::new(),
                            arg.1.clone()
                        ));
                    }
    
                    self.lambda_code.extend(self.compiled_form_body(b)?);
                }
                
                Ok(())
            }

            RynaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            RynaExpr::Return(_, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::UnaryOperation(_, _, _, e) => self.compile_lambda_expr(e, only_length),

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.compile_lambda_expr(a, only_length)?;
                self.compile_lambda_expr(b, only_length)?;

                Ok(())
            }

            RynaExpr::CompiledFor(_, _, _, _, a, b) |
            RynaExpr::While(_, a, b) |
            RynaExpr::NaryOperation(_, _, _, a, b) => {
                self.compile_lambda_expr(a, only_length)?;
                self.compile_lambdas(b, only_length)?;

                Ok(())
            }

            RynaExpr::If(_, ih, ib, ei, eb) => {
                self.compile_lambda_expr(ih, only_length)?;
                self.compile_lambdas(ib, only_length)?;

                for (ei_h, ei_b) in ei {
                    self.compile_lambda_expr(ei_h, only_length)?;
                    self.compile_lambdas(ei_b, only_length)?;
                }

                if let Some(eb_inner) = eb {
                    self.compile_lambdas(eb_inner, only_length)?;                    
                }

                Ok(())
            }

            RynaExpr::DoBlock(_, args, _) |
            RynaExpr::Tuple(_, args) |
            RynaExpr::FunctionCall(_, _, _, args) => self.compile_lambdas(args, only_length),

            RynaExpr::PrefixOperationDefinition(..) |
            RynaExpr::PostfixOperationDefinition(..) |
            RynaExpr::BinaryOperationDefinition(..) |
            RynaExpr::NaryOperationDefinition(..) |
            RynaExpr::FunctionDefinition(..) => Ok(()),

            RynaExpr::Macro(..) => { Ok(()) },

            _ => unimplemented!("{:?}", line)
        };
    }

    pub fn compile_lambdas(
        &mut self, 
        lines: &Vec<RynaExpr>,
        only_length: bool
    ) -> Result<(), RynaError> {
        for line in lines {
            self.compile_lambda_expr(line, only_length)?;
        }

        Ok(())
    }

    pub fn compile_function_lambdas(&mut self, lines: &Vec<RynaExpr>, only_length: bool) -> Result<(), RynaError> {
        for expr in lines {
            match expr {
                RynaExpr::FunctionDefinition(_, _, id, _, a, _, _) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.functions.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                let sub_b = self.cache.templates.functions.get_checked(&(*id, ov.clone(), arg_types.clone())).unwrap();
                                self.compile_lambdas(&sub_b, only_length)?;
                            }
                        }
                    }
                },

                RynaExpr::PrefixOperationDefinition(_, _, id, _, _, tp, _, _) |
                RynaExpr::PostfixOperationDefinition(_, _, id, _, _, tp, _, _) => {
                    if let Some(usages) = self.cache.usages.unary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(tp, self) {
                                let sub_b = self.cache.templates.unary.get_checked(&(*id, ov.clone(), vec!(tp.clone()))).unwrap();
                                self.compile_lambdas(&sub_b, only_length)?;
                            }
                        }
                    }
                },

                RynaExpr::BinaryOperationDefinition(_, _, id, _, (_, t1), (_, t2), _, _) => {
                    let and = Type::And(vec!(t1.clone(), t2.clone()));

                    if let Some(usages) = self.cache.usages.binary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {    
                                let sub_b = self.cache.templates.binary.get_checked(&(*id, ov.clone(), vec!(t1.clone(), t2.clone()))).unwrap();
                                self.compile_lambdas(&sub_b, only_length)?;
                            }
                        }
                    }
                },

                RynaExpr::NaryOperationDefinition(_, _, id, _, (_, a_t), a, _, _) => {
                    let mut arg_types = vec!(a_t.clone());
                    arg_types.extend(a.iter().map(|(_, t)| t).cloned());

                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.nary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                let sub_b = self.cache.templates.nary.get_checked(&(*id, ov.clone(), arg_types.clone())).unwrap();
                                self.compile_lambdas(&sub_b, only_length)?;
                            }
                        }
                    }
                },

                _ => {}
            }
        }

        Ok(())
    }

    pub fn compiled_form(&mut self, lines: &Vec<RynaExpr>) -> Result<Vec<RynaInstruction>, RynaError> {
        self.compile_function_lambdas(lines, true)?;
        self.compile_lambdas(lines, true)?;

        let mut program_size = 1 + self.lambda_code_length;

        // Define function indexes
        for expr in lines {
            match expr {
                RynaExpr::FunctionDefinition(_, _, id, t, a, ret, _) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.functions.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                // Find function overload id    
                                if self.cache.overloads.functions.get_checked(&(*id, args.clone(), ov.clone())).is_some() {
                                    let init_loc = program_size;
                                    self.cache.locations.functions.insert((*id, args.clone(), ov.clone()), program_size);

                                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect();
                                    let sub_b = self.cache.templates.functions.get_checked(&(*id, ov.clone(), arg_types)).unwrap(); 

                                    program_size += self.compiled_form_body_size(&sub_b, true)? + a.len();

                                    if t.is_empty() {
                                        let signature = format!(
                                            "fn {}({}) -> {}",
                                            self.functions[*id].name,
                                            a.iter().map(|(_, at)| {
                                                at.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
        
                                    } else {                                
                                        let signature = format!(
                                            "fn<{}> {}({}) -> {}",
                                            ov.iter().map(|i| {
                                                i.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            self.functions[*id].name,
                                            a.iter().map(|(_, at)| {
                                                at.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
                                    }    
                                }
                            }
                        }
                    }
                },
                
                RynaExpr::PrefixOperationDefinition(_, _, id, t, _, tp, ret, _) |
                RynaExpr::PostfixOperationDefinition(_, _, id, t, _, tp, ret, _) => {
                    if let Some(usages) = self.cache.usages.unary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(tp, self) {                                
                                // Find overload id    
                                if self.cache.overloads.unary.get_checked(&(*id, args.clone(), ov.clone())).is_some() {
                                    let init_loc = program_size;
                                    self.cache.locations.unary.insert((*id, args.clone(), ov.clone()), program_size);

                                    let prefix = if let Operator::Unary { prefix, .. } = self.unary_ops[*id] {
                                        prefix   
                                    } else {
                                        false
                                    };

                                    let sub_b = self.cache.templates.unary.get_checked(&(*id, ov.clone(), vec!(tp.clone()))).unwrap(); 
                                    program_size += self.compiled_form_body_size(&sub_b, true)? + 1;
                                    
                                    if t.is_empty() {        
                                        let signature = format!(
                                            "op {}({}){} -> {}",
                                            if prefix { self.unary_ops[*id].get_repr() } else { "".into() },
                                            tp.get_name_plain(self),
                                            if prefix { "".into() } else { self.unary_ops[*id].get_repr() },
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));

                                    } else {                                
                                        let signature = format!(
                                            "op<{}> {}({}){} -> {}",
                                            ov.iter().map(|i| {
                                                i.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            if prefix { self.unary_ops[*id].get_repr() } else { "".into() },
                                            tp.get_name_plain(self),
                                            if prefix { "".into() } else { self.unary_ops[*id].get_repr() },
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
                                    }    
                                }
                            }
                        }
                    }
                }
                
                RynaExpr::BinaryOperationDefinition(_, _, id, t, (_, t1), (_, t2), ret, _) => {
                    let and = Type::And(vec!(t1.clone(), t2.clone()));

                    if let Some(usages) = self.cache.usages.binary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {                                
                                // Find overload id    
                                if self.cache.overloads.binary.get_checked(&(*id, args.clone(), ov.clone())).is_some() {
                                    let init_loc = program_size;
                                    self.cache.locations.binary.insert((*id, args.clone(), ov.clone()), program_size);

                                    let sub_b = self.cache.templates.binary.get_checked(&(*id, ov.clone(), vec!(t1.clone(), t2.clone()))).unwrap(); 
                                    program_size += self.compiled_form_body_size(&sub_b, true)? + 2;

                                    if t.is_empty() {
                                        let signature = format!(
                                            "op ({}){}({}) -> {}",
                                            t1.get_name_plain(self),
                                            self.binary_ops[*id].get_repr(),
                                            t2.get_name_plain(self),
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));

                                    } else {                                
                                        let signature = format!(
                                            "op<{}> ({}){}({}) -> {}",
                                            ov.iter().map(|i| {
                                                i.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            t1.get_name_plain(self),
                                            self.binary_ops[*id].get_repr(),
                                            t2.get_name_plain(self),
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
                                    }    
                                }
                            }
                        }
                    }
                }
                
                RynaExpr::NaryOperationDefinition(_, _, id, t, (_, a_t), a, ret, _) => {
                    let mut arg_types = vec!(a_t.clone());
                    arg_types.extend(a.iter().map(|(_, t)| t).cloned());

                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.nary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                // Find overload id    
                                if self.cache.overloads.nary.get_checked(&(*id, args.clone(), ov.clone())).is_some() {
                                    let init_loc = program_size;
                                    self.cache.locations.nary.insert((*id, args.clone(), ov.clone()), program_size);

                                    let mut o_rep = "".to_string();
                                    let mut c_rep = "".to_string();

                                    if let Operator::Nary { open_rep, close_rep, .. } = &self.nary_ops[*id] {
                                        o_rep = open_rep.clone();
                                        c_rep = close_rep.clone();
                                    }

                                    let sub_b = self.cache.templates.nary.get_checked(&(*id, ov.clone(), arg_types.clone())).unwrap(); 
                                    program_size += self.compiled_form_body_size(&sub_b, true)? + a.len() + 1;
                                    
                                    if t.is_empty() {                                            
                                        let signature = format!(
                                            "op ({}){}({}){} -> {}",
                                            a_t.get_name_plain(self),
                                            o_rep,
                                            a.iter().map(|(_, at)| {
                                                at.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            c_rep,
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
        
                                    } else {
                                        let signature = format!(
                                            "op<{}> ({}){}({}){} -> {}",
                                            ov.iter().map(|i| {
                                                i.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            a_t.get_name_plain(self),
                                            o_rep,
                                            a.iter().map(|(_, at)| {
                                                at.get_name_plain(self)
                                            }).collect::<Vec<_>>().join(", "),
                                            c_rep,
                                            ret.get_name_plain(self)
                                        );

                                        self.cache.ranges.insert(signature, (init_loc, program_size));
                                    }    
                                }
                            }
                        }
                    }
                }

                _ => {}
            }
        }

        let mut res = vec!(RynaInstruction::from(CompiledRynaExpr::Jump(program_size + self.lambda_code.len())));

        self.compile_function_lambdas(lines, false)?;
        self.compile_lambdas(lines, false)?;

        res.append(&mut self.lambda_code);

        // Define functions
        for expr in lines {
            match expr {
                RynaExpr::FunctionDefinition(_, _, id, _, a, r, _) => {
                    let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.functions.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                // Store parameters
                                for (i, arg) in args.iter().enumerate() {
                                    if i == 0 {
                                        let comment = format!(
                                            "fn {}{}({}) -> {}",
                                            self.functions[*id].name.green(),
                                            if ov.is_empty() { "".into() } else { format!("<{}>", ov.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")) },
                                            a.iter().map(|(_, t)| t.get_name(self)).collect::<Vec<_>>().join(", "),
                                            r.get_name(self)
                                        );

                                        res.push(RynaInstruction::new_with_type(
                                            CompiledRynaExpr::StoreVariable(i), 
                                            comment,
                                            arg.clone()
                                        ));

                                    } else {
                                        res.push(RynaInstruction::new_with_type(
                                            CompiledRynaExpr::StoreVariable(i),
                                            String::new(),
                                            arg.clone()
                                        ));
                                    }
                                }

                                let sub_b = self.cache.templates.functions.get_checked(&(*id, ov.clone(), arg_types.clone())).unwrap();
                                res.extend(self.compiled_form_body(&sub_b)?);
                            }
                        }
                    }
                },

                RynaExpr::PrefixOperationDefinition(_, _, id, _, _, tp, r, _) |
                RynaExpr::PostfixOperationDefinition(_, _, id, _, _, tp, r, _) => {
                    if let Some(usages) = self.cache.usages.unary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(tp, self) {
                                let mut rep = String::new();
                                let mut is_prefix = false;
            
                                if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*id] {
                                    rep = representation.clone();
                                    is_prefix = *prefix;
                                }
            
                                // Store parameter
                                let comment = if is_prefix {
                                    format!("op {}({}) -> {}", rep, tp.get_name(self), r.get_name(self))
            
                                } else {
                                    format!("op ({}){} -> {}", tp.get_name(self), rep, r.get_name(self))
                                };
                                
                                res.push(RynaInstruction::new_with_type(
                                    CompiledRynaExpr::StoreVariable(0), 
                                    comment,
                                    args[0].clone()
                                ));
    
                                let sub_b = self.cache.templates.unary.get_checked(&(*id, ov.clone(), vec!(tp.clone()))).unwrap();
                                res.extend(self.compiled_form_body(&sub_b)?);
                            }
                        }
                    }
                },

                RynaExpr::BinaryOperationDefinition(_, _, id, _, (_, t1), (_, t2), r, _) => {
                    let and = Type::And(vec!(t1.clone(), t2.clone()));

                    if let Some(usages) = self.cache.usages.binary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {    
                                // Store parameters
                                let mut rep = String::new();

                                if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                                    rep = representation.clone();
                                }

                                let comment = format!("op ({}) {} ({}) -> {}", t1.get_name(self), rep, t2.get_name(self), r.get_name(self));

                                res.push(RynaInstruction::new_with_type(
                                    CompiledRynaExpr::StoreVariable(0), 
                                    comment,
                                    args[0].clone()
                                ));

                                res.push(RynaInstruction::new_with_type(
                                    CompiledRynaExpr::StoreVariable(1),
                                    String::new(),
                                    args[1].clone()
                                ));
    
                                let sub_b = self.cache.templates.binary.get_checked(&(*id, ov.clone(), vec!(t1.clone(), t2.clone()))).unwrap();
                                res.extend(self.compiled_form_body(&sub_b)?);
                            }
                        }
                    }
                },

                RynaExpr::NaryOperationDefinition(_, _, id, _, (_, a_t), a, r, _) => {
                    let mut arg_types = vec!(a_t.clone());
                    arg_types.extend(a.iter().map(|(_, t)| t).cloned());

                    let and = Type::And(arg_types.clone());

                    if let Some(usages) = self.cache.usages.nary.get_checked(id) {
                        for (args, ov) in usages {
                            if Type::And(args.clone()).bindable_to(&and, self) {
                                // Store parameters
                                let mut o_rep = String::new();
                                let mut c_rep = String::new();
    
                                if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                                    o_rep = open_rep.clone();
                                    c_rep = close_rep.clone();
                                }
    
                                for (i, arg) in args.iter().enumerate() {
                                    if i == 0 {
                                        let comment = format!(
                                            "op ({}){}{}{} -> {}", 
                                            a_t.get_name(self),
                                            o_rep, 
                                            a.iter().map(|(_, t)| t.get_name(self)).collect::<Vec<_>>().join(", "), 
                                            c_rep,
                                            r.get_name(self)
                                        );
    
                                        res.push(RynaInstruction::new_with_type(
                                            CompiledRynaExpr::StoreVariable(i), 
                                            comment,
                                            arg.clone()
                                        ));
    
                                    } else {
                                        res.push(RynaInstruction::new_with_type(
                                            CompiledRynaExpr::StoreVariable(i),
                                            String::new(),
                                            arg.clone()
                                        ));
                                    }
                                }
    
                                let sub_b = self.cache.templates.nary.get_checked(&(*id, ov.clone(), arg_types.clone())).unwrap();
                                res.extend(self.compiled_form_body(&sub_b)?);
                            }
                        }
                    }
                },

                _ => {}
            }
        }

        // Update first jump
        res[0] = RynaInstruction::from(CompiledRynaExpr::Jump(program_size));

        // Define everything else
        for expr in lines {
            match expr {
                RynaExpr::FunctionDefinition(..) | 
                RynaExpr::PrefixOperationDefinition(..) |
                RynaExpr::PostfixOperationDefinition(..) |
                RynaExpr::BinaryOperationDefinition(..) |
                RynaExpr::NaryOperationDefinition(..) => {},

                _ => res.extend(self.compiled_form_expr(expr, true)?)
            }
        }

        res.push(RynaInstruction::new(CompiledRynaExpr::Halt, "End of the program".into()));

        Ok(res)
    }

    pub fn compiled_form_size(&self, expr: &RynaExpr, root: bool, root_counter: &mut usize) -> Result<usize, RynaError> {
        use RynaExpr::*;

        match expr {
            Break(..) |
            Continue(..) => Ok(1),

            Variable(..) => {
                *root_counter += root as usize; // Add drop instruction

                Ok(1)
            }, 
            
            Literal(_, obj) => {
                *root_counter += root as usize; // Add drop instruction
                
                Ok(RynaContext::compiled_literal_size(obj))
            },

            AttributeAccess(_, e, _) => {
                *root_counter += root as usize; // Add drop instruction
                
                Ok(self.compiled_form_size(e, false, root_counter)? + 1)
            }

            AttributeAssignment(_, a, b, _) => {                
                Ok(self.compiled_form_size(a, false, root_counter)? + self.compiled_form_size(b, false, root_counter)? + 1)
            }
            
            CompiledLambda(_, _, c, ..) => {
                *root_counter += root as usize; // Add drop instruction
                
                Ok(1 + c.len())
            }

            UnaryOperation(_, id, t, arg) => {
                *root_counter += root as usize; // Add drop instruction

                let a_t = self.infer_type(arg)?;

                let ov_id = self.cache.overloads.unary.get_checked(&(*id, vec!(a_t.clone()), t.clone())).unwrap();
                let offset = self.cache.opcodes.unary.get_checked(&(*id, ov_id)).map(|i| i.1).unwrap_or(0);

                Ok(self.compiled_form_size(arg, false, root_counter)? + 1 + offset)
            }

            BinaryOperation(_, id, t, a, b) => {
                *root_counter += root as usize; // Add drop instruction

                let a_t = self.infer_type(a)?;
                let b_t = self.infer_type(b)?;

                let ov_id = self.cache.overloads.binary.get_checked(&(*id, vec!(a_t.clone(), b_t.clone()), t.clone())).unwrap();
                let (opcode, mut offset) = self.cache.opcodes.binary.get_checked(&(*id, ov_id)).unwrap_or((CompiledRynaExpr::Halt, 0));

                if (*id == AND_BINOP_ID || *id == OR_BINOP_ID) && *a_t.deref_type() == BOOL && *b_t.deref_type() == BOOL {
                    offset += 1;
                }

                if root && opcode.needs_no_drop() {
                    *root_counter -= 1; // No drop for Assign
                }

                Ok(self.compiled_form_size(a, false, root_counter)? + self.compiled_form_size(b, false, root_counter)? + 1 + offset)
            },

            NaryOperation(_, _, _, a, b) => {
                *root_counter += root as usize; // Add drop instruction

                Ok(self.compiled_form_size(a, false, root_counter)? + self.compiled_form_body_size(b, false)? + 1)
            },

            Return(_, e) | CompiledVariableDefinition(_, _, _, _, e) | CompiledVariableAssignment(_, _, _, _, e) => Ok(self.compiled_form_size(e, false, root_counter)? + 1),
            
            If(_, ih, ib, ei, e) => {
                let needs_deref = self.infer_type(ih).unwrap().is_ref();
                let mut res = self.compiled_form_size(ih, false, root_counter)? + self.compiled_form_body_size(ib, true)? + 2 + needs_deref as usize;

                for (h, b) in ei {
                    let needs_deref = self.infer_type(h).unwrap().is_ref();
                    res += self.compiled_form_size(h, false, root_counter)? + self.compiled_form_body_size(b, true)? + 2 + needs_deref as usize;
                }

                if let Some(b) = e {
                    res += self.compiled_form_body_size(b, true)?;
                }
                
                Ok(res)
            },

            CompiledFor(_, _, _, _, c, b) => Ok(self.compiled_form_size(c, false, root_counter)? + self.compiled_form_body_size(b, true)? + 9),

            While(_, c, b) => {
                let needs_deref = self.infer_type(c).unwrap().is_ref();

                Ok(self.compiled_form_size(c, false, root_counter)? + self.compiled_form_body_size(b, true)? + 2 + needs_deref as usize)
            },

            DoBlock(_, b, _) => {
                *root_counter += root as usize; // Add drop instruction

                Ok(self.compiled_form_body_size(b, true)?)
            }

            Tuple(_, b) => {            
                *root_counter += root as usize; // Add drop instruction

                Ok(1 + self.compiled_form_body_size(b, false)?)
            },

            FunctionCall(_, id, t, a) => {
                *root_counter += root as usize; // Add drop instruction

                let args_types = a.iter().map(|i| self.infer_type(i)).collect::<Result<Vec<_>, _>>()?;
                
                let ov_id = self.cache.overloads.functions.get_checked(&(*id, args_types.clone(), t.clone())).unwrap();
                let (opcode, offset) = self.cache.opcodes.functions.get_checked(&(*id, ov_id)).unwrap_or((CompiledRynaExpr::Halt, 0));

                if root && opcode.needs_no_drop() {
                    *root_counter -= 1; // No drop for Inc
                }

                Ok(self.compiled_form_body_size(a, false)? + 1 + offset)
            }, 

            _ => unreachable!("{:?}", expr)
        }
    }

    pub fn compiled_form_body_size(&self, lines: &Vec<RynaExpr>, root: bool) -> Result<usize, RynaError> {
        let mut counter = 0;
        let mut res = 0;

        for i in lines {
            res += self.compiled_form_size(i, root, &mut counter)?;
        }

        Ok(res + counter)
    }

    pub fn compiled_literal_size(obj: &Object) -> usize {
        match obj.get_type() {
            Type::Empty |
            Type::Basic(INT_ID) |
            Type::Basic(FLOAT_ID) |
            Type::Basic(BOOL_ID) |
            Type::Basic(STR_ID) => 1,

            Type::Basic(_) => {
                let obj_t = obj.get::<TypeInstance>();
                let mut res = 1;

                for i in obj_t.attributes.iter().rev() {
                    res += RynaContext::compiled_literal_size(i);
                }

                res
            },

            Type::Template(ARR_ID, _) => {
                let obj_t = obj.get::<RynaArray>();
                let mut res = 1;

                for i in obj_t.elements.iter().rev() {
                    res += RynaContext::compiled_literal_size(i);
                }

                res
            },

            _ => 1
        }
    }

    pub fn compile_literal(obj: &Object) -> Vec<RynaInstruction> {
        match obj.get_type() {
            Type::Empty => vec!(RynaInstruction::from(CompiledRynaExpr::Empty)),
            
            Type::Basic(INT_ID) => vec!(RynaInstruction::from(CompiledRynaExpr::Int(obj.get::<Integer>().clone()))),
            Type::Basic(FLOAT_ID) => vec!(RynaInstruction::from(CompiledRynaExpr::Float(*obj.get::<f64>()))),
            Type::Basic(BOOL_ID) => vec!(RynaInstruction::from(CompiledRynaExpr::Bool(*obj.get::<bool>()))),
            Type::Basic(STR_ID) => vec!(RynaInstruction::from(CompiledRynaExpr::Str(obj.get::<String>().clone()))),

            Type::Basic(id) => {
                let obj_t = obj.get::<TypeInstance>();
                let mut res = vec!();

                for i in obj_t.attributes.iter().rev() {
                    res.extend(RynaContext::compile_literal(i));
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::Construct(id, obj_t.attributes.len(), obj_t.params.clone())));

                res
            },

            Type::Template(ARR_ID, _) => {
                let obj_t = obj.get::<RynaArray>();
                let mut res = vec!();

                for i in obj_t.elements.iter().rev() {
                    res.extend(RynaContext::compile_literal(i));
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::Array(obj_t.elements.len(), *obj_t.elem_type.clone())));

                res
            },

            _ => unreachable!()
        }
    }

    pub fn compiled_form_expr(
        &self, expr: &RynaExpr,
        root: bool
    ) -> Result<Vec<RynaInstruction>, RynaError> {
        return match expr {
            RynaExpr::Break(l) => {
                Ok(vec!(
                    RynaInstruction::from(CompiledRynaExpr::Placeholder(PlaceholderType::Break)).set_loc(l) // Placeholder
                ))
            }
            
            RynaExpr::Continue(l) => {
                Ok(vec!(
                    RynaInstruction::from(CompiledRynaExpr::Placeholder(PlaceholderType::Continue)).set_loc(l) // Placeholder
                ))
            }

            RynaExpr::Literal(l, obj) => {
                let mut res = RynaContext::compile_literal(obj);
                
                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop));
                }

                Ok(res.into_iter().map(|i| i.set_loc(l)).collect())
            },

            RynaExpr::AttributeAccess(l, e, att_idx) => {
                let mut res = self.compiled_form_expr(e, root)?;
                let arg_type = self.infer_type(e)?;

                if let Type::Basic(id) | Type::Template(id, _) = arg_type.deref_type() {
                    let mut att_type = self.type_templates[*id].attributes[*att_idx].1.clone();

                    // Subtitute template parameters if needed
                    if let Type::Template(_, ts) = arg_type.deref_type() {
                        att_type = att_type.sub_templates(&ts.iter().cloned().enumerate().collect());
                    }
                    
                    let opcode = match (&arg_type, &att_type) {
                        (Type::MutRef(_), Type::Ref(_)) => CompiledRynaExpr::AttributeRef(*att_idx),
                        (Type::MutRef(_), _) => CompiledRynaExpr::AttributeMut(*att_idx),
                        (Type::Ref(_), _) => CompiledRynaExpr::AttributeRef(*att_idx),
                        (_, _) => CompiledRynaExpr::AttributeMove(*att_idx)
                    };

                    res.push(RynaInstruction::from(opcode).set_loc(l));
                
                    if root { // Drop if the return value is unused
                        res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                    }

                    Ok(res)

                } else {
                    unreachable!()
                }
            }

            RynaExpr::CompiledLambda(l, i, c, a, r, _) => {
                let mut res = vec!();

                for (_, i) in c.iter().rev() {
                    res.extend(self.compiled_form_expr(i, false)?);
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::Lambda(
                    *self.lambda_positions.get(i).unwrap(),
                    c.len(),
                    if a.len() == 1 {
                        a[0].1.clone()

                    } else {
                        Type::And(a.iter().map(|(_, t)| t).cloned().collect())
                    },
                    r.clone()
                )).set_loc(l));
                
                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            },

            RynaExpr::DoBlock(l, lines, _) => {
                let mut res = self.compiled_form_body(lines)?;
                let length = res.len();

                // Transform returns into relative jumps
                for (idx, i) in res.iter_mut().enumerate() {
                    if let CompiledRynaExpr::Return = i.instruction {
                        i.instruction = CompiledRynaExpr::RelativeJump((length - idx) as i32);
                    }
                }
                
                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            }

            RynaExpr::Tuple(l, e) => {
                let mut res = vec!();

                for i in e.iter().rev() {
                    res.extend(self.compiled_form_expr(i, false)?);
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::Tuple(e.len())).set_loc(l));
                
                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            }

            RynaExpr::Variable(l, id, _, t) => {
                let mut res = vec!(
                    if t.is_ref() {
                        RynaInstruction::new_with_type(
                            CompiledRynaExpr::CloneVariable(*id), "".into(), t.clone()
                        ).set_loc(l)
    
                    } else {
                        RynaInstruction::new_with_type(
                            CompiledRynaExpr::GetVariable(*id), "".into(), t.clone()
                        ).set_loc(l)    
                    }
                );
                
                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            }, 

            RynaExpr::AttributeAssignment(l, a, b, att_idx) => {
                let mut res = self.compiled_form_expr(a, false)?;
                res.append(&mut self.compiled_form_expr(b, false)?);

                res.push(RynaInstruction::from(CompiledRynaExpr::AttributeAssign(*att_idx)).set_loc(l));

                Ok(res)
            }

            RynaExpr::CompiledVariableDefinition(l, id, _, t, e) | RynaExpr::CompiledVariableAssignment(l, id, _, t, e) => {
                let mut res = self.compiled_form_expr(e, false)?;
                res.push(RynaInstruction::new_with_type(
                    CompiledRynaExpr::StoreVariable(*id),
                    String::new(),
                    t.clone()
                ).set_loc(l));

                Ok(res)
            },

            RynaExpr::UnaryOperation(l, id, t, e) => {
                let mut res = self.compiled_form_expr(e, false)?;

                let i_t = self.infer_type(e)?;

                let ov_id = self.cache.overloads.unary.get_checked(&(*id, vec!(i_t.clone()), t.clone())).unwrap();

                if let Some(pos) = self.cache.locations.unary.get_checked(&(*id, vec!(i_t.clone()), t.clone())) {
                    res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));

                } else if let Some((opcode, _)) = self.cache.opcodes.unary.get_checked(&(*id, ov_id)) {                    
                    // Deref if necessary
                    if opcode.needs_deref() && i_t.is_ref() {
                        res.push(RynaInstruction::from(CompiledRynaExpr::Deref).set_loc(l));
                    }

                    // Convert to float if necessary
                    if opcode.needs_float() {
                        if let Type::Basic(INT_ID) = i_t.deref_type() {
                            res.push(RynaInstruction::from(CompiledRynaExpr::ToFloat).set_loc(l));
                        }
                    }

                    res.push(RynaInstruction::from(opcode).set_loc(l));

                } else {
                    res.push(RynaInstruction::from(CompiledRynaExpr::UnaryOperatorCall(*id, ov_id, t.clone())).set_loc(l));
                }   

                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            },

            RynaExpr::BinaryOperation(l, id, t, a, b) => {
                let mut res_a = self.compiled_form_expr(b, false)?;
                let mut res_b = self.compiled_form_expr(a, false)?;
                
                let a_t = self.infer_type(a)?;
                let b_t = self.infer_type(b)?;

                let ov_id = self.cache.overloads.binary.get_checked(&(*id, vec!(a_t.clone(), b_t.clone()), t.clone())).unwrap();

                let res_op;

                // Short circuit
                let mut short_circuit = false;
                let mut short_circuit_on = true;
                let mut translated_opcode = CompiledRynaExpr::Halt; // Invalid opcode for now

                if let Some(pos) = self.cache.locations.binary.get_checked(&(*id, vec!(a_t.clone(), b_t.clone()), t.clone())) {
                    res_op = RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l);

                } else {
                    if (*id == AND_BINOP_ID || *id == OR_BINOP_ID) && *a_t.deref_type() == BOOL && *b_t.deref_type() == BOOL {
                        short_circuit = true;
                        short_circuit_on = *id == OR_BINOP_ID; // True on OR and false on AND
                    }

                    if let Some((opcode, _)) = self.cache.opcodes.binary.get_checked(&(*id, ov_id)) {
                        translated_opcode = opcode.clone();

                        // Deref if necessary
                        if opcode.needs_deref() {
                            if a_t.is_ref() {
                                res_b.push(RynaInstruction::from(CompiledRynaExpr::Deref).set_loc(l));
                            }
        
                            if b_t.is_ref() {
                                res_a.push(RynaInstruction::from(CompiledRynaExpr::Deref).set_loc(l));
                            }
                        }
    
                        // Convert to float if necessary
                        if opcode.needs_float() {
                            if let Type::Basic(INT_ID) = a_t.deref_type() {
                                res_b.push(RynaInstruction::from(CompiledRynaExpr::ToFloat).set_loc(l));
                            }
    
                            if let Type::Basic(INT_ID) = b_t.deref_type() {
                                res_a.push(RynaInstruction::from(CompiledRynaExpr::ToFloat).set_loc(l));
                            }
                        }

                        res_op = RynaInstruction::from(opcode).set_loc(l);

                    } else {
                        res_op = RynaInstruction::from(CompiledRynaExpr::BinaryOperatorCall(*id, ov_id, t.clone())).set_loc(l);
                    }
                }
                
                let mut res;

                if short_circuit {
                    res = res_b;

                    if short_circuit_on {
                        res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfTrue(res_a.len() + 2, true)).set_loc(l));

                    } else {
                        res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfFalse(res_a.len() + 2, true)).set_loc(l));
                    }

                    res.append(&mut res_a);
    
                } else {
                    res = res_a;
                    res.append(&mut res_b);
                }

                res.push(res_op);    

                if root && !translated_opcode.needs_no_drop() { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            },

            RynaExpr::NaryOperation(l, id, t, a, b) => {
                let mut res = vec!();

                for i in b.iter().rev() {
                    res.extend(self.compiled_form_expr(i, false)?);
                }
                
                res.extend(self.compiled_form_expr(a, false)?);

                let a_t = self.infer_type(a)?;
                let b_t = b.iter().map(|i| self.infer_type(i)).collect::<Result<Vec<_>, _>>()?;

                let mut arg_types = vec!(a_t.clone());
                arg_types.extend(b_t.iter().cloned());

                let ov_id = self.cache.overloads.nary.get_checked(&(*id, arg_types.clone(), t.clone())).unwrap();

                if let Some(pos) = self.cache.locations.nary.get_checked(&(*id, arg_types, t.clone())) {
                    res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));

                } else if let Some((opcode, _)) = self.cache.opcodes.nary.get_checked(&(*id, ov_id)) {
                    res.push(RynaInstruction::from(opcode).set_loc(l));
                
                } else {                    
                    res.push(RynaInstruction::from(CompiledRynaExpr::NaryOperatorCall(*id, ov_id, t.clone())).set_loc(l));
                }   

                if root { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            },

            RynaExpr::If(l, ih, ib, ei, e) => {
                let mut res = self.compiled_form_expr(ih, false)?;
                let if_body = self.compiled_form_body(ib)?;

                if self.infer_type(ih).unwrap().is_ref() {
                    res.push(RynaInstruction::from(CompiledRynaExpr::Deref).set_loc(l));
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfFalse(if_body.len() + 2, false)));
                res.extend(if_body);

                let mut elif = vec!();
                let mut else_body = vec!();
                let mut complete_size = 1;

                for (h, b) in ei {
                    let cond = self.compiled_form_expr(h, false)?;
                    let body = self.compiled_form_body(b)?;
                    let needs_deref = self.infer_type(h).unwrap().is_ref();
                    
                    complete_size += cond.len() + body.len() + 2 + needs_deref as usize;

                    elif.push((cond, body, needs_deref));
                }

                if let Some(b) = e {
                    else_body = self.compiled_form_body(b)?;
                    complete_size += else_body.len();
                }

                res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJump(complete_size as i32)));

                for (cond, body, needs_deref) in elif {
                    complete_size -= cond.len() + body.len() + 2;

                    res.extend(cond);
                    
                    if needs_deref {
                        res.push(RynaInstruction::from(CompiledRynaExpr::Deref));
                    }

                    res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfFalse(body.len() + 2, false)));
                    res.extend(body);
                    res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJump(complete_size as i32)));
                }

                res.extend(else_body);

                Ok(res)
            },

            RynaExpr::While(l, c, b) => {
                // Start with the condition
                let mut res = self.compiled_form_expr(c, false)?;
                let while_body = self.compiled_form_body(b)?;

                if self.infer_type(c).unwrap().is_ref() {
                    res.push(RynaInstruction::from(CompiledRynaExpr::Deref).set_loc(l));
                }
                
                // Add while body
                let beginning_jmp = CompiledRynaExpr::RelativeJump(-(while_body.len() as i32 + res.len() as i32 + 1));

                res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfFalse(while_body.len() + 2, false)));
                res.extend(while_body);

                // Jump to the beginning of the loop
                res.push(RynaInstruction::from(beginning_jmp));

                // Transform breaks and continues into relative jumps
                let length = res.len();

                for (idx, i) in res.iter_mut().enumerate() {
                    if let CompiledRynaExpr::Placeholder(PlaceholderType::Break) = i.instruction {
                        i.instruction = CompiledRynaExpr::RelativeJump((length - idx) as i32);
                    
                    } else if let CompiledRynaExpr::Placeholder(PlaceholderType::Continue) = i.instruction {
                        i.instruction = CompiledRynaExpr::RelativeJump(-(idx as i32));
                    }
                }

                Ok(res)
            },

            RynaExpr::CompiledFor(l, it_var_id, elem_var_id, _, c, b) => {
                let t = self.infer_type(c)?;

                let mut res = self.compiled_form_expr(c, false)?;

                // Get "iterator", "next" and "is_consumed" function overloads and check them
                let (it_ov_id, it_type, it_native, it_args) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(t.clone()), None, true, l)?;

                let it_mut = Type::MutRef(Box::new(it_type.clone()));

                let (next_ov_id, _, next_native, next_args) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), None, true, l)?;
                let (consumed_ov_id, consumed_res, consumed_native, consumed_args) = self.get_first_function_overload(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), None, true, l)?;

                if let Type::Basic(BOOL_ID) = consumed_res {
                    let for_body = self.compiled_form_body(b)?;
                    let for_body_len = for_body.len();

                    // Convert the iterable into an iterator
                    if it_native {
                        res.push(RynaInstruction::from(CompiledRynaExpr::NativeFunctionCall(ITERATOR_FUNC_ID, it_ov_id, it_args)).set_loc(l));

                    } else {
                        let pos = self.cache.locations.functions.get_checked(&(ITERATOR_FUNC_ID, vec!(t.clone()), it_args.clone())).unwrap();
                        res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));
                    }

                    // Store the iterator
                    res.push(RynaInstruction::from(CompiledRynaExpr::StoreVariable(*it_var_id)).set_loc(l));

                    // Check end of iterator
                    res.push(RynaInstruction::from(CompiledRynaExpr::GetVariable(*it_var_id)).set_loc(l));

                    if consumed_native {
                        res.push(RynaInstruction::from(CompiledRynaExpr::NativeFunctionCall(IS_CONSUMED_FUNC_ID, consumed_ov_id, consumed_args)).set_loc(l));

                    } else {
                        let pos = self.cache.locations.functions.get_checked(&(IS_CONSUMED_FUNC_ID, vec!(it_mut.clone()), consumed_args.clone())).unwrap();
                        res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));
                    }                                    

                    // Jump to end of loop
                    res.push(RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfTrue(for_body_len + 5, false)));

                    // Get next value
                    res.push(RynaInstruction::from(CompiledRynaExpr::GetVariable(*it_var_id)).set_loc(l));

                    if next_native {
                        res.push(RynaInstruction::from(CompiledRynaExpr::NativeFunctionCall(NEXT_FUNC_ID, next_ov_id, next_args)).set_loc(l));

                    } else {
                        let pos = self.cache.locations.functions.get_checked(&(NEXT_FUNC_ID, vec!(it_mut.clone()), next_args.clone())).unwrap();
                        res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));
                    }

                    // Store next value
                    res.push(RynaInstruction::from(CompiledRynaExpr::StoreVariable(*elem_var_id)).set_loc(l));

                    // Add for body
                    let beginning_jmp = CompiledRynaExpr::RelativeJump(-(for_body_len as i32 + 6));

                    res.extend(for_body);

                    // Jump to the beginning of the loop
                    res.push(RynaInstruction::from(beginning_jmp));

                    // Transform breaks and continues into relative jumps
                    let length = res.len();

                    for (idx, i) in res.iter_mut().enumerate() {
                        if let CompiledRynaExpr::Placeholder(PlaceholderType::Break) = i.instruction {
                            i.instruction = CompiledRynaExpr::RelativeJump((length - idx) as i32);
                        
                        } else if let CompiledRynaExpr::Placeholder(PlaceholderType::Continue) = i.instruction {
                            i.instruction = CompiledRynaExpr::RelativeJump((length - idx) as i32 - 1);
                        }
                    }

                    Ok(res)

                } else {
                    Err(RynaError::compiler_error(format!("Funtion overload is_consumed({}) returns {} (expected Bool)", it_mut.get_name(self), consumed_res.get_name(self)), l, vec!()))
                }
            },

            RynaExpr::Return(l, e) => {
                let mut res = self.compiled_form_expr(e, false)?;
                res.push(RynaInstruction::from(CompiledRynaExpr::Return).set_loc(l));

                Ok(res)
            },

            RynaExpr::FunctionCall(l, id, t, a) => {
                let mut res = vec!();

                for i in a.iter().rev() {
                    res.extend(self.compiled_form_expr(i, false)?);
                }
                
                let args_types = a.iter().map(|i| self.infer_type(i)).collect::<Result<Vec<_>, _>>()?;
                
                let ov_id = self.cache.overloads.functions.get_checked(&(*id, args_types.clone(), t.clone())).unwrap();
                let mut translated_opcode = CompiledRynaExpr::Halt; // Invalid opcode for now

                if let Some(pos) = self.cache.locations.functions.get_checked(&(*id, args_types, t.clone())) {
                    if *id == self.get_function_id("destroy".into()).unwrap() {
                        res.push(RynaInstruction::from(CompiledRynaExpr::CallDestructor(pos)).set_loc(l));

                    } else {
                        res.push(RynaInstruction::from(CompiledRynaExpr::Call(pos)).set_loc(l));
                    }

                } else if let Some((mut opcode, _)) = self.cache.opcodes.functions.get_checked(&(*id, ov_id)) {
                    // TODO: add conversions and derefs if necessary 
                    opcode = match opcode {
                        // Add type parameters to Construct opcodes
                        CompiledRynaExpr::Construct(id, length, _) => CompiledRynaExpr::Construct(id, length, t.clone()),
                        _ => opcode
                    };

                    translated_opcode = opcode.clone();

                    res.push(RynaInstruction::new_with_type(
                        opcode,
                        "".into(),
                        self.functions[*id].overloads[ov_id].ret.clone()
                    ).set_loc(l));

                } else {
                    res.push(RynaInstruction::from(CompiledRynaExpr::NativeFunctionCall(*id, ov_id, t.clone())).set_loc(l));
                }

                if root && !translated_opcode.needs_no_drop() { // Drop if the return value is unused
                    res.push(RynaInstruction::from(CompiledRynaExpr::Drop).set_loc(l));
                }

                Ok(res)
            }
            
            _ => { Ok(vec!()) }
        };
    }

    pub fn compiled_form_body(
        &self, lines: &[RynaExpr]
    ) -> Result<Vec<RynaInstruction>, RynaError> {
        return Ok(lines.iter().map(|i| self.compiled_form_expr(i, true)).flat_map(|i| i.unwrap()).collect());
    }

    pub fn define_module_macro(&mut self, definition: RynaExpr, defined_macros: &mut FxHashSet<Location>) -> Result<bool, RynaError> {
        if let RynaExpr::Macro(l, an, n, t, p, m) = definition {
            if !defined_macros.contains(&l) {
                if self.macros.iter().any(|i| i.name == n) {
                    return Err(RynaError::compiler_error(format!("Syntax with name '{n}' is already defined"), &l, vec!()));
                }
    
                self.macros.push(RynaMacro {
                    location: l.clone(),
                    annotations: an,
                    name: n,
                    m_type: t,
                    pattern: p,
                    generator: m,
                });

                defined_macros.insert(l.clone());

                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn define_module_class(&mut self, definition: RynaExpr) -> Result<(), RynaError> {
        match definition {
            RynaExpr::ClassDefinition(l, an, n, t, a, al, p) => {
                let err = self.implicit_syntax_check(&n, &t, &a, &p);

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, &l, vec!()));
                }

                let n_templates = t.len();
                let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                let err = if self.get_type_template(&n).is_some() {
                    self.redefine_type(l.clone(), an.clone(), n.clone(), t, a.clone(), al, p, Some(
                        |ctx, c_type, s| {
                            if let Ok((_, o)) = ctx.parse_literal_type(c_type, Span::new(s.as_str()), &RefCell::default()) {
                                return Ok(o);
                            }
    
                            Err(format!("Unable to parse {} from {}", c_type.name, s))
                        }
                    ))

                } else {
                    self.define_type(l.clone(), an.clone(), n.clone(), t, a.clone(), al, p, Some(
                        |ctx, c_type, s| {
                            if let Ok((_, o)) = ctx.parse_literal_type(c_type, Span::new(s.as_str()), &RefCell::default()) {
                                return Ok(o);
                            }
    
                            Err(format!("Unable to parse {} from {}", c_type.name, s))
                        }
                    ))
                };

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, &l, vec!()));
                }

                let func_id = self.define_function(n.clone()).unwrap_or_default(); // Define constructor function
                let class_id = self.get_type_id(n).unwrap();

                if n_templates == 0 {
                    // Define constructor instance
                    let res = self.define_native_function_overload(func_id, 0, &arg_types, Type::Basic(class_id), |_, r, a, _| {
                        if let Type::Basic(id) = r {
                            return Ok(Object::new(TypeInstance {
                                id: *id,
                                params: vec!(),
                                attributes: a
                            }))
                        }

                        unreachable!();
                    });

                    if let Err(msg) = res {
                        return Err(RynaError::compiler_error(msg, &l, vec!()));
                    
                    } else {
                        self.cache.opcodes.functions.insert((func_id, res.unwrap()), (CompiledRynaExpr::Construct(class_id, a.len(), vec!()), 0));
                    }

                } else {
                    let templ = (0..n_templates).map(|i| Type::TemplateParam(i, vec!())).collect::<Vec<_>>();

                    // Define constructor instance
                    let res = self.define_native_function_overload(func_id, n_templates, &arg_types, Type::Template(class_id, templ.clone()), |t, r, a, _| {
                        if let Type::Template(id, _) = r {
                            return Ok(Object::new(TypeInstance {
                                id: *id,
                                params: t.clone(),
                                attributes: a
                            }))
                        }

                        unreachable!();
                    });

                    if let Err(msg) = res {
                        return Err(RynaError::compiler_error(msg, &l, vec!()));

                    } else {
                        self.cache.opcodes.functions.insert((func_id, res.unwrap()), (CompiledRynaExpr::Construct(class_id, a.len(), vec!()), 0));
                    }
                }
            },

            _ => unreachable!()
        }

        Ok(())
    }

    pub fn define_module_macros(&mut self, code: &String) -> Result<(), RynaError> {
        let mut defined_macros = FxHashSet::default();
        let mut changed = true;

        while changed {
            changed = false;

            let ops = self.ryna_macros_parser(Span::new(code));
        
            if let Err(err) = ops {
                return Err(RynaError::from(err).in_module(self.module_name.clone()));
            }
            
            for i in ops.unwrap().1 {
                changed |= self.define_module_macro(i, &mut defined_macros)?;
            }
        }

        Ok(())
    }

    pub fn define_module_classes(&mut self, code: &String) -> Result<(), RynaError> {
        if let Ok((_, i_names)) = self.ryna_interface_definition_names_parser(Span::new(code)) {
            for i_name in i_names {
                self.define_interface(Location::none(), vec!(), i_name, vec!(), vec!(), vec!(), vec!(), vec!()).unwrap();
            }

            if let Ok((_, names)) = self.ryna_class_names_parser(Span::new(code)) {
                for name in names {
                    self.define_type(Location::none(), vec!(), name, vec!(), vec!(), None, vec!(), None).unwrap();
                }
    
                let interfaces = self.ryna_interface_definition_parser(Span::new(code))?;

                for i in interfaces.1 {
                    if let RynaExpr::InterfaceDefinition(l, an, n, t, v, u, b, nr) = i {
                        self.redefine_interface(l, an, n, t, v, u, b, nr).unwrap();
                    }
                }

                let interfaces_impl = self.ryna_interface_implementation_parser(Span::new(code))?;

                for i in interfaces_impl.1 {
                    if let RynaExpr::InterfaceImplementation(_, tm, t, n, i_tm) = i {
                        self.define_interface_impl(n, tm, t, i_tm).unwrap();
                    }
                }

                let ops = self.ryna_class_parser(Span::new(code))?;
                
                for i in ops.1 {
                    self.define_module_class(i)?;
                }
            }
        }

        Ok(())
    }
    
    pub fn define_module_operators(&mut self, code: &String) -> Result<(), RynaError> {
        let ops = self.ryna_operators_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(RynaError::from(err).in_module(self.module_name.clone()));
        }

        for i in ops.unwrap().1 {
            let (l, err) = match &i {
                RynaExpr::PrefixOperatorDefinition(l, n, p) => (l, self.define_unary_operator(n.clone(), true, *p)),
                RynaExpr::PostfixOperatorDefinition(l, n, p) => (l, self.define_unary_operator(n.clone(), false, *p)),
                RynaExpr::BinaryOperatorDefinition(l, n, f, p) => (l, self.define_binary_operator(n.clone(), *f, *p)),
                RynaExpr::NaryOperatorDefinition(l, o, c, p) => (l, self.define_nary_operator(o.clone(), c.clone(), *p)),

                _ => unreachable!()
            };

            if let Err(msg) = err {
                return Err(RynaError::compiler_error(msg, l, vec!()));
            }
        }

        Ok(())
    }
    
    pub fn define_module_functions(&mut self, code: &String) -> Result<(), RynaError> {
        let ops = self.ryna_function_headers_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(RynaError::from(err).in_module(self.module_name.clone()));
        }

        for i in ops.unwrap().1 {
            self.define_function(i.0).unwrap_or_default();
        }

        Ok(())
    }
    
    pub fn define_module_operations(&mut self, code: &String) -> Result<(), RynaError> {
        let ops = self.ryna_operations_parser(Span::new(code));

        if let Err(err) = ops {
            return Err(RynaError::from(err).in_module(self.module_name.clone()));
        }

        for i in ops.unwrap().1 {
            let (l, err) = match &i {
                RynaExpr::PrefixOperationDefinition(l, an, id, tm, _a, t, r, _) |
                RynaExpr::PostfixOperationDefinition(l, an, id, tm, _a, t, r, _) => (l, self.define_unary_operation(l.clone(), an.clone(), *id, tm.len(), t.clone(), r.clone(), None)),
                RynaExpr::BinaryOperationDefinition(l, an, id, tm, (_a, ta), (_b, tb), r, _) => (l, self.define_binary_operation(l.clone(), an.clone(), *id, tm.len(), ta.clone(), tb.clone(), r.clone(), None)),
                RynaExpr::NaryOperationDefinition(l, an, id, tm, (_a, ta), v, r, _) => (l, self.define_nary_operation(l.clone(), an.clone(), *id, tm.len(), ta.clone(), &v.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(), r.clone(), None)),

                _ => unreachable!()
            };

            if let Err(msg) = err {
                return Err(RynaError::compiler_error(msg, l, vec!()));
            }
        }

        Ok(())
    }

    pub fn define_module_function_overloads(&mut self, lines: &Vec<RynaExpr>) -> Result<(), RynaError> {
        for i in lines {
            if let RynaExpr::FunctionDefinition(l, an, id, t, a, r, _)  = i {
                let arg_types = a.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();
                let err = self.define_function_overload(l.clone(), an.clone(), *id, t.len(), &arg_types, r.clone(), None);

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }
            }
        }

        Ok(())
    }

    pub fn parse_ryna_module(&mut self, code: &String) -> Result<Vec<RynaExpr>, RynaError> {
        return match self.ryna_parser(Span::new(code)) {
            Ok((_, lines)) => Ok(lines),

            Err(nom::Err::Error(error)) |
            Err(nom::Err::Failure(error)) => Err(RynaError::from(error).in_module(self.module_name.clone())),

            _ => unreachable!()
        };
    }

    pub fn map_ryna_interface(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, String> {
        let other_i = &other.interfaces[id];
        let i_name = &other_i.name;

        if !id_mapper.interfaces.contains_key(&id) {
            let interface_id;

            // If the function has another id in the target context
            if let Some(f) = self.get_interface(i_name) {
                interface_id = f.id;

            } else { // Else the function needs to be defined
                interface_id = self.interfaces.len();
                id_mapper.interfaces.entry(id).or_insert(interface_id);

                let mapped_fns = other_i.fns.iter().map(|(an, n, t, a, r)| {
                    (
                        an.clone(),
                        n.clone(),
                        t.clone(),
                        a.iter().map(|(n, t)| (n.clone(), t.map_type(self, other, id_mapper, l))).collect(),
                        r.map_type(self, other, id_mapper, l)
                    )
                }).collect::<Vec<_>>();

                let mapped_uns = other_i.uns.iter().map(|(an, id, tm, a, at, ret)| {
                    Result::<_, RynaError>::Ok((
                        an.clone(),
                        self.map_ryna_unary_operator(other, *id, id_mapper, l)?,
                        tm.clone(),
                        a.clone(),
                        at.map_type(self, other, id_mapper, l),
                        ret.map_type(self, other, id_mapper, l)
                    ))
                }).collect::<Result<Vec<_>, _>>().unwrap();

                let mapped_bin = other_i.bin.iter().map(|(an, id, tm, (a0, a0t), (a1, a1t), ret)| {
                    Result::<_, RynaError>::Ok((
                        an.clone(),
                        self.map_ryna_binary_operator(other, *id, id_mapper, l)?,
                        tm.clone(),
                        (a0.clone(), a0t.map_type(self, other, id_mapper, l)),
                        (a1.clone(), a1t.map_type(self, other, id_mapper, l)),
                        ret.map_type(self, other, id_mapper, l)
                    ))
                }).collect::<Result<Vec<_>, _>>().unwrap();

                let mapped_nary = other_i.nary.iter().map(|(an, id, tm, (a0, a0t), a, ret)| {
                    Result::<_, RynaError>::Ok((
                        an.clone(),
                        self.map_ryna_binary_operator(other, *id, id_mapper, l)?,
                        tm.clone(),
                        (a0.clone(), a0t.map_type(self, other, id_mapper, l)),
                        a.iter().map(|(n, t)| (n.clone(), t.map_type(self, other, id_mapper, l))).collect(),
                        ret.map_type(self, other, id_mapper, l)
                    ))
                }).collect::<Result<Vec<_>, _>>().unwrap();

                self.define_interface(other_i.location.clone(), other_i.annotations.clone(), i_name.clone(), other_i.params.clone(), mapped_fns, mapped_uns, mapped_bin, mapped_nary)?;
            }

            return Ok(interface_id);
        }

        Ok(id_mapper.interfaces[&id])
    }

    pub fn map_ryna_class(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, String> {
        let other_cl = &other.type_templates[id];
        let c_name = &other_cl.name;

        if !id_mapper.classes.contains_key(&id) {
            let class_id;

            // If the function has another id in the target context
            if let Some(f) = self.get_type_template(c_name) {
                class_id = f.id;

            } else { // Else the function needs to be defined
                class_id = self.type_templates.len();
                id_mapper.classes.entry(id).or_insert(class_id);

                let mapped_attrs = other_cl.attributes.iter().map(|(n, t)| (n.clone(), t.map_type(self, other, id_mapper, l))).collect();
                let mapped_alias = other_cl.alias.as_ref().map(|i| i.map_type(self, other, id_mapper, l));

                self.define_type(other_cl.location.clone(), other_cl.annotations.clone(), c_name.clone(), other_cl.params.clone(), mapped_attrs, mapped_alias, other_cl.patterns.clone(), other_cl.parser)?;
            }

            return Ok(class_id);
        }

        Ok(id_mapper.classes[&id])
    }

    fn map_ryna_function(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, RynaError> {
        let f_name = &other.functions[id].name;

        if !id_mapper.functions.contains_key(&id) {
            let fn_id;

            // If the function has another id in the target context
            if let Some(f) = self.get_function(f_name) {
                fn_id = f.id;

            } else { // Else the function needs to be defined
                fn_id = self.functions.len();

                if let Err(err) = self.define_function(f_name.clone()) {
                    return Err(RynaError::compiler_error(err, l, vec!()));
                }
            }

            return Ok(*id_mapper.functions.entry(id).or_insert(fn_id));
        }

        Ok(id_mapper.functions[&id])
    }

    fn map_ryna_unary_operator(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, RynaError> {
        if let Operator::Unary{representation: r, prefix, precedence, ..} = &other.unary_ops[id] {
            if !id_mapper.unary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _)) = self.unary_ops.iter()
                                     .map(|op| if let Operator::Unary{id: op_id, representation: op_rep, ..} = op { (op_id, op_rep) } else { unreachable!() }).find(|(_, op_rep)| *op_rep == r) {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.unary_ops.len();

                    if let Err(err) = self.define_unary_operator(r.clone(), *prefix, *precedence) {
                        return Err(RynaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*id_mapper.unary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(RynaError::compiler_error(format!("Unable to find unary operator with id = {}", id), l, vec!()));
        }

        Ok(id_mapper.unary_operators[&id])
    }

    fn map_ryna_binary_operator(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, RynaError> {
        if let Operator::Binary{representation: r, right_associative, precedence, ..} = &other.binary_ops[id] {
            if !id_mapper.binary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _)) = self.binary_ops.iter()
                                     .map(|op| if let Operator::Binary{id: op_id, representation: op_rep, ..} = op { (op_id, op_rep) } else { unreachable!() }).find(|(_, op_rep)| *op_rep == r) {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.binary_ops.len();

                    if let Err(err) = self.define_binary_operator(r.clone(), *right_associative, *precedence) {
                        return Err(RynaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*id_mapper.binary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(RynaError::compiler_error(format!("Unable to find binary operator with id = {}", id), l, vec!()));
        }

        Ok(id_mapper.binary_operators[&id])
    }

    fn map_ryna_nary_operator(&mut self, other: &RynaContext, id: usize, id_mapper: &mut IdMapper, l: &Location) -> Result<usize, RynaError> {
        if let Operator::Nary{open_rep: or, close_rep: cr, precedence, ..} = &other.nary_ops[id] {
            if !id_mapper.nary_operators.contains_key(&id) {
                let mapped_op_id;
    
                // If the function has another id in the target context
                if let Some((op_id, _, _)) = self.nary_ops.iter()
                                     .map(|op| if let Operator::Nary{id: op_id, open_rep: op_or, close_rep: op_cr, ..} = op { (op_id, op_or, op_cr) } else { unreachable!() }).find(|(_, op_or, op_cr)| *op_or == or && *op_cr == cr) {
                    mapped_op_id = *op_id;
    
                } else { // Else the function needs to be defined
                    mapped_op_id = self.binary_ops.len();

                    if let Err(err) = self.define_nary_operator(or.clone(), cr.clone(), *precedence) {
                        return Err(RynaError::compiler_error(err, l, vec!()));
                    }
                }
    
                return Ok(*id_mapper.nary_operators.entry(id).or_insert(mapped_op_id));
            }
        
        } else {
            return Err(RynaError::compiler_error(format!("Unable to find binary operator with id = {}", id), l, vec!()));
        }

        Ok(id_mapper.nary_operators[&id])
    }

    pub fn map_ryna_expression(
        &mut self, expr: &mut RynaExpr, ctx: &RynaContext,
        id_mapper: &mut IdMapper
    ) -> Result<(), RynaError> {
        match expr {
            RynaExpr::Break(..) |
            RynaExpr::Continue(..) |
            RynaExpr::Literal(..) |
            RynaExpr::NameReference(..) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) => {}

            RynaExpr::VariableDefinition(l, _, t, e) => {
                *t = t.map_type(self, ctx, id_mapper, l);

                self.map_ryna_expression(e, ctx, id_mapper)?;
            }

            RynaExpr::VariableAssignment(_, _, e) => {
                self.map_ryna_expression(e, ctx, id_mapper)?;
            }

            RynaExpr::Tuple(_, b) => {
                for arg in b {
                    self.map_ryna_expression(arg, ctx, id_mapper)?;
                }
            }

            RynaExpr::DoBlock(l, b, t) => {
                *t = t.map_type(self, ctx, id_mapper, l);

                for arg in b {
                    self.map_ryna_expression(arg, ctx, id_mapper)?;
                }
            }

            RynaExpr::UnaryOperation(l, id, t, a) => {
                *id = self.map_ryna_unary_operator(ctx, *id, id_mapper, l)?;

                *t = t.iter().map(|t| t.map_type(self, ctx, id_mapper, l)).collect();

                self.map_ryna_expression(a, ctx, id_mapper)?;
            }

            RynaExpr::BinaryOperation(l, id, t, a, b) => {
                *id = self.map_ryna_binary_operator(ctx, *id, id_mapper, l)?;

                *t = t.iter().map(|t| t.map_type(self, ctx, id_mapper, l)).collect();

                self.map_ryna_expression(a, ctx, id_mapper)?;
                self.map_ryna_expression(b, ctx, id_mapper)?;
            }

            RynaExpr::NaryOperation(l, id, t, a, b) => {
                *id = self.map_ryna_nary_operator(ctx, *id, id_mapper, l)?;

                *t = t.iter().map(|t| t.map_type(self, ctx, id_mapper, l)).collect();

                self.map_ryna_expression(a, ctx, id_mapper)?;

                for arg in b {
                    self.map_ryna_expression(arg, ctx, id_mapper)?;
                }
            }

            RynaExpr::FunctionCall(l, id, t, args) => {
                *id = self.map_ryna_function(ctx, *id, id_mapper, l)?;

                *t = t.iter().map(|t| t.map_type(self, ctx, id_mapper, l)).collect();

                for arg in args {
                    self.map_ryna_expression(arg, ctx, id_mapper)?;
                }
            }

            RynaExpr::If(_, ih, ib, ei, eb) => {
                self.map_ryna_expression(ih, ctx, id_mapper)?;

                for line in ib {
                    self.map_ryna_expression(line, ctx, id_mapper)?;
                }

                for (ei_h, ei_b) in ei {
                    self.map_ryna_expression(ei_h, ctx, id_mapper)?;

                    for line in ei_b {
                        self.map_ryna_expression(line, ctx, id_mapper)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.map_ryna_expression(line, ctx, id_mapper)?;
                    }
                }
            }

            RynaExpr::While(_, c, lines) |
            RynaExpr::For(_, _, c, lines) => {
                self.map_ryna_expression(c, ctx, id_mapper)?;
                
                for line in lines {
                    self.map_ryna_expression(line, ctx, id_mapper)?;
                }
            }

            RynaExpr::Return(_, e) => {
                self.map_ryna_expression(e, ctx, id_mapper)?;
            }

            RynaExpr::Lambda(l, _, a, ret, lines) => {
                for (_, t) in a {
                    *t = t.map_type(self, ctx, id_mapper, l)
                }

                *ret = ret.map_type(self, ctx, id_mapper, l);

                for line in lines {
                    self.map_ryna_expression(line, ctx, id_mapper)?;
                }
            },

            e => unreachable!("{:?}", e)
        }

        Ok(())
    }

    pub fn import_code(
        &mut self, 
        code: &[RynaExpr], 
        source: &Vec<String>, 
        ctx: &RynaContext, 
        imports: &Imports
    ) -> Result<(Vec<RynaExpr>, Vec<String>), RynaError> {
        let mut res = vec!();
        let mut new_source = vec!();
        let mut id_mapper = IdMapper::default();

        for (line_idx, (line, module)) in code.iter().zip(source).enumerate() {
            match line {
                RynaExpr::Macro(_, _, n, _, p, _) => {
                    if needs_import(module, ImportType::Syntax, n, imports, &mut self.cache.imports.macros, (n.clone(), p.clone())) {
                        self.define_module_macro(line.clone(), &mut FxHashSet::default()).map(|_| ())?;
                    }
                }

                RynaExpr::InterfaceImplementation(l, t, tp, n, ts) => {
                    if needs_import(module, ImportType::Interface, n, imports, &mut self.cache.imports.interface_impl, (t.clone(), tp.clone(), n.clone(), ts.clone())) {
                        let mapped_type = tp.map_type(self, ctx, &mut id_mapper, l);
                        let mapped_args = ts.iter().map(|i| i.map_type(self, ctx, &mut id_mapper, l)).collect::<Vec<_>>();

                        self.define_interface_impl(n.clone(), t.clone(), mapped_type.clone(), mapped_args.clone()).unwrap();

                        let mapped_expr = RynaExpr::InterfaceImplementation(l.clone(), t.clone(), mapped_type, n.clone(), mapped_args);

                        res.push(mapped_expr);
                        new_source.push(module.clone());
                    }
                }

                RynaExpr::InterfaceDefinition(l, an, n, t, fns, uns, bin, nary) => {
                    if needs_import(module, ImportType::Interface, n, imports, &mut self.cache.imports.interface_def, (n.clone(), t.clone())) {
                        self.map_ryna_interface(ctx, ctx.get_interface_id(n.clone()).unwrap(), &mut id_mapper, l).unwrap();

                        let mapped_fns = fns.iter().map(|(an, n, t, a, r)| {
                            (
                                an.clone(),
                                n.clone(),
                                t.clone(),
                                a.iter().map(|(n, t)| (n.clone(), t.map_type(self, ctx, &mut id_mapper, l))).collect(),
                                r.map_type(self, ctx, &mut id_mapper, l)
                            )
                        }).collect();

                        let mapped_uns = uns.iter().map(|(an, id, tm, a, at, ret)| {
                            Result::<_, RynaError>::Ok((
                                an.clone(),
                                self.map_ryna_unary_operator(ctx, *id, &mut id_mapper, l)?,
                                tm.clone(),
                                a.clone(),
                                at.map_type(self, ctx, &mut id_mapper, l),
                                ret.map_type(self, ctx, &mut id_mapper, l)
                            ))
                        }).collect::<Result<Vec<_>, _>>()?;

                        let mapped_bin = bin.iter().map(|(an, id, tm, (a0, a0t), (a1, a1t), ret)| {
                            Result::<_, RynaError>::Ok((
                                an.clone(),
                                self.map_ryna_binary_operator(ctx, *id, &mut id_mapper, l)?,
                                tm.clone(),
                                (a0.clone(), a0t.map_type(self, ctx, &mut id_mapper, l)),
                                (a1.clone(), a1t.map_type(self, ctx, &mut id_mapper, l)),
                                ret.map_type(self, ctx, &mut id_mapper, l)
                            ))
                        }).collect::<Result<Vec<_>, _>>().unwrap();

                        let mapped_nary = nary.iter().map(|(an, id, tm, (a0, a0t), a, ret)| {
                            Result::<_, RynaError>::Ok((
                                an.clone(),
                                self.map_ryna_binary_operator(ctx, *id, &mut id_mapper, l)?,
                                tm.clone(),
                                (a0.clone(), a0t.map_type(self, ctx, &mut id_mapper, l)),
                                a.iter().map(|(n, t)| (n.clone(), t.map_type(self, ctx, &mut id_mapper, l))).collect(),
                                ret.map_type(self, ctx, &mut id_mapper, l)
                            ))
                        }).collect::<Result<Vec<_>, _>>().unwrap();

                        let mapped_expr = RynaExpr::InterfaceDefinition(l.clone(), an.clone(), n.clone(), t.clone(), mapped_fns, mapped_uns, mapped_bin, mapped_nary);

                        res.push(mapped_expr);
                        new_source.push(module.clone());
                    }
                }

                RynaExpr::ClassDefinition(l, an, n, t, atts, al, p) => {
                    if needs_import(module, ImportType::Class, n, imports, &mut self.cache.imports.classes, (n.clone(), t.clone())) {
                        let mapped_atts = atts.iter().map(|(n, t)| (n.clone(), t.map_type(self, ctx, &mut id_mapper, l))).collect();
                        let mapped_al = al.clone().map(|i| i.map_type(self, ctx, &mut id_mapper, l));
                        let mapped_expr = RynaExpr::ClassDefinition(l.clone(), an.clone(), n.clone(), t.clone(), mapped_atts, mapped_al, p.clone());

                        self.define_module_class(mapped_expr.clone())?;
                        
                        res.push(mapped_expr);
                        new_source.push(module.clone());
                    }
                }

                RynaExpr::FunctionDefinition(l, an, id, t, a, r, b) => {
                    let f_name = &ctx.functions[*id].name;
                    let fn_id = self.map_ryna_function(ctx, *id, &mut id_mapper, l)?;

                    let mapped_args = a.iter().map(|(n, t)| (n.clone(), t.map_type(self, ctx, &mut id_mapper, l))).collect::<Vec<_>>();
                    let mapped_return = r.map_type(self, ctx, &mut id_mapper, l);

                    if needs_import(module, ImportType::Fn, f_name, imports, &mut self.cache.imports.functions, (fn_id, t.clone(), mapped_args.clone(), mapped_return.clone())) {
                        let mut mapped_body = b.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_ryna_expression(line, ctx, &mut id_mapper)?;
                        }

                        let arg_types = mapped_args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                        if let Err(err) = self.define_function_overload(l.clone(), an.clone(), fn_id, t.len(), &arg_types, mapped_return.clone(), None) {
                            return Err(RynaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(RynaExpr::FunctionDefinition(l.clone(), an.clone(), fn_id, t.clone(), mapped_args.clone(), mapped_return, mapped_body));
                        new_source.push(module.clone());
                    }
                }

                RynaExpr::PrefixOperationDefinition(l, an, id, t, arg, arg_t, r, body) |
                RynaExpr::PostfixOperationDefinition(l, an, id, t, arg, arg_t, r, body) => {
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

                    let op_id = self.map_ryna_unary_operator(ctx, *id, &mut id_mapper, l)?;

                    let mapped_arg_t = arg_t.map_type(self, ctx, &mut id_mapper, l);
                    let mapped_return = r.map_type(self, ctx, &mut id_mapper, l);

                    if needs_import(module, op_import_type, rep, imports, &mut self.cache.imports.unary, (op_id, t.clone(), mapped_arg_t.clone(), mapped_return.clone())) {
                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_ryna_expression(line, ctx, &mut id_mapper)?;
                        }

                        if let Err(err) = self.define_unary_operation(l.clone(), an.clone(), *id, t.len(), mapped_arg_t.clone(), mapped_return.clone(), None) {
                            return Err(RynaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        if *op_prefix {
                            res.push(RynaExpr::PrefixOperationDefinition(l.clone(), an.clone(), op_id, t.clone(), arg.clone(), mapped_arg_t, mapped_return, mapped_body));

                        } else {
                            res.push(RynaExpr::PostfixOperationDefinition(l.clone(), an.clone(), op_id, t.clone(), arg.clone(), mapped_arg_t, mapped_return, mapped_body));
                        }

                        new_source.push(module.clone());
                    }
                },

                RynaExpr::BinaryOperationDefinition(l, an, id, t, a, b, r, body) => {
                    let rep = ctx.binary_ops[*id].get_repr();

                    let op_id = self.map_ryna_binary_operator(ctx, *id, &mut id_mapper, l)?;

                    let mapped_arg1 = (a.0.clone(), a.1.map_type(self, ctx, &mut id_mapper, l));
                    let mapped_arg2 = (b.0.clone(), b.1.map_type(self, ctx, &mut id_mapper, l));
                    let mapped_return = r.map_type(self, ctx, &mut id_mapper, l);

                    if needs_import(module, ImportType::Binary, &rep, imports, &mut self.cache.imports.binary, (op_id, t.clone(), mapped_arg1.1.clone(), mapped_arg2.1.clone(), mapped_return.clone())) {
                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_ryna_expression(line, ctx, &mut id_mapper)?;
                        }

                        if let Err(err) = self.define_binary_operation(l.clone(), an.clone(), *id, t.len(), mapped_arg1.1.clone(), mapped_arg2.1.clone(), mapped_return.clone(), None) {
                            return Err(RynaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(RynaExpr::BinaryOperationDefinition(l.clone(), an.clone(), op_id, t.clone(), mapped_arg1, mapped_arg2, mapped_return, mapped_body));
                        new_source.push(module.clone());
                    }
                },

                RynaExpr::NaryOperationDefinition(l, an, id, t, arg, args, r, body) => {
                    let rep = ctx.nary_ops[*id].get_repr();

                    let op_id = self.map_ryna_nary_operator(ctx, *id, &mut id_mapper, l)?;

                    let mapped_arg = (arg.0.clone(), arg.1.map_type(self, ctx, &mut id_mapper, l));
                    let mapped_args = args.iter().map(|(n, t)| (n.clone(), t.map_type(self, ctx, &mut id_mapper, l))).collect::<Vec<_>>();
                    let mapped_return = r.map_type(self, ctx, &mut id_mapper, l);

                    if needs_import(module, ImportType::Binary, &rep, imports, &mut self.cache.imports.nary, (*id, t.clone(), mapped_arg.1.clone(), mapped_args.clone(), mapped_return.clone())) {
                        let mut mapped_body = body.clone();

                        // Map each line of the definition to the target context
                        for line in mapped_body.iter_mut() {
                            self.map_ryna_expression(line, ctx, &mut id_mapper)?;
                        }

                        let arg_types = mapped_args.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>();

                        if let Err(err) = self.define_nary_operation(l.clone(), an.clone(), *id, t.len(), mapped_arg.1.clone(), &arg_types, mapped_return.clone(), None) {
                            return Err(RynaError::compiler_error(err, l, vec!()));
                        }

                        // Add the mapped function to the list of new expressions
                        res.push(RynaExpr::NaryOperationDefinition(l.clone(), an.clone(), op_id, t.clone(), mapped_arg, mapped_args, mapped_return, mapped_body));
                        new_source.push(module.clone());
                    }
                },

                expr => {
                    if needs_line_import(module, line_idx, &mut self.cache.imports.lines) {
                        let mut mapped_expr = expr.clone();
                        self.map_ryna_expression(&mut mapped_expr, ctx, &mut id_mapper)?;
    
                        res.push(mapped_expr);
                        new_source.push(module.clone());    
                    }
                }
            }
        }

        Ok((res, new_source))
    }

    // BFS on imports
    fn cascade_imports(
        imports: &mut ImportMap,
        modules: &HashMap<String, &RynaModule>
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
            ImportType::Interface => self.get_interface_id(name.clone()).unwrap(),
            ImportType::Class => self.get_type_id(name.clone()).unwrap(),
            ImportType::Fn => self.get_function_id(name.clone()).unwrap(),

            ImportType::Prefix |
            ImportType::Postfix => self.unary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),
            
            ImportType::Binary => self.binary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),
            ImportType::Nary => self.nary_ops.iter().find(|i| i.get_repr() == *name).unwrap().get_id(),

            _ => unimplemented!()
        };
    }

    fn rev_map_import(&self, import: &ImportType, id: usize) -> String {
        match import {
            ImportType::Interface => self.interfaces[id].name.clone(),
            ImportType::Class => self.type_templates[id].name.clone(),
            ImportType::Fn => self.functions[id].name.clone(),

            ImportType::Prefix |
            ImportType::Postfix => self.unary_ops[id].get_repr(),
            
            ImportType::Binary => self.binary_ops[id].get_repr(),
            ImportType::Nary => self.nary_ops[id].get_repr(),

            _ => unimplemented!()
        }
    }

    // BFS on imports (inner dependencies)
    fn cascade_imports_inner(
        imports: &mut ImportMap,
        modules: &HashMap<String, &RynaModule>
    )
    {
        for (m, imps) in imports {
            let mut new_imports = Imports::new();
            let module = &modules.get(m).unwrap();

            for (t, names) in imps.iter() {
                if let ImportType::Syntax | ImportType::All = t { // Syntaxes do not have to be mapped
                    new_imports.entry(t.clone()).or_default().extend(names.iter().cloned());                    

                } else {
                    for name in names.iter() {
                        // Import all
                        if name == "*" {
                            new_imports.entry(t.clone()).or_default().insert(name.clone());

                        } else {
                            let id = module.ctx.map_import(t, name);
    
                            module.inner_dependencies.dfs(&(t.clone(), id), |(tp, id)| {
                                let mapped_name = module.ctx.rev_map_import(tp, *id);
                                new_imports.entry(tp.clone()).or_default().insert(mapped_name);
                            }); 
                        }
                    }
                }
            }

            for (t, names) in new_imports {
                imps.entry(t.clone()).or_default().extend(names);
            }
        }
    }

    pub fn parse_with_dependencies(
        &mut self, 
        name: &str,
        code: &String, 
        modules: &HashMap<String, &RynaModule>
    ) -> Result<(Vec<RynaExpr>, Vec<String>), RynaError> {
        let mut res = vec!();
        let mut source = vec!();
        let mut imports = ryna_module_imports_parser(Span::new(code), self.module_name.clone()).unwrap().1; // TODO: should cache this

        Self::cascade_imports(&mut imports, modules);
        Self::cascade_imports_inner(&mut imports, modules);

        // Import code from dependencies
        for (m, i) in imports {
            let other = modules.get(&m).unwrap();

            let (mut new_code, mut new_source) = self.import_code(&other.code, &other.source, &other.ctx, &i)?;
            source.append(&mut new_source);
            res.append(&mut new_code);
        }

        let mut main_code = self.parse_without_precompiling(code)?;
        source.extend(std::iter::repeat(name.to_owned()).take(main_code.len()));
        res.append(&mut main_code);

        Ok((res, source))
    }

    pub fn parse_without_precompiling(&mut self, code: &String) -> Result<Vec<RynaExpr>, RynaError> {
        self.define_module_macros(code)?;
        self.define_module_operators(code)?;
        self.define_module_classes(code)?;
        self.define_module_functions(code)?;
        self.define_module_operations(code)?;

        let lines = self.parse_ryna_module(code)?;

        self.define_module_function_overloads(&lines)?;

        Ok(lines)
    }

    pub fn precompile_module(&mut self, lines: &mut Vec<RynaExpr>) -> Result<(), RynaError> {        
        self.compile(lines, &vec!())?;

        // Static checks before doing anything else
        for expr in lines.iter_mut() {
            self.static_check(expr)?;
        }

        // Get every function and operation call in the program
        self.get_template_calls_body(lines)?;

        // Optimize the program
        if self.optimize {
            // Early optimization
            self.optimize(lines);

            for body in self.cache.templates.functions.inner_borrow_mut().values_mut() {
                self.optimize(body);
            }

            for body in self.cache.templates.unary.inner_borrow_mut().values_mut() {
                self.optimize(body);
            }

            for body in self.cache.templates.binary.inner_borrow_mut().values_mut() {
                self.optimize(body);
            }

            for body in self.cache.templates.nary.inner_borrow_mut().values_mut() {
                self.optimize(body);
            }

            // Late optimization
            macro_rules! optimize_cache {
                ($cache: expr) => {
                    let keys = $cache.inner_borrow_mut().keys().cloned().collect::<Vec<_>>();

                    for key in keys {
                        let mut body = $cache.get_checked(&key).unwrap().clone();
                        
                        self.late_optimize(&mut body);
        
                        $cache.insert(key, body);
                    }          
                };
            }

            optimize_cache!(self.cache.templates.functions);
            optimize_cache!(self.cache.templates.unary);
            optimize_cache!(self.cache.templates.binary);
            optimize_cache!(self.cache.templates.nary);

            self.late_optimize(lines);
        }

        Ok(())
    }

    pub fn parse_and_precompile(&mut self, code: &String) -> Result<Vec<RynaExpr>, RynaError> {
        let mut lines = self.parse_without_precompiling(code)?;
        self.precompile_module(&mut lines)?;

        Ok(lines)
    }

    pub fn parse_and_compile(&mut self, code: &String) -> Result<Vec<RynaInstruction>, RynaError> {
        let lines = self.parse_and_precompile(code)?;

        self.compiled_form(&lines)
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use malachite::Integer;

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
        inc<Int>(5);
        ";

        let (_, mut code) = ctx.ryna_parser(Span::new(code_1_str)).unwrap();
        ctx.compile(&mut code, &vec!()).unwrap();

        assert_eq!(code, vec!(
            RynaExpr::FunctionCall(Location::none(), 0, vec!(), vec!(
                RynaExpr::Literal(Location::none(), Object::new(Integer::from(5)))
            ))
        ));
        
        let (_, mut code) = ctx.ryna_parser(Span::new(code_str)).unwrap();

        assert!(ctx.compile(&mut code, &vec!()).is_ok());
    }

    #[test]
    fn compiled_form() {
        let mut ctx = standard_ctx();

        let code_str = "
            fn test(a: Int) -> Int {
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