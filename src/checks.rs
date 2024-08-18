use std::collections::{HashSet, HashMap};

use colored::Colorize;
use rustc_hash::FxHashSet;

use crate::annotations::Annotation;
use crate::compilation::RynaError;
use crate::context::RynaContext;
use crate::formats::{check_class_name, check_fn_name, check_interface_name, check_template_name};
use crate::located_ryna_warning;
use crate::parser::{RynaExpr, Location};
use crate::operations::Operator;
use crate::types::{Type, BOOL};
use crate::patterns::Pattern;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl RynaContext {
    pub fn ensured_return_check(expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::PrefixOperationDefinition(l, _, _, _, _, _, _, body) |
            RynaExpr::PostfixOperationDefinition(l, _, _, _, _, _, _, body) |
            RynaExpr::BinaryOperationDefinition(l, _, _, _, _, _, _, body) |
            RynaExpr::NaryOperationDefinition(l, _, _, _, _, _, _, body)  => RynaContext::ensured_return_check_body(body, l, "Operation"),

            RynaExpr::CompiledLambda(l, _, _, _, _, body) |
            RynaExpr::FunctionDefinition(l, _, _, _, _, _, body) => RynaContext::ensured_return_check_body(body, l, "Function"),

            RynaExpr::DoBlock(l, body, _) => RynaContext::ensured_return_check_body(body, l, "Do block"),

            _ => Ok(())
        }
    }

    pub fn ensured_return_check_body(lines: &Vec<RynaExpr>, l: &Location, instance: &str) -> Result<(), RynaError> {
        for line in lines {
            match line {
                RynaExpr::Return(_, _) => return Ok(()),

                RynaExpr::If(_, _, ib, ei, Some(eb_inner)) => {
                    let mut returns = RynaContext::ensured_return_check_body(ib, l, instance).is_ok() && 
                                      RynaContext::ensured_return_check_body(eb_inner, l, instance).is_ok();

                    if returns { // Check every branch
                        for (_, ei_b) in ei {
                            if RynaContext::ensured_return_check_body(ei_b, l, instance).is_err() {
                                returns = false;
                                break;
                            }
                        }
                    }

                    if returns {
                        return Ok(());
                    }
                }

                _ => {}
            }
        }
        
        Err(RynaError::compiler_error(format!("{instance} may not always return a value"), l, vec!()))
    }

    pub fn return_check(&self, expr: &RynaExpr, ret_type: &Option<Type>) -> Result<(), RynaError> {
        match (expr, ret_type) {
            (RynaExpr::Break(..), _) |
            (RynaExpr::Continue(..), _) |
            (RynaExpr::Literal(..), _) |
            (RynaExpr::Tuple(..), _) |
            (RynaExpr::Variable(..), _) |
            (RynaExpr::UnaryOperation(..), _) |
            (RynaExpr::BinaryOperation(..), _) |
            (RynaExpr::NaryOperation(..), _) |
            (RynaExpr::FunctionCall(..), _) |
            (RynaExpr::AttributeAccess(..), _) |
            (RynaExpr::AttributeAssignment(..), _) |
            (RynaExpr::PrefixOperatorDefinition(..), _) |
            (RynaExpr::PostfixOperatorDefinition(..), _) |
            (RynaExpr::BinaryOperatorDefinition(..), _) |
            (RynaExpr::NaryOperatorDefinition(..), _) |
            (RynaExpr::InterfaceDefinition(..), _) |
            (RynaExpr::InterfaceImplementation(..), _) |
            (RynaExpr::ClassDefinition(..), _) => Ok(()),

            (RynaExpr::CompiledVariableDefinition(_, _, _, _, e), _) |
            (RynaExpr::CompiledVariableAssignment(_, _, _, _, e), _) => self.return_check(e, &None),

            (RynaExpr::Return(l, _), None) => {
                Err(RynaError::compiler_error(
                    "Return statements are only allowed inside function and operation definition bodies".into(), 
                    l, vec!()
                ))
            },

            (RynaExpr::Return(l, e), Some(expected_t)) => {
                self.return_check(e, ret_type)?;
                let t = self.infer_type(e)?;

                if t.bindable_to(expected_t, self) {
                    Ok(())

                } else {
                    Err(RynaError::compiler_error(
                        format!("Value of type {} is not bindable to expected return value of type {}", t.get_name(self), expected_t.get_name(self)), 
                        l, vec!()
                    ))
                }
            },

            (RynaExpr::FunctionDefinition(_, _, _, t, _, ret, body), None) |
            (RynaExpr::PrefixOperationDefinition(_, _, _, t, _, _, ret, body), None) |
            (RynaExpr::PostfixOperationDefinition(_, _, _, t, _, _, ret, body), None) |
            (RynaExpr::BinaryOperationDefinition(_, _, _, t, _, _, ret, body), None) |
            (RynaExpr::NaryOperationDefinition(_, _, _, t, _, _, ret, body), None) => {
                if t.is_empty() {
                    let expected_ret = Some(ret.clone());

                    for line in body {
                        self.return_check(line, &expected_ret)?;
                    }
                }

                RynaContext::ensured_return_check(expr)
            }

            (RynaExpr::CompiledLambda(_, _, _, _, ret, body), _) => {
                let expected_ret = Some(ret.clone());

                for line in body {
                    self.return_check(line, &expected_ret)?;
                }

                self.repeated_arguments_check(expr)?;
                self.lambda_check(expr)?;
                RynaContext::ensured_return_check(expr)
            }

            (RynaExpr::DoBlock(_, body, ret), _) => {
                let expected_ret = Some(ret.clone());

                for line in body {
                    self.return_check(line, &expected_ret)?;
                }

                RynaContext::ensured_return_check(expr)
            }

            (RynaExpr::While(_, cond, body), ret) |
            (RynaExpr::CompiledFor(_, _, _, _, cond, body), ret) => {
                self.return_check(cond, ret)?;

                for line in body {
                    self.return_check(line, ret)?;
                }

                Ok(())
            },

            (RynaExpr::If(_, ih, ib, ei, eb), ret) => {
                self.return_check(ih, ret)?;

                for line in ib {
                    self.return_check(line, ret)?;
                }

                for (ei_h, ei_b) in ei {
                    self.return_check(ei_h, ret)?;

                    for line in ei_b {
                        self.return_check(line, ret)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.return_check(line, ret)?;
                    }
                }

                Ok(())
            },

            (RynaExpr::Macro(..), _) => { Ok(()) },

            _ => unimplemented!("{:?}", expr)
        }
    }

    pub fn ambiguity_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::Break(..) |
            RynaExpr::Continue(..) |
            RynaExpr::Literal(..) |
            RynaExpr::Variable(..) |
            RynaExpr::PrefixOperatorDefinition(..) |
            RynaExpr::PostfixOperatorDefinition(..) |
            RynaExpr::BinaryOperatorDefinition(..) |
            RynaExpr::NaryOperatorDefinition(..) |
            RynaExpr::InterfaceDefinition(..) |
            RynaExpr::InterfaceImplementation(..) |
            RynaExpr::ClassDefinition(..) => Ok(()),

            RynaExpr::DoBlock(_, body, _) |
            RynaExpr::CompiledLambda(_, _, _, _, _, body) |
            RynaExpr::Tuple(_, body) => {
                for line in body {
                    self.ambiguity_check(line)?;
                }

                Ok(())
            }

            RynaExpr::FunctionCall(l, id, _ , args) => {
                let mut arg_types = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    self.ambiguity_check(arg)?;

                    let t = self.infer_type(arg)?;
                    arg_types.push(t);
                }

                if let Some(ov) = self.is_function_overload_ambiguous(*id, arg_types.clone()) {
                    let f_name = &self.functions[*id].name;
                    let possibilities = ov.iter().map(|(a, r)| format!("{}{} -> {}", f_name, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();
                    
                    Err(RynaError::compiler_error(
                        format!(
                            "Function call {}({}) is ambiguous",
                            f_name.green(),
                            arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")
                        ), l,
                        possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                    ))

                } else {
                    Ok(())
                }
            },

            RynaExpr::UnaryOperation(l, id, _, arg) => {
                self.ambiguity_check(arg)?;
                let t = self.infer_type(arg)?;

                if let Some(ov) = self.is_unary_op_ambiguous(*id, t.clone()) {
                    if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*id] {
                        if *prefix {
                            let possibilities = ov.iter().map(|(a, r)| format!("{}({}) -> {}", representation, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();
                            
                            Err(RynaError::compiler_error(
                                format!(
                                    "Unary operation {}({}) is ambiguous",
                                    representation,
                                    t.get_name(self)
                                ), l, 
                                possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                            ))

                        } else {
                            let possibilities = ov.iter().map(|(a, r)| format!("({}){} -> {}", a.get_name(self), representation, r.get_name(self))).collect::<Vec<_>>();
        
                            Err(RynaError::compiler_error(
                                format!(
                                    "Unary operation ({}){} is ambiguous",
                                    t.get_name(self),
                                    representation
                                ), l, 
                                possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                            ))
                        }
                        
                    } else {
                        unreachable!();
                    }

                } else {
                    Ok(())
                }
            },

            RynaExpr::BinaryOperation(l, id, _, arg1, arg2) => {
                self.ambiguity_check(arg1)?;
                self.ambiguity_check(arg2)?;

                let t1 = self.infer_type(arg1)?;
                let t2 = self.infer_type(arg2)?;
                
                if let Some(ov) = self.is_binary_op_ambiguous(*id, t1.clone(), t2.clone()) {
                    if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                        let possibilities = ov.iter()
                            .map(|(a1, a2, r)| format!("({}){}({}) -> {}", a1.get_name(self), representation, a2.get_name(self), r.get_name(self)))
                            .collect::<Vec<_>>();                
                        
                        Err(RynaError::compiler_error(
                            format!(
                                "Binary operation ({}){}({}) is ambiguous",
                                t1.get_name(self),
                                representation, 
                                t2.get_name(self)
                            ), l, 
                            possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                        ))

                    } else {
                        unreachable!();
                    }

                } else {
                    Ok(())
                }
            },

            RynaExpr::NaryOperation(l, id, _, first, args) => {
                self.ambiguity_check(first)?;
                let t = self.infer_type(first)?;

                let mut arg_types = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    self.ambiguity_check(arg)?;
                    arg_types.push(self.infer_type(arg)?);
                }

                if let Some(ov) = self.is_nary_op_ambiguous(*id, t.clone(), arg_types.clone()) {
                    if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                        let possibilities = ov.iter()
                            .map(|(f, a, r)| 
                                format!(
                                    "{}{}{}{} -> {}", 
                                    f.get_name(self), 
                                    open_rep,
                                    a.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                    close_rep,
                                    r.get_name(self)
                                )
                            )
                            .collect::<Vec<_>>();
                        
                        Err(RynaError::compiler_error(
                            format!(
                                "N-ary operation {}{}{}{} is ambiguous",
                                t.get_name(self), 
                                open_rep,
                                arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                close_rep
                            ), l, 
                            possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                        ))

                    } else {
                        unreachable!()
                    }

                } else {
                    Ok(())
                }
            },

            RynaExpr::If(_, ih, ib, ei, eb) => {
                self.ambiguity_check(ih)?;

                for line in ib {
                    self.ambiguity_check(line)?;
                }

                for (ei_h, ei_b) in ei {
                    self.ambiguity_check(ei_h)?;

                    for line in ei_b {
                        self.ambiguity_check(line)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.ambiguity_check(line)?;
                    }
                }

                Ok(())
            },

            RynaExpr::While(_, cond, body) => {
                self.ambiguity_check(cond)?;

                for line in body {
                    self.ambiguity_check(line)?;
                }

                Ok(())
            },

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.ambiguity_check(a)?;
                self.ambiguity_check(b)
            }

            RynaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => {
                self.ambiguity_check(e)?;
                self.infer_type(e)?;

                Ok(())
            }

            RynaExpr::PrefixOperationDefinition(_, _, _, t, _, _, _, b) |
            RynaExpr::PostfixOperationDefinition(_, _, _, t, _, _, _, b) |
            RynaExpr::BinaryOperationDefinition(_, _, _, t, _, _, _, b) |
            RynaExpr::NaryOperationDefinition(_, _, _, t, _, _, _, b) |
            RynaExpr::FunctionDefinition(_, _, _, t, _, _, b) => {
                if t.is_empty() {
                    for line in b {
                        self.ambiguity_check(line)?;
                    }
                }

                Ok(())
            },

            RynaExpr::CompiledFor(_, _, _, _, _, b) => {
                for line in b {
                    self.ambiguity_check(line)?;
                }

                Ok(())
            }

            RynaExpr::Macro(..) => { Ok(()) },

            _ => unimplemented!("{:?}", expr)
        }
    }

    pub fn break_continue_check(expr: &RynaExpr, allowed: bool) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::ClassDefinition(..) |
            RynaExpr::InterfaceImplementation(..) |
            RynaExpr::PrefixOperatorDefinition(..) |
            RynaExpr::PostfixOperatorDefinition(..) |
            RynaExpr::BinaryOperatorDefinition(..) |
            RynaExpr::NaryOperatorDefinition(..) |
            RynaExpr::InterfaceDefinition(..) |
            RynaExpr::Macro(..) |
            RynaExpr::Variable(..) |
            RynaExpr::Literal(..) => Ok(()),

            RynaExpr::Break(..) if allowed => Ok(()),
            RynaExpr::Break(l) if !allowed => {
                Err(RynaError::compiler_error("Break statement is not allowed in this context".into(), l, vec!()))
            }

            RynaExpr::Continue(..) if allowed => Ok(()),
            RynaExpr::Continue(l) if !allowed => {
                Err(RynaError::compiler_error("Continue statement is not allowed in this context".into(), l, vec!()))
            }

            RynaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e) => {
                RynaContext::break_continue_check(e, allowed)
            }

            RynaExpr::Tuple(_, args) => args.iter().try_for_each(|i| RynaContext::break_continue_check(i, allowed)),
            
            RynaExpr::If(_, i, ib, ei, eb) => {
                RynaContext::break_continue_check(i, allowed)?;

                for i in ib {
                    RynaContext::break_continue_check(i, allowed)?;
                }
                
                for (ei_h, ei_b) in ei {
                    RynaContext::break_continue_check(ei_h, allowed)?;

                    for i in ei_b {
                        RynaContext::break_continue_check(i, allowed)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for i in eb_inner {
                        RynaContext::break_continue_check(i, allowed)?;
                    }    
                }

                Ok(())
            },

            RynaExpr::DoBlock(_, b, _) => {
                for i in b {
                    RynaContext::break_continue_check(i, allowed)?;
                }

                Ok(())
            }

            RynaExpr::CompiledFor(_, _, _, _, c, b) |
            RynaExpr::While(_, c, b) => {
                RynaContext::break_continue_check(c, true)?;

                for i in b {
                    RynaContext::break_continue_check(i, true)?;
                }

                Ok(())
            },

            RynaExpr::UnaryOperation(_, _, _, e) => {
                RynaContext::break_continue_check(e, allowed)?;

                Ok(())
            }

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                RynaContext::break_continue_check(a, allowed)?;
                RynaContext::break_continue_check(b, allowed)?;

                Ok(())
            },

            RynaExpr::NaryOperation(_, _, _, a, args) => {
                RynaContext::break_continue_check(a, allowed)?;
                
                for i in args {
                    RynaContext::break_continue_check(i, allowed)?;
                }

                Ok(())
            },

            RynaExpr::FunctionCall(_, _, _, args) => {
                for i in args {
                    RynaContext::break_continue_check(i, allowed)?;
                }

                Ok(())
            },

            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => RynaContext::break_continue_check(e, allowed),

            RynaExpr::CompiledLambda(_, _, _, _, _, b) => {
                for i in b {
                    RynaContext::break_continue_check(i, false)?;
                }

                Ok(())
            },

            RynaExpr::PrefixOperationDefinition(_, _, _, tm, _, _, _, b) => {
                if tm.is_empty() {
                    for i in b {
                        RynaContext::break_continue_check(i, false)?;
                    }
                }

                Ok(())
            },

            RynaExpr::PostfixOperationDefinition(_, _, _, tm, _, _, _, b) => {
                if tm.is_empty() {
                    for i in b {
                        RynaContext::break_continue_check(i, false)?;    
                    }
                }
                
                Ok(())
            },

            RynaExpr::BinaryOperationDefinition(_, _, _, tm, (_, _), (_, _), _, b) => {
                if tm.is_empty() {
                    for i in b {
                        RynaContext::break_continue_check(i, false)?;    
                    }          
                }
                
                Ok(())
            },

            RynaExpr::NaryOperationDefinition(_, _, _, tm, (_, _), _, _, b) => {
                if tm.is_empty() {
                    for i in b {
                        RynaContext::break_continue_check(i, false)?;    
                    }
                }
                
                Ok(())
            },


            RynaExpr::FunctionDefinition(_, _, _, tm, _, _, b) => {
                if tm.is_empty() {
                    for i in b {
                        RynaContext::break_continue_check(i, false)?;    
                    }
                }
                
                Ok(())
            },

            e => unreachable!("{:?}", e)
        };
    }

    pub fn invalid_type_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::PrefixOperatorDefinition(..) |
            RynaExpr::PostfixOperatorDefinition(..) |
            RynaExpr::BinaryOperatorDefinition(..) |
            RynaExpr::NaryOperatorDefinition(..) |
            RynaExpr::InterfaceDefinition(..) |
            RynaExpr::Macro(..) |
            RynaExpr::Break(..) |
            RynaExpr::Continue(..) |
            RynaExpr::Variable(..) |
            RynaExpr::Literal(..) => Ok(()),

            RynaExpr::CompiledVariableAssignment(l, _, _, t, e) |
            RynaExpr::CompiledVariableDefinition(l, _, _, t, e) => {
                if t.has_self() {
                    return Err(RynaError::compiler_error(
                        format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                        l, vec!()
                    ));
                }

                self.invalid_type_check(e)
            }

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.invalid_type_check(a)?;
                self.invalid_type_check(b)
            }

            RynaExpr::Tuple(_, args) => args.iter().try_for_each(|i| self.invalid_type_check(i)),
            
            RynaExpr::If(_, i, ib, ei, eb) => {
                self.invalid_type_check(i)?;

                for i in ib {
                    self.invalid_type_check(i)?;
                }
                
                for (ei_h, ei_b) in ei {
                    self.invalid_type_check(ei_h)?;

                    for i in ei_b {
                        self.invalid_type_check(i)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for i in eb_inner {
                        self.invalid_type_check(i)?;
                    }    
                }

                Ok(())
            },

            RynaExpr::DoBlock(l, b, t) => {
                if t.has_self() {
                    return Err(RynaError::compiler_error(
                        format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                        l, vec!()
                    ));
                }

                for i in b {
                    self.invalid_type_check(i)?;
                }

                Ok(())
            }

            RynaExpr::CompiledFor(_, _, _, _, c, b) |
            RynaExpr::While(_, c, b) => {
                self.invalid_type_check(c)?;

                for i in b {
                    self.invalid_type_check(i)?;
                }

                Ok(())
            },

            RynaExpr::UnaryOperation(l, _, tm, e) => {
                self.invalid_type_check(e)?;

                for t in tm {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            }

            RynaExpr::BinaryOperation(l, _, tm, a, b) => {
                self.invalid_type_check(a)?;
                self.invalid_type_check(b)?;

                for t in tm {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            },

            RynaExpr::NaryOperation(l, _, tm, a, args) => {
                self.invalid_type_check(a)?;
                
                for i in args {
                    self.invalid_type_check(i)?;
                }

                for t in tm {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            },

            RynaExpr::FunctionCall(l, _, tm, args) => {
                for i in args {
                    self.invalid_type_check(i)?;
                }

                for t in tm {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            },

            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => self.invalid_type_check(e),

            RynaExpr::CompiledLambda(l, _, c, args, ret, b) => {
                for (_, i) in c {
                    self.invalid_type_check(i)?;
                }

                for i in b {
                    self.invalid_type_check(i)?;
                }

                for t in args.iter().map(|(_, t)| t).chain([ret]) {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            },

            RynaExpr::PrefixOperationDefinition(l, _, _, tm, _, a, ret, b) => {
                if tm.is_empty() {
                    for i in b {
                        self.invalid_type_check(i)?;
                    }
    
                    for t in [a, ret] {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }
                }

                Ok(())
            },

            RynaExpr::PostfixOperationDefinition(l, _, _, tm, _, a, ret, b) => {
                if tm.is_empty() {
                    for i in b {
                        self.invalid_type_check(i)?;    
                    }
    
                    for t in [a, ret] {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }
                }
                
                Ok(())
            },

            RynaExpr::BinaryOperationDefinition(l, _, _, tm, (_, a_1), (_, a_2), ret, b) => {
                if tm.is_empty() {
                    for i in b {
                        self.invalid_type_check(i)?;    
                    }
    
                    for t in [a_1, a_2, ret] {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }                
                }
                
                Ok(())
            },

            RynaExpr::NaryOperationDefinition(l, _, _, tm, (_, a), args, ret, b) => {
                if tm.is_empty() {
                    for i in b {
                        self.invalid_type_check(i)?;    
                    }
    
                    for t in args.iter().map(|(_, t)| t).chain([a, ret]) {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }
                }
                
                Ok(())
            },


            RynaExpr::FunctionDefinition(l, _, _, tm, args, ret, b) => {
                if tm.is_empty() {
                    for i in b {
                        self.invalid_type_check(i)?;    
                    }
    
                    for t in args.iter().map(|(_, t)| t).chain([ret]) {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }
                }
                
                Ok(())
            },

            RynaExpr::ClassDefinition(l, _, _, _, args, alias, _) => {
                if let Some(t) = alias {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                
                } else {
                    for t in args.iter().map(|(_, t)| t) {
                        if t.has_self() {
                            return Err(RynaError::compiler_error(
                                format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                                l, vec!()
                            ));    
                        }
                    }
                }

                Ok(())
            },

            RynaExpr::InterfaceImplementation(l, _, ret, _, args) => {
                for t in args.iter().chain([ret]) {
                    if t.has_self() {
                        return Err(RynaError::compiler_error(
                            format!("{} type found outside an interface", Type::SelfType.get_name(self)),
                            l, vec!()
                        ));    
                    }
                }

                Ok(())
            },

            e => unreachable!("{:?}", e)
        };
    }

    pub fn check_type_well_formed(&self, t: &Type, l: &Location) -> Result<(), RynaError> {
        return match t {
            Type::Empty |
            Type::SelfType |
            Type::Basic(_) |
            Type::Wildcard |
            Type::InferenceMarker => Ok(()),

            Type::Ref(i) |
            Type::MutRef(i) => self.check_type_well_formed(i, l),

            Type::Or(v) |
            Type::And(v) => v.iter().try_for_each(|i| self.check_type_well_formed(i, l)),

            Type::TemplateParamStr(n, _) => {
                Err(RynaError::compiler_error(
                    format!(
                        "Template {} is not defined", 
                        format!("'{}", n).green(),
                    ), 
                    l, 
                    vec!()
                ))
            }

            Type::TemplateParam(_, cs) => {
                for c in cs {
                    let interface = &self.interfaces[c.id];
                    let interface_args = interface.params.len();

                    if c.args.len() != interface_args {
                        return Err(
                            RynaError::compiler_error(
                                format!(
                                    "Interface {}{} expected {} arguments (got {})", 
                                    interface.name.cyan(), 
                                    if interface_args == 0 { 
                                        "".into() 
                                    } else { 
                                        format!("<{}>", interface.params.iter().map(|i| i.green().to_string()).collect::<Vec<_>>().join(", ")) 
                                    },
                                    interface_args, 
                                    c.args.len()
                                ), 
                                l, 
                                vec!()
                            )
                        );
                    }

                    for arg in &c.args {
                        self.check_type_well_formed(arg, l)?;
                    }
                }

                Ok(())
            },

            Type::Template(id, args) => {
                let t = &self.type_templates[*id];
                let num_params = t.params.len();

                if num_params != args.len() {
                    return Err(
                        RynaError::compiler_error(
                            format!(
                                "Type {}{} expected {} arguments (got {})", 
                                t.name.cyan(), 
                                if num_params == 0 { "".into() } else { format!("<{}>", t.params.iter().map(|i| i.green().to_string()).collect::<Vec<_>>().join(", ")) },
                                num_params, 
                                args.len()
                            ), 
                            l, 
                            vec!()
                        )
                    );
                }

                args.iter().try_for_each(|i| self.check_type_well_formed(i, l))
            },
            
            Type::Function(a, b) => {
                self.check_type_well_formed(a, l)?;
                self.check_type_well_formed(b, l)
            },
        }
    }

    pub fn type_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::Break(..) |
            RynaExpr::Continue(..) |
            RynaExpr::Literal(..) |
            RynaExpr::Variable(..) |
            RynaExpr::PrefixOperatorDefinition(..) |
            RynaExpr::PostfixOperatorDefinition(..) |
            RynaExpr::BinaryOperatorDefinition(..) |
            RynaExpr::NaryOperatorDefinition(..) => Ok(()),

            RynaExpr::DoBlock(_, args, _) |
            RynaExpr::Tuple(_, args) => {
                for arg in args {
                    self.type_check(arg)?;
                }

                Ok(())
            }

            RynaExpr::CompiledVariableDefinition(l, _, n, t, e) |
            RynaExpr::CompiledVariableAssignment(l, _, n, t, e) => {
                self.check_type_well_formed(t, l)?;
                self.type_check(e)?;

                let it = self.infer_type(e)?;

                if it.bindable_to(t, self) {
                    Ok(())

                } else{
                    Err(RynaError::compiler_error(format!(
                        "Unable to bind value of type {} to variable {}, which is of type {}",
                        it.get_name(self),
                        n.cyan(),
                        t.get_name(self)
                    ), l, vec!()))
                }
            },

            RynaExpr::AttributeAssignment(l, a, b, attr_idx) => {
                self.type_check(a)?;
                self.type_check(b)?;

                let lhs_attr = self.infer_type(a)?;

                let (attr_name, lhs) = if let Type::Basic(id) | Type::Template(id, _) = lhs_attr.deref_type() {
                    self.type_templates[*id].attributes[*attr_idx].clone()
                } else {
                    unreachable!()
                };

                if let Type::Ref(_) = lhs_attr {
                    return Err(RynaError::compiler_error(format!(
                        "Unable assign value to attribute {} because it is accessed from a constant reference",
                        attr_name.cyan()
                    ), l, vec!()));
                }

                if !matches!(lhs_attr, Type::MutRef(_)) {
                    return Err(RynaError::compiler_error(format!(
                        "Unable assign value to attribute {} because it is not accesed from a mutable reference",
                        attr_name.cyan()
                    ), l, vec!()));
                }

                let rhs = self.infer_type(b)?;

                if rhs.bindable_to(&lhs, self) {
                    Ok(())

                } else {
                    return Err(RynaError::compiler_error(format!(
                        "Unable to bind value of type {} to attribute {}, which is of type {}",
                        rhs.get_name(self),
                        attr_name.cyan(),
                        lhs.get_name(self)
                    ), l, vec!()));
                }
            },

            RynaExpr::FunctionCall(l, id, templates, args) => {
                for t in templates {
                    self.check_type_well_formed(t, l)?;
                }

                let mut arg_types = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    self.type_check(arg)?;
                    arg_types.push(self.infer_type(arg)?);
                }

                let (ov_id, _, _, _) = self.get_first_function_overload(*id, arg_types.clone(), Some(templates.clone()), false, l)?;

                //Invalid number of template arguments
                if self.functions[*id].overloads[ov_id].templates != templates.len() {
                    Err(RynaError::compiler_error(format!(
                        "Function overload for {}{}({}) expected {} type arguments (got {})",
                        self.functions[*id].name.green(),
                        if templates.is_empty() { "".into() } else { format!("<{}>", templates.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")) },
                        arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                        self.functions[*id].overloads[ov_id].templates, templates.len()
                    ), l, vec!()))
                
                } else {
                    // Update caches
                    self.cache.usages.functions.add_new(*id, arg_types.clone(), templates.clone());
                    self.cache.overloads.functions.insert((*id, arg_types.clone(), templates.clone()), ov_id);

                    Ok(())
                }
            },

            RynaExpr::UnaryOperation(l, id, templates, arg) => {
                for t in templates {
                    self.check_type_well_formed(t, l)?;
                }

                self.type_check(arg)?;
                let t = self.infer_type(arg)?;

                let (ov_id, _, _, _) = self.get_first_unary_op(*id, t.clone(), Some(templates.clone()), false, l)?;

                if let Operator::Unary{prefix, representation, operations, ..} = &self.unary_ops[*id] {
                    if operations[ov_id].templates != templates.len() {
                        if *prefix {
                            Err(RynaError::compiler_error(format!(
                                "Unary operator overload for {}({}) expected {} type arguments (got {})",
                                representation,
                                t.get_name(self),
                                operations[ov_id].templates, templates.len()
                            ), l, vec!()))

                        } else {
                            Err(RynaError::compiler_error(format!(
                                "Unary operator overload for ({}){} expected {} type arguments (got {})",
                                t.get_name(self),
                                representation,
                                operations[ov_id].templates, templates.len()
                            ), l, vec!()))
                        }

                    } else {
                        // Update caches
                        self.cache.usages.unary.add_new(*id, vec!(t.clone()), templates.clone());
                        self.cache.overloads.unary.insert((*id, vec!(t.clone()), templates.clone()), ov_id);

                        Ok(())                            
                    }
                
                } else {
                    unreachable!()
                }
            },

            RynaExpr::BinaryOperation(l, id, templates, arg1, arg2) => {
                for t in templates {
                    self.check_type_well_formed(t, l)?;
                }
                
                self.type_check(arg1)?;
                self.type_check(arg2)?;

                let t1 = self.infer_type(arg1)?;
                let t2 = self.infer_type(arg2)?;
                
                let (ov_id, _, _, _) = self.get_first_binary_op(*id, t1.clone(), t2.clone(), Some(templates.clone()), false, l)?;

                if let Operator::Binary{representation, operations, ..} = &self.binary_ops[*id] {
                    if operations[ov_id].templates != templates.len() {
                        Err(RynaError::compiler_error(format!(
                            "Binary operator overload for ({}){}({}) expected {} type arguments (got {})",
                            t1.get_name(self),
                            representation,
                            t2.get_name(self),
                            operations[ov_id].templates, templates.len()
                        ), l, vec!()))    

                    } else {
                        // Update caches
                        self.cache.usages.binary.add_new(*id, vec!(t1.clone(), t2.clone()), templates.clone());
                        self.cache.overloads.binary.insert((*id, vec!(t1.clone(), t2.clone()), templates.clone()), ov_id);

                        Ok(())
                    }

                } else {
                    unreachable!()
                }
            },

            RynaExpr::NaryOperation(l, id, templates, first, args) => {
                for t in templates {
                    self.check_type_well_formed(t, l)?;
                }
                
                self.type_check(first)?;
                let t = self.infer_type(first)?;

                let mut arg_types = Vec::with_capacity(args.len());

                for arg in args.iter() {
                    self.type_check(arg)?;
                    arg_types.push(self.infer_type(arg)?);
                }

                let (ov_id, _, _, _) = self.get_first_nary_op(*id, t.clone(), arg_types.clone(), Some(templates.clone()), false, l)?;

                if let Operator::Nary{open_rep, close_rep, operations, ..} = &self.nary_ops[*id] {
                    if operations[ov_id].templates != templates.len() {
                        Err(RynaError::compiler_error(format!(
                            "N-ary operator overload for {}{}{}{} expected {} type arguments (got {})",
                            t.get_name(self),
                            open_rep,
                            arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                            close_rep,
                            operations[ov_id].templates, templates.len()
                        ), l, vec!()))

                    } else {
                        let mut all_args = vec!(t.clone());
                        all_args.extend(arg_types);

                        // Update caches
                        self.cache.usages.nary.add_new(*id, all_args.clone(), templates.clone());
                        self.cache.overloads.nary.insert((*id, all_args, templates.clone()), ov_id);
                        
                        Ok(())    
                    }

                } else {
                    unreachable!()
                }
            },

            RynaExpr::If(l, ih, ib, ei, eb) => {
                self.type_check(ih)?;

                let t = self.infer_type(ih)?;

                if *t.deref_type() != BOOL {
                    return Err(RynaError::compiler_error(format!("If condition inferred to be of type {} (expected Bool, &Bool or @Bool)", t.get_name(self)), l, vec!()));
                }

                for line in ib {
                    self.type_check(line)?;
                }

                for (ei_h, ei_b) in ei {
                    self.type_check(ei_h)?;
                    let t = self.infer_type(ei_h)?;

                    if *t.deref_type() != BOOL {
                        return Err(RynaError::compiler_error(format!("If condition inferred to be of type {} (expected Bool, &Bool or @Bool)", t.get_name(self)), l, vec!()));
                    }

                    for line in ei_b {
                        self.type_check(line)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.type_check(line)?;
                    }
                }

                Ok(())
            },

            RynaExpr::CompiledFor(_, _, _, _, iter, body) => {
                self.type_check(iter)?;

                for line in body {
                    self.type_check(line)?;
                }

                Ok(())
            }

            RynaExpr::While(l, cond, body) => {
                self.type_check(cond)?;
                let t = self.infer_type(cond)?;

                if *t.deref_type() != BOOL {
                    return Err(RynaError::compiler_error(format!("While condition inferred to be of type {} (expected Bool, &Bool or @Bool)", t.get_name(self)), l, vec!()));
                }

                for line in body {
                    self.type_check(line)?;
                }

                Ok(())
            },

            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => {
                self.type_check(e)?;
                self.infer_type(e)?;
                
                Ok(())
            }

            RynaExpr::CompiledLambda(l, _, c, args, _, b) => {
                for (_, i) in c {
                    self.type_check(i)?;
                }     

                for (_, t) in args {
                    self.check_type_well_formed(t, l)?;
                }
                
                for line in b {
                    self.type_check(line)?;
                }                

                Ok(())
            }

            RynaExpr::PrefixOperationDefinition(l, _, _, t, _, arg, r, b) |
            RynaExpr::PostfixOperationDefinition(l, _, _, t, _, arg, r, b) => {
                self.check_type_well_formed(arg, l)?;
                self.check_type_well_formed(r, l)?;

                if t.is_empty() {
                    for line in b {
                        self.type_check(line)?;
                    }
                
                } else {
                    let mut templates = HashSet::new();
                    arg.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (i, n) in t.iter().enumerate() {
                        if !templates.contains(&i) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::BinaryOperationDefinition(l, _, _, t, (_, ta), (_, tb), r, b) => {
                self.check_type_well_formed(ta, l)?;
                self.check_type_well_formed(tb, l)?;
                self.check_type_well_formed(r, l)?;

                if t.is_empty() {
                    for line in b {
                        self.type_check(line)?;
                    }
                
                } else {
                    let mut templates = HashSet::new();
                    ta.template_dependencies(&mut templates);
                    tb.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (i, n) in t.iter().enumerate() {
                        if !templates.contains(&i) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::NaryOperationDefinition(l, _, _, t, (_, ta), args, r, b) => {
                self.check_type_well_formed(ta, l)?;
                self.check_type_well_formed(r, l)?;

                for (_, t) in args {
                    self.check_type_well_formed(t, l)?;
                }
                
                if t.is_empty() {
                    for line in b {
                        self.type_check(line)?;
                    }
                
                } else {
                    let mut templates = HashSet::new();
                    ta.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (_, i) in args {
                        i.template_dependencies(&mut templates);
                    }

                    for (i, n) in t.iter().enumerate() {
                        if !templates.contains(&i) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }
                    }
                }

                Ok(())
            },

            RynaExpr::FunctionDefinition(l, _, _, t, args, r, b) => {
                self.check_type_well_formed(r, l)?;

                for (_, t) in args {
                    self.check_type_well_formed(t, l)?;
                }

                if t.is_empty() {
                    for line in b {
                        self.type_check(line)?;
                    }
                
                } else {
                    let mut templates = HashSet::new();
                    r.template_dependencies(&mut templates);

                    for (_, i) in args {
                        i.template_dependencies(&mut templates);
                    }

                    for (i, n) in t.iter().enumerate() {
                        if !templates.contains(&i) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }
                    }
                }

                Ok(())
            }

            RynaExpr::InterfaceDefinition(l, _, _, t, fns, uns, bin, nary) => {
                let mut templates = HashSet::new();

                for (_, _, f_t, args, r) in fns {
                    let mut templates_f = HashSet::new();

                    self.check_type_well_formed(r, l)?;
                    r.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates_f);

                    for (_, i) in args {
                        self.check_type_well_formed(i, l)?;
                        i.template_dependencies(&mut templates);
                        i.template_dependencies(&mut templates_f);
                    }

                    // Function templates
                    if let Some(inner) = f_t {
                        for (i, n) in inner.iter().enumerate() {
                            let offset_id = i + t.len();

                            if !templates.contains(&offset_id) {
                                return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                            }    
                        }
                    }
                }

                for (_, _, f_t, _, at, r) in uns {
                    self.check_type_well_formed(at, l)?;
                    self.check_type_well_formed(r, l)?;

                    at.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (i, n) in f_t.iter().enumerate() {
                        let offset_id = i + t.len();

                        if !templates.contains(&offset_id) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }    
                    }
                }

                for (_, _, f_t, (_, a0t), (_, a1t), r) in bin {
                    self.check_type_well_formed(a0t, l)?;
                    self.check_type_well_formed(a1t, l)?;
                    self.check_type_well_formed(r, l)?;

                    a0t.template_dependencies(&mut templates);
                    a1t.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (i, n) in f_t.iter().enumerate() {
                        let offset_id = i + t.len();

                        if !templates.contains(&offset_id) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }    
                    }
                }

                for (_, _, f_t, (_, a0t), args, r) in nary {
                    self.check_type_well_formed(a0t, l)?;
                    self.check_type_well_formed(r, l)?;

                    a0t.template_dependencies(&mut templates);
                    r.template_dependencies(&mut templates);

                    for (_, i) in args {
                        self.check_type_well_formed(i, l)?;
                        i.template_dependencies(&mut templates);
                    }

                    for (i, n) in f_t.iter().enumerate() {
                        let offset_id = i + t.len();

                        if !templates.contains(&offset_id) {
                            return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                        }    
                    }
                }

                for (i, n) in t.iter().enumerate() {
                    if !templates.contains(&i) {
                        return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                    }
                }
                
                Ok(())
            }

            RynaExpr::InterfaceImplementation(l, t, tp, _, args) => {
                let mut templates = HashSet::new();
                self.check_type_well_formed(tp, l)?;
                tp.template_dependencies(&mut templates);

                for i in args {
                    self.check_type_well_formed(i, l)?;
                    i.template_dependencies(&mut templates);
                }

                for (i, n) in t.iter().enumerate() {
                    if !templates.contains(&i) {
                        return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                    }
                }

                Ok(())
            }

            RynaExpr::ClassDefinition(l, _, _, t, attrs, alias, _) => {
                let mut templates = HashSet::new();

                if let Some(a) = alias {
                    self.check_type_well_formed(a, l)?;
                    a.template_dependencies(&mut templates);    
                
                } else {
                    for (_, i) in attrs {
                        self.check_type_well_formed(i, l)?;
                        i.template_dependencies(&mut templates);
                    }
                }

                for (i, n) in t.iter().enumerate() {
                    if !templates.contains(&i) {
                        return Err(RynaError::compiler_error(format!("Template parameter {} is not used anywhere", n.green()), l, vec!()));
                    }
                }

                Ok(())
            }

            RynaExpr::Macro(..) => { Ok(()) },

            _ => unimplemented!("{:?}", expr)
        }
    }

    #[allow(clippy::never_loop)] // This seems like an bug in clippy
    pub fn implicit_syntax_check(&self, name: &String, templates: &[String], attributes: &[(String, Type)], syntaxes: &Vec<Pattern>) -> Result<(), String> {
        if !syntaxes.is_empty() && !templates.is_empty() {
            return Err("Implicit syntaxes are not allowed when classes have type parameters".to_string())
        }

        let atts = attributes.iter().map(|(n, _)| n.clone()).collect::<HashSet<_>>();

        for s in syntaxes {
            let args = s.get_markers();

            for diff in args.symmetric_difference(&atts) {
                if args.contains(diff) {
                    return Err(format!("Syntax argument with name \"{}\" is not an attribute of {}", diff, name));
                }

                return Err(format!("Attribute \"{}\" does not appear in syntax definition for {}", diff, name));
            }
        }

        Ok(())
    }

    pub fn class_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::ClassDefinition(l, _, n, _, attributes, _, _) => {
                for (att, _) in attributes {
                    if attributes.iter().filter(|(i, _)| i == att).count() > 1 {
                        return Err(RynaError::compiler_error(format!("Repeated attribute \"{}\" in class {}", att, n), l, vec!()));
                    }
                }
                
                Ok(())
            }

            _ => Ok(())
        }
    }

    pub fn macro_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::Macro(l, _, n, _, p, b) => {
                let pattern_args = p.get_markers();
                let macro_args = b.get_markers();
                
                for p in &pattern_args {
                    if !macro_args.contains(&(false, p.clone())) {
                        return Err(RynaError::compiler_error(
                            format!("Argument {} is not used inside {} syntax", p.green(), n.blue()),
                            l, vec!()
                        ));
                    }
                }
                
                for p in macro_args {
                    if !p.0 && !pattern_args.contains(&p.1) {
                        return Err(RynaError::compiler_error(
                            format!("Argument {} is referenced inside {} syntax, but is not present in its RDL pattern", p.1.green(), n.blue()),
                            l, vec!()
                        ));
                    }
                }

                Ok(())
            }

            _ => Ok(())
        }
    }

    pub fn interface_impl_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::InterfaceImplementation(l, _, t, n, ts) => {
                match self.get_interface_id(n.clone()) {
                    Ok(int_id) => {
                        let fns = &self.interfaces[int_id].fns;
                        let uns = &self.interfaces[int_id].uns;
                        let bin = &self.interfaces[int_id].bin;
                        let nary = &self.interfaces[int_id].nary;

                        let max_tms = fns.iter().map(|i| i.2.as_ref().map(|i| i.len()).unwrap_or(0)).max().unwrap_or(0) + 
                                      uns.iter().map(|i| i.2.len()).max().unwrap_or(0) + 
                                      bin.iter().map(|i| i.2.len()).max().unwrap_or(0) + 
                                      nary.iter().map(|i| i.2.len()).max().unwrap_or(0) + 
                                      self.interfaces[int_id].params.len();

                        let mut offset_t = t.clone();
                        let mut offset_ts = ts.clone();
                        offset_t.offset_templates(max_tms);
                        offset_ts.iter_mut().for_each(|i| i.offset_templates(max_tms));

                        let t_subs = (0..offset_ts.len()).zip(offset_ts.clone()).collect::<HashMap<_, _>>();
                        
                        for (_, f_n, _, args, ret) in fns {
                            match self.get_function_id(f_n.clone()) {
                                Ok(fn_id) => {
                                    let ret_sub = ret.sub_self(&offset_t).sub_templates(&t_subs);
                                    let args_sub = args.iter().map(|(_, tp)| tp.sub_self(&offset_t).sub_templates(&t_subs)).collect::<Vec<_>>();

                                    match self.is_function_overload_ambiguous(fn_id, args_sub.clone()) {
                                        None => {
                                            if let Ok((_, r, _, _)) = self.get_first_function_overload(fn_id, args_sub.clone(), None, true, l) {
                                                if !r.bindable_to(&ret_sub, self) {
                                                    return Err(RynaError::compiler_error(
                                                        format!(
                                                            "Function overload for {}({}) needed by interface {} returns {}, which is not bindable to the required {}", 
                                                            f_n, args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                            n.green(), r.get_name(self), ret_sub.get_name(self)
                                                        ), 
                                                        l, vec!()
                                                    ));    
                                                }

                                            } else {
                                                return Err(RynaError::compiler_error(
                                                    format!(
                                                        "Unable to find the function overload for {}({}) needed by interface {}", 
                                                        f_n, args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                        n.green()
                                                    ), 
                                                    l, vec!()
                                                ));    
                                            }
                                        },
                                        
                                        Some(ov) => {
                                            // Do not check templated types. Check later on calls
                                            if t.has_templates() || ts.iter().any(|i| i.has_templates()) {
                                                return Ok(());
                                            }
                                            
                                            let possibilities = ov.iter().map(|(a, r)| format!("{}{} -> {}", self.functions[fn_id].name, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();

                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Function call {}({}) is ambiguous", 
                                                    f_n, args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")
                                                ), 
                                                l, possibilities
                                            ));
                                        },
                                    }
                                }

                                Err(err) => {
                                    return Err(RynaError::compiler_error(err, l, vec!()));
                                }
                            }
                        }

                        for (_, op_id, _, _, at, ret) in uns {
                            let ret_sub = ret.sub_self(&offset_t).sub_templates(&t_subs);
                            let arg_sub = at.sub_self(&offset_t).sub_templates(&t_subs);

                            match self.is_unary_op_ambiguous(*op_id, arg_sub.clone()) {
                                None => {
                                    if let Ok((_, r, _, _)) = self.get_first_unary_op(*op_id, arg_sub.clone(), None, true, l) {
                                        if !r.bindable_to(&ret_sub, self) {
                                            if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*op_id] {
                                                if *prefix {
                                                    return Err(RynaError::compiler_error(
                                                        format!(
                                                            "Unary operation overload for {}({}) needed by interface {} returns {}, which is not bindable to the required {}", 
                                                            representation, arg_sub.get_name(self),
                                                            n.green(), r.get_name(self), ret_sub.get_name(self)
                                                        ), 
                                                        l, vec!()
                                                    ));     

                                                } else {
                                                    return Err(RynaError::compiler_error(
                                                        format!(
                                                            "Unary operation overload for ({}){} needed by interface {} returns {}, which is not bindable to the required {}", 
                                                            arg_sub.get_name(self), representation,
                                                            n.green(), r.get_name(self), ret_sub.get_name(self)
                                                        ), 
                                                        l, vec!()
                                                    ));      
                                                }
                                            }        
                                        }
                                    
                                    } else if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*op_id] {
                                        if *prefix {
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unable to find the unary operation overload overload for {}({}) needed by interface {}", 
                                                    representation, arg_sub.get_name(self),
                                                    n.green()
                                                ), 
                                                l, vec!()
                                            ));   

                                        } else {
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unable to find the unary operation overload overload for ({}){} needed by interface {}", 
                                                    arg_sub.get_name(self), representation,
                                                    n.green()
                                                ), 
                                                l, vec!()
                                            ));   
                                        }                                     
                                    }
                                },
                                
                                Some(ov) => {
                                    // Do not check templated types. Check later on calls
                                    if t.has_templates() || ts.iter().any(|i| i.has_templates()) {
                                        return Ok(());
                                    }
                                    
                                    if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*op_id] {
                                        if *prefix {
                                            let possibilities = ov.iter().map(|(a, r)| format!("{}({}) -> {}", representation, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();
                                            
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unary operation {}({}) is ambiguous",
                                                    representation,
                                                    t.get_name(self)
                                                ), l, 
                                                possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                                            ));
                
                                        } else {
                                            let possibilities = ov.iter().map(|(a, r)| format!("({}){} -> {}", a.get_name(self), representation, r.get_name(self))).collect::<Vec<_>>();
                        
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unary operation ({}){} is ambiguous",
                                                    t.get_name(self),
                                                    representation
                                                ), l, 
                                                possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                                            ));
                                        }
                                        
                                    } else {
                                        unreachable!();
                                    }
                                },
                            }
                        }

                        for (_, op_id, _, (_, a0t), (_, a1t), ret) in bin {
                            let ret_sub = ret.sub_self(&offset_t).sub_templates(&t_subs);
                            let arg0_sub = a0t.sub_self(&offset_t).sub_templates(&t_subs);
                            let arg1_sub = a1t.sub_self(&offset_t).sub_templates(&t_subs);
                    
                            match self.is_binary_op_ambiguous(*op_id, arg0_sub.clone(), arg1_sub.clone()) {
                                None => {
                                    if let Operator::Binary{representation, ..} = &self.binary_ops[*op_id] {
                                        if let Ok((_, r, _, _)) = self.get_first_binary_op(*op_id, arg0_sub.clone(), arg1_sub.clone(), None, true, l) {
                                            if !r.bindable_to(&ret_sub, self) {
                                                return Err(RynaError::compiler_error(
                                                    format!(
                                                        "Binary operation overload for ({}){}({}) needed by interface {} returns {}, which is not bindable to the required {}", 
                                                        arg0_sub.get_name(self), representation, arg1_sub.get_name(self),
                                                        n.green(), r.get_name(self), ret_sub.get_name(self)
                                                    ), 
                                                    l, vec!()
                                                ));    
                                            }
    
                                        } else {
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unable to find the binary operation overload for ({}){}({}) needed by interface {}", 
                                                    arg0_sub.get_name(self), representation, arg1_sub.get_name(self),
                                                    n.green()
                                                ), 
                                                l, vec!()
                                            ));    
                                        }    
                                    }
                                },
                                
                                Some(ov) => {
                                    // Do not check templated types. Check later on calls
                                    if t.has_templates() || ts.iter().any(|i| i.has_templates()) {
                                        return Ok(());
                                    }
                                    
                                    if let Operator::Binary{representation, ..} = &self.binary_ops[*op_id] {
                                        let possibilities = ov.iter()
                                            .map(|(a1, a2, r)| format!("({}){}({}) -> {}", a1.get_name(self), representation, a2.get_name(self), r.get_name(self)))
                                            .collect::<Vec<_>>();                
                                        
                                        return Err(RynaError::compiler_error(
                                            format!(
                                                "Binary operation ({}){}({}) is ambiguous",
                                                arg0_sub.get_name(self),
                                                representation, 
                                                arg1_sub.get_name(self)
                                            ), l, 
                                            possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                                        ));
                                    }
                                }
                            }
                        }

                        for (_, op_id, _, (_, a0t), args, ret) in nary {
                            let ret_sub = ret.sub_self(&offset_t).sub_templates(&t_subs);
                            let arg0_sub = a0t.sub_self(&offset_t).sub_templates(&t_subs);
                            let args_sub = args.iter().map(|(_, tp)| tp.sub_self(&offset_t).sub_templates(&t_subs)).collect::<Vec<_>>();

                            match self.is_nary_op_ambiguous(*op_id, arg0_sub.clone(), args_sub.clone()) {
                                None => {
                                    if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*op_id] {
                                        if let Ok((_, r, _, _)) = self.get_first_nary_op(*op_id, arg0_sub.clone(), args_sub.clone(), None, true, l) {
                                            if !r.bindable_to(&ret_sub, self) {
                                                return Err(RynaError::compiler_error(
                                                    format!(
                                                        "N-ary operation overload for {}{}{}{} needed by interface {} returns {}, which is not bindable to the required {}", 
                                                        arg0_sub.get_name(self), 
                                                        open_rep,
                                                        args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                        close_rep,        
                                                        n.green(), r.get_name(self), ret_sub.get_name(self)
                                                    ), 
                                                    l, vec!()
                                                ));    
                                            }

                                        } else {
                                            return Err(RynaError::compiler_error(
                                                format!(
                                                    "Unable to find the n-ary operation overload for {}{}{}{} needed by interface {}", 
                                                    arg0_sub.get_name(self), 
                                                    open_rep,
                                                    args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                    close_rep,        
                                                    n.green()
                                                ), 
                                                l, vec!()
                                            ));    
                                        }
                                    }
                                },

                                Some(ov) => {
                                    if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*op_id] {
                                        let possibilities = ov.iter()
                                            .map(|(f, a, r)| 
                                                format!(
                                                    "{}{}{}{} -> {}", 
                                                    f.get_name(self), 
                                                    open_rep,
                                                    a.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                    close_rep,
                                                    r.get_name(self)
                                                )
                                            )
                                            .collect::<Vec<_>>();
                                
                                        return Err(RynaError::compiler_error(
                                            format!(
                                                "N-ary operation {}{}{}{} is ambiguous",
                                                arg0_sub.get_name(self), 
                                                open_rep,
                                                args_sub.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                                close_rep
                                            ), l, 
                                            possibilities.into_iter().map(|i| format!("Possible overload: {}", i)).collect()
                                        ));
                                    }
                                }
                            }
                        }

                        Ok(())
                    }

                    Err(err) => Err(RynaError::compiler_error(err, l, vec!()))
                }
            }

            _ => Ok(())
        };
    }

    pub fn no_template_check_type(&self, t: &Type, l: &Location) -> Result<(), RynaError> {
        if t.has_templates() {
            Err(RynaError::compiler_error("Template types are not allowed in this context".into(), l, vec!()))

        } else {
            Ok(())
        }
    }

    pub fn no_template_check_types(&self, t: &[Type], l: &Location) -> Result<(), RynaError> {
        if t.iter().any(Type::has_templates) {
            Err(RynaError::compiler_error("Template types are not allowed in this context".into(), l, vec!()))

        } else {
            Ok(())
        }
    }

    pub fn no_template_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::Literal(..) |
            RynaExpr::CompiledLambda(..) => Ok(()),

            RynaExpr::Variable(l, _, _, t) => self.no_template_check_type(t, l),

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.no_template_check(a)?;
                self.no_template_check(b)
            }

            RynaExpr::AttributeAccess(_, e, _) => {
                self.no_template_check(e)
            }

            RynaExpr::CompiledVariableAssignment(l, _, _, t, e) |
            RynaExpr::CompiledVariableDefinition(l, _, _, t, e) => {
                self.no_template_check_type(t, l)?;
                self.no_template_check(e)
            }

            RynaExpr::DoBlock(_, e, _) |
            RynaExpr::Tuple(_, e) => {
                for i in e {
                    self.no_template_check(i)?;
                }

                Ok(())
            }
            
            RynaExpr::UnaryOperation(l, _, tm, e) => {
                self.no_template_check_types(tm, l)?;
                self.no_template_check(e)
            }
            
            RynaExpr::BinaryOperation(l, _, tm, a, b) => {
                self.no_template_check_types(tm, l)?;
                self.no_template_check(a)?;
                self.no_template_check(b)
            }
            
            RynaExpr::NaryOperation(l, _, tm, a, b) => {
                self.no_template_check_types(tm, l)?;
                self.no_template_check(a)?;

                for i in b {
                    self.no_template_check(i)?;
                }

                Ok(())
            }
            
            RynaExpr::FunctionCall(l, _, tm, e) => {
                self.no_template_check_types(tm, l)?;

                for i in e {
                    self.no_template_check(i)?;
                }

                Ok(())
            }

            RynaExpr::CompiledFor(_, _, _, _, e, b) |
            RynaExpr::While(_, e, b) => {
                self.no_template_check(e)?;

                for i in b {
                    self.no_template_check(i)?;
                }

                Ok(())
            }

            RynaExpr::If(_, ih, ib, ei, eb) => {
                self.no_template_check(ih)?;

                for i in ib {
                    self.no_template_check(i)?;
                }

                for (ei_h, ei_b) in ei {
                    self.no_template_check(ei_h)?;

                    for i in ei_b {
                        self.no_template_check(i)?;
                    }   
                }

                if let Some(eb_inner) = eb {
                    for i in eb_inner {
                        self.no_template_check(i)?;
                    }
                }

                Ok(())
            }

            RynaExpr::Return(_, e) => self.no_template_check(e),
            
            _ => unimplemented!("{:?}", expr)
        }
    }

    pub fn lambda_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        if let RynaExpr::CompiledLambda(l, _, c, a, r, b) = expr {
            for (_, i) in c {
                self.no_template_check(i)?;
            }

            if r.has_templates() {
                return Err(RynaError::compiler_error("Parametric types are not allowed in lambda return types".into(), l, vec!()));
            }

            if a.iter().map(|(_, t)| t).any(Type::has_templates) {
                return Err(RynaError::compiler_error("Parametric types are not allowed in lambda parameters".into(), l, vec!()));
            }

            for line in b {
                self.no_template_check(line)?;
            }

            Ok(())
       
        } else {
            unreachable!()
        }
    }

    pub fn repeated_args(&self, args: &Vec<&String>, item: &str) -> Result<(), String> {
        let mut args_set = HashSet::new();

        for i in args {
            if args_set.contains(i) {
                return Err(format!("{} \"{}\" is defined multiple times", item, i));
            }

            args_set.insert(i);
        }

        Ok(())
    }

    pub fn repeated_arguments_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        return match expr {
            RynaExpr::PostfixOperationDefinition(l, _, _, t, n, _, _, _) |
            RynaExpr::PrefixOperationDefinition(l, _, _, t, n, _, _, _) => {
                let err = self.repeated_args(&vec!(n), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let err = self.repeated_args(&t.iter().collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                Ok(())
            }

            RynaExpr::BinaryOperationDefinition(l, _, _, t, (n1, _), (n2, _), _, _) => {
                let err = self.repeated_args(&vec!(n1, n2), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let err = self.repeated_args(&t.iter().collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                Ok(())
            }

            RynaExpr::NaryOperationDefinition(l, _, _, t, (n1, _), n, _, _) => {
                let mut args = vec!(n1);
                args.extend(n.iter().map(|(i, _)| i));

                let err = self.repeated_args(&args, "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let err = self.repeated_args(&t.iter().collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                Ok(())
            }

            RynaExpr::FunctionDefinition(l, _, _, t, a, _, _) => {
                let err = self.repeated_args(&a.iter().map(|(n, _)| n).collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let err = self.repeated_args(&t.iter().collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                Ok(())
            }

            RynaExpr::CompiledLambda(l, _, c, a, _, _) => {
                let err = self.repeated_args(&a.iter().map(|(n, _)| n).collect(), "Parameter");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let err = self.repeated_args(&c.iter().map(|(n, _)| n).collect(), "Capture");

                if let Err(msg) = err {
                    return Err(RynaError::compiler_error(msg, l, vec!()));
                }

                let cap_names = &c.iter().map(|(n, _)| n).collect::<FxHashSet<_>>();
                let arg_names = &a.iter().map(|(n, _)| n).collect::<FxHashSet<_>>();

                for n in cap_names {
                    if arg_names.contains(n) {
                        return Err(RynaError::compiler_error(format!("Capture \"{}\" is also defined as a parameter", n), l, vec!()));
                    }
                }

                Ok(())
            }

            _ => Ok(())
        };
    }

    pub fn check_test_annotation(&self, annot: &Annotation, t: &Vec<String>, args: &Vec<(String, Type)>, ret: &Type) -> Result<(), String> {
        annot.check_args(&[], &[])?;

        if t.len() > 0 {
            return Err(format!("Functions annotated with {} cannot be generic", "test".cyan()));
        }

        if args.len() > 0 {
            return Err(format!("Functions annotated with {} cannot take any parameters", "test".cyan()));
        }

        if ret.deref_type() != &BOOL {
            return Err(format!(
                "Functions annotated with {} must return {}, {} or {}", 
                "test".cyan(), BOOL.get_name(self), BOOL.to_ref().get_name(self), BOOL.to_mut().get_name(self)
            ));
        }
        
        Ok(())
    }

    pub fn check_fn_doc_annotation(&self, annot: &Annotation, args: &Vec<(String, Type)>) -> Result<(), String> {
        annot.check_args(
            &["0", "1"], 
            args.iter()
                .map(|(n, _)| n.as_str())
                .collect::<Vec<_>>().as_slice()
        )?;
        
        Ok(())
    }

    pub fn check_noret_doc_annotation(&self, annot: &Annotation, args: &Vec<(String, Type)>) -> Result<(), String> {
        annot.check_args(
            &["0"], 
            args.iter()
                .map(|(n, _)| n.as_str())
                .collect::<Vec<_>>().as_slice()
        )?;
        
        Ok(())
    }

    pub fn annotation_checks(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        match expr {
            RynaExpr::Macro(l, an, _, _, _, _) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => Err(format!("Macros cannot have the {} annotation", "test".cyan())),
                        "doc" => self.check_noret_doc_annotation(a, &vec!()),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            RynaExpr::ClassDefinition(l, an, _, _, atts, _, _) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => Err(format!("Classes cannot have the {} annotation", "test".cyan())),
                        "doc" => self.check_noret_doc_annotation(a, atts),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            RynaExpr::InterfaceDefinition(l, an, _, _, fns, unops, binops, naryops) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => Err(format!("Interfaces cannot have the {} annotation", "test".cyan())),
                        "doc" => self.check_noret_doc_annotation(a, &vec!()),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }

                for (inner_an, _, _, args, _) in fns {
                    for a in inner_an {
                        let res = match a.name.as_str() {
                            "test" => Err(format!("Interface function headers cannot have the {} annotation", "test".cyan())),
                            "doc" => self.check_fn_doc_annotation(a, args),
    
                            n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                        };
                        
                        res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                    }    
                }

                for (inner_an, _, _, n, t, _) in unops {
                    for a in inner_an {
                        let res = match a.name.as_str() {
                            "test" => Err(format!("Interface operation headers cannot have the {} annotation", "test".cyan())),
                            "doc" => self.check_fn_doc_annotation(a, &vec!((n.clone(), t.clone()))),
    
                            n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                        };
                        
                        res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                    }    
                }

                for (inner_an, _, _, arg_a, arg_b, _) in binops {
                    for a in inner_an {
                        let res = match a.name.as_str() {
                            "test" => Err(format!("Interface operation headers cannot have the {} annotation", "test".cyan())),
                            "doc" => self.check_fn_doc_annotation(a, &vec!(arg_a.clone(), arg_b.clone())),
    
                            n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                        };
                        
                        res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                    }    
                }

                for (inner_an, _, _, arg_a, arg_b, _) in naryops {
                    let mut all_args = vec!(arg_a.clone());
                    all_args.extend(arg_b.iter().cloned());
    
                    for a in inner_an {
                        let res = match a.name.as_str() {
                            "test" => Err(format!("Interface operation headers cannot have the {} annotation", "test".cyan())),
                            "doc" => self.check_fn_doc_annotation(a, &all_args),
    
                            n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                        };
                        
                        res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                    }    
                }
            }

            RynaExpr::FunctionDefinition(l, an, _, t, args, r, _) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => self.check_test_annotation(a, t, args, r),
                        "doc" => self.check_fn_doc_annotation(a, args),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            RynaExpr::PrefixOperationDefinition(l, an, _, t, arg_n, arg_t, r, _) |
            RynaExpr::PostfixOperationDefinition(l, an, _, t, arg_n, arg_t, r, _) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => self.check_test_annotation(a, t, &vec!((arg_n.clone(), arg_t.clone())), r),
                        "doc" => self.check_fn_doc_annotation(a, &vec!((arg_n.clone(), arg_t.clone()))),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            RynaExpr::BinaryOperationDefinition(l, an, _, t, arg_a, arg_b, r, _) => {
                for a in an {
                    let res = match a.name.as_str() {
                        "test" => self.check_test_annotation(a, t, &vec!(arg_a.clone(), arg_b.clone()), r),
                        "doc" => self.check_fn_doc_annotation(a, &vec!(arg_a.clone(), arg_b.clone())),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            RynaExpr::NaryOperationDefinition(l, an, _, t, arg_a, arg_b, r, _) => {
                let mut all_args = vec!(arg_a.clone());
                all_args.extend(arg_b.iter().cloned());

                for a in an {
                    let res = match a.name.as_str() {
                        "test" => self.check_test_annotation(a, t, &all_args, r),
                        "doc" => self.check_fn_doc_annotation(a, &all_args),

                        n => Err(format!("Annotation with name {} does not exist", n.cyan()))  
                    };
                    
                    res.map_err(|m| RynaError::compiler_error(m, l, vec!()))?;
                }
            }

            _ => { }
        }

        Ok(())
    }

    pub fn check_formats(&self, expr: &RynaExpr) {
        match expr {
            RynaExpr::ClassDefinition(l, _, n, ts, _, _, _) => {
                if let Err(warn) = check_class_name(n) {
                    located_ryna_warning!(l, "{}", warn);
                }

                for t in ts {
                    if let Err(warn) = check_template_name(t) {
                        located_ryna_warning!(l, "{}", warn);
                    }
                }
            }

            RynaExpr::FunctionDefinition(l, _, id, ts, _, _, _) => {
                if let Err(warn) = check_fn_name(&self.functions[*id].name) {
                    located_ryna_warning!(l, "{}", warn);
                }

                for t in ts {
                    if let Err(warn) = check_template_name(t) {
                        located_ryna_warning!(l, "{}", warn);
                    }
                }
            }

            RynaExpr::InterfaceDefinition(l, _, n, ts, fns, _, _, _) => {
                if let Err(warn) = check_interface_name(n) {
                    located_ryna_warning!(l, "{}", warn);
                }

                for t in ts {
                    if let Err(warn) = check_template_name(t) {
                        located_ryna_warning!(l, "{}", warn);
                    }
                }

                for f in fns {
                    if let Err(warn) = check_fn_name(&f.1) {
                        located_ryna_warning!(l, "{}", warn);
                    }

                    for t in f.2.as_ref().unwrap_or(&vec!()) {
                        if let Err(warn) = check_template_name(t) {
                            located_ryna_warning!(l, "{}", warn);
                        }
                    }
                }
            }

            _ => {}
        }
    }

    pub fn static_check_expected(&self, expr: &RynaExpr, expected: &Option<Type>) -> Result<(), RynaError> {
        self.repeated_arguments_check(expr)?;
        self.invalid_type_check(expr)?;
        self.type_check(expr)?;
        self.ambiguity_check(expr)?;
        self.return_check(expr, expected)?;
        RynaContext::break_continue_check(expr, false)?;
        self.class_check(expr)?;
        self.macro_check(expr)?;
        self.interface_impl_check(expr)?;
        self.annotation_checks(expr)?;
        self.check_formats(expr);

        Ok(())
    }

    pub fn static_check(&self, expr: &RynaExpr) -> Result<(), RynaError> {
        self.static_check_expected(expr, &None)
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::context::*;

    #[test]
    fn type_checks() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let n: Int = 10;

            let a: Int = 5 + n;
            let b: String = \"Test\";
            let c: Array<Int> = arr<Int>();

            a = 3;
            b = \"Test 2\";
            c = arr<Int>();
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: String = 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Int = 5;

            a = \"Test\";
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Array<Int> = 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Array<Int> = arr<Int>();

            a = arr<String>();
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn function_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn inc(a: String) -> @String {
                return a;
            }

            let a: Int = 5;

            a.inc();
            inc(\"Test\");
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: Bool | String) -> String {
                return \"Test\";
            }
            
            fn test(a: Bool | Int) -> String {
                return \"Test\";
            }

            test(true);
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn unary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op !(a: String) -> @String {
                return a;
            }

            !\"Test\";
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op !(a: Int | String) -> String {
                return \"Test\";
            }
            
            op !(a: Int | Array<*>) -> String {
                return \"Test\";
            }

            !5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn binary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String) + (b: Bool) -> @String {
                return a;
            }

            \"Test\" + true;
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String) + (b: Int | Bool) -> String {
                return \"Test\";
            }
            
            op (a: String) + (b: Int | Array<*>) -> String {
                return \"Test\";
            }

            \"Test\" + 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn nary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String)[b: Bool] -> @String {
                return a;
            }

            \"Test\"[true];
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String)[b: Bool | String] -> String {
                return \"Test\";
            }
            
            op (a: String)[b: Bool | Array<*>] -> String {
                return \"Test\";
            }

            \"Test\"[true];
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn return_type_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: String) -> @String {
                return a;
            }

            test(\"Test\");
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: String) -> Int {
                return a;
            }

            test(\"Test\");
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn ensured_return_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: String) -> @String {
                return a;
            }
            
            fn test(a: Int) -> Int {
                if true {
                    return 0;
                    
                } else {
                    return 1;
                }
            }
            
            fn test(a: Bool) -> Int {
                if true {
                    let a = 0;
                    
                } else {
                    return 1;
                }

                return 0;
            }
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: Bool) -> Int {
                if true {
                    let a = 0;
                    
                } else {
                    return 1;
                }
            }
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn class_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            class Test {
                att_1: Int;
                att_2: (String, Int);
                att_3: Int | Array<Int>;
            }
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            class Test {
                att_1: Int;
                att_1: (String, Int);
                att_3: Int | Array<Int>;
            }
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            class Test {
                syntax from Arg(1{d}, att_1) Arg(\"true\" | \"false\", att_2);

                att_1: Int;
                att_2: Bool;
            }
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        
        let mut ctx = standard_ctx();
        
        let code_str = "
            class Test {
                syntax from Arg(1{d}, att_1);

                att_1: Int;
                att_2: Bool;
            }
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }
}