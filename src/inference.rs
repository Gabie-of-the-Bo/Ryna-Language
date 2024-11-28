use std::collections::HashMap;

use colored::Colorize;

use crate::compilation::RynaError;
use crate::context::RynaContext;
use crate::interfaces::InterfaceConstraint;
use crate::interfaces::ITERABLE_ID;
use crate::parser::Location;
use crate::parser::RynaExpr;
use crate::functions::*;
use crate::operations::*;
use crate::types::Type;

impl RynaContext {
    pub fn get_first_unary_op(&self, id: usize, arg_type: Type, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            'outer: for (i, op_ov) in operations.iter().enumerate() {
                if let (true, subs) = arg_type.bindable_to_subtitutions(&op_ov.args, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }
                    
                    let t_args = (0..op_ov.templates).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { op_ov.ret.sub_templates(&subs) } else { op_ov.ret.clone() }, op_ov.operation.is_some(), t_args));
                }
            }
        }

        if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[id] {
            if *prefix {
                Err(RynaError::compiler_error(format!(
                    "Unable to get unary operator overload for {}({})",
                    representation,
                    arg_type.get_name(self)
                ), l, vec!()))

            } else {
                Err(RynaError::compiler_error(format!(
                    "Unable to get unary operator overload for ({}){}",
                    arg_type.get_name(self),
                    representation
                ), l, vec!()))
            }

        } else {
            unreachable!()
        }
    }

    pub fn is_unary_op_ambiguous(&self, id: usize, arg_type: Type) -> Option<Vec<(Type, Type)>> {
        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            let overloads = operations.iter()
                            .map(|op_ov| (op_ov.args.clone(), op_ov.ret.clone()))
                            .filter(|(a, _)| arg_type.bindable_to(a, self)).collect::<Vec<_>>();

            // Return Some(overloads) if the call is ambiguous, else return None
            if overloads.len() > 1 {
                return Some(overloads);

            } else {
                return None;
            }
        }

        unreachable!();
    }

    pub fn get_first_binary_op(&self, id: usize, a_type: Type, b_type: Type, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        let t = Type::And(vec!(a_type.clone(), b_type.clone()));

        if let Operator::Binary{operations, ..} = &self.binary_ops[id] {
            'outer: for (i, op_ov) in operations.iter().enumerate() {
                if let (true, subs) = t.bindable_to_subtitutions(&op_ov.args, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }

                    let t_args = (0..op_ov.templates).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { op_ov.ret.sub_templates(&subs) } else { op_ov.ret.clone() }, op_ov.operation.is_some(), t_args));
                }
            }
        }

        if let Operator::Binary{representation, ..} = &self.binary_ops[id] {
            Err(RynaError::compiler_error(format!(
                "Unable to get binary operator overload for ({}){}({})",
                a_type.get_name(self),
                representation,
                b_type.get_name(self)
            ), l, vec!()))

        } else {
            unreachable!()
        }
    }

    pub fn is_binary_op_ambiguous(&self, id: usize, a_type: Type, b_type: Type) -> Option<Vec<(Type, Type, Type)>> {
        let t = Type::And(vec!(a_type, b_type));

        if let Operator::Binary{operations, ..} = &self.binary_ops[id] {
            let overloads = operations.iter()
                            .filter(|op_ov| t.bindable_to(&op_ov.args, self))
                            .map(|op_ov| {
                                if let Type::And(t) = &op_ov.args {
                                    (t[0].clone(), t[1].clone(), op_ov.ret.clone())

                                } else {
                                    unreachable!()
                                }
                            })
                            .collect::<Vec<_>>();

            // Return Some(overloads) if the call is ambiguous, else return None
            if overloads.len() > 1 {
                return Some(overloads);

            } else {
                return None;
            }
        }

        unreachable!();
    }

    pub fn get_first_nary_op(&self, id: usize, a_type: Type, b_type: Vec<Type>, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        let mut arg_types = vec!(a_type.clone());
        arg_types.extend(b_type.iter().cloned());

        let t = Type::And(arg_types.clone());

        if let Operator::Nary{operations, ..} = &self.nary_ops[id] {
            'outer: for (i, op_ov) in operations.iter().enumerate() {
                if let (true, subs) = t.bindable_to_subtitutions(&op_ov.args, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }

                    let t_args = (0..op_ov.templates).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { op_ov.ret.sub_templates(&subs) } else { op_ov.ret.clone() }, op_ov.operation.is_some(), t_args));
                }
            }
        }

        if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[id] {
            Err(RynaError::compiler_error(format!(
                "Unable to get n-ary operator overload for {}{}{}{}",
                a_type.get_name(self),
                open_rep,
                b_type.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                close_rep
            ), l, vec!()))

        } else {
            unreachable!()
        }
    }

    pub fn is_nary_op_ambiguous(&self, id: usize, a_type: Type, b_type: Vec<Type>) -> Option<Vec<(Type, Vec<Type>, Type)>> {
        let mut arg_types = vec!(a_type.clone());
        arg_types.extend(b_type.iter().cloned());

        let t = Type::And(arg_types);
        
        if let Operator::Nary{operations, ..} = &self.nary_ops[id] {
            let overloads = operations.iter()
                            .filter(|op_ov| t.bindable_to(&op_ov.args, self))
                            .map(|op_ov| {
                                if let Type::And(t) = &op_ov.args {
                                    (t[0].clone(), t[1..].to_vec(), op_ov.ret.clone())

                                } else {
                                    unreachable!()
                                }
                            })
                            .collect::<Vec<_>>();

            // Return Some(overloads) if the call is ambiguous, else return None
            if overloads.len() > 1 {
                return Some(overloads);

            } else {
                return None;
            }
        }

        unreachable!();
    }

    pub fn get_first_function_overload(&self, id: usize, arg_type: Vec<Type>, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        let t = Type::And(arg_type.clone());

        'outer: for (i, f_ov) in self.functions[id].overloads.iter().enumerate() {
            if let (true, subs) = t.bindable_to_subtitutions(&f_ov.args, self) { // Take first that matches
                if let Some(call_t) = &call_templates {
                    for (i, t) in call_t.iter().enumerate() {
                        if let Some(s_t) = subs.get(&i) {
                            if t != s_t {
                                break 'outer;
                            }   
                        }
                    }
                }
                
                let t_args = (0..f_ov.templates).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                return Ok((i, if sub_t { f_ov.ret.sub_templates(&subs) } else { f_ov.ret.clone() }, f_ov.function.is_some(), t_args));
            }
        }

        Err(RynaError::compiler_error(format!(
            "Unable to get function overload for {}{}({})",
            self.functions[id].name.green(),
            if call_templates.is_none() || call_templates.as_ref().unwrap().is_empty() { 
                "".into() 
            } else { 
                format!("<{}>", call_templates.unwrap().iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")) 
            },
            arg_type.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")
        ), l, vec!()))
    }

    pub fn is_function_overload_ambiguous(&self, id: usize, arg_type: Vec<Type>) -> Option<Vec<(Type, Type)>> {
        let t = Type::And(arg_type);

        let overloads = self.functions[id].overloads.iter()
                            .map(|f_ov| (f_ov.args.clone(), f_ov.ret.clone()))
                            .filter(|(a, _)| t.bindable_to(a, self)).collect::<Vec<_>>();

        // Return Some(overloads) if the call is ambiguous, else return None
        if overloads.len() > 1 {
            Some(overloads)

        } else {
            None
        }
    }

    pub fn implements_iterable(&self, container_type: &Type) -> bool {
        for i in &self.interface_impls {
            if i.interface_id == ITERABLE_ID && container_type.bindable_to(&i.interface_type, self) {
                return true;
            }
        }

        false
    }

    pub fn get_iterator_type(&self, container_type: &Type, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(container_type.clone()), None, true, l)
    }

    pub fn get_iterator_output_type(&self, iterator_type: &Type, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), RynaError> {
        let it_mut = Type::MutRef(Box::new(iterator_type.clone()));

        self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), None, true, l)
    }

    pub fn implements_destroyable(&self, t: &Type) -> bool {
        let dint_id = self.get_interface_id("Destroyable".into()).unwrap();

        self.implements_interface(t, &InterfaceConstraint::new(dint_id, vec!()), &mut HashMap::new(), &mut HashMap::new())
    }

    pub fn infer_type(&self, expr: &RynaExpr) -> Result<Type, RynaError> {
        return match expr {
            RynaExpr::Literal(_, obj) => Ok(obj.get_type()),

            RynaExpr::DoBlock(_, _, t) => Ok(t.clone()),

            RynaExpr::AttributeAccess(_, e, att_idx) => {
                use Type::*;

                let arg_type = self.infer_type(e)?;

                if let Basic(id) | Template(id, _) = arg_type.deref_type() {
                    let mut att_type = self.type_templates[*id].attributes[*att_idx].1.clone();

                    // Subtitute template parameters if needed
                    if let Template(_, ts) = arg_type.deref_type() {
                        att_type = att_type.sub_templates(&ts.iter().cloned().enumerate().collect());
                    }
                    
                    return match (&arg_type, &att_type) {
                        (MutRef(_), Ref(_) | MutRef(_)) => Ok(att_type.clone()),
                        (MutRef(_), _) => Ok(MutRef(Box::new(att_type.clone()))),

                        (Ref(_), MutRef(i)) => Ok(Ref(i.clone())),
                        (Ref(_), Ref(_)) => Ok(att_type.clone()),
                        (Ref(_), _) => Ok(Ref(Box::new(att_type.clone()))),

                        (_, _) => Ok(att_type.clone())
                    };

                } else {
                    unreachable!()
                }
            }

            RynaExpr::CompiledLambda(_, _, _, a, r, _) => Ok(
                if a.len() == 1 {
                    Type::Function(
                        Box::new(a[0].1.clone()),
                        Box::new(r.clone())
                    )

                } else {
                    Type::Function(
                        Box::new(Type::And(a.iter().map(|(_, t)| t).cloned().collect())),
                        Box::new(r.clone())
                    )
                }
            ),
            
            RynaExpr::Tuple(_, e) => {
                let mut args = vec!();

                for i in e {
                    args.push(self.infer_type(i)?);
                }

                Ok(Type::And(args))
            },

            RynaExpr::Variable(_, _, _, t, _) => {
                match t {
                    Type::Ref(_) | Type::MutRef(_) => Ok(t.clone()),
                    t => Ok(Type::MutRef(Box::new(t.clone())))
                }
            },

            RynaExpr::UnaryOperation(l, id, t, a) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let args_type = self.infer_type(a)?.sub_templates(&t_sub_call);

                let (_, r, _, subs) = self.get_first_unary_op(*id, args_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            RynaExpr::BinaryOperation(l, id, t, a, b) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let a_type = self.infer_type(a)?.sub_templates(&t_sub_call);
                let b_type = self.infer_type(b)?.sub_templates(&t_sub_call);

                let (_, r, _, subs) = self.get_first_binary_op(*id, a_type, b_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            RynaExpr::NaryOperation(l, id, t, a, b) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let a_type = self.infer_type(a)?.sub_templates(&t_sub_call);
                let b_type = b.iter().map(|i| self.infer_type(i))
                                     .collect::<Result<Vec<_>, RynaError>>()?
                                     .into_iter()
                                     .map(|i| i.sub_templates(&t_sub_call))
                                     .collect();

                let (_, r, _, subs) = self.get_first_nary_op(*id, a_type, b_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            RynaExpr::FunctionCall(l, id, t, args) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let arg_types = args.iter().map(|i| self.infer_type(i))
                                           .collect::<Result<Vec<_>, RynaError>>()?
                                           .into_iter()
                                           .map(|i| i.sub_templates(&t_sub_call))
                                           .collect();

                let (_, r, _, subs) = self.get_first_function_overload(*id, arg_types, None, true, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            }

            RynaExpr::QualifiedName(l, _, Some(id)) => {
                let func = &self.functions[*id];

                if func.overloads.len() == 1 {
                    let ov = &func.overloads[0];

                    if ov.templates != 0 {
                        return Err(RynaError::compiler_error(
                            format!(
                                "Implicit lambda for function with name {} cannot be formed from generic overload",
                                func.name.green()
                            ), 
                            l, vec!()
                        ));
                    }
                    
                    if let Type::And(a) = &ov.args {
                        if a.len() == 1 {
                            return Ok(Type::Function(
                                Box::new(a[0].clone()),
                                Box::new(ov.ret.clone())
                            ))
        
                        } else {
                            return Ok(Type::Function(
                                Box::new(Type::And(a.clone())),
                                Box::new(ov.ret.clone())
                            ))
                        }
                    }

                    return Ok(Type::Function(
                        Box::new(ov.args.clone()),
                        Box::new(ov.ret.clone())
                    ))
                }

                return Err(RynaError::compiler_error(
                    format!(
                        "Implicit lambda for function with name {} is ambiguous (found {} overloads)",
                        func.name.green(),
                        func.overloads.len()
                    ), 
                    l, vec!()
                ));
            }

            RynaExpr::QualifiedName(l, _, _) |
            RynaExpr::AttributeAssignment(l, _, _, _) |
            RynaExpr::CompiledVariableDefinition(l, _, _, _, _, _) |
            RynaExpr::CompiledVariableAssignment(l, _, _, _, _, _) |
            RynaExpr::CompiledFor(l, _, _, _, _, _) |
            RynaExpr::Macro(l, _, _, _, _, _) |
            RynaExpr::Lambda(l, _, _, _, _) |
            RynaExpr::NameReference(l, _) |
            RynaExpr::VariableDefinition(l, _, _, _) |
            RynaExpr::VariableAssignment(l, _, _) |
            RynaExpr::FunctionDefinition(l, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(l, _, _) |
            RynaExpr::PostfixOperatorDefinition(l, _, _) |
            RynaExpr::BinaryOperatorDefinition(l, _, _, _) |
            RynaExpr::NaryOperatorDefinition(l, _, _, _) |
            RynaExpr::ClassDefinition(l, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(l, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(l, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(l, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(l, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(l, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(l, _, _, _, _, _, _, _) |
            RynaExpr::If(l, _, _, _, _) |
            RynaExpr::Break(l) |
            RynaExpr::Continue(l) |
            RynaExpr::While(l, _, _) |
            RynaExpr::For(l, _, _, _) |
            RynaExpr::Return(l, _) => Err(RynaError::compiler_error(
                "Expression cannot be evaluated to a type".into(), 
                l, vec!()
            ))
        };
    }
}