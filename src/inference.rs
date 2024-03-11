use colored::Colorize;

use crate::compilation::NessaError;
use crate::context::NessaContext;
use crate::interfaces::ITERABLE_ID;
use crate::parser::Location;
use crate::parser::NessaExpr;
use crate::functions::*;
use crate::operations::*;
use crate::types::Type;

impl NessaContext {
    pub fn get_first_unary_op(&self, id: usize, arg_type: Type, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            'outer: for (i, (t_len, a, r, f)) in operations.iter().enumerate() {
                if let (true, subs) = arg_type.bindable_to_subtitutions(a, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }
                    
                    let t_args = (0..*t_len).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { r.sub_templates(&subs) } else { r.clone() }, f.is_some(), t_args));
                }
            }
        }

        if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[id] {
            if *prefix {
                Err(NessaError::compiler_error(format!(
                    "Unable to get unary operator overload for {}({})",
                    representation,
                    arg_type.get_name(self)
                ), l, vec!()))

            } else {
                Err(NessaError::compiler_error(format!(
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
                            .map(|(_, a, r, _)| (a.clone(), r.clone()))
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

    pub fn get_first_binary_op(&self, id: usize, a_type: Type, b_type: Type, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        let t = Type::And(vec!(a_type.clone(), b_type.clone()));

        if let Operator::Binary{operations, ..} = &self.binary_ops[id] {
            'outer: for (i, (t_len, a, r, f)) in operations.iter().enumerate() {
                if let (true, subs) = t.bindable_to_subtitutions(a, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }

                    let t_args = (0..*t_len).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { r.sub_templates(&subs) } else { r.clone() }, f.is_some(), t_args));
                }
            }
        }

        if let Operator::Binary{representation, ..} = &self.binary_ops[id] {
            Err(NessaError::compiler_error(format!(
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
                            .filter(|(_, a, _, _)| t.bindable_to(a, self))
                            .map(|(_, a, r, _)| {
                                if let Type::And(t) = a {
                                    (t[0].clone(), t[1].clone(), r.clone())

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

    pub fn get_first_nary_op(&self, id: usize, a_type: Type, b_type: Vec<Type>, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        let mut arg_types = vec!(a_type.clone());
        arg_types.extend(b_type.iter().cloned());

        let t = Type::And(arg_types.clone());

        if let Operator::Nary{operations, ..} = &self.nary_ops[id] {
            'outer: for (i, (t_len, a, r, f)) in operations.iter().enumerate() {
                if let (true, subs) = t.bindable_to_subtitutions(a, self) { // Take first that matches
                    if let Some(call_t) = call_templates {
                        for (i, t) in call_t.iter().enumerate() {
                            if let Some(s_t) = subs.get(&i) {
                                if t != s_t {
                                    break 'outer;
                                }   
                            }
                        }
                    }

                    let t_args = (0..*t_len).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                    return Ok((i, if sub_t { r.sub_templates(&subs) } else { r.clone() }, f.is_some(), t_args));
                }
            }
        }

        if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[id] {
            Err(NessaError::compiler_error(format!(
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
                            .filter(|(_, a, _, _)| t.bindable_to(a, self))
                            .map(|(_, a, r, _)| {
                                if let Type::And(t) = a {
                                    (t[0].clone(), t[1..].to_vec(), r.clone())

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

    pub fn get_first_function_overload(&self, id: usize, arg_type: Vec<Type>, call_templates: Option<Vec<Type>>, sub_t: bool, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        let t = Type::And(arg_type.clone());

        'outer: for (i, (t_len, a, r, f)) in self.functions[id].overloads.iter().enumerate() {
            if let (true, subs) = t.bindable_to_subtitutions(a, self) { // Take first that matches
                if let Some(call_t) = &call_templates {
                    for (i, t) in call_t.iter().enumerate() {
                        if let Some(s_t) = subs.get(&i) {
                            if t != s_t {
                                break 'outer;
                            }   
                        }
                    }
                }
                
                let t_args = (0..*t_len).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i, vec!()))).collect();
                return Ok((i, if sub_t { r.sub_templates(&subs) } else { r.clone() }, f.is_some(), t_args));
            }
        }

        Err(NessaError::compiler_error(format!(
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
                            .map(|(_, a, r, _)| (a.clone(), r.clone()))
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

    pub fn get_iterator_type(&self, container_type: &Type, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(container_type.clone()), None, true, l)
    }

    pub fn get_iterator_output_type(&self, iterator_type: &Type, l: &Location) -> Result<(usize, Type, bool, Vec<Type>), NessaError> {
        let it_mut = Type::MutRef(Box::new(iterator_type.clone()));

        self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), None, true, l)
    }

    pub fn infer_type(&self, expr: &NessaExpr) -> Result<Type, NessaError> {
        return match expr {
            NessaExpr::Literal(_, obj) => Ok(obj.get_type()),

            NessaExpr::DoBlock(_, _, t) => Ok(t.clone()),

            NessaExpr::CompiledLambda(_, _, a, r, _) => Ok(
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
            
            NessaExpr::Tuple(_, e) => {
                let mut args = vec!();

                for i in e {
                    args.push(self.infer_type(i)?);
                }

                Ok(Type::And(args))
            },

            NessaExpr::Variable(_, _, _, t) => {
                match t {
                    Type::Ref(_) | Type::MutRef(_) => Ok(t.clone()),
                    t => Ok(Type::MutRef(Box::new(t.clone())))
                }
            },

            NessaExpr::UnaryOperation(l, id, t, a) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let args_type = self.infer_type(a)?.sub_templates(&t_sub_call);

                let (_, r, _, subs) = self.get_first_unary_op(*id, args_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            NessaExpr::BinaryOperation(l, id, t, a, b) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let a_type = self.infer_type(a)?.sub_templates(&t_sub_call);
                let b_type = self.infer_type(b)?.sub_templates(&t_sub_call);

                let (_, r, _, subs) = self.get_first_binary_op(*id, a_type, b_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            NessaExpr::NaryOperation(l, id, t, a, b) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let a_type = self.infer_type(a)?.sub_templates(&t_sub_call);
                let b_type = b.iter().map(|i| self.infer_type(i))
                                     .collect::<Result<Vec<_>, NessaError>>()?
                                     .into_iter()
                                     .map(|i| i.sub_templates(&t_sub_call))
                                     .collect();

                let (_, r, _, subs) = self.get_first_nary_op(*id, a_type, b_type, None, false, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            },

            NessaExpr::FunctionCall(l, id, t, args) => {
                let t_sub_call = t.iter().cloned().enumerate().collect();
                let arg_types = args.iter().map(|i| self.infer_type(i))
                                           .collect::<Result<Vec<_>, NessaError>>()?
                                           .into_iter()
                                           .map(|i| i.sub_templates(&t_sub_call))
                                           .collect();

                let (_, r, _, subs) = self.get_first_function_overload(*id, arg_types, None, true, l)?;

                let t_sub_ov = subs.iter().cloned().enumerate().collect();

                return Ok(r.sub_templates(&t_sub_ov).sub_templates(&t_sub_call));
            }

            NessaExpr::FunctionName(l, _) |
            NessaExpr::CompiledVariableDefinition(l, _, _, _, _) |
            NessaExpr::CompiledVariableAssignment(l, _, _, _, _) |
            NessaExpr::CompiledFor(l, _, _, _, _, _) |
            NessaExpr::Macro(l, _, _, _, _) |
            NessaExpr::Lambda(l, _, _, _) |
            NessaExpr::NameReference(l, _) |
            NessaExpr::VariableDefinition(l, _, _, _) |
            NessaExpr::VariableAssignment(l, _, _) |
            NessaExpr::FunctionDefinition(l, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(l, _, _) |
            NessaExpr::PostfixOperatorDefinition(l, _, _) |
            NessaExpr::BinaryOperatorDefinition(l, _, _, _) |
            NessaExpr::NaryOperatorDefinition(l, _, _, _) |
            NessaExpr::ClassDefinition(l, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(l, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(l, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(l, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(l, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(l, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(l, _, _, _, _, _, _) |
            NessaExpr::If(l, _, _, _, _) |
            NessaExpr::Break(l) |
            NessaExpr::Continue(l) |
            NessaExpr::While(l, _, _) |
            NessaExpr::For(l, _, _, _) |
            NessaExpr::Return(l, _) => Err(NessaError::compiler_error(
                "Expression cannot be evaluated to a type".into(), 
                l, vec!()
            ))
        };
    }
}