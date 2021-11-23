use crate::context::NessaContext;
use crate::parser::NessaExpr;
use crate::functions::*;
use crate::operations::*;
use crate::types::Type;

impl NessaContext {
    pub fn get_first_unary_op(&self, id: usize, arg_type: Type) -> Option<(usize, Type, bool)> {
        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            for (i, (a, r, f)) in operations.iter().enumerate() {
                if arg_type.bindable_to(&a) { // Take first that matches
                    return Some((i, r.clone(), f.is_some()));
                }
            }
        }

        return None;
    }

    pub fn is_unary_op_ambiguous(&self, id: usize, arg_type: Type) -> Option<Vec<(Type, Type)>> {
        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            let overloads = operations.iter()
                            .map(|(a, r, _)| (a.clone(), r.clone()))
                            .filter(|(a, _)| arg_type.bindable_to(a)).collect::<Vec<_>>();

            // Return Some(overloads) if the call is ambiguous, else return None
            if overloads.len() > 1 {
                return Some(overloads);

            } else {
                return None;
            }
        }

        unreachable!();
    }

    pub fn get_first_binary_op(&self, id: usize, a_type: Type, b_type: Type) -> Option<(usize, Type, bool)> {
        let t = Type::And(vec!(a_type, b_type));

        if let Operator::Binary{operations, ..} = &self.binary_ops[id] {
            for (i, (a, r, f)) in operations.iter().enumerate() {
                if t.bindable_to(&a) { // Take first that matches
                    return Some((i, r.clone(), f.is_some()));
                }
            }
        }

        return None;
    }

    pub fn is_binary_op_ambiguous(&self, id: usize, a_type: Type, b_type: Type) -> Option<Vec<(Type, Type, Type)>> {
        let t = Type::And(vec!(a_type, b_type));

        if let Operator::Binary{operations, ..} = &self.binary_ops[id] {
            let overloads = operations.iter()
                            .filter(|(a, _, _)| t.bindable_to(&a))
                            .map(|(a, r, _)| {
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

    pub fn get_first_nary_op(&self, id: usize, a_type: Type, b_type: Vec<Type>) -> Option<(usize, Type, bool)> {
        let mut arg_types = vec!(a_type.clone());
        arg_types.extend(b_type.iter().cloned());

        let t = Type::And(arg_types);

        if let Operator::Nary{operations, ..} = &self.nary_ops[id] {
            for (i, (a, r, f)) in operations.iter().enumerate() {
                if t.bindable_to(&a) { // Take first that matches
                    return Some((i, r.clone(), f.is_some()));
                }
            }
        }

        return None;
    }

    pub fn is_nary_op_ambiguous(&self, id: usize, a_type: Type, b_type: Vec<Type>) -> Option<Vec<(Type, Vec<Type>, Type)>> {
        let mut arg_types = vec!(a_type.clone());
        arg_types.extend(b_type.iter().cloned());

        let t = Type::And(arg_types);
        
        if let Operator::Nary{operations, ..} = &self.nary_ops[id] {
            let overloads = operations.iter()
                            .filter(|(a, _, _)| t.bindable_to(a))
                            .map(|(a, r, _)| {
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

    pub fn get_first_function_overload(&self, id: usize, arg_type: Vec<Type>, sub_t: bool) -> Option<(usize, Type, bool, Vec<Type>)> {
        let t = Type::And(arg_type);

        for (i, (t_len, a, r, f)) in self.functions[id].overloads.iter().enumerate() {
            if let (true, subs) = t.bindable_to_subtitutions(&a) { // Take first that matches
                let t_args = (0..*t_len).map(|i| subs.get(&i).cloned().unwrap_or(Type::TemplateParam(i))).collect();
                return Some((i, if sub_t { r.sub_templates(&subs) } else { r.clone() }, f.is_some(), t_args));
            }
        }

        return None;
    }

    pub fn is_function_overload_ambiguous(&self, id: usize, arg_type: Vec<Type>) -> Option<Vec<(Type, Type)>> {
        let t = Type::And(arg_type);

        let overloads = self.functions[id].overloads.iter()
                            .map(|(_, a, r, _)| (a.clone(), r.clone()))
                            .filter(|(a, _)| t.bindable_to(a)).collect::<Vec<_>>();

        // Return Some(overloads) if the call is ambiguous, else return None
        if overloads.len() > 1 {
            return Some(overloads);

        } else {
            return None;
        }
    }

    pub fn get_iterator_type(&self, container_type: &Type) -> Result<Type, String> {
        if let Some((_, it_type, _, _)) = self.get_first_function_overload(ITERATOR_FUNC_ID, vec!(container_type.clone()), true) {
            return Ok(it_type.clone());
        }

        return Err(format!("Unable to infer iterator type from container of type {}", container_type.get_name(self)));
    }

    pub fn get_iterator_output_type(&self, iterator_type: &Type) -> Result<Type, String> {
        let it_mut = Type::MutRef(Box::new(iterator_type.clone()));

        if let Some((_, r, _, _)) = self.get_first_function_overload(NEXT_FUNC_ID, vec!(it_mut.clone()), true) {
            return Ok(r);
        }

        return Err(format!("Unable to infer element type from iterator of type {}", iterator_type.get_name(self)));
    }

    pub fn infer_type(&self, expr: &NessaExpr) -> Option<Type> {
        return match expr {
            NessaExpr::Literal(obj) => Some(obj.get_type()),
            
            NessaExpr::Tuple(e) => {
                let mut args = vec!();

                for i in e {
                    args.push(self.infer_type(i)?);
                }

                Some(Type::And(args))
            },

            NessaExpr::Variable(_, _, t) => {
                match t {
                    Type::Ref(_) | Type::MutRef(_) => Some(t.clone()),
                    t => Some(Type::MutRef(Box::new(t.clone())))
                }
            },

            NessaExpr::UnaryOperation(id, a) => {
                let args_type = self.infer_type(a)?;

                let (_, r, _) = self.get_first_unary_op(*id, args_type).unwrap();

                return Some(r.clone());
            },

            NessaExpr::BinaryOperation(id, a, b) => {
                let a_type = self.infer_type(a)?;
                let b_type = self.infer_type(b)?;

                let (_, r, _) = self.get_first_binary_op(*id, a_type, b_type).unwrap();

                return Some(r.clone());
            },

            NessaExpr::NaryOperation(id, _, a, b) => {
                let a_type = self.infer_type(a)?;
                let b_type = b.iter().map(|i| self.infer_type(i).unwrap()).collect();

                let (_, r, _) = self.get_first_nary_op(*id, a_type, b_type).unwrap();

                return Some(r.clone());
            },

            NessaExpr::FunctionCall(id, t, args) => {
                let arg_types = args.iter().map(|i| self.infer_type(i).unwrap()).collect::<Vec<_>>();

                let (_, r, _, _) = self.get_first_function_overload(*id, arg_types, false).unwrap();

                return Some(r.sub_templates(&t.iter().cloned().enumerate().collect()));
            },

            _ => None
        };
    }
}