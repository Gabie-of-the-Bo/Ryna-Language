use crate::context::NessaContext;
use crate::parser::NessaExpr;
use crate::functions::*;
use crate::operations::*;
use crate::types::Type;

impl NessaContext {
    pub fn get_first_unary_op(&self, id: usize, arg_type: Type) -> Option<(usize, Type, bool)> {
        let t = Type::And(vec!(arg_type));

        let mut native = false;

        if let Operator::Unary{operations, ..} = &self.unary_ops[id] {
            for (i, (a, r, f)) in operations.iter().enumerate() {
                if t.bindable_to(&a) { // Take first that matches
                    return Some((i, r.clone(), f.is_some()));
                }
            }
        }

        return None;
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

    pub fn get_first_function_overload(&self, id: usize, arg_type: Vec<Type>) -> Option<(usize, Type, bool)> {
        let t = Type::And(arg_type);

        for (i, (a, r, f)) in self.functions[id].overloads.iter().enumerate() {
            if t.bindable_to(&a) { // Take first that matches
                return Some((i, r.clone(), f.is_some()));
            }
        }

        return None;
    }

    pub fn infer_type(&self, expr: &NessaExpr) -> Option<Type> {
        return match expr {
            NessaExpr::Literal(obj) => Some(obj.get_type()),
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

            NessaExpr::FunctionCall(id, _, args) => {
                let arg_types = args.iter().map(|i| self.infer_type(i).unwrap()).collect::<Vec<_>>();

                let (_, r, _) = self.get_first_function_overload(*id, arg_types).unwrap();

                return Some(r.clone());
            },

            _ => None
        };
    }
}