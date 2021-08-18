use crate::types::*;
use crate::variables::*;
use crate::operations::*;
use crate::object::*;
use crate::functions::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Default, Clone)]
pub struct NessaContext {
    pub type_templates: Vec<TypeTemplate>, 

    pub unary_ops: Vec<Operator>,
    pub binary_ops: Vec<Operator>,
    pub nary_ops: Vec<Operator>,

    pub functions: Vec<Function>,

    pub variables: Vec<Option<Variable>>
}

impl NessaContext {

    /*
        ╒════════════════════════════╕
        │ Type template manipulation │
        ╘════════════════════════════╛
    */

    pub fn define_type(&mut self, representation: String, params: Vec<String>) -> Result<(), String> {
        for t in &self.type_templates {
            if t.name == representation {
                return Err(format!("Type \"{}\" is already defined", representation))
            }
        }

        self.type_templates.push(TypeTemplate {
            id: self.type_templates.len(),
            name: representation,
            params: params
        });

        return Ok(());
    }

    /*
        ╒═════════════════════════════╕
        │ Unary operator manipulation │
        ╘═════════════════════════════╛
    */

    pub fn define_unary_operator(&mut self, representation: String, prefix: bool, precedence: usize) -> Result<(), String> {
        for o in &self.unary_ops {
            if let Operator::Unary{representation: r, ..} = o {
                if *r == representation {
                    return Err(format!("Unary operator \"{}\" is already defined", representation))
                }
            }
        }

        self.unary_ops.push(Operator::Unary {
            id: self.unary_ops.len(),
            representation: representation,
            prefix: prefix,
            precedence: precedence,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_unary_operations(&self, id: usize, a: Type) -> Vec<&(Type, Type, UnaryFunction)> {
        if let Operator::Unary{operations: o, ..} = &self.unary_ops[id] {
            return o.iter().filter(|(t, _, _)| a.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_unary_operation(&mut self, id: usize, a: Type, ret: Type, f: UnaryFunction) -> Result<(), String> {
        let op = &self.unary_ops[id];

        if let Operator::Unary{operations: o, representation: r, ..} = op {
            for (t, _, _) in o { // Check subsumption
                if a.bindable_to(&t) {
                    return Err(format!("Unary operation {}{} is subsumed by {}{}, so it cannot be defined", 
                                        r, a.get_name(self), r, t.get_name(self)));
                }
    
                if t.bindable_to(&a) {
                    return Err(format!("Unary operation {}{} subsumes {}{}, so it cannot be defined", 
                                        r, a.get_name(self), r, t.get_name(self)));
                }
            }
        }

        if let Operator::Unary{operations: o, ..} = &mut self.unary_ops[id] {
            o.push((a, ret, f));
        }

        return Ok(());
    }

    /*
        ╒══════════════════════════════╕
        │ Binary operator manipulation │
        ╘══════════════════════════════╛
    */

    pub fn define_binary_operator(&mut self, representation: String, precedence: usize) -> Result<(), String> {
        for o in &self.binary_ops {
            if let Operator::Binary{representation: r, ..} = o {
                if *r == representation {
                    return Err(format!("Binary operator \"{}\" is already defined", representation))
                }
            }
        }

        self.binary_ops.push(Operator::Binary {
            id: self.binary_ops.len(),
            representation: representation,
            precedence: precedence,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_binary_operations(&self, id: usize, a: Type, b: Type) -> Vec<&(Type, Type, BinaryFunction)> {
        let and = Type::And(vec!(a, b));

        if let Operator::Binary{operations: o, ..} = &self.binary_ops[id] {
            return o.iter().filter(|(t, _, _)| and.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_binary_operation(&mut self, id: usize, a: Type, b: Type, ret: Type, f: BinaryFunction) -> Result<(), String> {
        let and = Type::And(vec!(a.clone(), b.clone()));
        let op = &self.binary_ops[id];

        if let Operator::Binary{operations: o, representation: r, ..} = op {
            for (t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t) {
                        return Err(format!("Binary operation {} {} {} is subsumed by {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r, b.get_name(self), 
                                            v[0].get_name(self), r, v[1].get_name(self)));
                    }

                    if t.bindable_to(&and) {
                        return Err(format!("Binary operation {} {} {} subsumes {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r, b.get_name(self), 
                                            v[0].get_name(self), r, v[1].get_name(self)));
                    }
                }
            }
        }

        if let Operator::Binary{operations: o, ..} = &mut self.binary_ops[id] {
            o.push((and, ret, f));
        }

        return Ok(());
    }

    /*
        ╒═════════════════════════════╕
        │ N-ary operator manipulation │
        ╘═════════════════════════════╛
    */

    pub fn define_nary_operator(&mut self, open_rep: String, close_rep: String, precedence: usize) -> Result<(), String> {
        for o in &self.nary_ops {
            if let Operator::Nary{open_rep: or, close_rep: cr, ..} = o {
                if *or == open_rep || *cr == close_rep {
                    return Err(format!("N-ary operator \"{}{}\" has a syntax overlap with \"{}{}\", so it cannot be defined", 
                                        open_rep, close_rep, or, cr))
                }
            }
        }

        self.nary_ops.push(Operator::Nary {
            id: self.nary_ops.len(),
            open_rep: open_rep,
            close_rep: close_rep,
            precedence: precedence,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_nary_operations(&self, id: usize, from: Type, args: &[Type]) -> Vec<&(Type, Type, NaryFunction)> {
        let mut subtypes = vec!(from);
        subtypes.extend(args.iter().cloned());

        let and = Type::And(subtypes);

        if let Operator::Nary{operations: o, ..} = &self.nary_ops[id] {
            return o.iter().filter(|(t, _, _)| and.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_nary_operation(&mut self, id: usize, from: Type, args: &[Type], ret: Type, f: NaryFunction) -> Result<(), String> {
        let mut subtypes = vec!(from.clone());
        subtypes.extend(args.into_iter().cloned());

        let and = Type::And(subtypes);
        let op = &self.nary_ops[id];

        if let Operator::Nary{operations: o, open_rep: or, close_rep: cr, ..} = op {
            for (t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t) {
                        return Err(format!("N-ary operation {}{}{}{} is subsumed by {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or, args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr, 
                                            v[0].get_name(self), or, v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr));
                    }

                    if t.bindable_to(&and) {
                        return Err(format!("N-ary operation {}{}{}{} subsumes {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or, args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr, 
                                            v[0].get_name(self), or, v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr));
                    }
                }
            }
        }

        if let Operator::Nary{operations: o, ..} = &mut self.nary_ops[id] {
            o.push((and, ret, f));
        }

        return Ok(());
    }

    /*
        ╒═══════════════════════╕
        │ Function manipulation │
        ╘═══════════════════════╛
    */

    pub fn define_function(&mut self, name: String) -> Result<(), String> {
        for f in &self.functions {
            if f.name == name {
                return Err(format!("Function \"{}\" is already defined", name))
            }
        }

        self.functions.push(Function {
            id: self.functions.len(),
            name: name,
            overloads: vec!()
        });

        return Ok(());
    }

    pub fn get_function_overloads(&self, id: usize, args: &[Type]) -> Vec<&(Type, Type, FunctionOverload)> {
        let and = Type::And(args.to_vec());

        return self.functions[id].overloads.iter().filter(|(t, _, _)| and.bindable_to(&t)).collect::<Vec<_>>();
    }

    pub fn define_function_overload(&mut self, id: usize, args: &[Type], ret: Type, f: FunctionOverload) -> Result<(), String> {
        let and = Type::And(args.to_vec());
        let func = &self.functions[id];

        for (t, _, _) in &func.overloads{ // Check subsumption
            if let Type::And(v) = t {
                if and.bindable_to(&t) {
                    return Err(format!("Function overload {}({}) is subsumed by {}({}), so it cannot be defined", 
                                        func.name, args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name, v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }

                if t.bindable_to(&and) {
                    return Err(format!("Function overload {}({}) subsumes {}({}), so it cannot be defined", 
                                        func.name, args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name, v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }
            }
        }

        self.functions[id].overloads.push((and, ret, f));

        return Ok(());
    }

    /*
        ╒═══════════════════════╕
        │ Variable manipulation │
        ╘═══════════════════════╛
    */

    pub fn define_variable(&mut self, idx: usize, name: String, var_type: Type) -> Result<(), String> {
        let variable = self.variables.get_mut(idx).unwrap();

        if variable.is_some() {
            return Err(format!("Redefinition of variable \"{}\"", name));
        }

        *variable = Some(Variable{
            id: idx,
            name: name,
            var_type: var_type,
            value: None
        });

        return Ok(());
    }

    pub fn assign_variable(&mut self, idx: usize, value: Object) -> Result<(), String> {
        let variable = self.variables.get_mut(idx).unwrap();

        if let Some(v) = variable {
            if value.get_type().bindable_to(&v.var_type) {
                v.value = Some(value);

                return Ok(());
            }

        } else {
            return Err(format!("Variable with index {} is not defined", idx));
        }

        let val_type = value.get_type();
        let var_type = self.variables[idx].as_ref().unwrap().value.as_ref().unwrap().get_type();

        return Err(format!("Unable to bind value of type {} to variable of type {}", val_type.get_name(self), var_type.get_name(self)));
    }

    pub fn delete_variable(&mut self, idx: usize) -> Result<(), String> {
        let variable = self.variables.get_mut(idx).unwrap();

        if variable.is_none() {
            return Err(format!("Variable with index {} is not defined", idx));
        }

        *variable = None;

        return Ok(());
    }

    pub fn get_variable(&self, idx: usize) -> Result<&Variable, String> {
        let variable = &self.variables[idx];

        if variable.is_none() {
            return Err(format!("Variable with index {} is not defined", idx));
        }

        return Ok(variable.as_ref().unwrap());
    }
}

/*
                                                  ╒═════════╕
    ============================================= │  TESTS  │ =============================================
                                                  ╘═════════╛
*/

#[cfg(test)]
mod tests {
    use crate::types::Type;
    use crate::context::*;
    use crate::number::*;

    #[test]
    fn operation_subsumption() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_unary_operation(0, Type::Basic(1), Type::Basic(1), |a| { a.clone() });
        let def_2 = ctx.define_unary_operation(0, Type::Basic(0), Type::Basic(1), |a| { a.clone() });
        let def_3 = ctx.define_unary_operation(0, Type::Wildcard, Type::Wildcard, |a| { a.clone() });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_binary_operation(0, Type::Basic(0), Type::Basic(1), Type::Basic(1), |a, _| { a.clone() });
        let def_2 = ctx.define_binary_operation(0, Type::Basic(1), Type::Basic(1), Type::Basic(1), |a, _| { a.clone() });
        let def_3 = ctx.define_binary_operation(0, Type::Wildcard, Type::Wildcard, Type::Wildcard, |a, _| { a.clone() });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_nary_operation(0, Type::Basic(0), &[Type::Basic(0)], Type::Basic(0), |a, _| { a.clone() });
        let def_2 = ctx.define_nary_operation(0, Type::Basic(1), &[Type::Ref(Box::new(Type::Basic(1)))], Type::Basic(1), |a, _| { a.clone() });
        let def_3 = ctx.define_nary_operation(0, Type::Basic(1), &[Type::Basic(1)], Type::Basic(1), |a, _| { a.clone() });
        let def_4 = ctx.define_nary_operation(0, Type::Wildcard, &[Type::Wildcard], Type::Wildcard, |a, _| { a.clone() });

        assert!(def_1.is_ok());
        assert!(def_2.is_ok());
        assert!(def_3.is_ok());
        assert!(def_4.is_err());
    }

    #[test]
    fn function_subsumption() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_function_overload(0, &[Type::Basic(1)], Type::Basic(0), |a| { a[0].clone() });
        let def_2 = ctx.define_function_overload(0, &[Type::Basic(0)], Type::Basic(0), |a| { a[0].clone() });
        let def_3 = ctx.define_function_overload(0, &[Type::Wildcard], Type::Basic(0), |a| { a[0].clone() });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());
    }

    #[test]
    fn operator_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_unary_operator("~".into(), true, 0);
        let def_2 = ctx.define_unary_operator("-".into(), true, 0);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());

        let def_1 = ctx.define_binary_operator("-".into(), 0);
        let def_2 = ctx.define_binary_operator("+".into(), 0);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());

        let def_1 = ctx.define_nary_operator("`".into(), "´".into(), 0);
        let def_2 = ctx.define_nary_operator("(".into(), ")".into(), 0);
        let def_3 = ctx.define_nary_operator("{".into(), ")".into(), 0);
        let def_4 = ctx.define_nary_operator("(".into(), "}".into(), 0);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());
        assert!(def_4.is_err());
    }

    #[test]
    fn type_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_type("Matrix".into(), vec!());
        let def_2 = ctx.define_type("Number".into(), vec!());

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
    }

    #[test]
    fn function_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_function("abs".into());
        let def_2 = ctx.define_function("inc".into());

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
    }

    #[test]
    fn variables() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_variable(0, "test".into(), Type::Basic(0));
        let def_2 = ctx.define_variable(0, "test2".into(), Type::Basic(0));

        assert!(def_1.is_ok());
        assert!(def_2.is_err());

        let assignment_1 = ctx.assign_variable(0, Object::new(Number::from(5)));
        let assignment_2 = ctx.assign_variable(0, Object::new("Test".to_string()));
        let assignment_3 = ctx.assign_variable(1, Object::new(Number::from(5)));

        assert!(assignment_1.is_ok());
        assert!(assignment_2.is_err());
        assert!(assignment_3.is_err());

        let get_1 = ctx.get_variable(0);
        let get_2 = ctx.get_variable(1);

        assert!(get_1.is_ok());
        assert!(get_2.is_err());

        assert_eq!(*get_1.unwrap().value.as_ref().unwrap().get::<Number>(), Number::from(5));

        let delete_1 = ctx.delete_variable(0);
        let delete_2 = ctx.delete_variable(0);
        let delete_3 = ctx.delete_variable(1); 

        assert!(delete_1.is_ok());
        assert!(delete_2.is_err());
        assert!(delete_3.is_err());

        let get_1 = ctx.get_variable(0);
        let get_2 = ctx.get_variable(1);

        assert!(get_1.is_err());
        assert!(get_2.is_err());
    }
}

/*
                                                  ╒════════════════╕
    ============================================= │  STANDARD CTX  │ =============================================
                                                  ╘════════════════╛
*/

pub fn standard_ctx() -> NessaContext {
    let mut ctx = NessaContext::default();

    standard_types(&mut ctx);

    standard_unary_operations(&mut ctx);
    standard_binary_operations(&mut ctx);
    standard_nary_operations(&mut ctx);

    standard_functions(&mut ctx);

    ctx.variables = vec!(None; 100); // 100 "registers" by default

    return ctx;
}