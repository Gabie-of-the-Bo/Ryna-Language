use colored::Colorize;

use crate::macros::NessaMacro;
use crate::types::*;
use crate::operations::*;
use crate::object::*;
use crate::functions::*;
use crate::patterns::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Default)]
pub struct NessaContext {
    pub type_templates: Vec<TypeTemplate>, 

    pub unary_ops: Vec<Operator>,
    pub binary_ops: Vec<Operator>,
    pub nary_ops: Vec<Operator>,
    pub sorted_ops: Vec<Operator>,

    pub functions: Vec<Function>,

    pub macros: Vec<(String, Pattern, NessaMacro)>,

    pub variables: Vec<Option<Object>>,

    pub lambdas: usize
}

impl NessaContext {

    /*
        ╒════════════════════════════╕
        │ Type template manipulation │
        ╘════════════════════════════╛
    */

    pub fn define_type(&mut self, representation: String, params: Vec<String>, attributes: Vec<(String, Type)>, patterns: Vec<Pattern>, parser: Option<ParsingFunction>) -> Result<(), String> {
        for t in &self.type_templates {
            if t.name == representation {
                return Err(format!("Type \"{}\" is already defined", representation))
            }
        }

        self.type_templates.push(TypeTemplate {
            id: self.type_templates.len(),
            name: representation,
            params: params,
            attributes: attributes,
            patterns: patterns,
            parser: parser
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

        let op = Operator::Unary {
            id: self.unary_ops.len(),
            representation: representation,
            prefix: prefix,
            precedence: precedence,
            operations: vec!()
        };

        self.unary_ops.push(op.clone());

        match self.sorted_ops.binary_search_by(|i| i.get_precedence().cmp(&precedence)) {
            Ok(_) => return Err(format!("Precedence {} is already taken", precedence)),
            Err(pos) => self.sorted_ops.insert(pos, op)
        }

        return Ok(());
    }

    pub fn get_unary_operations(&self, id: usize, a: Type) -> Vec<&(usize, Type, Type, UnaryFunction)> {
        if let Operator::Unary{operations: o, ..} = &self.unary_ops[id] {
            return o.iter().filter(|(_, t, _, _)| a.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_unary_operation(&mut self, id: usize, templates: usize, a: Type, ret: Type, f: fn(&Vec<Type>, &Type, Object) -> Result<Object, String>) -> Result<(), String> {
        return self.define_unary_operation(id, templates, a, ret, Some(f));
    }

    pub fn define_unary_operation(&mut self, id: usize, templates: usize, a: Type, ret: Type, f: UnaryFunction) -> Result<(), String> {
        let op = &self.unary_ops[id];

        if let Operator::Unary{operations: o, representation: r, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if a.bindable_to(&t) {
                    return Err(format!("Unary operation {}{} is subsumed by {}{}, so it cannot be defined", 
                                        r.green(), a.get_name(self), r.green(), t.get_name(self)));
                }
    
                if t.bindable_to(&a) {
                    return Err(format!("Unary operation {}{} subsumes {}{}, so it cannot be defined", 
                                        r.green(), a.get_name(self), r.green(), t.get_name(self)));
                }
            }
        }

        if let Operator::Unary{operations: o, ..} = &mut self.unary_ops[id] {
            o.push((templates, a, ret, f));
        }

        return Ok(());
    }

    /*
        ╒══════════════════════════════╕
        │ Binary operator manipulation │
        ╘══════════════════════════════╛
    */

    pub fn define_binary_operator(&mut self, representation: String, right_associative: bool, precedence: usize) -> Result<(), String> {
        for o in &self.binary_ops {
            if let Operator::Binary{representation: r, ..} = o {
                if *r == representation {
                    return Err(format!("Binary operator \"{}\" is already defined", representation))
                }
            }
        }

        let op = Operator::Binary {
            id: self.binary_ops.len(),
            right_associative: right_associative,
            representation: representation,
            precedence: precedence,
            operations: vec!()
        };

        self.binary_ops.push(op.clone());

        match self.sorted_ops.binary_search_by(|i| i.get_precedence().cmp(&precedence)) {
            Ok(_) => return Err(format!("Precedence {} is already taken", precedence)),
            Err(pos) => self.sorted_ops.insert(pos, op)
        }

        return Ok(());
    }

    pub fn get_binary_operations(&self, id: usize, a: Type, b: Type) -> Vec<&(usize, Type, Type, BinaryFunction)> {
        let and = Type::And(vec!(a, b));

        if let Operator::Binary{operations: o, ..} = &self.binary_ops[id] {
            return o.iter().filter(|(_, t, _, _)| and.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_binary_operation(&mut self, id: usize, templates: usize, a: Type, b: Type, ret: Type, f: fn(&Vec<Type>, &Type, Object, Object) -> Result<Object, String>) -> Result<(), String> {
        return self.define_binary_operation(id, templates, a, b, ret, Some(f));
    }

    pub fn define_binary_operation(&mut self, id: usize, templates: usize, a: Type, b: Type, ret: Type, f: BinaryFunction) -> Result<(), String> {
        let and = Type::And(vec!(a.clone(), b.clone()));
        let op = &self.binary_ops[id];

        if let Operator::Binary{operations: o, representation: r, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t) {
                        return Err(format!("Binary operation {} {} {} is subsumed by {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r.green(), b.get_name(self), 
                                            v[0].get_name(self), r.green(), v[1].get_name(self)));
                    }

                    if t.bindable_to(&and) {
                        return Err(format!("Binary operation {} {} {} subsumes {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r.green(), b.get_name(self), 
                                            v[0].get_name(self), r.green(), v[1].get_name(self)));
                    }
                }
            }
        }

        if let Operator::Binary{operations: o, ..} = &mut self.binary_ops[id] {
            o.push((templates, and, ret, f));
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

        let op = Operator::Nary {
            id: self.nary_ops.len(),
            open_rep: open_rep,
            close_rep: close_rep,
            precedence: precedence,
            operations: vec!()
        };

        self.nary_ops.push(op.clone());

        match self.sorted_ops.binary_search_by(|i| i.get_precedence().cmp(&precedence)) {
            Ok(_) => return Err(format!("Precedence {} is already taken", precedence)),
            Err(pos) => self.sorted_ops.insert(pos, op)
        }

        return Ok(());
    }

    pub fn get_nary_operations(&self, id: usize, from: Type, args: &[Type]) -> Vec<&(usize, Type, Type, NaryFunction)> {
        let mut subtypes = vec!(from);
        subtypes.extend(args.iter().cloned());

        let and = Type::And(subtypes);

        if let Operator::Nary{operations: o, ..} = &self.nary_ops[id] {
            return o.iter().filter(|(_, t, _, _)| and.bindable_to(&t)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_nary_operation(&mut self, id: usize, templates: usize, from: Type, args: &[Type], ret: Type, f: fn((&mut Vec<Object>, &mut usize, &mut Vec<(i32, usize, i32)>, &mut i32), &Vec<Type>, &Type) -> Result<(), String>) -> Result<(), String> {
        return self.define_nary_operation(id, templates, from, args, ret, Some(f));
    }

    pub fn define_nary_operation(&mut self, id: usize, templates: usize, from: Type, args: &[Type], ret: Type, f: NaryFunction) -> Result<(), String> {
        let mut subtypes = vec!(from.clone());
        subtypes.extend(args.into_iter().cloned());

        let and = Type::And(subtypes);
        let op = &self.nary_ops[id];

        if let Operator::Nary{operations: o, open_rep: or, close_rep: cr, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t) {
                        return Err(format!("N-ary operation {}{}{}{} is subsumed by {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green(), 
                                            v[0].get_name(self), or.green(), v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green()));
                    }

                    if t.bindable_to(&and) {
                        return Err(format!("N-ary operation {}{}{}{} subsumes {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green(), 
                                            v[0].get_name(self), or.green(), v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green()));
                    }
                }
            }
        }

        if let Operator::Nary{operations: o, ..} = &mut self.nary_ops[id] {
            o.push((templates, and, ret, f));
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

    pub fn get_function_overloads(&self, id: usize, templates: &[Type], args: &[Type]) -> Vec<&(usize, Type, Type, FunctionOverload)> {
        let and = Type::And(args.to_vec());

        return self.functions[id].overloads.iter().filter(|(_, t, _, _)| and.bindable_to_template(&t, templates)).collect::<Vec<_>>();
    }

    pub fn define_native_function_overload(&mut self, id: usize, templates: usize, args: &[Type], ret: Type, f: fn(&Vec<Type>, &Type, Vec<Object>) -> Result<Object, String>) -> Result<(), String> {
        return self.define_function_overload(id, templates, args, ret, Some(f));
    }

    pub fn define_function_overload(&mut self, id: usize, templates: usize, args: &[Type], ret: Type, f: FunctionOverload) -> Result<(), String> {
        let and = Type::And(args.to_vec());
        let func = &self.functions[id];

        for (_, t, _, _) in &func.overloads{ // Check subsumption
            if let Type::And(v) = t {
                if and.bindable_to(&t) {
                    return Err(format!("Function overload {}({}) is subsumed by {}({}), so it cannot be defined", 
                                        func.name.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name.green(), v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }

                if t.bindable_to(&and) {
                    return Err(format!("Function overload {}({}) subsumes {}({}), so it cannot be defined", 
                                        func.name.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name.green(), v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }
            }
        }

        self.functions[id].overloads.push((templates, and, ret, f));

        return Ok(());
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

    #[test]
    fn operation_subsumption() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_native_unary_operation(0, 0, Type::Basic(1), Type::Basic(1), |_, _, a| { Ok(a.clone()) });
        let def_2 = ctx.define_native_unary_operation(0, 0, Type::Basic(0), Type::Basic(1), |_, _, a| { Ok(a.clone()) });
        let def_3 = ctx.define_native_unary_operation(0, 0, Type::Wildcard, Type::Wildcard, |_, _, a| { Ok(a.clone()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_native_binary_operation(0, 0, Type::Basic(0), Type::Basic(1), Type::Basic(1), |_, _, a, _| { Ok(a.clone()) });
        let def_2 = ctx.define_native_binary_operation(0, 0, Type::Basic(1), Type::Basic(1), Type::Basic(1), |_, _, a, _| { Ok(a.clone()) });
        let def_3 = ctx.define_native_binary_operation(0, 0, Type::Wildcard, Type::Wildcard, Type::Wildcard, |_, _, a, _| { Ok(a.clone()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_native_nary_operation(0, 0, Type::Basic(0), &[Type::Basic(0)], Type::Basic(0), |_, _, _| { Ok(()) });
        let def_2 = ctx.define_native_nary_operation(0, 0, Type::Basic(1), &[Type::Ref(Box::new(Type::Basic(1)))], Type::Basic(1), |_, _, _| { Ok(()) });
        let def_3 = ctx.define_native_nary_operation(0, 0, Type::Basic(1), &[Type::Basic(1)], Type::Basic(1), |_, _, _| { Ok(()) });
        let def_4 = ctx.define_native_nary_operation(0, 0, Type::Wildcard, &[Type::Wildcard], Type::Wildcard, |_, _, _| { Ok(()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_ok());
        assert!(def_3.is_ok());
        assert!(def_4.is_err());
    }

    #[test]
    fn function_subsumption() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_native_function_overload(0, 0, &[Type::Basic(1)], Type::Basic(0), |_, _, a| { Ok(a[0].clone()) });
        let def_2 = ctx.define_native_function_overload(0, 0, &[Type::MutRef(Box::new(Type::Basic(0)))], Type::Basic(0), |_, _, a| { Ok(a[0].clone()) });
        let def_3 = ctx.define_native_function_overload(0, 0, &[Type::Wildcard], Type::Basic(0), |_, _, a| { Ok(a[0].clone()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());
    }

    #[test]
    fn operator_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_unary_operator("~".into(), true, 0);
        let def_2 = ctx.define_unary_operator("-".into(), true, 1);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());

        let def_1 = ctx.define_binary_operator("$".into(), false, 2);
        let def_2 = ctx.define_binary_operator("+".into(), false, 3);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());

        let def_1 = ctx.define_nary_operator("`".into(), "´".into(), 4);
        let def_2 = ctx.define_nary_operator("(".into(), ")".into(), 5);
        let def_3 = ctx.define_nary_operator("{".into(), ")".into(), 6);
        let def_4 = ctx.define_nary_operator("(".into(), "}".into(), 7);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());
        assert!(def_4.is_err());
    }

    #[test]
    fn type_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_type("Matrix".into(), vec!(), vec!(), vec!(), None);
        let def_2 = ctx.define_type("Number".into(), vec!(), vec!(), vec!(), None);

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

    ctx.variables = vec!(None; 1000); // 1000 variables by default

    return ctx;
}