use std::collections::HashMap;
use std::collections::HashSet;

use colored::Colorize;

use crate::cache::NessaCache;
use crate::interfaces::Interface;
use crate::interfaces::InterfaceConstraint;
use crate::interfaces::InterfaceImpl;
use crate::interfaces::standard_interfaces;
use crate::macros::NessaMacro;
use crate::translation::load_optimized_opcodes;
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

#[derive(Default, Clone)]
pub struct NessaContext {
    pub type_templates: Vec<TypeTemplate>, 
    pub interfaces: Vec<Interface>,
    pub interface_impls: Vec<InterfaceImpl>,

    pub unary_ops: Vec<Operator>,
    pub binary_ops: Vec<Operator>,
    pub nary_ops: Vec<Operator>,
    pub sorted_ops: Vec<Operator>,

    pub functions: Vec<Function>,

    pub macros: Vec<(String, Pattern, NessaMacro)>,

    pub variables: Vec<Option<Object>>,

    pub lambdas: usize,

    pub cache: NessaCache
}

impl NessaContext {

    /*
        ╒════════════════════════════╕
        │ Type template manipulation │
        ╘════════════════════════════╛
    */

    pub fn redefine_type(&mut self, representation: String, params: Vec<String>, attributes: Vec<(String, Type)>, alias: Option<Type>, patterns: Vec<Pattern>, parser: Option<ParsingFunction>) -> Result<(), String> {
        for t in self.type_templates.iter_mut() {
            if t.name == representation {
                *t = TypeTemplate {
                    id: t.id,
                    name: representation,
                    params: params,
                    attributes: attributes,
                    alias: alias,
                    patterns: patterns,
                    parser: parser
                };

                return Ok(());
            }
        }

        return Err(format!("Class {} was not defined", representation));
    }

    pub fn define_type(&mut self, representation: String, params: Vec<String>, attributes: Vec<(String, Type)>, alias: Option<Type>, patterns: Vec<Pattern>, parser: Option<ParsingFunction>) -> Result<(), String> {
        for t in &self.type_templates {
            if t.name == representation {
                return Err(format!("Type \"{}\" is already defined", representation))
            }
        }

        self.cache.class_id.insert(representation.clone(), Ok(self.type_templates.len()));

        self.type_templates.push(TypeTemplate {
            id: self.type_templates.len(),
            name: representation,
            params: params,
            attributes: attributes,
            alias: alias,
            patterns: patterns,
            parser: parser
        });

        return Ok(());
    }

    pub fn redefine_interface(&mut self, representation: String, params: Vec<String>, fns: Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>) -> Result<(), String> {
        for i in self.interfaces.iter_mut() {
            if i.name == representation {
                *i = Interface {
                    id: i.id,
                    name: representation,
                    params: params,
                    fns: fns
                };

                return Ok(());
            }
        }

        return Err(format!("Interface {} was not defined", representation));
    }

    pub fn define_interface(&mut self, representation: String, params: Vec<String>, fns: Vec<(String, Option<Vec<String>>, Vec<(String, Type)>, Type)>) -> Result<(), String> {
        for i in &self.interfaces {
            if i.name == representation {
                return Err(format!("Interface \"{}\" is already defined", representation))
            }
        }

        self.cache.interface_id.insert(representation.clone(), Ok(self.interfaces.len()));

        self.interfaces.push(Interface {
            id: self.interfaces.len(),
            name: representation,
            params: params,
            fns: fns
        });

        return Ok(());
    }

    pub fn define_interface_impl(&mut self, representation: String, templates: Vec<String>, mut tp: Type, mut t_args: Vec<Type>) -> Result<(), String> {
        tp.compile_templates(&templates);
        t_args.iter_mut().for_each(|t| {
            t.compile_templates(&templates);
        });

        self.interface_impls.push(InterfaceImpl {
            interface_id: self.get_interface_id(representation)?,
            args: t_args,
            interface_type: tp
        });

        return Ok(());
    }

    /*
        ╒══════════════════╕
        │ Interface checks │
        ╘══════════════════╛
    */

    pub fn implements_interface(&self, t: &Type, constraint: &InterfaceConstraint, t_assignments: &mut HashMap<usize, Type>, t_deps: &mut HashMap<usize, HashSet<usize>>) -> bool {
        let cons_and = Type::And(constraint.args.clone());

        for i in &self.interface_impls {
            if i.interface_id == constraint.id {
                let mut t_assignments_cpy = t_assignments.clone();
                let mut t_deps_cpy = t_deps.clone();

                let args_match = Type::And(i.args.clone()).template_bindable_to(&cons_and, &mut t_assignments_cpy, &mut t_deps_cpy, self);
                let type_matches = i.interface_type.template_bindable_to(&t, &mut t_assignments_cpy, &mut t_deps_cpy, self);
    
                if args_match && type_matches {
                    *t_assignments = t_assignments_cpy;
                    *t_deps = t_deps_cpy;

                    return true;
                }
            }
        }

        return false;
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
            return o.iter().filter(|(_, t, _, _)| a.bindable_to(&t, self)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_unary_operation(&mut self, id: usize, templates: usize, a: Type, ret: Type, f: fn(&Vec<Type>, &Type, Object) -> Result<Object, String>) -> Result<usize, String> {
        return self.define_unary_operation(id, templates, a, ret, Some(f));
    }

    pub fn define_unary_operation(&mut self, id: usize, templates: usize, a: Type, ret: Type, f: UnaryFunction) -> Result<usize, String> {
        let op = &self.unary_ops[id];

        if let Operator::Unary{operations: o, representation: r, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if a.bindable_to(&t, self) {
                    return Err(format!("Unary operation {}{} is subsumed by {}{}, so it cannot be defined", 
                                        r.green(), a.get_name(self), r.green(), t.get_name(self)));
                }
    
                if t.bindable_to(&a, self) {
                    return Err(format!("Unary operation {}{} subsumes {}{}, so it cannot be defined", 
                                        r.green(), a.get_name(self), r.green(), t.get_name(self)));
                }
            }
        }

        if let Operator::Unary{operations: o, ..} = &mut self.unary_ops[id] {
            o.push((templates, a, ret, f));

            return Ok(o.len() - 1);
        
        } else {
            unreachable!()
        }

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
            return o.iter().filter(|(_, t, _, _)| and.bindable_to(&t, self)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_binary_operation(&mut self, id: usize, templates: usize, a: Type, b: Type, ret: Type, f: fn(&Vec<Type>, &Type, Object, Object) -> Result<Object, String>) -> Result<usize, String> {
        return self.define_binary_operation(id, templates, a, b, ret, Some(f));
    }

    pub fn define_binary_operation(&mut self, id: usize, templates: usize, a: Type, b: Type, ret: Type, f: BinaryFunction) -> Result<usize, String> {
        let and = Type::And(vec!(a.clone(), b.clone()));
        let op = &self.binary_ops[id];

        if let Operator::Binary{operations: o, representation: r, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t, self) {
                        return Err(format!("Binary operation {} {} {} is subsumed by {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r.green(), b.get_name(self), 
                                            v[0].get_name(self), r.green(), v[1].get_name(self)));
                    }

                    if t.bindable_to(&and, self) {
                        return Err(format!("Binary operation {} {} {} subsumes {} {} {}, so it cannot be defined", 
                                            a.get_name(self), r.green(), b.get_name(self), 
                                            v[0].get_name(self), r.green(), v[1].get_name(self)));
                    }
                }
            }
        }

        if let Operator::Binary{operations: o, ..} = &mut self.binary_ops[id] {
            o.push((templates, and, ret, f));

            return Ok(o.len() - 1);

        } else {
            unreachable!();
        }
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
            return o.iter().filter(|(_, t, _, _)| and.bindable_to(&t, self)).collect::<Vec<_>>();
        }

        return vec!();
    }

    pub fn define_native_nary_operation(&mut self, id: usize, templates: usize, from: Type, args: &[Type], ret: Type, f: fn((&mut Vec<Object>, &mut usize, &mut Vec<(i32, usize, i32)>, &mut i32), &Vec<Type>, &Type) -> Result<(), String>) -> Result<usize, String> {
        return self.define_nary_operation(id, templates, from, args, ret, Some(f));
    }

    pub fn define_nary_operation(&mut self, id: usize, templates: usize, from: Type, args: &[Type], ret: Type, f: NaryFunction) -> Result<usize, String> {
        let mut subtypes = vec!(from.clone());
        subtypes.extend(args.into_iter().cloned());

        let and = Type::And(subtypes);
        let op = &self.nary_ops[id];

        if let Operator::Nary{operations: o, open_rep: or, close_rep: cr, ..} = op {
            for (_, t, _, _) in o { // Check subsumption
                if let Type::And(v) = t {
                    if and.bindable_to(&t, self) {
                        return Err(format!("N-ary operation {}{}{}{} is subsumed by {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green(), 
                                            v[0].get_name(self), or.green(), v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green()));
                    }

                    if t.bindable_to(&and, self) {
                        return Err(format!("N-ary operation {}{}{}{} subsumes {}{}{}{}, so it cannot be defined", 
                                            from.get_name(self), or.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green(), 
                                            v[0].get_name(self), or.green(), v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), cr.green()));
                    }
                }
            }
        }

        if let Operator::Nary{operations: o, ..} = &mut self.nary_ops[id] {
            o.push((templates, and, ret, f));

            return Ok(o.len() - 1);

        } else {
            unreachable!()
        }
    }

    /*
        ╒═══════════════════════╕
        │ Function manipulation │
        ╘═══════════════════════╛
    */

    pub fn define_function(&mut self, name: String) -> Result<usize, String> {        
        for f in &self.functions {
            if f.name == name {
                return Err(format!("Function \"{}\" is already defined", name))
            }
        }

        self.cache.function_id.insert(name.clone(), Ok(self.functions.len()));
        
        self.functions.push(Function {
            id: self.functions.len(),
            name: name,
            overloads: vec!()
        });

        return Ok(self.functions.len() - 1);
    }

    pub fn get_function_overloads(&self, id: usize, templates: &[Type], args: &[Type]) -> Vec<&(usize, Type, Type, FunctionOverload)> {
        let and = Type::And(args.to_vec());

        return self.functions[id].overloads.iter().filter(|(_, t, _, _)| and.bindable_to_template(&t, templates, self)).collect::<Vec<_>>();
    }

    pub fn define_native_function_overload(&mut self, id: usize, templates: usize, args: &[Type], ret: Type, f: fn(&Vec<Type>, &Type, Vec<Object>, &NessaContext) -> Result<Object, String>) -> Result<usize, String> {
        return self.define_function_overload(id, templates, args, ret, Some(f));
    }

    pub fn define_function_overload(&mut self, id: usize, templates: usize, args: &[Type], ret: Type, f: FunctionOverload) -> Result<usize, String> {
        let and = Type::And(args.to_vec());
        let func = &self.functions[id];

        for (_, t, _, _) in &func.overloads{ // Check subsumption
            if let Type::And(v) = t {
                if and.bindable_to(&t, self) {
                    return Err(format!("Function overload {}({}) is subsumed by {}({}), so it cannot be defined", 
                                        func.name.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name.green(), v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }

                if t.bindable_to(&and, self) {
                    return Err(format!("Function overload {}({}) subsumes {}({}), so it cannot be defined", 
                                        func.name.green(), args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), 
                                        func.name.green(), v.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")));
                }
            }
        }

        self.functions[id].overloads.push((templates, and, ret, f));

        return Ok(self.functions[id].overloads.len() - 1);
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

        let def_1 = ctx.define_native_unary_operation(0, 0, STR, STR, |_, _, a| { Ok(a.clone()) });
        let def_2 = ctx.define_native_unary_operation(0, 0, INT, STR, |_, _, a| { Ok(a.clone()) });
        let def_3 = ctx.define_native_unary_operation(0, 0, Type::Wildcard, Type::Wildcard, |_, _, a| { Ok(a.clone()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_native_binary_operation(0, 0, INT, STR, STR, |_, _, a, _| { Ok(a.clone()) });
        let def_2 = ctx.define_native_binary_operation(0, 0, STR, STR, STR, |_, _, a, _| { Ok(a.clone()) });
        let def_3 = ctx.define_native_binary_operation(0, 0, Type::Wildcard, Type::Wildcard, Type::Wildcard, |_, _, a, _| { Ok(a.clone()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
        assert!(def_3.is_err());

        let def_1 = ctx.define_native_nary_operation(0, 0, INT, &[INT], INT, |_, _, _| { Ok(()) });
        let def_2 = ctx.define_native_nary_operation(0, 0, STR, &[Type::Ref(Box::new(STR))], STR, |_, _, _| { Ok(()) });
        let def_3 = ctx.define_native_nary_operation(0, 0, STR, &[STR], STR, |_, _, _| { Ok(()) });
        let def_4 = ctx.define_native_nary_operation(0, 0, Type::Wildcard, &[Type::Wildcard], Type::Wildcard, |_, _, _| { Ok(()) });

        assert!(def_1.is_ok());
        assert!(def_2.is_ok());
        assert!(def_3.is_ok());
        assert!(def_4.is_err());
    }

    #[test]
    fn function_subsumption() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_native_function_overload(0, 0, &[STR], INT, |_, _, a, _| { Ok(a[0].clone()) });
        let def_2 = ctx.define_native_function_overload(0, 0, &[Type::MutRef(Box::new(INT))], INT, |_, _, a, _| { Ok(a[0].clone()) });
        let def_3 = ctx.define_native_function_overload(0, 0, &[Type::Wildcard], INT, |_, _, a, _| { Ok(a[0].clone()) });

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

        let def_1 = ctx.define_type("Matrix".into(), vec!(), vec!(), None, vec!(), None);
        let def_2 = ctx.define_type("Int".into(), vec!(), vec!(), None, vec!(), None);

        assert!(def_1.is_ok());
        assert!(def_2.is_err());
    }

    #[test]
    fn function_redefinition() {
        let mut ctx = standard_ctx();

        let def_1 = ctx.define_function("example".into());
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

pub const NUM_STD_TYPES: usize = 6;
pub const NUM_STD_INT_IMPL: usize = 8;

pub fn standard_ctx() -> NessaContext {
    let mut ctx = NessaContext::default();

    standard_types(&mut ctx);
    standard_interfaces(&mut ctx);

    standard_unary_operations(&mut ctx);
    standard_binary_operations(&mut ctx);
    standard_nary_operations(&mut ctx);

    standard_functions(&mut ctx);

    load_optimized_opcodes(&mut ctx);

    ctx.variables = vec!(None; 1000); // 1000 variables by default

    return ctx;
}