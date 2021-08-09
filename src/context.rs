use crate::types::*;
use crate::object::*;
use crate::operations::*;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

#[derive(Default, Clone)]
pub struct NessaContext {
    pub type_templates: Vec<TypeTemplate>, 

    pub unary_ops: Vec<UnaryOperator>,
    pub binary_ops: Vec<BinaryOperator>,
    pub nary_ops: Vec<NaryOperator>
}

impl NessaContext {

    /*
        ╒════════════════════════════╕
        │ Type template manipulation │
        ╘════════════════════════════╛
    */

    pub fn define_type(&mut self, representation: String, params: Vec<String>) -> Result<(), String> {
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

    pub fn define_unary_operator(&mut self, representation: String) -> Result<(), String> {
        self.unary_ops.push(UnaryOperator {
            id: self.unary_ops.len(),
            representation: representation,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_unary_operation(&self, id: usize, a: &Object) -> Option<&UnaryFunction> {
        let a_type = a.get_type();
        
        for (t, op) in &self.unary_ops[id].operations{
            if a_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_unary_operation(&mut self, id: usize, a: Type, f: UnaryFunction) -> Result<(), String> {
        let op = &self.unary_ops[id];

        for (t, _) in &op.operations{ // Check subsumption
            if a.bindable_to(&t) {
                return Err(format!("Unary operation {}{} is subsumed by {}{}, so it cannot be defined", 
                                    op.representation, a.get_name(self), op.representation, t.get_name(self)));
            }
        }

        self.unary_ops[id].operations.push((a, f));

        return Ok(());
    }

    /*
        ╒══════════════════════════════╕
        │ Binary operator manipulation │
        ╘══════════════════════════════╛
    */

    pub fn define_binary_operator(&mut self, representation: String) -> Result<(), String> {
        self.binary_ops.push(BinaryOperator {
            id: self.binary_ops.len(),
            representation: representation,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_binary_operation(&self, id: usize, a: &Object, b: &Object) -> Option<&BinaryFunction> {
        let args_type = Type::And(vec!(a.get_type(), b.get_type()));

        for (t, op) in &self.binary_ops[id].operations{
            if args_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_binary_operation(&mut self, id: usize, a: Type, b: Type, f: BinaryFunction) -> Result<(), String> {
        let and = Type::And(vec!(a.clone(), b.clone()));
        let op = &self.binary_ops[id];

        for (t, _) in &op.operations{ // Check subsumption
            if and.bindable_to(&t) {
                if let Type::And(v) = t {
                    return Err(format!("Binary operation {} {} {} is subsumed by {} {} {}, so it cannot be defined", 
                                        a.get_name(self), op.representation, b.get_name(self), 
                                        v[0].get_name(self), op.representation, v[1].get_name(self)));
                }
            }
        }

        self.binary_ops[id].operations.push((and, f));

        return Ok(());
    }

    /*
        ╒═════════════════════════════╕
        │ N-ary operator manipulation │
        ╘═════════════════════════════╛
    */

    pub fn define_nary_operator(&mut self, open_rep: String, close_rep: String) -> Result<(), String> {
        self.nary_ops.push(NaryOperator {
            id: self.nary_ops.len(),
            open_rep: open_rep,
            close_rep: close_rep,
            operations: vec!()
        });

        return Ok(());
    }

    pub fn get_nary_operation(&self, id: usize, from: &Object, args: &[&Object]) -> Option<&NaryFunction> {
        let mut subtypes = vec!(from.get_type());
        subtypes.extend(args.iter().map(|i| i.get_type()));

        let args_type = Type::And(subtypes);

        for (t, op) in &self.nary_ops[id].operations{ // Check subsumption
            if args_type.bindable_to(&t) {
                return Some(op);
            }
        }
        
        return None;
    }

    pub fn def_nary_operation(&mut self, id: usize, from: &Type, args: &[Type], f: NaryFunction) -> Result<(), String> {
        let mut subtypes = vec!(from.clone());
        subtypes.extend(args.into_iter().cloned());

        let and = Type::And(subtypes);
        let op = &self.nary_ops[id];

        for (t, _) in &op.operations{ // Check subsumption
            if and.bindable_to(&t) {
                if let Type::And(v) = t {
                    return Err(format!("Binary operation {}{}{}{} is subsumed by {}{}{}{}, so it cannot be defined", 
                                        from.get_name(self), op.open_rep, args.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), op.close_rep, 
                                        v[0].get_name(self), op.open_rep, v[1..].iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "), op.close_rep));
                }
            }
        }

        self.nary_ops[id].operations.push((and, f));

        return Ok(());
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

    return ctx;
}