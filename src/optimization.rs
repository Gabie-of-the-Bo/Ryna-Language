use rustc_hash::FxHashMap;

use crate::{compilation::{CompiledNessaExpr, NessaInstruction}, context::NessaContext, jump_map::JumpMap, number::{Integer, ONE}, object::Object, operations::{ADD_BINOP_ID, ASSIGN_BINOP_ID, DEREF_UNOP_ID, MUL_BINOP_ID, SHL_BINOP_ID, SUB_BINOP_ID}, parser::NessaExpr, types::{Type, INT}};

/*
    ╒═══════════════════════════╕
    │ Syntax tree optimizations │
    ╘═══════════════════════════╛
*/

impl NessaContext {
    pub fn count_usages_expr(&self, expr: &NessaExpr, var_usages: &mut FxHashMap<usize, usize>, offset: usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *var_usages.entry(*id).or_default() += offset;
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.count_usages_expr(e, var_usages, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.count_usages_expr(e, var_usages, offset);
                }
            },

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.count_usages_expr(c, var_usages, offset); // Set offset of 2 in order not to insert moves inside loops

                for e in exprs {
                    self.count_usages_expr(e, var_usages, 2);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.count_usages_expr(c, var_usages, offset);

                for e in exprs {
                    self.count_usages_expr(e, var_usages, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.count_usages_expr(a, var_usages, offset);
                self.count_usages_expr(b, var_usages, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.count_usages_expr(ic, var_usages, offset);
                
                for e in ib {
                    self.count_usages_expr(e, var_usages, offset);
                }

                for (ei_h, ei_b) in ei {
                    self.count_usages_expr(ei_h, var_usages, offset);
                    
                    for e in ei_b {
                        self.count_usages_expr(e, var_usages, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.count_usages_expr(e, var_usages, offset);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn insert_moves_expr(&self, expr: &mut NessaExpr, var_usages: &mut FxHashMap<usize, usize>) {
        match expr {
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.insert_moves_expr(e, var_usages),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.insert_moves_expr(e, var_usages);
                }
            },

            NessaExpr::FunctionCall(_, id, _, exprs) => {
                let move_id = self.get_function_id("move".into()).unwrap();
                let deref_id = self.get_function_id("deref".into()).unwrap();

                for e in exprs.iter_mut() {
                    self.insert_moves_expr(e, var_usages);
                }

                if *id == deref_id && exprs.len() == 1 {
                    if let NessaExpr::Variable(_, var_id, _, var_type) = &exprs[0] {
                        if *var_usages.entry(*var_id).or_default() == 1 && !var_type.is_ref() {
                            *id = move_id;

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                        }
                    }
                }
            },

            NessaExpr::UnaryOperation(l, id, tm, e) => {
                self.insert_moves_expr(e, var_usages);
                
                let move_id = self.get_function_id("move".into()).unwrap();

                if *id == DEREF_UNOP_ID {
                    if let NessaExpr::Variable(_, var_id, _, var_type) = &**e {
                        if *var_usages.entry(*var_id).or_default() == 1 && !var_type.is_ref() {
                            *expr = NessaExpr::FunctionCall(l.clone(), move_id, tm.clone(), vec!((**e).clone()));

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                        }
                    }
                }
            }

            NessaExpr::NaryOperation(_, _, _, c, exprs) |
            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.insert_moves_expr(c, var_usages);

                for e in exprs {
                    self.insert_moves_expr(e, var_usages);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.insert_moves_expr(a, var_usages);
                self.insert_moves_expr(b, var_usages);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.insert_moves_expr(ic, var_usages);
                
                for e in ib {
                    self.insert_moves_expr(e, var_usages);
                }

                for (ei_h, ei_b) in ei {
                    self.insert_moves_expr(ei_h, var_usages);
                    
                    for e in ei_b {
                        self.insert_moves_expr(e, var_usages);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.insert_moves_expr(e, var_usages);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn insert_moves(&self, body: &mut Vec<NessaExpr>) {
        let mut var_usages = FxHashMap::default();

        // Count usages
        for i in body.iter() {
            self.count_usages_expr(i, &mut var_usages, 1);
        }
        
        // insert moves
        for i in body {
            self.insert_moves_expr(i, &mut var_usages);
        }
    }

    pub fn strength_reduction_expr(&self, expr: &mut NessaExpr) {
        match expr {
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.strength_reduction_expr(e),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.strength_reduction_expr(e);
                }
            },

            NessaExpr::FunctionCall(_, id, t, exprs) => {
                for e in exprs.iter_mut() {
                    self.strength_reduction_expr(e);
                }

                // Compile as
                let as_id = self.get_function_id("as".into()).unwrap();

                if *id == as_id && exprs.len() == 1 && t.len() == 1 {
                    let expected_type = &t[0];
                    let given_type = self.infer_type(&exprs[0]).unwrap();

                    // Assume that the function will succeed
                    if given_type == *expected_type {
                        *expr = exprs[0].clone();
                        return;
                    }
                }

                // Compile fwd
                let fwd_id = self.get_function_id("fwd".into()).unwrap();
                let cfwd_id = self.get_function_id("cfwd".into()).unwrap();

                if (*id == fwd_id || *id == cfwd_id) && exprs.len() == 1 && t.len() == 1 {
                    let move_id = self.get_function_id("move".into()).unwrap();
                    let deref_id = self.get_function_id("deref".into()).unwrap();
                    let demut_id = self.get_function_id("demut".into()).unwrap();
                    let ref_id = self.get_function_id("ref".into()).unwrap();
                    let mut_id = self.get_function_id("mut".into()).unwrap();
    
                    let expected_type = &t[0];
                    let given_type = self.infer_type(&exprs[0]).unwrap();

                    // Identity
                    if given_type == *expected_type {
                        *expr = exprs[0].clone();
                        return;
                    }

                    // Move / Deref
                    if given_type == expected_type.clone().to_mut() {
                        *id = if *id == fwd_id { move_id } else { deref_id };

                        // Sanity check and overload registration
                        self.static_check(expr).unwrap();

                        return;
                    }

                    // Deref
                    if given_type == expected_type.clone().to_ref() {
                        *id = deref_id;

                        // Sanity check and overload registration
                        self.static_check(expr).unwrap();
                        
                        return;
                    }

                    // Ref
                    if given_type.clone().to_ref() == *expected_type {
                        if let Type::Ref(inner) = expected_type {
                            t[0] = *inner.clone();
                            *id = ref_id;

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                            
                            return;                            
                        }
                    }

                    // Mut
                    if given_type.clone().to_mut() == *expected_type {
                        if let Type::MutRef(inner) = expected_type {
                            t[0] = *inner.clone();
                            *id = mut_id;

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                            
                            return;                            
                        }
                    }

                    // Demut
                    if let Type::MutRef(inner_1) = &given_type {
                        if let Type::Ref(inner_2) = expected_type {
                            if inner_1 == inner_2 {
                                t[0] = *inner_2.clone();
                                *id = demut_id;

                                // Sanity check and overload registration
                                self.static_check(expr).unwrap();
                                
                                return;
                            }
                        }
                    }
                }
            }

            NessaExpr::NaryOperation(_, _, _, c, exprs) |
            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.strength_reduction_expr(c);

                for e in exprs {
                    self.strength_reduction_expr(e);
                }
            },

            NessaExpr::BinaryOperation(l, id, _, a, b) => {
                self.strength_reduction_expr(a);
                self.strength_reduction_expr(b);

                let t_a = self.infer_type(a).unwrap();
                let t_b = self.infer_type(b).unwrap();

                // Introduce increments and decrements
                if *id == ASSIGN_BINOP_ID && t_a == INT.to_mut() && t_b == INT {
                    if let NessaExpr::BinaryOperation(_, ADD_BINOP_ID, _, a_inner, b_inner) = &**b {
                        if a_inner == a {
                            if let NessaExpr::Literal(_, obj) = &**b_inner {
                                if obj.get_type() == INT && *obj.get::<Integer>() == *ONE {
                                    *expr = NessaExpr::FunctionCall(
                                        l.clone(), 
                                        self.get_function_id("inc".into()).unwrap(), 
                                        vec!(), 
                                        vec!(*a.clone())
                                    );

                                    // Sanity check and overload registration
                                    self.static_check(expr).unwrap();

                                    return;
                                }
                            }
                        }
                    }

                    if let NessaExpr::BinaryOperation(_, SUB_BINOP_ID, _, a_inner, b_inner) = &**b {
                        if a_inner == a {
                            if let NessaExpr::Literal(_, obj) = &**b_inner {
                                if obj.get_type() == INT && *obj.get::<Integer>() == *ONE {
                                    *expr = NessaExpr::FunctionCall(
                                        l.clone(), 
                                        self.get_function_id("dec".into()).unwrap(), 
                                        vec!(), 
                                        vec!(*a.clone())
                                    );

                                    // Sanity check and overload registration
                                    self.static_check(expr).unwrap();

                                    return;
                                }
                            }
                        }
                    }
                }

                // Change multiplications for shifts when applicable
                if *id == MUL_BINOP_ID && *t_a.deref_type() == INT && *t_b.deref_type() == INT {
                    if let NessaExpr::Literal(_, obj) = &**a {
                        if obj.get_type() == INT && obj.get::<Integer>().is_positive_power_of_two() {
                            let shift = u64::BITS - obj.get::<Integer>().limbs[0].leading_zeros();

                            *expr = NessaExpr::BinaryOperation(
                                l.clone(),
                                SHL_BINOP_ID, 
                                vec!(), 
                                b.clone(), 
                                Box::new(NessaExpr::Literal(l.clone(), Object::new(Integer::from(shift - 1))))
                            );

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();

                            return;
                        }
                    }
                    
                    if let NessaExpr::Literal(_, obj) = &**b {
                        if obj.get_type() == INT && obj.get::<Integer>().is_positive_power_of_two() {
                            let shift = u64::BITS - obj.get::<Integer>().limbs[0].leading_zeros();

                            *expr = NessaExpr::BinaryOperation(
                                l.clone(),
                                SHL_BINOP_ID, 
                                vec!(), 
                                a.clone(), 
                                Box::new(NessaExpr::Literal(l.clone(), Object::new(Integer::from(shift - 1))))
                            );

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();

                            return;
                        }
                    }
                }
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.strength_reduction_expr(ic);
                
                for e in ib {
                    self.strength_reduction_expr(e);
                }

                for (ei_h, ei_b) in ei {
                    self.strength_reduction_expr(ei_h);
                    
                    for e in ei_b {
                        self.strength_reduction_expr(e);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.strength_reduction_expr(e);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn strength_reduction(&self, body: &mut Vec<NessaExpr>) {
        for i in body {
            self.strength_reduction_expr(i);
        } 
    }

    pub fn optimize(&self, body: &mut Vec<NessaExpr>) {
        self.insert_moves(body);
        self.strength_reduction(body);
    }
}

/*
    ╒════════════════════════╕
    │ Peephole optimizations │
    ╘════════════════════════╛
*/

fn apply_jump_change(change: (usize, usize), program: &mut Vec<NessaInstruction>) {
    match &mut program[change.0].instruction {
        CompiledNessaExpr::Lambda(to, _, _) |
        CompiledNessaExpr::Call(to) |
        CompiledNessaExpr::Jump(to) => *to = change.1,
                
        CompiledNessaExpr::RelativeJumpIfFalse(to, _) |
        CompiledNessaExpr::RelativeJumpIfTrue(to, _) => *to = change.1 - change.0,

        CompiledNessaExpr::RelativeJump(to) => *to = change.1 as i32 - change.0 as i32,

        _ => unreachable!()
    }
}

impl NessaContext {
    fn peephole_optimization(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        let mut jumps = JumpMap::from_code(&program);
        let mut changed = true;

        macro_rules! remove_instruction {
            ($idx: expr) => {
                program.remove($idx);

                for (_, change) in jumps.remove_line($idx) {
                    apply_jump_change(change, program);
                }
            };
        }

        while changed {
            changed = false;

            for i in 0..program.len() {
                // Delete size 1 jumps
                if let RelativeJump(1) = &program[i].instruction {
                    remove_instruction!(i);

                    changed = true;
                    break;
                }
            }
        }

        changed = true;

        while changed {
            changed = false;

            // Look for optimizations
            for i in 0..(program.len() - 1) {
                macro_rules! change_first {
                    ($new_expr: expr) => {
                        program[i].instruction = $new_expr;
                        remove_instruction!(i + 1);

                        changed = true;
                        break;
                    };
                }

                macro_rules! change_second {
                    ($new_expr: expr) => {
                        program[i + 1].instruction = $new_expr;
                        remove_instruction!(i);

                        changed = true;
                        break;
                    };
                }

                macro_rules! remove_both {
                    () => {
                        remove_instruction!(i);
                        remove_instruction!(i);

                        changed = true;
                        break;
                    };
                }

                macro_rules! is_not_ref {
                    ($idx: expr) => {
                        {
                            let instr_t = &program[$idx].var_type;

                            instr_t.is_some() &&
                            !instr_t.as_ref().unwrap().is_ref()    
                        }
                    };
                }
                
                // Size 2
                match [&program[i].instruction, &program[i + 1].instruction] {
                    [Not, RelativeJumpIfFalse(offset, false)] => { change_second!(RelativeJumpIfTrue(*offset, false)); }
                    [Not, RelativeJumpIfTrue(offset, false)] => { change_second!(RelativeJumpIfFalse(*offset, false)); }

                    [NativeFunctionCall(func_id, ov_id, type_args), Drop] => {
                        change_first!(NativeFunctionCallNoRet(*func_id, *ov_id, type_args.clone()));
                    }

                    [UnaryOperatorCall(op_id, ov_id, type_args), Drop] => {
                        change_first!(UnaryOperatorCallNoRet(*op_id, *ov_id, type_args.clone()));
                    }

                    [BinaryOperatorCall(op_id, ov_id, type_args), Drop] => {
                        change_first!(BinaryOperatorCallNoRet(*op_id, *ov_id, type_args.clone()));
                    }

                    [GetVariable(id), Demut] => { change_first!(RefVariable(*id)); },
                    [GetVariable(id), Copy] => { change_first!(CopyVariable(*id)); },
                    [RefVariable(id), Copy] => { change_first!(CopyVariable(*id)); },
                    [GetVariable(id), Deref] => { change_first!(DerefVariable(*id)); },
                    [RefVariable(id), Deref] => { change_first!(DerefVariable(*id)); },
                    [GetVariable(id), Move] => { change_first!(MoveVariable(*id)); },
                    
                    [AttributeMut(id), Demut] => { change_first!(AttributeRef(*id)); },
                    [AttributeMut(id), Copy] => { change_first!(AttributeCopy(*id)); },
                    [AttributeRef(id), Copy] => { change_first!(AttributeCopy(*id)); },
                    [AttributeMut(id), Deref] => { change_first!(AttributeDeref(*id)); },
                    [AttributeRef(id), Deref] => { change_first!(AttributeDeref(*id)); },
                    
                    [TupleElemMut(id), Demut] => { change_first!(TupleElemRef(*id)); },
                    [TupleElemMut(id), Copy] => { change_first!(TupleElemCopy(*id)); },
                    [TupleElemRef(id), Copy] => { change_first!(TupleElemCopy(*id)); },
                    [TupleElemMut(id), Deref] => { change_first!(TupleElemDeref(*id)); },
                    [TupleElemRef(id), Deref] => { change_first!(TupleElemDeref(*id)); },

                    [Not, Not] |
                    [Negi, Negi] |
                    [Negf, Negf] => { remove_both!(); }
                    
                    [Ref, Deref] |
                    [Mut, Deref] => { remove_both!(); }

                    [Eqi, Not] => { change_first!(Neqi); },
                    [Eqf, Not] => { change_first!(Neqf); },
                    [Neqi, Not] => { change_first!(Eqi); },
                    [Neqf, Not] => { change_first!(Eqf); },
                    [Lti, Not] => { change_first!(Gteqi); },
                    [Ltf, Not] => { change_first!(Gteqf); },
                    [Gti, Not] => { change_first!(Lteqi); },
                    [Gtf, Not] => { change_first!(Lteqf); },
                    [Lteqi, Not] => { change_first!(Gti); },
                    [Lteqf, Not] => { change_first!(Gtf); },
                    [Gteqi, Not] => { change_first!(Lti); },
                    [Gteqf, Not] => { change_first!(Ltf); },

                    [Or, And] => { change_first!(Nand); },
                    [Or, Not] => { change_first!(Nor); },

                    // Flow optimizations
                    [StoreVariable(id_1), MoveVariable(id_2)] if id_1 == id_2 && is_not_ref!(i) => { remove_both!(); },

                    _ => {}
                }
            }
        }

        changed = true;

        while changed {
            changed = false;

            for i in 0..program.len() {
                // Delete identity functions
                if let Call(loc) = &program[i].instruction {
                    if let Return = &program[*loc].instruction {
                        let mut idxs = program.iter()
                                              .enumerate()
                                              .filter(|i| i.1.instruction == Call(*loc))
                                              .map(|i| i.0)
                                              .collect::<Vec<_>>();

                        idxs.push(*loc);
                        idxs.sort();

                        for idx in idxs.iter().rev() {
                            remove_instruction!(*idx);
                        }

                        changed = true;
                        break;
                    }
                }
            }
        }
    }
}

/*
    ╒══════════════════════╕
    │ Optimization routine │
    ╘══════════════════════╛
*/

impl NessaContext {
    pub fn optimize_instructions(&self, program: &mut Vec<NessaInstruction>) {
        self.peephole_optimization(program);
    }
}

/*
    ╒═══════╕
    │ Tests │
    ╘═══════╛
*/

#[cfg(test)]
mod tests {
    use crate::{compilation::{CompiledNessaExpr, NessaInstruction}, context::standard_ctx};

    #[test]
    fn peephole_optimization() {
        let ctx = standard_ctx();

        let mut program = vec!(
            NessaInstruction::from(CompiledNessaExpr::Jump(3)),
            NessaInstruction::from(CompiledNessaExpr::Jump(4)),
            NessaInstruction::from(CompiledNessaExpr::Jump(5)),
            NessaInstruction::from(CompiledNessaExpr::Jump(6)),
            NessaInstruction::from(CompiledNessaExpr::Not),
            NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfTrue(2, false)),
            NessaInstruction::from(CompiledNessaExpr::Empty),
            NessaInstruction::from(CompiledNessaExpr::Jump(4)),
            NessaInstruction::from(CompiledNessaExpr::Jump(5)),
            NessaInstruction::from(CompiledNessaExpr::Jump(6)),
            NessaInstruction::from(CompiledNessaExpr::Jump(7)),
            NessaInstruction::from(CompiledNessaExpr::Halt)
        );

        ctx.peephole_optimization(&mut program);

        assert_eq!(
            program,
            vec!(
                NessaInstruction::from(CompiledNessaExpr::Jump(3)),
                NessaInstruction::from(CompiledNessaExpr::Jump(4)),
                NessaInstruction::from(CompiledNessaExpr::Jump(4)),
                NessaInstruction::from(CompiledNessaExpr::Jump(5)),
                NessaInstruction::from(CompiledNessaExpr::RelativeJumpIfFalse(2, false)),
                NessaInstruction::from(CompiledNessaExpr::Empty),
                NessaInstruction::from(CompiledNessaExpr::Jump(4)),
                NessaInstruction::from(CompiledNessaExpr::Jump(4)),
                NessaInstruction::from(CompiledNessaExpr::Jump(5)),
                NessaInstruction::from(CompiledNessaExpr::Jump(6)),
                NessaInstruction::from(CompiledNessaExpr::Halt)
            )
        )
    }
}