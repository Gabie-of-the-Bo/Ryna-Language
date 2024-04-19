use rustc_hash::{FxHashMap, FxHashSet};
use malachite::Integer;

use crate::{compilation::{CompiledNessaExpr, NessaInstruction}, context::NessaContext, integer_ext::{is_valid_index, to_usize, ONE}, object::Object, operations::{ADD_BINOP_ID, ASSIGN_BINOP_ID, DEREF_UNOP_ID, DIV_BINOP_ID, MOD_BINOP_ID, MUL_BINOP_ID, NEG_UNOP_ID, SHL_BINOP_ID, SUB_BINOP_ID}, parser::{Location, NessaExpr}, types::{Type, FLOAT, INT}};

/*
    ╒═══════════════════════════╕
    │ Syntax tree optimizations │
    ╘═══════════════════════════╛
*/

const INLINE_THRESHOLD: f32 = 50.0;

lazy_static! {
    pub static ref CONST_UNOP_IDS: FxHashSet<usize> = [NEG_UNOP_ID].iter().cloned().collect();
    pub static ref CONST_BINOP_IDS: FxHashSet<usize> = [ADD_BINOP_ID, SUB_BINOP_ID, MUL_BINOP_ID, DIV_BINOP_ID, MOD_BINOP_ID].iter().copied().collect();
    pub static ref CONST_NARYOP_IDS: FxHashSet<usize> = [].iter().copied().collect();
    pub static ref CONST_FUNC_IDS: FxHashSet<usize> = [].iter().copied().collect();
}

impl NessaContext {
    pub fn count_usages_expr(expr: &NessaExpr, var_usages: &mut FxHashMap<usize, usize>, offset: usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *var_usages.entry(*id).or_default() += offset;
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => NessaContext::count_usages_expr(e, var_usages, offset),

            NessaExpr::CompiledLambda(_, _, c, _, _, _) => {
                for (_, e) in c {
                    NessaContext::count_usages_expr(e, var_usages, 2); // Set offset of 2 in order not to insert moves inside captures
                }
            }

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    NessaContext::count_usages_expr(e, var_usages, offset);
                }
            },

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                NessaContext::count_usages_expr(c, var_usages, offset); // Set offset of 2 in order not to insert moves inside loops

                for e in exprs {
                    NessaContext::count_usages_expr(e, var_usages, 2);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                NessaContext::count_usages_expr(c, var_usages, offset);

                for e in exprs {
                    NessaContext::count_usages_expr(e, var_usages, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                NessaContext::count_usages_expr(a, var_usages, offset);
                NessaContext::count_usages_expr(b, var_usages, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                NessaContext::count_usages_expr(ic, var_usages, offset);
                
                for e in ib {
                    NessaContext::count_usages_expr(e, var_usages, offset);
                }

                for (ei_h, ei_b) in ei {
                    NessaContext::count_usages_expr(ei_h, var_usages, offset);
                    
                    for e in ei_b {
                        NessaContext::count_usages_expr(e, var_usages, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        NessaContext::count_usages_expr(e, var_usages, offset);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
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

            NessaExpr::CompiledLambda(_, _, c, _, _, exprs) => {
                let mut var_usages_lambda = FxHashMap::default();

                for (_, e) in c {
                    NessaContext::count_usages_expr(e, &mut var_usages_lambda, 2); // Set offset of 2 in order not to insert moves inside captures
                }

                for e in exprs {
                    self.insert_moves_expr(e, &mut var_usages_lambda);
                }                
            }

            NessaExpr::DoBlock(_, exprs, _) |
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
            NessaExpr::Continue(_) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn insert_moves(&self, body: &mut Vec<NessaExpr>) {
        let mut var_usages = FxHashMap::default();

        // Count usages
        for i in body.iter() {
            NessaContext::count_usages_expr(i, &mut var_usages, 1);
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
            NessaExpr::CompiledLambda(_, _, _, _, _, exprs) |
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
                        if obj.get_type() == INT {
                            let n = obj.get::<Integer>().clone();

                            if is_valid_index(&n) && to_usize(&n).is_power_of_two() {
                                let shift = u64::BITS - to_usize(&n).leading_zeros();
    
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
                    }

                    if let NessaExpr::Literal(_, obj) = &**b {
                        if obj.get_type() == INT {
                            let n = obj.get::<Integer>().clone();

                            if is_valid_index(&n) && to_usize(&n).is_power_of_two() {
                                let shift = u64::BITS - to_usize(&n).leading_zeros();
    
                                *expr = NessaExpr::BinaryOperation(
                                    l.clone(),
                                    SHL_BINOP_ID, 
                                    vec!(), 
                                    a.clone(), 
                                    Box::new(NessaExpr::Literal(l.clone(), Object::new(Integer::from(shift - 1))))
                                );
    
                                // Sanity check and overload registration
                                self.static_check(expr).unwrap();
                            }
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
            NessaExpr::Continue(_) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inlining_weight(&self, body: &Vec<NessaExpr>) -> f32 {
        self.compiled_form_body_size(body, false).unwrap() as f32
    }

    pub fn max_variable(expr: &NessaExpr, offset: &mut usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *offset = (*offset).max(*id);
            },

            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, id, _, _, e) => {
                *offset = (*offset).max(*id);
                NessaContext::max_variable(e, offset)
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) => NessaContext::max_variable(e, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, _, exprs) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    NessaContext::max_variable(e, offset);
                }
            },

            NessaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *offset = (*offset).max(*iterator_idx);
                *offset = (*offset).max(*element_idx);

                NessaContext::max_variable(c, offset);

                for e in exprs {
                    NessaContext::max_variable(e, offset);
                }
            },

            NessaExpr::While(_, c, exprs) => {
                NessaContext::max_variable(c, offset);

                for e in exprs {
                    NessaContext::max_variable(e, offset);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                NessaContext::max_variable(c, offset);

                for e in exprs {
                    NessaContext::max_variable(e, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                NessaContext::max_variable(a, offset);
                NessaContext::max_variable(b, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                NessaContext::max_variable(ic, offset);
                
                for e in ib {
                    NessaContext::max_variable(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    NessaContext::max_variable(ei_h, offset);
                    
                    for e in ei_b {
                        NessaContext::max_variable(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        NessaContext::max_variable(e, offset);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn offset_variables(expr: &mut NessaExpr, offset: usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *id += offset;
            },

            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, id, _, _, e) => {
                *id += offset;
                NessaContext::offset_variables(e, offset)
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) => NessaContext::offset_variables(e, offset),

            NessaExpr::CompiledLambda(_, _, c, _, _, exprs) => {
                for (_, e) in c {
                    NessaContext::offset_variables(e, offset);
                }

                for e in exprs {
                    NessaContext::offset_variables(e, offset);
                }
            }

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    NessaContext::offset_variables(e, offset);
                }
            },

            NessaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *iterator_idx += offset;
                *element_idx += offset;

                NessaContext::offset_variables(c, offset);
                
                for e in exprs {
                    NessaContext::offset_variables(e, offset);
                }
            }

            NessaExpr::While(_, c, exprs) => {
                NessaContext::offset_variables(c, offset);

                for e in exprs {
                    NessaContext::offset_variables(e, offset);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                NessaContext::offset_variables(c, offset);

                for e in exprs {
                    NessaContext::offset_variables(e, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                NessaContext::offset_variables(a, offset);
                NessaContext::offset_variables(b, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                NessaContext::offset_variables(ic, offset);
                
                for e in ib {
                    NessaContext::offset_variables(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    NessaContext::offset_variables(ei_h, offset);
                    
                    for e in ei_b {
                        NessaContext::offset_variables(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        NessaContext::offset_variables(e, offset);
                    }
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inline_body(&self, mut body: Vec<NessaExpr>, args: Vec<NessaExpr>, var_offset: &mut usize, l: &Location) -> Vec<NessaExpr> {
        let mut res = vec!();

        // Get number of vars
        let arg_num = args.len();
        let mut func_offset = 0;

        for line in body.iter() {
            NessaContext::max_variable(line, &mut func_offset);
        }

        // Define variables
        for (idx, arg) in args.into_iter().enumerate() {
            res.push(NessaExpr::CompiledVariableDefinition(
                l.clone(),
                idx + *var_offset + 1,
                format!("__arg_{}__", idx + *var_offset + 1),
                self.infer_type(&arg).unwrap(),
                Box::new(arg)
            ))
        }

        // Map body
        for line in body.iter_mut() {
            NessaContext::offset_variables(line, *var_offset + 1);
        }

        res.append(&mut body);

        // Update variable number
        *var_offset += func_offset + arg_num + 1;

        res
    }

    pub fn inline_functions_expr(&self, expr: &mut NessaExpr, offset: &mut usize) {
        match expr {
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.inline_functions_expr(e, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                self.inline_functions(exprs, offset);
            },

            NessaExpr::UnaryOperation(l, id, t, e) => {
                self.inline_functions_expr(e, offset);

                let arg_types = self.infer_type(e).unwrap();
                let templates = Type::And(t.clone());

                let cache_entry = self.cache.templates.unary.inner_borrow_mut();

                let body = cache_entry.iter().find(|i| {
                    let other_tm = Type::And(i.0.1.clone());

                    i.0.0 == *id && templates.bindable_to(&other_tm, self) && arg_types.bindable_to(&i.0.2[0], self)
                }).map(|i| i.1);

                if let Some(inner) = body {
                    let weight = self.inlining_weight(inner);

                    if weight < INLINE_THRESHOLD {
                        let mut inlined_body = self.inline_body(inner.clone(), vec!(*e.clone()), offset, l);
                        let return_type = self.infer_type(expr).unwrap();

                        // Add empty return if needed
                        if let Type::Empty = return_type {
                            if NessaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = NessaExpr::DoBlock(Location::none(), inlined_body, return_type);
                    }
                }
            }

            NessaExpr::BinaryOperation(l, id, t, a, b) => {
                self.inline_functions_expr(a, offset);
                self.inline_functions_expr(b, offset);

                let arg_types = Type::And(vec!(self.infer_type(a).unwrap(), self.infer_type(b).unwrap()));
                let templates = Type::And(t.clone());

                let cache_entry = self.cache.templates.binary.inner_borrow_mut();

                let body = cache_entry.iter().find(|i| {
                    let other_tm = Type::And(i.0.1.clone());
                    let other_args = Type::And(i.0.2.clone());

                    i.0.0 == *id && templates.bindable_to(&other_tm, self) && arg_types.bindable_to(&other_args, self)
                }).map(|i| i.1);

                if let Some(inner) = body {
                    let weight = self.inlining_weight(inner);

                    if weight < INLINE_THRESHOLD {
                        let mut inlined_body = self.inline_body(inner.clone(), vec!(*a.clone(), *b.clone()), offset, l);
                        let return_type = self.infer_type(expr).unwrap();

                        // Add empty return if needed
                        if let Type::Empty = return_type {
                            if NessaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = NessaExpr::DoBlock(Location::none(), inlined_body, return_type);
                    }
                }
            }

            NessaExpr::NaryOperation(l, id, t, c, exprs) => {
                self.inline_functions_expr(c, offset);
                self.inline_functions(exprs, offset);

                let mut arg_types_vec = vec!(self.infer_type(c).unwrap());
                arg_types_vec.extend(exprs.iter().map(|i| self.infer_type(i).unwrap()));
                let arg_types = Type::And(arg_types_vec);
                let templates = Type::And(t.clone());

                let cache_entry = self.cache.templates.nary.inner_borrow_mut();

                let body = cache_entry.iter().find(|i| {
                    let other_tm = Type::And(i.0.1.clone());
                    let other_args = Type::And(i.0.2.clone());

                    i.0.0 == *id && templates.bindable_to(&other_tm, self) && arg_types.bindable_to(&other_args, self)
                }).map(|i| i.1);

                if let Some(inner) = body {
                    let weight = self.inlining_weight(inner);

                    if weight < INLINE_THRESHOLD {
                        let mut args_vec = vec!(*c.clone());
                        args_vec.extend(exprs.iter().cloned());
        
                        let mut inlined_body = self.inline_body(inner.clone(), args_vec, offset, l);
                        let return_type = self.infer_type(expr).unwrap();

                        // Add empty return if needed
                        if let Type::Empty = return_type {
                            if NessaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = NessaExpr::DoBlock(Location::none(), inlined_body, return_type);
                    }
                }
            }

            NessaExpr::FunctionCall(l, id, t, args) => {
                self.inline_functions(args, offset);

                let arg_types = Type::And(args.iter().map(|i| self.infer_type(i).unwrap()).collect());
                let templates = Type::And(t.clone());

                let cache_entry = self.cache.templates.functions.inner_borrow_mut();

                let body = cache_entry.iter().find(|i| {
                    let other_tm = Type::And(i.0.1.clone());
                    let other_args = Type::And(i.0.2.clone());

                    i.0.0 == *id && templates.bindable_to(&other_tm, self) && arg_types.bindable_to(&other_args, self)
                }).map(|i| i.1);

                if let Some(inner) = body {
                    let weight = self.inlining_weight(inner);

                    if weight < INLINE_THRESHOLD {
                        let mut inlined_body = self.inline_body(inner.clone(), args.clone(), offset, l);
                        let return_type = self.infer_type(expr).unwrap();

                        // Add empty return if needed
                        if let Type::Empty = return_type {
                            if NessaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(NessaExpr::Return(Location::none(), Box::new(NessaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = NessaExpr::DoBlock(Location::none(), inlined_body, return_type);
                    }
                }
            }

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.inline_functions_expr(c, offset);
                self.inline_functions(exprs, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.inline_functions_expr(ic, offset);
                self.inline_functions(ib, offset);

                for (ei_h, ei_b) in ei {
                    self.inline_functions_expr(ei_h, offset);
                    self.inline_functions(ei_b, offset);
                }

                if let Some(inner) = eb {
                    self.inline_functions(inner, offset);
                }
            },
            
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inline_functions(&self, body: &mut Vec<NessaExpr>, offset: &mut usize) {
        for i in body {
            self.inline_functions_expr(i, offset);
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

    pub fn is_constant_expr(&self, expr: &NessaExpr, consts: &FxHashMap<usize, bool>) -> bool {
        macro_rules! is_num {
            ($expr: expr) => {
                match *self.infer_type($expr).unwrap().deref_type().deref_type() {
                    Type::Basic(crate::types::INT_ID) | Type::Basic(crate::types::FLOAT_ID) => true,
                    _ => false
                }
            };
        }

        match expr {
            NessaExpr::Literal(..) => true,
            NessaExpr::Variable(_, id, _, _) => *consts.get(id).unwrap_or(&false),

            NessaExpr::UnaryOperation(_, id, _, e) => {
                CONST_UNOP_IDS.contains(id) && self.is_constant_expr(e, consts) && is_num!(e)
            },
            NessaExpr::BinaryOperation(_, id, _, a, b) => {
                CONST_BINOP_IDS.contains(id) && self.is_constant_expr(a, consts) && self.is_constant_expr(b, consts) && is_num!(a) && is_num!(b)
            },

            NessaExpr::NaryOperation(_, id, _, c, exprs) => {
                CONST_NARYOP_IDS.contains(id) && self.is_constant_expr(c, consts) &&
                exprs.iter().all(|i| self.is_constant_expr(i, consts))
            },

            NessaExpr::FunctionCall(_, id, _, exprs) => {
                CONST_FUNC_IDS.contains(id) && exprs.iter().all(|i| self.is_constant_expr(i, consts))
            }
            
            _ => false
        }
    }

    pub fn compute_constant_expr(expr: &NessaExpr, const_exprs: &FxHashMap<usize, NessaExpr>) -> Object {
        match expr {
            NessaExpr::Literal(_, obj) => obj.clone(),
            NessaExpr::Variable(_, id, _, _) => NessaContext::compute_constant_expr(const_exprs.get(id).unwrap(), const_exprs),

            NessaExpr::UnaryOperation(_, id, _, e) => {
                let inner = NessaContext::compute_constant_expr(e, const_exprs);
                let t = inner.get_type();

                match *id {
                    NEG_UNOP_ID if t == INT => Object::new(-inner.get::<Integer>().clone()),
                    NEG_UNOP_ID if t == FLOAT => Object::new(-*inner.get::<f64>()),
                    _ => unreachable!()
                }    
            },

            NessaExpr::BinaryOperation(_, id, _, a, b) => {
                let a_inner = NessaContext::compute_constant_expr(a, const_exprs);
                let b_inner = NessaContext::compute_constant_expr(b, const_exprs);
                let ta = a_inner.get_type();
                let tb = b_inner.get_type();

                macro_rules! bin_op {
                    ($op: tt, $type_a: ident, $type_b: ident) => {
                        Object::new(a_inner.get::<$type_a>().clone() $op b_inner.get::<$type_b>().clone())
                    };
                }

                macro_rules! bin_op_l {
                    ($op: tt, $type_a: ident, $type_b: ident, $func: ident) => {
                        {
                            use crate::integer_ext::*;
                            Object::new($func(&*a_inner.get::<$type_a>()) $op b_inner.get::<$type_b>().clone())    
                        }
                    };
                }

                macro_rules! bin_op_r {
                    ($op: tt, $type_a: ident, $type_b: ident, $func: ident) => {
                        {
                            use crate::integer_ext::*;
                            Object::new(a_inner.get::<$type_a>().clone() $op $func(&*b_inner.get::<$type_b>()))    
                        }
                    };
                }

                match *id {
                    ADD_BINOP_ID if ta == INT && tb == INT => bin_op!(+, Integer, Integer),
                    ADD_BINOP_ID if ta == FLOAT && tb == FLOAT => bin_op!(+, f64, f64),
                    ADD_BINOP_ID if ta == INT && tb == FLOAT => bin_op_l!(+, Integer, f64, to_f64),
                    ADD_BINOP_ID if ta == FLOAT && tb == INT => bin_op_r!(+, f64, Integer, to_f64),
                    
                    SUB_BINOP_ID if ta == INT && tb == INT => bin_op!(-, Integer, Integer),
                    SUB_BINOP_ID if ta == FLOAT && tb == FLOAT => bin_op!(-, f64, f64),
                    SUB_BINOP_ID if ta == INT && tb == FLOAT => bin_op_l!(-, Integer, f64, to_f64),
                    SUB_BINOP_ID if ta == FLOAT && tb == INT => bin_op_r!(-, f64, Integer, to_f64),
                    
                    MUL_BINOP_ID if ta == INT && tb == INT => bin_op!(*, Integer, Integer),
                    MUL_BINOP_ID if ta == FLOAT && tb == FLOAT => bin_op!(*, f64, f64),
                    MUL_BINOP_ID if ta == INT && tb == FLOAT => bin_op_l!(*, Integer, f64, to_f64),
                    MUL_BINOP_ID if ta == FLOAT && tb == INT => bin_op_r!(*, f64, Integer, to_f64),
                    
                    DIV_BINOP_ID if ta == INT && tb == INT => bin_op!(/, Integer, Integer),
                    DIV_BINOP_ID if ta == FLOAT && tb == FLOAT => bin_op!(/, f64, f64),
                    DIV_BINOP_ID if ta == INT && tb == FLOAT => bin_op_l!(/, Integer, f64, to_f64),
                    DIV_BINOP_ID if ta == FLOAT && tb == INT => bin_op_r!(/, f64, Integer, to_f64),
                    
                    MOD_BINOP_ID if ta == INT && tb == INT => bin_op!(%, Integer, Integer),
                    MOD_BINOP_ID if ta == FLOAT && tb == FLOAT => bin_op!(%, f64, f64),
                    MOD_BINOP_ID if ta == INT && tb == FLOAT => bin_op_l!(%, Integer, f64, to_f64),
                    MOD_BINOP_ID if ta == FLOAT && tb == INT => bin_op_r!(%, f64, Integer, to_f64),

                    _ => unreachable!()
                }
            },

            NessaExpr::NaryOperation(_, _, _, _, _) => todo!(),
            NessaExpr::FunctionCall(_, _, _, _) => todo!(),
            
            _ => unreachable!()
        }
    }
    
    pub fn get_constants(&self, expr: &NessaExpr, consts: &mut FxHashMap<usize, bool>, const_exprs: &mut FxHashMap<usize, NessaExpr>) {
        match expr {
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e)  => self.get_constants(e, consts, const_exprs),
            
            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) => { 
                if self.is_constant_expr(e, consts) {
                    consts.insert(*id, true);
                    const_exprs.insert(*id, NessaExpr::Literal(Location::none(), NessaContext::compute_constant_expr(e, const_exprs)));    
                }
            },
            NessaExpr::CompiledVariableAssignment(_, id, _, _, _) => { consts.insert(*id, false); },

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.get_constants(c, consts, const_exprs);

                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.get_constants(c, consts, const_exprs);

                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.get_constants(a, consts, const_exprs);
                self.get_constants(b, consts, const_exprs);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.get_constants(ic, consts, const_exprs);
                
                for e in ib {
                    self.get_constants(e, consts, const_exprs);
                }

                for (ei_h, ei_b) in ei {
                    self.get_constants(ei_h, consts, const_exprs);
                    
                    for e in ei_b {
                        self.get_constants(e, consts, const_exprs);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.get_constants(e, consts, const_exprs);
                    }
                }
            },
            
            NessaExpr::CompiledLambda(_, _, _, _, _, _) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }
    
    fn sub_variables(&self, expr: &mut NessaExpr, assigned_exprs: &mut FxHashMap<usize, NessaExpr>) {
        match expr {
            NessaExpr::Variable(l, id, _, _) => {
                if assigned_exprs.contains_key(id) {
                    let mut_id = self.get_function_id("mut".into()).unwrap();
                    let const_expr = assigned_exprs[id].clone();
                    let t = self.infer_type(&const_expr).unwrap();

                    *expr = NessaExpr::FunctionCall(l.clone(), mut_id, vec!(t), vec!(const_expr));
                    
                    // Sanity check and overload registration
                    self.static_check(expr).unwrap();
                }
            }

            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e)  => self.sub_variables(e, assigned_exprs),
            
            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }
            },

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.sub_variables(c, assigned_exprs);

                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.sub_variables(c, assigned_exprs);

                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.sub_variables(a, assigned_exprs);
                self.sub_variables(b, assigned_exprs);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.sub_variables(ic, assigned_exprs);
                
                for e in ib {
                    self.sub_variables(e, assigned_exprs);
                }

                for (ei_h, ei_b) in ei {
                    self.sub_variables(ei_h, assigned_exprs);
                    
                    for e in ei_b {
                        self.sub_variables(e, assigned_exprs);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.sub_variables(e, assigned_exprs);
                    }
                }
            },
            
            NessaExpr::CompiledLambda(_, _, _, _, _, _) |
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    fn remove_assignments(&self, exprs: &mut Vec<NessaExpr>, assigned_exprs: &mut FxHashMap<usize, NessaExpr>) {
        fn filter_assignments(exprs: &mut Vec<NessaExpr>, assigned_exprs: &mut FxHashMap<usize, NessaExpr>) {
            exprs.retain(|i| !matches!(i, NessaExpr::CompiledVariableDefinition(_, id, _, _, _) if assigned_exprs.contains_key(id)));
        }

        filter_assignments(exprs, assigned_exprs);

        for e in exprs {
            self.remove_assignments_expr(e, assigned_exprs);
        }
    }

    fn remove_assignments_expr(&self, expr: &mut NessaExpr, assigned_exprs: &mut FxHashMap<usize, NessaExpr>) {
        match expr {
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) |
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e)  => self.remove_assignments_expr(e, assigned_exprs),
            
            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => self.remove_assignments(exprs, assigned_exprs),

            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.remove_assignments_expr(c, assigned_exprs);
                self.remove_assignments(exprs, assigned_exprs);
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.remove_assignments_expr(c, assigned_exprs);
                self.remove_assignments(exprs, assigned_exprs);
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.remove_assignments_expr(a, assigned_exprs);
                self.remove_assignments_expr(b, assigned_exprs);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.remove_assignments_expr(ic, assigned_exprs);
                self.remove_assignments(ib, assigned_exprs);

                for (ei_h, ei_b) in ei {
                    self.remove_assignments_expr(ei_h, assigned_exprs);
                    self.remove_assignments(ei_b, assigned_exprs)
                }

                if let Some(inner) = eb {
                    self.remove_assignments(inner, assigned_exprs);
                }
            },

            NessaExpr::CompiledLambda(_, _, _, _, _, _) |
            NessaExpr::Variable(_, _, _, _) |
            NessaExpr::Break(_) |
            NessaExpr::Continue(_) |
            NessaExpr::Literal(_, _) |
            NessaExpr::Macro(_, _, _, _, _) |
            NessaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            NessaExpr::PrefixOperatorDefinition(_, _, _) |
            NessaExpr::PostfixOperatorDefinition(_, _, _) |
            NessaExpr::BinaryOperatorDefinition(_, _, _, _) |
            NessaExpr::NaryOperatorDefinition(_, _, _, _) |
            NessaExpr::ClassDefinition(_, _, _, _, _, _) |
            NessaExpr::InterfaceDefinition(_, _, _, _, _, _, _) |
            NessaExpr::InterfaceImplementation(_, _, _, _, _) |
            NessaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            NessaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _) |
            NessaExpr::NaryOperationDefinition(_, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn constant_propagation(&self, body: &mut Vec<NessaExpr>) {
        let mut var_usages = FxHashMap::default();
        let mut consts = FxHashMap::default();
        let mut assigned_exprs = FxHashMap::default();

        // Compute constants
        for i in body.iter() {
            NessaContext::count_usages_expr(i, &mut var_usages, 1);
            self.get_constants(i, &mut consts, &mut assigned_exprs);
        }

        // Filter by usages
        assigned_exprs.retain(|i, _| *consts.get(i).unwrap_or(&false) && *var_usages.get(i).unwrap_or(&0) == 1);
        
        // Sub variables
        for i in body.iter_mut() {
            self.sub_variables(i, &mut assigned_exprs);
        }

        // Remove assignments
        self.remove_assignments(body, &mut assigned_exprs);
    }

    pub fn late_optimize(&self, body: &mut Vec<NessaExpr>) {
        let mut var_offset = 0;

        for line in body.iter() {
            NessaContext::max_variable(line, &mut var_offset);
        }

        self.inline_functions(body, &mut var_offset);
        self.constant_propagation(body);
    }
}

/*
    ╒════════════════════════╕
    │ Peephole optimizations │
    ╘════════════════════════╛
*/

fn compute_labels(program: &mut [NessaInstruction]) {
    let mut labels = FxHashMap::default(); 
    let mut curr_idx = 0;

    // Generate labels
    for (idx, i) in program.iter_mut().enumerate() {
        match &mut i.instruction {
            CompiledNessaExpr::Lambda(to, _, _, _) |
            CompiledNessaExpr::Call(to) |
            CompiledNessaExpr::Jump(to) => {
                if !labels.contains_key(to) {
                    labels.entry(*to).or_insert(curr_idx);
                    *to = curr_idx;
                    curr_idx += 1;

                } else {
                    *to = labels[to];
                }
            },
                    
            CompiledNessaExpr::RelativeJumpIfFalse(to, _) |
            CompiledNessaExpr::RelativeJumpIfTrue(to, _) => {
                let p = idx + *to;

                if !labels.contains_key(&p) {
                    labels.entry(p).or_insert(curr_idx);
                    *to = curr_idx;
                    curr_idx += 1;

                } else {
                    *to = labels[&p];
                }
            },
    
            CompiledNessaExpr::RelativeJump(to) => {
                let p = (idx as i32 + *to) as usize;

                if !labels.contains_key(&p) {
                    labels.entry(p).or_insert(curr_idx);
                    *to = curr_idx as i32;
                    curr_idx += 1;

                } else {
                    *to = labels[&p] as i32;
                }
            },
    
            _ => { }
        }
    }

    // Insert labels
    for (line, tag) in &labels {
        program[*line].debug_info.labels.insert(*tag);
    }
}

fn reassign_labels(program: &mut [NessaInstruction]) {
    let mut positions = FxHashMap::default(); 

    // Generate label positions
    for (idx, i) in program.iter_mut().enumerate() {
        for l in &i.debug_info.labels {
            positions.entry(*l).or_insert(idx);
        }

        i.debug_info.labels.clear();
    }

    // Recompute positions
    for (idx, i) in program.iter_mut().enumerate() {
        match &mut i.instruction {
            CompiledNessaExpr::Lambda(to, _, _, _) |
            CompiledNessaExpr::Call(to) |
            CompiledNessaExpr::Jump(to) => {
                *to = positions[to];
            },
                    
            CompiledNessaExpr::RelativeJumpIfFalse(to, _) |
            CompiledNessaExpr::RelativeJumpIfTrue(to, _) => {
                *to = positions[to] - idx;
            },
    
            CompiledNessaExpr::RelativeJump(to) => {
                *to = positions[&(*to as usize)] as i32 - idx as i32;
            },
    
            _ => { }
        }
    }
}

impl NessaContext {
    fn peephole_optimization(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        compute_labels(program);

        let mut changed = true;

        macro_rules! remove_instruction {
            ($idx: expr) => {
                let labels_to_remove = program[$idx].debug_info.labels.clone();
                program[$idx + 1].debug_info.labels.extend(&labels_to_remove);
                program.remove($idx);
            };
        }

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
                            let instr_t = &program[$idx].debug_info.var_type;

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

                    [Int(obj), StoreVariable(id)] => { change_second!(StoreIntVariable(*id, obj.clone())); },
                    [Str(obj), StoreVariable(id)] => { change_second!(StoreStringVariable(*id, obj.clone())); },
                    [Float(obj), StoreVariable(id)] => { change_second!(StoreFloatVariable(*id, *obj)); },
                    [Bool(obj), StoreVariable(id)] => { change_second!(StoreBoolVariable(*id, *obj)); },

                    [GetVariable(id) | CloneVariable(id), Assign] if is_not_ref!(i) => { change_first!(AssignToVar(*id)); },
                    [GetVariable(id) | CloneVariable(id), Assign] => { change_first!(AssignToVarDirect(*id)); },

                    [GetVariable(id) | CloneVariable(id), Demut] => { change_first!(RefVariable(*id)); },
                    [GetVariable(id) | CloneVariable(id), Copy] => { change_first!(CopyVariable(*id)); },
                    [RefVariable(id), Copy] => { change_first!(CopyVariable(*id)); },
                    [GetVariable(id) | CloneVariable(id), Deref] => { change_first!(DerefVariable(*id)); },
                    [RefVariable(id), Deref] => { change_first!(DerefVariable(*id)); },
                    [GetVariable(id) | CloneVariable(id), Move] => { change_first!(MoveVariable(*id)); },
                    
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
                    
                    [IdxMut, Demut] => { change_first!(IdxRef); },
                    [IdxMut, Move] => { change_first!(IdxMoveRef); },

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
        
        reassign_labels(program);
    }

    fn remove_empty_calls(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        let mut lines_to_remove = vec!();

        // Look for empty calls
        for (idx, i) in program.iter().enumerate() {
            match i.instruction {
                Call(loc) if program[loc].instruction == Return => {
                    lines_to_remove.push(idx);
                    lines_to_remove.push(loc);
                },
                _ => { }
            }
        }

        lines_to_remove.sort();
        lines_to_remove.dedup();

        // Remove lines
        compute_labels(program);

        for line in lines_to_remove.into_iter().rev() {
            let other = program[line].debug_info.clone();
            program[line + 1].debug_info.merge_with(&other);
            program.remove(line);
        }

        reassign_labels(program);
    }

    fn remove_single_relative_jumps(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        let mut lines_to_remove = vec!();

        // Look for empty calls
        for (idx, i) in program.iter().enumerate() {
            if let RelativeJump(1) = i.instruction {
                lines_to_remove.push(idx);
            }
        }

        lines_to_remove.sort();
        lines_to_remove.dedup();

        // Remove lines
        compute_labels(program);

        for line in lines_to_remove.into_iter().rev() {
            let other = program[line].debug_info.clone();
            program[line + 1].debug_info.merge_with(&other);
            program.remove(line);
        }

        reassign_labels(program);
    }

    fn remove_jump_chains(&self, program: &mut [NessaInstruction]) {
        use CompiledNessaExpr::*;

        let mut changed = true;

        while changed {
            changed = false;
            
            let mut increments = FxHashMap::default();
            let mut early_rets = FxHashSet::default();

            // Look for jump chains
            for (idx, i) in program.iter().enumerate() {
                if let RelativeJump(loc) = i.instruction {
                    if let RelativeJump(loc_2) = program[(idx as i32 + loc) as usize].instruction {
                        increments.entry(idx).or_insert(loc_2);
                    
                    } else if let Return = program[(idx as i32 + loc) as usize].instruction {
                        early_rets.insert(idx);
                    }
                }
            }

            // Apply skips
            for (idx, inc) in increments {
                if let RelativeJump(loc) = &mut program[idx].instruction {
                    *loc += inc;
                    changed = true;
                }
            }

            for idx in early_rets {
                program[idx].instruction = Return;
                changed = true;
            }
        }
    }

    fn tail_call_optimization(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        compute_labels(program);

        let mut changed = true;

        while changed {
            changed = false;
            
            macro_rules! remove_instruction {
                ($idx: expr) => {
                    let other = program[$idx].debug_info.clone();
                    program[$idx + 1].debug_info.merge_with(&other);
                    program.remove($idx);
                };
            }

            for i in 0..(program.len() - 1) {
                macro_rules! change_first {
                    ($new_expr: expr) => {
                        program[i].instruction = $new_expr;
                        remove_instruction!(i + 1);
    
                        changed = true;
                        break;
                    };
                }
                
                if let [Call(loc), Return] = [&program[i].instruction, &program[i + 1].instruction] {
                    change_first!(Jump(*loc));
                }
            }
        }

        reassign_labels(program);
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
        self.remove_single_relative_jumps(program);
        self.remove_empty_calls(program);
        self.remove_jump_chains(program);
        self.tail_call_optimization(program);
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