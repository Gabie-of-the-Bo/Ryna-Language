use rustc_hash::{FxHashMap, FxHashSet};
use malachite::Integer;

use crate::{compilation::{CompiledRynaExpr, RynaInstruction}, context::RynaContext, integer_ext::{is_valid_index, to_usize, ONE}, object::Object, operations::{ADD_BINOP_ID, ASSIGN_BINOP_ID, DEREF_UNOP_ID, DIV_BINOP_ID, MOD_BINOP_ID, MUL_BINOP_ID, NEG_UNOP_ID, SHL_BINOP_ID, SUB_BINOP_ID}, parser::{Location, RynaExpr}, types::{Type, FLOAT, INT}};

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

impl RynaContext {
    pub fn count_usages_expr(expr: &RynaExpr, var_usages: &mut FxHashMap<usize, usize>, offset: usize) {
        match expr {
            RynaExpr::Variable(_, id, _, _, g) => {
                *var_usages.entry(*id).or_default() += offset + (*g as usize); // Never move global variables
            },

            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) => RynaContext::count_usages_expr(e, var_usages, offset),

            RynaExpr::CompiledLambda(_, _, c, _, _, _) => {
                for (_, e) in c {
                    RynaContext::count_usages_expr(e, var_usages, 2); // Set offset of 2 in order not to insert moves inside captures
                }
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    RynaContext::count_usages_expr(e, var_usages, offset);
                }
            },

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                RynaContext::count_usages_expr(c, var_usages, offset); // Set offset of 2 in order not to insert moves inside loops

                for e in exprs {
                    RynaContext::count_usages_expr(e, var_usages, 2);
                }
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                RynaContext::count_usages_expr(c, var_usages, offset);

                for e in exprs {
                    RynaContext::count_usages_expr(e, var_usages, offset);
                }
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                RynaContext::count_usages_expr(a, var_usages, offset);
                RynaContext::count_usages_expr(b, var_usages, offset);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
                RynaContext::count_usages_expr(ic, var_usages, offset);
                
                for e in ib {
                    RynaContext::count_usages_expr(e, var_usages, offset);
                }

                for (ei_h, ei_b) in ei {
                    RynaContext::count_usages_expr(ei_h, var_usages, offset);
                    
                    for e in ei_b {
                        RynaContext::count_usages_expr(e, var_usages, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        RynaContext::count_usages_expr(e, var_usages, offset);
                    }
                }
            },
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn insert_moves_expr(&self, expr: &mut RynaExpr, var_usages: &mut FxHashMap<usize, usize>) {
        match expr {
            RynaExpr::Return(_, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) => self.insert_moves_expr(e, var_usages),

            RynaExpr::CompiledLambda(_, _, c, _, _, exprs) => {
                let mut var_usages_lambda = FxHashMap::default();

                for (_, e) in c {
                    RynaContext::count_usages_expr(e, &mut var_usages_lambda, 2); // Set offset of 2 in order not to insert moves inside captures
                }

                for e in exprs {
                    self.insert_moves_expr(e, &mut var_usages_lambda);
                }                
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.insert_moves_expr(e, var_usages);
                }
            },

            RynaExpr::FunctionCall(_, id, _, exprs) => {
                let move_id = self.get_function_id("move".into()).unwrap();
                let deref_id = self.get_function_id("deref".into()).unwrap();

                for e in exprs.iter_mut() {
                    self.insert_moves_expr(e, var_usages);
                }

                if *id == deref_id && exprs.len() == 1 {
                    if let RynaExpr::Variable(_, var_id, _, var_type, _) = &exprs[0] {
                        if *var_usages.entry(*var_id).or_default() == 1 && !var_type.is_ref() {
                            *id = move_id;

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                        }
                    }
                }
            },

            RynaExpr::UnaryOperation(l, id, tm, e) => {
                self.insert_moves_expr(e, var_usages);
                
                let move_id = self.get_function_id("move".into()).unwrap();

                if *id == DEREF_UNOP_ID {
                    if let RynaExpr::Variable(_, var_id, _, var_type, _) = &**e {
                        if *var_usages.entry(*var_id).or_default() == 1 && !var_type.is_ref() {
                            *expr = RynaExpr::FunctionCall(l.clone(), move_id, tm.clone(), vec!((**e).clone()));

                            // Sanity check and overload registration
                            self.static_check(expr).unwrap();
                        }
                    }
                }
            }

            RynaExpr::NaryOperation(_, _, _, c, exprs) |
            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.insert_moves_expr(c, var_usages);

                for e in exprs {
                    self.insert_moves_expr(e, var_usages);
                }
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.insert_moves_expr(a, var_usages);
                self.insert_moves_expr(b, var_usages);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Variable(_, _, _, _, _) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn insert_moves(&self, body: &mut Vec<RynaExpr>) {
        let mut var_usages = FxHashMap::default();

        // Count usages
        for i in body.iter() {
            RynaContext::count_usages_expr(i, &mut var_usages, 1);
        }
        
        // insert moves
        for i in body {
            self.insert_moves_expr(i, &mut var_usages);
        }
    }

    pub fn strength_reduction_expr(&self, expr: &mut RynaExpr) {
        match expr {
            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::Return(_, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) => self.strength_reduction_expr(e),

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.strength_reduction_expr(a);
                self.strength_reduction_expr(b);
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::CompiledLambda(_, _, _, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.strength_reduction_expr(e);
                }
            },

            RynaExpr::FunctionCall(_, id, t, exprs) => {
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

            RynaExpr::NaryOperation(_, _, _, c, exprs) |
            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.strength_reduction_expr(c);

                for e in exprs {
                    self.strength_reduction_expr(e);
                }
            },

            RynaExpr::BinaryOperation(l, id, _, a, b) => {
                self.strength_reduction_expr(a);
                self.strength_reduction_expr(b);

                let t_a = self.infer_type(a).unwrap();
                let t_b = self.infer_type(b).unwrap();

                // Introduce increments and decrements
                if *id == ASSIGN_BINOP_ID && t_a == INT.to_mut() && t_b == INT {
                    if let RynaExpr::BinaryOperation(_, ADD_BINOP_ID, _, a_inner, b_inner) = &**b {
                        if a_inner == a {
                            if let RynaExpr::Literal(_, obj) = &**b_inner {
                                if obj.get_type() == INT && *obj.get::<Integer>() == *ONE {
                                    *expr = RynaExpr::FunctionCall(
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

                    if let RynaExpr::BinaryOperation(_, SUB_BINOP_ID, _, a_inner, b_inner) = &**b {
                        if a_inner == a {
                            if let RynaExpr::Literal(_, obj) = &**b_inner {
                                if obj.get_type() == INT && *obj.get::<Integer>() == *ONE {
                                    *expr = RynaExpr::FunctionCall(
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
                    if let RynaExpr::Literal(_, obj) = &**a {
                        if obj.get_type() == INT {
                            let n = obj.get::<Integer>().clone();

                            if is_valid_index(&n) && to_usize(&n).is_power_of_two() {
                                let shift = u64::BITS - to_usize(&n).leading_zeros();
    
                                *expr = RynaExpr::BinaryOperation(
                                    l.clone(),
                                    SHL_BINOP_ID, 
                                    vec!(), 
                                    b.clone(), 
                                    Box::new(RynaExpr::Literal(l.clone(), Object::new(Integer::from(shift - 1))))
                                );
    
                                // Sanity check and overload registration
                                self.static_check(expr).unwrap();
    
                                return;
                            }
                        }
                    }

                    if let RynaExpr::Literal(_, obj) = &**b {
                        if obj.get_type() == INT {
                            let n = obj.get::<Integer>().clone();

                            if is_valid_index(&n) && to_usize(&n).is_power_of_two() {
                                let shift = u64::BITS - to_usize(&n).leading_zeros();
    
                                *expr = RynaExpr::BinaryOperation(
                                    l.clone(),
                                    SHL_BINOP_ID, 
                                    vec!(), 
                                    a.clone(), 
                                    Box::new(RynaExpr::Literal(l.clone(), Object::new(Integer::from(shift - 1))))
                                );
    
                                // Sanity check and overload registration
                                self.static_check(expr).unwrap();
                            }
                        }
                    }
                }
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Variable(_, _, _, _, _) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inlining_weight(&self, body: &Vec<RynaExpr>) -> f32 {
        self.compiled_form_body_size(body, false).unwrap() as f32
    }

    pub fn max_variable(expr: &RynaExpr, offset: &mut usize) {
        match expr {
            RynaExpr::Variable(_, id, _, _, _) => {
                *offset = (*offset).max(*id);
            },

            RynaExpr::CompiledVariableDefinition(_, id, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, id, _, _, e, _) => {
                *offset = (*offset).max(*id);
                RynaContext::max_variable(e, offset)
            },

            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => RynaContext::max_variable(e, offset),

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::CompiledLambda(_, _, _, _, _, exprs) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    RynaContext::max_variable(e, offset);
                }
            },

            RynaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *offset = (*offset).max(*iterator_idx);
                *offset = (*offset).max(*element_idx);

                RynaContext::max_variable(c, offset);

                for e in exprs {
                    RynaContext::max_variable(e, offset);
                }
            },

            RynaExpr::While(_, c, exprs) => {
                RynaContext::max_variable(c, offset);

                for e in exprs {
                    RynaContext::max_variable(e, offset);
                }
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                RynaContext::max_variable(c, offset);

                for e in exprs {
                    RynaContext::max_variable(e, offset);
                }
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                RynaContext::max_variable(a, offset);
                RynaContext::max_variable(b, offset);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
                RynaContext::max_variable(ic, offset);
                
                for e in ib {
                    RynaContext::max_variable(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    RynaContext::max_variable(ei_h, offset);
                    
                    for e in ei_b {
                        RynaContext::max_variable(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        RynaContext::max_variable(e, offset);
                    }
                }
            },
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn offset_variables(expr: &mut RynaExpr, offset: usize) {
        match expr {
            RynaExpr::Variable(_, id, _, _, g) => {
                if !*g {
                    *id += offset;                    
                }
            },

            RynaExpr::CompiledVariableDefinition(_, id, _, _, e, g) |
            RynaExpr::CompiledVariableAssignment(_, id, _, _, e, g) => {
                if !*g {
                    *id += offset;
                }

                RynaContext::offset_variables(e, offset)
            },

            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e) => RynaContext::offset_variables(e, offset),

            RynaExpr::CompiledLambda(_, _, c, _, _, exprs) => {
                for (_, e) in c {
                    RynaContext::offset_variables(e, offset);
                }

                for e in exprs {
                    RynaContext::offset_variables(e, offset);
                }
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    RynaContext::offset_variables(e, offset);
                }
            },

            RynaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *iterator_idx += offset;
                *element_idx += offset;

                RynaContext::offset_variables(c, offset);
                
                for e in exprs {
                    RynaContext::offset_variables(e, offset);
                }
            }

            RynaExpr::While(_, c, exprs) => {
                RynaContext::offset_variables(c, offset);

                for e in exprs {
                    RynaContext::offset_variables(e, offset);
                }
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                RynaContext::offset_variables(c, offset);

                for e in exprs {
                    RynaContext::offset_variables(e, offset);
                }
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                RynaContext::offset_variables(a, offset);
                RynaContext::offset_variables(b, offset);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
                RynaContext::offset_variables(ic, offset);
                
                for e in ib {
                    RynaContext::offset_variables(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    RynaContext::offset_variables(ei_h, offset);
                    
                    for e in ei_b {
                        RynaContext::offset_variables(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        RynaContext::offset_variables(e, offset);
                    }
                }
            },
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inline_body(&self, mut body: Vec<RynaExpr>, args: Vec<RynaExpr>, var_offset: &mut usize, l: &Location) -> Vec<RynaExpr> {
        let mut res = vec!();

        // Get number of vars
        let arg_num = args.len();
        let mut func_offset = 0;

        for line in body.iter() {
            RynaContext::max_variable(line, &mut func_offset);
        }

        // Define variables
        for (idx, arg) in args.into_iter().enumerate() {
            res.push(RynaExpr::CompiledVariableDefinition(
                l.clone(),
                idx + *var_offset + 1,
                format!("__arg_{}__", idx + *var_offset + 1),
                self.infer_type(&arg).unwrap(),
                Box::new(arg),
                false
            ))
        }

        // Map body
        for line in body.iter_mut() {
            RynaContext::offset_variables(line, *var_offset + 1);
        }

        res.append(&mut body);

        // Update variable number
        *var_offset += func_offset + arg_num + 1;

        res
    }

    pub fn inline_functions_expr(&self, expr: &mut RynaExpr, offset: &mut usize) {
        match expr {
            RynaExpr::Return(_, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) => self.inline_functions_expr(e, offset),

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.inline_functions_expr(a, offset);
                self.inline_functions_expr(b, offset);
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::CompiledLambda(_, _, _, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                self.inline_functions(exprs, offset);
            },

            RynaExpr::UnaryOperation(l, id, t, e) => {
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
                            if RynaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(RynaExpr::Return(Location::none(), Box::new(RynaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = RynaExpr::DoBlock(Location::none(), inlined_body, return_type);

                        // Sanity check
                        self.static_check(expr).unwrap();
                    }
                }
            }

            RynaExpr::BinaryOperation(l, id, t, a, b) => {
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
                            if RynaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(RynaExpr::Return(Location::none(), Box::new(RynaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = RynaExpr::DoBlock(Location::none(), inlined_body, return_type);

                        // Sanity check
                        self.static_check(expr).unwrap();
                    }
                }
            }

            RynaExpr::NaryOperation(l, id, t, c, exprs) => {
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
                            if RynaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(RynaExpr::Return(Location::none(), Box::new(RynaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = RynaExpr::DoBlock(Location::none(), inlined_body, return_type);

                        // Sanity check
                        self.static_check(expr).unwrap();
                    }
                }
            }

            RynaExpr::FunctionCall(l, id, t, args) => {
                self.inline_functions(args, offset);
                
                // Do not inline destructor calls
                if self.is_dtor_id(*id) {
                    return;
                }

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
                            if RynaContext::ensured_return_check_body(&inlined_body, &Location::none(), "Operation").is_err() {
                                inlined_body.push(RynaExpr::Return(Location::none(), Box::new(RynaExpr::Literal(Location::none(), Object::empty()))));
                            }
                        }

                        *expr = RynaExpr::DoBlock(Location::none(), inlined_body, return_type);

                        // Sanity check
                        self.static_check(expr).unwrap();
                    }
                }
            }

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.inline_functions_expr(c, offset);
                self.inline_functions(exprs, offset);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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
            
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Variable(_, _, _, _, _) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn inline_functions(&self, body: &mut Vec<RynaExpr>, offset: &mut usize) {
        for i in body {
            self.inline_functions_expr(i, offset);
        } 
    }

    pub fn strength_reduction(&self, body: &mut Vec<RynaExpr>) {
        for i in body {
            self.strength_reduction_expr(i);
        } 
    }

    pub fn optimize(&self, body: &mut Vec<RynaExpr>) {
        self.insert_moves(body);
        self.strength_reduction(body);
    } 

    pub fn is_constant_expr(&self, expr: &RynaExpr, consts: &FxHashMap<usize, bool>) -> bool {
        macro_rules! is_num {
            ($expr: expr) => {
                match *self.infer_type($expr).unwrap().deref_type().deref_type() {
                    Type::Basic(crate::types::INT_ID) | Type::Basic(crate::types::FLOAT_ID) => true,
                    _ => false
                }
            };
        }

        match expr {
            RynaExpr::Literal(..) => true,
            RynaExpr::Variable(_, id, _, _, g) => !g && *consts.get(id).unwrap_or(&false), // Assume globals are not constant

            RynaExpr::UnaryOperation(_, id, _, e) => {
                CONST_UNOP_IDS.contains(id) && self.is_constant_expr(e, consts) && is_num!(e)
            },
            RynaExpr::BinaryOperation(_, id, _, a, b) => {
                CONST_BINOP_IDS.contains(id) && self.is_constant_expr(a, consts) && self.is_constant_expr(b, consts) && is_num!(a) && is_num!(b)
            },

            RynaExpr::NaryOperation(_, id, _, c, exprs) => {
                CONST_NARYOP_IDS.contains(id) && self.is_constant_expr(c, consts) &&
                exprs.iter().all(|i| self.is_constant_expr(i, consts))
            },

            RynaExpr::FunctionCall(_, id, _, exprs) => {
                CONST_FUNC_IDS.contains(id) && exprs.iter().all(|i| self.is_constant_expr(i, consts))
            }
            
            _ => false
        }
    }

    pub fn compute_constant_expr(expr: &RynaExpr, const_exprs: &FxHashMap<usize, RynaExpr>) -> Object {
        match expr {
            RynaExpr::Literal(_, obj) => obj.clone(),
            RynaExpr::Variable(_, id, _, _, _) => RynaContext::compute_constant_expr(const_exprs.get(id).unwrap(), const_exprs),

            RynaExpr::UnaryOperation(_, id, _, e) => {
                let inner = RynaContext::compute_constant_expr(e, const_exprs);
                let t = inner.get_type();

                match *id {
                    NEG_UNOP_ID if t == INT => Object::new(-inner.get::<Integer>().clone()),
                    NEG_UNOP_ID if t == FLOAT => Object::new(-*inner.get::<f64>()),
                    _ => unreachable!()
                }    
            },

            RynaExpr::BinaryOperation(_, id, _, a, b) => {
                let a_inner = RynaContext::compute_constant_expr(a, const_exprs);
                let b_inner = RynaContext::compute_constant_expr(b, const_exprs);
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

            RynaExpr::NaryOperation(_, _, _, _, _) => todo!(),
            RynaExpr::FunctionCall(_, _, _, _) => todo!(),
            
            _ => unreachable!()
        }
    }
    
    pub fn get_constants(&self, expr: &RynaExpr, consts: &mut FxHashMap<usize, bool>, const_exprs: &mut FxHashMap<usize, RynaExpr>) {
        match expr {
            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e)  => self.get_constants(e, consts, const_exprs),
            
            RynaExpr::CompiledVariableDefinition(_, id, _, _, e, g) => { 
                if !g && self.is_constant_expr(e, consts) { // Globals are not constant
                    consts.insert(*id, true);
                    const_exprs.insert(*id, RynaExpr::Literal(Location::none(), RynaContext::compute_constant_expr(e, const_exprs)));    
                }
            },
            RynaExpr::CompiledVariableAssignment(_, id, _, _, _, _) => { consts.insert(*id, false); },

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.get_constants(c, consts, const_exprs);

                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.get_constants(c, consts, const_exprs);

                for e in exprs {
                    self.get_constants(e, consts, const_exprs);
                }
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.get_constants(a, consts, const_exprs);
                self.get_constants(b, consts, const_exprs);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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
            
            RynaExpr::CompiledLambda(_, _, _, _, _, _) |
            RynaExpr::Variable(_, _, _, _, _) |
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }
    
    fn sub_variables(&self, expr: &mut RynaExpr, assigned_exprs: &mut FxHashMap<usize, RynaExpr>) {
        match expr {
            RynaExpr::Variable(l, id, _, _, g) => {
                if !*g && assigned_exprs.contains_key(id) { // Do not subtitute globals
                    let mut_id = self.get_function_id("mut".into()).unwrap();
                    let const_expr = assigned_exprs[id].clone();
                    let t = self.infer_type(&const_expr).unwrap();

                    *expr = RynaExpr::FunctionCall(l.clone(), mut_id, vec!(t), vec!(const_expr));
                    
                    // Sanity check and overload registration
                    self.static_check(expr).unwrap();
                }
            }

            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e)  => self.sub_variables(e, assigned_exprs),

            RynaExpr::UnaryOperation(_, _, _, e) => {
                self.sub_variables(e, assigned_exprs);

                // Static check and overload resolution
                self.static_check(&expr).unwrap();
            }

            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }
            },

            RynaExpr::FunctionCall(_, _, _, exprs) => {
                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }

                // Static check and overload resolution
                self.static_check(&expr).unwrap();
            }

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.sub_variables(c, assigned_exprs);

                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.sub_variables(c, assigned_exprs);

                for e in exprs {
                    self.sub_variables(e, assigned_exprs);
                }

                // Static check and overload resolution
                self.static_check(&expr).unwrap();
            },

            RynaExpr::AttributeAssignment(_, a, b, _) => {
                self.sub_variables(a, assigned_exprs);
                self.sub_variables(b, assigned_exprs);
            }

            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.sub_variables(a, assigned_exprs);
                self.sub_variables(b, assigned_exprs);

                // Static check and overload resolution
                self.static_check(&expr).unwrap();
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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
            
            RynaExpr::CompiledLambda(_, _, _, _, _, _) |
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    fn remove_assignments(&self, exprs: &mut Vec<RynaExpr>, assigned_exprs: &mut FxHashMap<usize, RynaExpr>) {
        fn filter_assignments(exprs: &mut Vec<RynaExpr>, assigned_exprs: &mut FxHashMap<usize, RynaExpr>) {
            exprs.retain(|i| !matches!(i, RynaExpr::CompiledVariableDefinition(_, id, _, _, _, _) if assigned_exprs.contains_key(id)));
        }

        filter_assignments(exprs, assigned_exprs);

        for e in exprs {
            self.remove_assignments_expr(e, assigned_exprs);
        }
    }

    fn remove_assignments_expr(&self, expr: &mut RynaExpr, assigned_exprs: &mut FxHashMap<usize, RynaExpr>) {
        match expr {
            RynaExpr::CompiledVariableDefinition(_, _, _, _, e, _) |
            RynaExpr::CompiledVariableAssignment(_, _, _, _, e, _) |
            RynaExpr::UnaryOperation(_, _, _, e) |
            RynaExpr::AttributeAccess(_, e, _) |
            RynaExpr::Return(_, e)  => self.remove_assignments_expr(e, assigned_exprs),
            
            RynaExpr::DoBlock(_, exprs, _) |
            RynaExpr::FunctionCall(_, _, _, exprs) |
            RynaExpr::Tuple(_, exprs) => self.remove_assignments(exprs, assigned_exprs),

            RynaExpr::CompiledFor(_, _, _, _, c, exprs) |
            RynaExpr::While(_, c, exprs) => {
                self.remove_assignments_expr(c, assigned_exprs);
                self.remove_assignments(exprs, assigned_exprs);
            },

            RynaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.remove_assignments_expr(c, assigned_exprs);
                self.remove_assignments(exprs, assigned_exprs);
            },

            RynaExpr::AttributeAssignment(_, a, b, _) |
            RynaExpr::BinaryOperation(_, _, _, a, b) => {
                self.remove_assignments_expr(a, assigned_exprs);
                self.remove_assignments_expr(b, assigned_exprs);
            },

            RynaExpr::If(_, ic, ib, ei, eb) => {
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

            RynaExpr::CompiledLambda(_, _, _, _, _, _) |
            RynaExpr::Variable(_, _, _, _, _) |
            RynaExpr::Break(_) |
            RynaExpr::Continue(_) |
            RynaExpr::Literal(_, _) |
            RynaExpr::Macro(_, _, _, _, _, _) |
            RynaExpr::FunctionDefinition(_, _, _, _, _, _, _) |
            RynaExpr::PrefixOperatorDefinition(_, _, _) |
            RynaExpr::PostfixOperatorDefinition(_, _, _) |
            RynaExpr::BinaryOperatorDefinition(_, _, _, _) |
            RynaExpr::NaryOperatorDefinition(_, _, _, _) |
            RynaExpr::ClassDefinition(_, _, _, _, _, _, _) |
            RynaExpr::InterfaceDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::InterfaceImplementation(_, _, _, _, _) |
            RynaExpr::PrefixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::PostfixOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::BinaryOperationDefinition(_, _, _, _, _, _, _, _) |
            RynaExpr::NaryOperationDefinition(_, _, _, _, _, _, _, _) => { },

            e => unreachable!("{:?}", e)
        }
    }

    pub fn constant_propagation(&self, body: &mut Vec<RynaExpr>) {
        let mut var_usages = FxHashMap::default();
        let mut consts = FxHashMap::default();
        let mut assigned_exprs = FxHashMap::default();

        // Compute constants
        for i in body.iter() {
            RynaContext::count_usages_expr(i, &mut var_usages, 1);
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

    pub fn late_optimize(&self, body: &mut Vec<RynaExpr>) {
        let mut var_offset = 0;

        for line in body.iter() {
            RynaContext::max_variable(line, &mut var_offset);
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

fn compute_labels(program: &mut [RynaInstruction]) {
    let mut labels = FxHashMap::default(); 
    let mut curr_idx = 0;

    // Generate labels
    for (idx, i) in program.iter_mut().enumerate() {
        match &mut i.instruction {
            CompiledRynaExpr::Lambda(to, _, _, _) |
            CompiledRynaExpr::CallDestructor(to) |
            CompiledRynaExpr::Call(to) |
            CompiledRynaExpr::Jump(to) => {
                if !labels.contains_key(to) {
                    labels.entry(*to).or_insert(curr_idx);
                    *to = curr_idx;
                    curr_idx += 1;

                } else {
                    *to = labels[to];
                }
            },
                    
            CompiledRynaExpr::RelativeJumpIfFalse(to, _) |
            CompiledRynaExpr::RelativeJumpIfTrue(to, _) => {
                let p = idx + *to;

                if !labels.contains_key(&p) {
                    labels.entry(p).or_insert(curr_idx);
                    *to = curr_idx;
                    curr_idx += 1;

                } else {
                    *to = labels[&p];
                }
            },
    
            CompiledRynaExpr::RelativeJump(to) => {
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

fn reassign_labels(program: &mut [RynaInstruction]) {
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
            CompiledRynaExpr::Lambda(to, _, _, _) |
            CompiledRynaExpr::Call(to) |
            CompiledRynaExpr::CallDestructor(to) |
            CompiledRynaExpr::Jump(to) => {
                *to = positions[to];
            },
                    
            CompiledRynaExpr::RelativeJumpIfFalse(to, _) |
            CompiledRynaExpr::RelativeJumpIfTrue(to, _) => {
                *to = positions[to] - idx;
            },
    
            CompiledRynaExpr::RelativeJump(to) => {
                *to = positions[&(*to as usize)] as i32 - idx as i32;
            },
    
            _ => { }
        }
    }
}

impl RynaContext {
    fn peephole_optimization(&self, program: &mut Vec<RynaInstruction>) {
        use CompiledRynaExpr::*;

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

                    [Int(obj), StoreVariable(id, g)] => { change_second!(StoreIntVariable(*id, *g, obj.clone())); },
                    [Str(obj), StoreVariable(id, g)] => { change_second!(StoreStringVariable(*id, *g, obj.clone())); },
                    [Float(obj), StoreVariable(id, g)] => { change_second!(StoreFloatVariable(*id, *g, *obj)); },
                    [Bool(obj), StoreVariable(id, g)] => { change_second!(StoreBoolVariable(*id, *g, *obj)); },

                    [GetVariable(id, g) | CloneVariable(id, g), Assign] if is_not_ref!(i) => { change_first!(AssignToVar(*id, *g)); },
                    [GetVariable(id, g) | CloneVariable(id, g), Assign] => { change_first!(AssignToVarDirect(*id, *g)); },

                    [GetVariable(id, g) | CloneVariable(id, g), Demut] => { change_first!(RefVariable(*id, *g)); },
                    [GetVariable(id, g) | CloneVariable(id, g), Copy] => { change_first!(CopyVariable(*id, *g)); },
                    [RefVariable(id, g), Copy] => { change_first!(CopyVariable(*id, *g)); },
                    [GetVariable(id, g) | CloneVariable(id, g), Deref] => { change_first!(DerefVariable(*id, *g)); },
                    [RefVariable(id, g), Deref] => { change_first!(DerefVariable(*id, *g)); },
                    [GetVariable(id, g) | CloneVariable(id, g), Move] => { change_first!(MoveVariable(*id, *g)); },
                    
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
                    [StoreVariable(id_1, g_1), MoveVariable(id_2, g_2)] if id_1 == id_2 && g_1 == g_2 && is_not_ref!(i) => { remove_both!(); },

                    _ => {}
                }
            }
        }
        
        reassign_labels(program);
    }

    fn remove_empty_calls(&self, program: &mut Vec<RynaInstruction>) {
        use CompiledRynaExpr::*;

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

    fn remove_single_relative_jumps(&self, program: &mut Vec<RynaInstruction>) {
        use CompiledRynaExpr::*;

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

    fn remove_jump_chains(&self, program: &mut [RynaInstruction]) {
        use CompiledRynaExpr::*;

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

    fn tail_call_optimization(&self, program: &mut Vec<RynaInstruction>) {
        use CompiledRynaExpr::*;

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

impl RynaContext {
    pub fn optimize_instructions(&self, program: &mut Vec<RynaInstruction>) {
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
    use crate::{compilation::{CompiledRynaExpr, RynaInstruction}, context::standard_ctx};

    #[test]
    fn peephole_optimization() {
        let ctx = standard_ctx();

        let mut program = vec!(
            RynaInstruction::from(CompiledRynaExpr::Jump(3)),
            RynaInstruction::from(CompiledRynaExpr::Jump(4)),
            RynaInstruction::from(CompiledRynaExpr::Jump(5)),
            RynaInstruction::from(CompiledRynaExpr::Jump(6)),
            RynaInstruction::from(CompiledRynaExpr::Not),
            RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfTrue(2, false)),
            RynaInstruction::from(CompiledRynaExpr::Empty),
            RynaInstruction::from(CompiledRynaExpr::Jump(4)),
            RynaInstruction::from(CompiledRynaExpr::Jump(5)),
            RynaInstruction::from(CompiledRynaExpr::Jump(6)),
            RynaInstruction::from(CompiledRynaExpr::Jump(7)),
            RynaInstruction::from(CompiledRynaExpr::Halt)
        );

        ctx.peephole_optimization(&mut program);

        assert_eq!(
            program,
            vec!(
                RynaInstruction::from(CompiledRynaExpr::Jump(3)),
                RynaInstruction::from(CompiledRynaExpr::Jump(4)),
                RynaInstruction::from(CompiledRynaExpr::Jump(4)),
                RynaInstruction::from(CompiledRynaExpr::Jump(5)),
                RynaInstruction::from(CompiledRynaExpr::RelativeJumpIfFalse(2, false)),
                RynaInstruction::from(CompiledRynaExpr::Empty),
                RynaInstruction::from(CompiledRynaExpr::Jump(4)),
                RynaInstruction::from(CompiledRynaExpr::Jump(4)),
                RynaInstruction::from(CompiledRynaExpr::Jump(5)),
                RynaInstruction::from(CompiledRynaExpr::Jump(6)),
                RynaInstruction::from(CompiledRynaExpr::Halt)
            )
        )
    }
}