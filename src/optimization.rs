use rustc_hash::FxHashMap;

use crate::{compilation::{CompiledNessaExpr, NessaInstruction}, context::NessaContext, number::{Integer, ONE}, object::Object, operations::{ADD_BINOP_ID, ASSIGN_BINOP_ID, DEREF_UNOP_ID, MUL_BINOP_ID, SHL_BINOP_ID, SUB_BINOP_ID}, parser::{Location, NessaExpr}, types::{Type, INT}};

/*
    ╒═══════════════════════════╕
    │ Syntax tree optimizations │
    ╘═══════════════════════════╛
*/

const INLINE_THRESHOLD: f32 = 50.0;

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

    pub fn inlining_weight(&self, body: &Vec<NessaExpr>) -> f32 {
        self.compiled_form_body_size(body, false).unwrap() as f32
    }

    pub fn max_variable(&self, expr: &NessaExpr, offset: &mut usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *offset = (*offset).max(*id);
            },

            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, id, _, _, e) => {
                *offset = (*offset).max(*id);
                self.max_variable(e, offset)
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) => self.max_variable(e, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.max_variable(e, offset);
                }
            },

            NessaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *offset = (*offset).max(*iterator_idx);
                *offset = (*offset).max(*element_idx);

                self.max_variable(c, offset);

                for e in exprs {
                    self.max_variable(e, offset);
                }
            },

            NessaExpr::While(_, c, exprs) => {
                self.max_variable(c, offset);

                for e in exprs {
                    self.max_variable(e, offset);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.max_variable(c, offset);

                for e in exprs {
                    self.max_variable(e, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.max_variable(a, offset);
                self.max_variable(b, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.max_variable(ic, offset);
                
                for e in ib {
                    self.max_variable(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    self.max_variable(ei_h, offset);
                    
                    for e in ei_b {
                        self.max_variable(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.max_variable(e, offset);
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

    pub fn offset_variables(&self, expr: &mut NessaExpr, offset: usize) {
        match expr {
            NessaExpr::Variable(_, id, _, _) => {
                *id += offset;
            },

            NessaExpr::CompiledVariableDefinition(_, id, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, id, _, _, e) => {
                *id += offset;
                self.offset_variables(e, offset)
            },

            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) => self.offset_variables(e, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::FunctionCall(_, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                for e in exprs {
                    self.offset_variables(e, offset);
                }
            },

            NessaExpr::CompiledFor(_, iterator_idx, element_idx, _, c, exprs) => {
                *iterator_idx += offset;
                *element_idx += offset;

                self.offset_variables(c, offset);
                
                for e in exprs {
                    self.offset_variables(e, offset);
                }
            }

            NessaExpr::While(_, c, exprs) => {
                self.offset_variables(c, offset);

                for e in exprs {
                    self.offset_variables(e, offset);
                }
            },

            NessaExpr::NaryOperation(_, _, _, c, exprs) => {
                self.offset_variables(c, offset);

                for e in exprs {
                    self.offset_variables(e, offset);
                }
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.offset_variables(a, offset);
                self.offset_variables(b, offset);
            },

            NessaExpr::If(_, ic, ib, ei, eb) => {
                self.offset_variables(ic, offset);
                
                for e in ib {
                    self.offset_variables(e, offset);
                }

                for (ei_h, ei_b) in ei {
                    self.offset_variables(ei_h, offset);
                    
                    for e in ei_b {
                        self.offset_variables(e, offset);
                    }
                }

                if let Some(inner) = eb {
                    for e in inner {
                        self.offset_variables(e, offset);
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

    pub fn inline_body(&self, mut body: Vec<NessaExpr>, args: Vec<NessaExpr>, var_offset: &mut usize, l: &Location) -> Vec<NessaExpr> {
        let mut res = vec!();

        // Get number of vars
        let arg_num = args.len();
        let mut func_offset = 0;

        for line in body.iter() {
            self.max_variable(line, &mut func_offset);
        }

        // Define variables
        for (idx, arg) in args.into_iter().enumerate() {
            res.push(NessaExpr::CompiledVariableAssignment(
                l.clone(),
                idx + *var_offset + 1,
                format!("__arg_{}__", idx + *var_offset + 1),
                self.infer_type(&arg).unwrap(),
                Box::new(arg)
            ))
        }

        // Map body
        for line in body.iter_mut() {
            self.offset_variables(line, *var_offset + 1);
        }

        res.append(&mut body);

        // Update variable number
        *var_offset += func_offset + arg_num + 1;

        res
    }

    pub fn inline_functions_expr(&self, expr: &mut NessaExpr, offset: &mut usize) {
        match expr {
            NessaExpr::UnaryOperation(_, _, _, e) |
            NessaExpr::Return(_, e) |
            NessaExpr::CompiledVariableDefinition(_, _, _, _, e) |
            NessaExpr::CompiledVariableAssignment(_, _, _, _, e) => self.inline_functions_expr(e, offset),

            NessaExpr::DoBlock(_, exprs, _) |
            NessaExpr::CompiledLambda(_, _, _, _, exprs) |
            NessaExpr::Tuple(_, exprs) => {
                self.inline_functions(exprs, offset);
            },

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

            NessaExpr::NaryOperation(_, _, _, c, exprs) |
            NessaExpr::CompiledFor(_, _, _, _, c, exprs) |
            NessaExpr::While(_, c, exprs) => {
                self.inline_functions_expr(c, offset);
                self.inline_functions(exprs, offset);
            },

            NessaExpr::BinaryOperation(_, _, _, a, b) => {
                self.inline_functions_expr(a, offset);
                self.inline_functions_expr(b, offset);
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

    pub fn late_optimize(&self, body: &mut Vec<NessaExpr>) {
        let mut var_offset = 0;

        for line in body.iter() {
            self.max_variable(line, &mut var_offset);
        }

        self.inline_functions(body, &mut var_offset);
    }
}

/*
    ╒════════════════════════╕
    │ Peephole optimizations │
    ╘════════════════════════╛
*/

fn compute_labels(program: &mut Vec<NessaInstruction>) {
    let mut labels = FxHashMap::default(); 
    let mut curr_idx = 0;

    // Generate labels
    for (idx, i) in program.iter_mut().enumerate() {
        match &mut i.instruction {
            CompiledNessaExpr::Lambda(to, _, _) |
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
        program[*line].labels.insert(*tag);
    }
}

fn reassign_labels(program: &mut Vec<NessaInstruction>) {
    let mut positions = FxHashMap::default(); 

    // Generate label positions
    for (idx, i) in program.iter_mut().enumerate() {
        for l in &i.labels {
            positions.entry(*l).or_insert(idx);
        }

        i.labels.clear();
    }

    // Recompute positions
    for (idx, i) in program.iter_mut().enumerate() {
        match &mut i.instruction {
            CompiledNessaExpr::Lambda(to, _, _) |
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
                let labels_to_remove = program[$idx].labels.clone();
                program[$idx + 1].labels.extend(&labels_to_remove);
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
            let labels_to_remove = program[line].labels.clone();
            program[line + 1].labels.extend(&labels_to_remove);
            program.remove(line);
        }

        reassign_labels(program);
    }

    fn remove_single_relative_jumps(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        let mut lines_to_remove = vec!();

        // Look for empty calls
        for (idx, i) in program.iter().enumerate() {
            match i.instruction {
                RelativeJump(1) => lines_to_remove.push(idx),
                _ => { }
            }
        }

        lines_to_remove.sort();
        lines_to_remove.dedup();

        // Remove lines
        compute_labels(program);

        for line in lines_to_remove.into_iter().rev() {
            let labels_to_remove = program[line].labels.clone();
            program[line + 1].labels.extend(&labels_to_remove);
            program.remove(line);
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