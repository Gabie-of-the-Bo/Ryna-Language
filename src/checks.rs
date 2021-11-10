use crate::context::NessaContext;
use crate::parser::NessaExpr;
use crate::operations::Operator;
use crate::types::Type;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    pub fn ambiguity_check(&self, expr: &NessaExpr) -> Result<(), String> {
        return match expr {
            NessaExpr::Literal(_) |
            NessaExpr::Variable(..) |
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) => Ok(()),

            NessaExpr::CompiledVariableDefinition(_, n, t, e) |
            NessaExpr::CompiledVariableAssignment(_, n, t, e) => {
                self.ambiguity_check(e)?;

                let inferred_type = self.infer_type(e);

                if let Some(it) = inferred_type {
                    if it.bindable_to(t) {
                        Ok(())

                    } else{
                        Err(format!(
                            "Unable to bind value of type {} to variable \"{}\", which is of type {}",
                            it.get_name(self),
                            n,
                            t.get_name(self)
                        ))
                    }

                } else {
                    Err("Unable to infer return value of right-hand of assignment".into())
                }
            },

            NessaExpr::FunctionCall(id, _ , args) => {
                let mut arg_types = Vec::with_capacity(args.len());

                for (i, arg) in args.iter().enumerate() {
                    self.ambiguity_check(arg)?;

                    if let Some(t) = self.infer_type(arg) {
                        arg_types.push(t);
                    
                    } else {
                        return Err(format!("Unable to infer return value for argument with index {}", i))
                    }
                }

                if let Some(ov) = self.is_function_overload_ambiguous(*id, arg_types.clone()) {
                    let f_name = &self.functions[*id].name;
                    let possibilities = ov.iter().map(|(a, r)| format!("\t-> {}{} -> {}", f_name, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();

                    Err(format!(
                        "Function call {}({}) is ambiguous (found {} possibilities):\n{}",
                        f_name,
                        arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                        possibilities.len(),
                        possibilities.join("\n")
                    ))

                } else {
                    Ok(())
                }
            },

            NessaExpr::UnaryOperation(id, arg) => {
                self.ambiguity_check(arg)?;

                let inferred_type = self.infer_type(arg);

                if let Some(t) = inferred_type {
                    if let Some(ov) = self.is_unary_op_ambiguous(*id, t.clone()) {
                        if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*id] {
                            if *prefix {
                                let possibilities = ov.iter().map(|(a, r)| format!("\t-> {}({}) -> {}", representation, a.get_name(self), r.get_name(self))).collect::<Vec<_>>();
        
                                Err(format!(
                                    "Unary operation {}({}) is ambiguous (found {} possibilities):\n{}",
                                    representation,
                                    t.get_name(self),
                                    possibilities.len(),
                                    possibilities.join("\n")
                                ))

                            } else {
                                let possibilities = ov.iter().map(|(a, r)| format!("\t-> ({}){} -> {}", a.get_name(self), representation, r.get_name(self))).collect::<Vec<_>>();
            
                                Err(format!(
                                    "Unary operation ({}){} is ambiguous (found {} possibilities):\n{}",
                                    t.get_name(self),
                                    representation,
                                    possibilities.len(),
                                    possibilities.join("\n")
                                ))
                            }
                            
                        } else {
                            unreachable!();
                        }
    
                    } else {
                        Ok(())
                    }

                } else {
                    if let Operator::Unary{representation, ..} = &self.unary_ops[*id] {
                        Err(format!("Unable to infer return value of argument of unary operator {}", representation))

                    } else {
                        unreachable!();
                    }
                }
            },

            NessaExpr::BinaryOperation(id, arg1, arg2) => {
                self.ambiguity_check(arg1)?;
                self.ambiguity_check(arg2)?;

                let inferred_type_1 = self.infer_type(arg1);
                let inferred_type_2 = self.infer_type(arg2);

                if let Some(t1) = inferred_type_1 {
                    if let Some(t2) = inferred_type_2 {
                        if let Some(ov) = self.is_binary_op_ambiguous(*id, t1.clone(), t2.clone()) {
                            if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                                let possibilities = ov.iter()
                                    .map(|(a1, a2, r)| format!("\t-> ({}){}({}) -> {}", a1.get_name(self), representation, a2.get_name(self), r.get_name(self)))
                                    .collect::<Vec<_>>();
                
                                Err(format!(
                                    "Binary operation ({}){}({}) is ambiguous (found {} possibilities):\n{}",
                                    t1.get_name(self),
                                    representation, 
                                    t2.get_name(self),
                                    possibilities.len(),
                                    possibilities.join("\n")
                                ))
                                
                            } else {
                                unreachable!();
                            }
        
                        } else {
                            Ok(())
                        }
                        
                    } else {
                        if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                            Err(format!("Unable to infer return value of right argument of binary operator {}", representation))
    
                        } else {
                            unreachable!();
                        }
                    }
                    
                } else {
                    if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                        Err(format!("Unable to infer return value of left argument of binary operator {}", representation))

                    } else {
                        unreachable!();
                    }
                }
            },

            NessaExpr::NaryOperation(id, _, first, args) => {
                self.ambiguity_check(first)?;

                let first_type = self.infer_type(first);

                if let Some(t) = first_type {
                    let mut arg_types = Vec::with_capacity(args.len());

                    for (i, arg) in args.iter().enumerate() {
                        self.ambiguity_check(arg)?;
    
                        if let Some(t) = self.infer_type(arg) {
                            arg_types.push(t);
                        
                        } else {
                            return Err(format!("Unable to infer return value for argument with index {}", i))
                        }
                    }
    
                    if let Some(ov) = self.is_nary_op_ambiguous(*id, t.clone(), arg_types.clone()) {
                        if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                            let possibilities = ov.iter()
                                .map(|(f, a, r)| 
                                    format!(
                                        "\t-> {}{}{}{} -> {}", 
                                        f.get_name(self), 
                                        open_rep,
                                        a.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                        close_rep,
                                        r.get_name(self)
                                    )
                                )
                                .collect::<Vec<_>>();
        
                            Err(format!(
                                "Function call {}{}{}{} is ambiguous (found {} possibilities):\n{}",
                                t.get_name(self), 
                                open_rep,
                                arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                close_rep,
                                possibilities.len(),
                                possibilities.join("\n")
                            ))

                        } else {
                            unreachable!()
                        }
    
                    } else {
                        Ok(())
                    }

                } else {
                    if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                        Err(format!("Unable to infer return value of first argument of n-ary operator {}{}", open_rep, close_rep))

                    } else {
                        unreachable!()
                    }
                }
            },

            NessaExpr::If(ih, ib, ei, eb) => {
                self.ambiguity_check(ih)?;

                let if_header_type = self.infer_type(ih);

                if let Some(t) = if_header_type {
                    if t != Type::Basic(2) {
                        return Err(format!("If condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                    }

                } else {
                    return Err("Unable to infer return value of if condition".into())
                }

                for line in ib {
                    self.ambiguity_check(line)?;
                }

                for (ei_h, ei_b) in ei {
                    self.ambiguity_check(ei_h)?;

                    let elif_header_type = self.infer_type(ei_h);

                    if let Some(t) = elif_header_type {
                        if t != Type::Basic(2) {
                            return Err(format!("If condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                        }
    
                    } else {
                        return Err("Unable to infer return value of if condition".into())
                    }

                    for line in ei_b {
                        self.ambiguity_check(line)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.ambiguity_check(line)?;
                    }
                }

                Ok(())
            },

            NessaExpr::While(cond, body) => {
                self.ambiguity_check(cond)?;

                let while_header_type = self.infer_type(cond);

                if let Some(t) = while_header_type {
                    if t != Type::Basic(2) {
                        return Err(format!("While condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                    }

                } else {
                    return Err("Unable to infer return value of while condition".into())
                }

                for line in body {
                    self.ambiguity_check(line)?;
                }

                Ok(())
            },

            NessaExpr::Return(e) => {
                self.ambiguity_check(e)?;

                if self.infer_type(e).is_some() {
                    Ok(())

                } else {
                    return Err("Unable to infer return value of return statement".into())
                }
            }

            NessaExpr::CompiledFunctionDefinition(_, t, _, _, b, _) => {
                if t.is_empty() {
                    for line in b {
                        self.ambiguity_check(line)?;
                    }
                }

                Ok(())
            },

            NessaExpr::CompiledPrefixOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledPostfixOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledBinaryOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledNaryOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledFor(_, _, _, _, b) => {
                for line in b {
                    self.ambiguity_check(line)?;
                }

                Ok(())
            }

            _ => unimplemented!("{:?}", expr)
        }
    }

    pub fn type_check(&self, expr: &NessaExpr) -> Result<(), String> {
        return match expr {
            NessaExpr::Literal(_) |
            NessaExpr::Variable(..) |
            NessaExpr::PrefixOperatorDefinition(..) |
            NessaExpr::PostfixOperatorDefinition(..) |
            NessaExpr::BinaryOperatorDefinition(..) |
            NessaExpr::NaryOperatorDefinition(..) => Ok(()),

            NessaExpr::CompiledVariableDefinition(_, n, t, e) |
            NessaExpr::CompiledVariableAssignment(_, n, t, e) => {
                self.type_check(e)?;

                let inferred_type = self.infer_type(e);

                if let Some(it) = inferred_type {
                    if it.bindable_to(t) {
                        Ok(())

                    } else{
                        Err(format!(
                            "Unable to bind value of type {} to variable \"{}\", which is of type {}",
                            it.get_name(self),
                            n,
                            t.get_name(self)
                        ))
                    }

                } else {
                    Err("Unable to infer return value of right-hand of assignment".into())
                }
            },

            NessaExpr::FunctionCall(id, _ , args) => {
                let mut arg_types = Vec::with_capacity(args.len());

                for (i, arg) in args.iter().enumerate() {
                    self.type_check(arg)?;

                    if let Some(t) = self.infer_type(arg) {
                        arg_types.push(t);
                    
                    } else {
                        return Err(format!("Unable to infer return value for argument with index {}", i))
                    }
                }

                if self.get_first_function_overload(*id, arg_types.clone()).is_none() {
                    Err(format!(
                        "Unable to get function overload for {}({})",
                        self.functions[*id].name,
                        arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", ")
                    ))

                } else {
                    Ok(())
                }
            },

            NessaExpr::UnaryOperation(id, arg) => {
                self.type_check(arg)?;

                let inferred_type = self.infer_type(arg);

                if let Some(t) = inferred_type {
                    if self.get_first_unary_op(*id, t.clone()).is_none() {
                        if let Operator::Unary{representation, prefix, ..} = &self.unary_ops[*id] {
                            if *prefix {
                                Err(format!(
                                    "Unable to get unary operator overload for {}({})",
                                    representation,
                                    t.get_name(self)
                                ))

                            } else {
                                Err(format!(
                                    "Unable to get unary operator overload for ({}){}",
                                    t.get_name(self),
                                    representation
                                ))
                            }

                        } else {
                            unreachable!()
                        }

                    } else {
                        Ok(())
                    }

                } else {
                    if let Operator::Unary{representation, ..} = &self.unary_ops[*id] {
                        Err(format!("Unable to infer return value of argument of unary operator {}", representation))

                    } else {
                        unreachable!();
                    }
                }
            },

            NessaExpr::BinaryOperation(id, arg1, arg2) => {
                self.type_check(arg1)?;
                self.type_check(arg2)?;

                let inferred_type_1 = self.infer_type(arg1);
                let inferred_type_2 = self.infer_type(arg2);

                if let Some(t1) = inferred_type_1 {
                    if let Some(t2) = inferred_type_2 {
                        if self.get_first_binary_op(*id, t1.clone(), t2.clone()).is_none() {
                            if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                                Err(format!(
                                    "Unable to get binary operator overload for ({}){}({})",
                                    t1.get_name(self),
                                    representation,
                                    t2.get_name(self)
                                ))
    
                            } else {
                                unreachable!()
                            }
    
                        } else {
                            Ok(())
                        }
                        
                    } else {
                        if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                            Err(format!("Unable to infer return value of right argument of binary operator {}", representation))
    
                        } else {
                            unreachable!();
                        }
                    }
                    
                } else {
                    if let Operator::Binary{representation, ..} = &self.binary_ops[*id] {
                        Err(format!("Unable to infer return value of left argument of binary operator {}", representation))

                    } else {
                        unreachable!();
                    }
                }
            },

            NessaExpr::NaryOperation(id, _, first, args) => {
                self.type_check(first)?;

                let first_type = self.infer_type(first);

                if let Some(t) = first_type {
                    let mut arg_types = Vec::with_capacity(args.len());

                    for (i, arg) in args.iter().enumerate() {
                        self.type_check(arg)?;
    
                        if let Some(t) = self.infer_type(arg) {
                            arg_types.push(t);
                        
                        } else {
                            return Err(format!("Unable to infer return value for argument with index {}", i))
                        }
                    }
    
                    if self.get_first_nary_op(*id, t.clone(), arg_types.clone()).is_none() {
                        if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                            Err(format!(
                                "Unable to get n-ary operator overload for {}{}{}{}",
                                t.get_name(self),
                                open_rep,
                                arg_types.iter().map(|i| i.get_name(self)).collect::<Vec<_>>().join(", "),
                                close_rep
                            ))

                        } else {
                            unreachable!()
                        }
    
                    } else {
                        Ok(())
                    }

                } else {
                    if let Operator::Nary{open_rep, close_rep, ..} = &self.nary_ops[*id] {
                        Err(format!("Unable to infer return value of first argument of n-ary operator {}{}", open_rep, close_rep))

                    } else {
                        unreachable!()
                    }
                }
            },

            NessaExpr::If(ih, ib, ei, eb) => {
                self.type_check(ih)?;

                let if_header_type = self.infer_type(ih);

                if let Some(t) = if_header_type {
                    if t != Type::Basic(2) {
                        return Err(format!("If condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                    }

                } else {
                    return Err("Unable to infer return value of if condition".into())
                }

                for line in ib {
                    self.type_check(line)?;
                }

                for (ei_h, ei_b) in ei {
                    self.type_check(ei_h)?;

                    let elif_header_type = self.infer_type(ei_h);

                    if let Some(t) = elif_header_type {
                        if t != Type::Basic(2) {
                            return Err(format!("If condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                        }
    
                    } else {
                        return Err("Unable to infer return value of if condition".into())
                    }

                    for line in ei_b {
                        self.type_check(line)?;
                    }
                }

                if let Some(eb_inner) = eb {
                    for line in eb_inner {
                        self.type_check(line)?;
                    }
                }

                Ok(())
            },

            NessaExpr::While(cond, body) => {
                self.type_check(cond)?;

                let while_header_type = self.infer_type(cond);

                if let Some(t) = while_header_type {
                    if t != Type::Basic(2) {
                        return Err(format!("While condition inferred to be of type {} (expected Bool)", t.get_name(self)))
                    }

                } else {
                    return Err("Unable to infer return value of while condition".into())
                }

                for line in body {
                    self.type_check(line)?;
                }

                Ok(())
            },

            NessaExpr::Return(e) => {
                self.type_check(e)?;

                if self.infer_type(e).is_some() {
                    Ok(())

                } else {
                    return Err("Unable to infer return value of return statement".into())
                }
            }

            NessaExpr::CompiledFunctionDefinition(_, t, _, _, b, _) => {
                if t.is_empty() {
                    for line in b {
                        self.type_check(line)?;
                    }
                }

                Ok(())
            },

            NessaExpr::CompiledPrefixOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledPostfixOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledBinaryOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledNaryOperationDefinition(_, _, _, _, b, _) |
            NessaExpr::CompiledFor(_, _, _, _, b) => {
                for line in b {
                    self.type_check(line)?;
                }

                Ok(())
            }

            _ => unimplemented!("{:?}", expr)
        };
    }

    pub fn static_check(&self, expr: &NessaExpr) -> Result<(), String> {
        self.type_check(expr)?;
        self.ambiguity_check(expr)?;

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
    use crate::context::*;

    #[test]
    fn type_checks() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let n: Number = 10;

            let a: Number = 5 + n;
            let b: String = \"Test\";
            let c: Array<Number> = arr<Number>();

            a = 3;
            b = \"Test 2\";
            c = arr<Number>();
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: String = 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Number = 5;

            a = \"Test\";
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Array<Number> = 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());

        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Array<Number> = arr<Number>();

            a = arr<String>();
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn function_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn inc(a: String) -> &&String {
                return a;
            }

            let a: Number = 5;

            a.inc();
            inc(\"Test\");
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            fn test(a: Bool | String) -> String {
                return \"Test\";
            }
            
            fn test(a: Bool | Number) -> String {
                return \"Test\";
            }

            test(true);
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn unary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op !(a: String) -> &&String {
                return a;
            }

            !\"Test\";
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op !(a: Number | String) -> String {
                return \"Test\";
            }
            
            op !(a: Number | Array<*>) -> String {
                return \"Test\";
            }

            !5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn binary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String) + (b: Bool) -> &&String {
                return a;
            }

            \"Test\" + true;
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String) + (b: Number | Bool) -> String {
                return \"Test\";
            }
            
            op (a: String) + (b: Number | Array<*>) -> String {
                return \"Test\";
            }

            \"Test\" + 5;
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }

    #[test]
    fn nary_ambiguity_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String)[b: Bool] -> &&String {
                return a;
            }

            \"Test\"[true];
        ".to_string();

        ctx.parse_and_compile(&code_str).unwrap();
        let mut ctx = standard_ctx();
        
        let code_str = "
            op (a: String)[b: Bool | String] -> String {
                return \"Test\";
            }
            
            op (a: String)[b: Bool | Array<*>] -> String {
                return \"Test\";
            }

            \"Test\"[true];
        ".to_string();

        assert!(ctx.parse_and_compile(&code_str).is_err());
    }
}