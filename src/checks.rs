use crate::context::NessaContext;
use crate::parser::NessaExpr;

/*
                                                  ╒══════════════════╕
    ============================================= │  IMPLEMENTATION  │ =============================================
                                                  ╘══════════════════╛
*/

impl NessaContext {
    pub fn type_check(&self, expr: &NessaExpr) -> Result<(), String> {
        return match expr {
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
            _ => Ok(())
        };
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
    fn assignment_type_check() {
        let mut ctx = standard_ctx();
        
        let code_str = "
            let a: Number = 5;
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
}