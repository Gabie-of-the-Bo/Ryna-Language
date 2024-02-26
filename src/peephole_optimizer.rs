use crate::{compilation::{CompiledNessaExpr, NessaInstruction}, context::NessaContext, jump_map::JumpMap};

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
    pub fn peephole_optimization(&self, program: &mut Vec<NessaInstruction>) {
        use CompiledNessaExpr::*;

        let mut jumps = JumpMap::from_code(&program);
        let mut changed = true;

        macro_rules! remove_instruction {
            ($idx: expr) => {
                program.remove($idx);

                for change in jumps.remove_line($idx) {
                    apply_jump_change(change, program);
                }
            };
        }

        while changed {
            changed = false;

            // Delete size 1 jumps
            for i in 0..program.len() {
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
    }
}


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