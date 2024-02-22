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

        while changed {
            changed = false;

            // Look for optimizations
            for i in 0..(program.len() - 1) {
                // Size 2
                match [&program[i].instruction, &program[i + 1].instruction] {
                    [Not, RelativeJumpIfFalse(offset, false)] => {
                        program[i + 1].instruction = RelativeJumpIfTrue(*offset, false);
                        program.remove(i);

                        for change in jumps.remove_line(i) {
                            apply_jump_change(change, program);
                        }

                        changed = true;
                        break;
                    }
    
                    [Not, RelativeJumpIfTrue(offset, false)] => {
                        program[i + 1].instruction = RelativeJumpIfFalse(*offset, false);
                        program.remove(i);

                        for change in jumps.remove_line(i) {
                            apply_jump_change(change, program);
                        }

                        changed = true;
                        break;
                    }
    
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