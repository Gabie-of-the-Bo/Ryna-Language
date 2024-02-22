use crate::compilation::{CompiledNessaExpr, NessaInstruction};

type Jump = (usize, usize); // (from, to) tuples

#[derive(Debug, Default)]
pub struct JumpMap {
    jumps: Vec<Jump>
}

impl JumpMap {
    pub fn from_code(code: &Vec<NessaInstruction>) -> Self {
        let mut res = JumpMap::default();

        // Generate jumps
        for (idx, i) in code.iter().enumerate() {
            match i.instruction {
                CompiledNessaExpr::Lambda(to, _, _) |
                CompiledNessaExpr::Call(to) |
                CompiledNessaExpr::Jump(to) => res.jumps.push((idx, to)),
                
                CompiledNessaExpr::RelativeJumpIfFalse(to, _) |
                CompiledNessaExpr::RelativeJumpIfTrue(to, _) => res.jumps.push((idx, idx + to)),

                CompiledNessaExpr::RelativeJump(to) => res.jumps.push((idx, (idx as i32 + to) as usize)),

                _ => {}
            }
        }

        return res;
    }

    pub fn remove_line(&mut self, line: usize) -> Vec<Jump> {
        let mut changes = vec!();

        self.jumps.retain(|(from, _)| *from != line);

        for (from, to) in self.jumps.iter_mut() {
            let mut changed = false;

            if *to > line {
                *to -= 1;
                changed = true;
            }

            if *from > line {
                *from -= 1;
                changed = true;
            }

            if changed {
                changes.push((*from, *to));
            }
        }

        changes
    }
}