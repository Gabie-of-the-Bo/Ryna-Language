use std::hash::Hash;

use rustc_hash::FxHashMap;

use crate::compilation::{CompiledNessaExpr, NessaInstruction};

type Jump = (usize, usize); // (from, to) tuples

#[derive(Debug, Default)]
pub struct JumpMap<Data, Key> where Data: Default, Key: Default + Eq + Hash + Clone {
    jumps: Vec<Jump>,
    data: Vec<Data>,
    keys: FxHashMap<Key, usize>
}

impl JumpMap<(), ()> {
    pub fn from_code(code: &Vec<NessaInstruction>) -> JumpMap<(), ()> {
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
}

impl<Data, Key> JumpMap<Data, Key> where Data: Default, Key: Default + Eq + Hash + Clone {
    pub fn get_range(&self, k: &Key) -> Option<&Jump> {
        self.jumps.get(*self.keys.get(k)?)
    }

    pub fn get_data(&self, k: &Key) -> &Data {
        &self.data[self.keys[k]]
    }

    pub fn get_data_mut(&mut self, k: &Key) -> &mut Data {
        &mut self.data[self.keys[k]]
    }

    // Get the data that corresponds to the range of the given line
    pub fn get_data_by_loc(&self, line: usize) -> &Data {
        let idx = self.jumps.iter()
                            .enumerate()
                            .filter(|(_, (s, e))| line >= *s && line < *e)
                            .next().unwrap().0;

        &self.data[idx]
    }

    pub fn get_data_by_loc_mut(&mut self, line: usize) -> &mut Data {
        let idx = self.jumps.iter()
                            .enumerate()
                            .filter(|(_, (s, e))| line >= *s && line < *e)
                            .next().unwrap().0;

        &mut self.data[idx]
    }

    pub fn from_ranges(ranges: Vec<Jump>, data: Vec<Data>, keys: Vec<Key>) -> JumpMap<Data, Key> {
        JumpMap { jumps: ranges, data, keys: keys.into_iter().enumerate().map(|(a, b)| (b, a)).collect() }
    }

    pub fn remove_lines(&mut self, line: usize, amount: usize) -> Vec<(Jump, Jump)> {
        let mut changes = vec!();

        let to_retain = self.jumps.iter().map(|(from, _)| !(*from >= line && *from < (line + amount))).collect::<Vec<_>>();

        // Create key correspondence
        let key_corr = self.keys.iter().map(|(k, v)| (self.jumps[*v], k.clone())).collect::<FxHashMap<_, _>>();
        
        // Remove jumps in range
        let mut idx = 0;
        self.jumps.retain(|_| { idx += 1; to_retain[idx - 1] });

        let mut idx = 0;
        self.data.retain(|_| { idx += 1; to_retain[idx - 1] });

        // Update key map
        if !self.keys.is_empty() {
            self.keys.clear();

            for (idx, jump) in self.jumps.iter().enumerate() {
                self.keys.insert(key_corr[jump].clone(), idx);
            }
        }

        // Update jumps
        for (from, to) in self.jumps.iter_mut() {
            let mut changed = false;
            let prev = (*from, *to);

            if *to > line {
                *to -= amount;
                changed = true;
            }

            if *from > line {
                *from -= amount;
                changed = true;
            }

            if changed {
                changes.push((prev, (*from, *to)));
            }
        }

        changes
    }

    pub fn remove_line(&mut self, line: usize) -> Vec<(Jump, Jump)> {
        self.remove_lines(line, 1)
    }

    pub fn add_lines(&mut self, line: usize, amount: usize) -> Vec<(Jump, Jump)> {
        let mut changes = vec!();

        for (from, to) in self.jumps.iter_mut() {
            let mut changed = false;
            let prev = (*from, *to);

            if *to > line {
                *to += amount;
                changed = true;
            }

            if *from > line {
                *from += amount;
                changed = true;
            }

            if changed {
                changes.push((prev, (*from, *to)));
            }
        }

        changes
    }

    pub fn add_line(&mut self, line: usize) -> Vec<(Jump, Jump)> {
        self.add_lines(line, 1)
    }
}