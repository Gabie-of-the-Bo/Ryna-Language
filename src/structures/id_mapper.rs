use rustc_hash::FxHashMap;

#[derive(Default)]
pub struct IdMapper {
    pub functions: FxHashMap<usize, usize>,
    pub unary_operators: FxHashMap<usize, usize>,
    pub binary_operators: FxHashMap<usize, usize>,
    pub nary_operators: FxHashMap<usize, usize>,
    pub classes: FxHashMap<usize, usize>,
    pub interfaces: FxHashMap<usize, usize>
}