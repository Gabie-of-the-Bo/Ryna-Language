use std::{collections::{HashMap, HashSet}, hash::Hash};

#[derive(Debug, Clone)]
pub struct DirectedGraph<Vertex: Eq + Hash + Clone, Edge: Eq + Hash> {
    vertices: HashMap<Vertex, usize>,
    idxs: HashMap<usize, Vertex>,

    connections: HashMap<usize, HashSet<(usize, Edge)>>
}

impl<Vertex: Eq + Hash + Clone, Edge: Eq + Hash> DirectedGraph<Vertex, Edge> {
    pub fn new() -> Self {
        return DirectedGraph { 
            vertices: HashMap::new(), 
            idxs: HashMap::new(), 
            connections: HashMap::new() 
        };
    }

    pub fn contains(&self, v: &Vertex) -> bool {
        return self.vertices.contains_key(v);
    }

    fn vertex_by_idx(&self, idx: usize) -> &Vertex {
        return self.idxs.get(&idx).unwrap();
    }

    fn vertex_idx_unchecked(&self, v: &Vertex) -> usize {
        return *self.vertices.get(v).unwrap();
    }

    fn vertex_idx(&mut self, v: Vertex) -> usize {
        if let Some(idx) = self.vertices.get(&v) {
            return *idx;

        } else {
            let idx = self.vertices.len();
            self.vertices.insert(v.clone(), idx);
            self.idxs.insert(idx, v);

            return idx;
        }
    }

    pub fn connections(&self, v: &Vertex) -> Result<HashSet<(&Vertex, &Edge)>, String> {
        if self.contains(v) {
            let idx = self.vertex_idx_unchecked(v);
            return Ok(self.connections.get(&idx).unwrap().iter().map(|(k, v)| (self.vertex_by_idx(*k), v)).collect());
        }
        
        return Err("Vertex is not in the graph".into());
    }

    pub fn connect(&mut self, from: Vertex, to: Vertex, edge: Edge) {
        let idx_from = self.vertex_idx(from);
        let idx_to = self.vertex_idx(to);

        self.connections.entry(idx_from).or_default().insert((idx_to, edge));
    }

    pub fn dfs<F: FnMut(&Vertex)>(&self, start: &Vertex, mut op: F) {
        let mut seen: HashSet<usize> = HashSet::new();
        let mut stack = vec!(self.vertex_idx_unchecked(start));

        while stack.len() > 0 {
            let elem = stack.pop().unwrap();
            seen.insert(elem);

            op(self.vertex_by_idx(elem));

            if self.connections.contains_key(&elem) {
                for (i, _) in self.connections.get(&elem).unwrap() {
                    if !seen.contains(i) {
                        stack.push(*i);
                    }
                }
            }
        }
    }

    pub fn bfs<F: FnMut(&Vertex)>(&self, start: &Vertex, mut op: F) {
        let mut seen: HashSet<usize> = HashSet::new();
        let mut layer = vec!(self.vertex_idx_unchecked(start));

        while layer.len() > 0 {
            let mut new_layer = vec!();
            
            for elem in &layer {
                seen.insert(*elem);
                op(self.vertex_by_idx(*elem));

                if self.connections.contains_key(&elem) {
                    for (i, _) in self.connections.get(&elem).unwrap() {
                        if !seen.contains(i) {
                            new_layer.push(*i);
                        }
                    }
                }
            }

            layer = new_layer;
        }
    }

    pub fn to_dot<F: Fn(&Vertex) -> String>(&self, repr: F) -> String {
        let mut lines = vec!();

        for (from, dest) in &self.connections {
            let rf = repr(self.vertex_by_idx(*from));
            for (to, _) in dest {
                let rt = repr(self.vertex_by_idx(*to));
                lines.push(format!("\"{}\" -> \"{}\"", rf, rt));
            }
        }

        return format!("digraph G {{\n{}\n}}", lines.join("\n"));
    }
}