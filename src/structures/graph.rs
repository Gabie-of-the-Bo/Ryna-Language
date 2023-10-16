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

            if let Some(conn) = self.connections.get(&idx) {
                return Ok(conn.iter().map(|(k, v)| (self.vertex_by_idx(*k), v)).collect());
            }
        }
        
        return Err("Vertex is not in the graph".into());
    }

    pub fn connect(&mut self, from: Vertex, to: Vertex, edge: Edge) {
        let idx_from = self.vertex_idx(from);
        let idx_to = self.vertex_idx(to);

        self.connections.entry(idx_from).or_default().insert((idx_to, edge));
    }

    pub fn dfs<F: FnMut(&Vertex)>(&self, start: &Vertex, mut op: F) {
        if self.vertices.contains_key(start) {
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
    }

    pub fn bfs<F: FnMut(&Vertex)>(&self, start: &Vertex, mut op: F) {
        if self.vertices.contains_key(start) {
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::DirectedGraph;

    #[test]
    fn graph_construction() {
        let mut g = DirectedGraph::new();

        g.connect("A", "B", 1);
        g.connect("A", "C", 2);
        g.connect("A", "D", 3);

        g.connect("B", "C", 2);
        g.connect("B", "D", 2);
        g.connect("B", "C", 3);

        g.connect("D", "D", 4);

        assert!(g.contains(&"A"));
        assert!(g.contains(&"B"));
        assert!(g.contains(&"C"));
        assert!(g.contains(&"D"));
        assert!(!g.contains(&"E"));

        assert_eq!(
            g.connections(&"A"),
            Ok([(&"B", &1), (&"C", &2), (&"D", &3)].iter().cloned().collect())
        );

        assert_eq!(
            g.connections(&"B"),
            Ok([(&"C", &2), (&"D", &2), (&"C", &3)].iter().cloned().collect())
        );

        assert!(g.connections(&"C").is_err());

        assert_eq!(
            g.connections(&"D"),
            Ok([(&"D", &4)].iter().cloned().collect())
        );

        let mut dfs_nodes = HashSet::new();
        g.dfs(&"A", |i| { dfs_nodes.insert(*i); });

        assert_eq!(dfs_nodes.len(), 4);

        let mut bfs_nodes = HashSet::new();
        g.bfs(&"A", |i| { bfs_nodes.insert(*i); });

        assert_eq!(bfs_nodes.len(), 4);

        assert_eq!(g.to_dot(|i| i.to_string()).len(), 90);
    }
}