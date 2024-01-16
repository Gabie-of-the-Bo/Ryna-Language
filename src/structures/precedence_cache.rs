use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct PrecedenceCache<T> {
    inner: FxHashMap<usize, (Vec<usize>, Vec<T>)> // Needs two vecs in order to be logarithmic on lookup
}

impl<T> Default for PrecedenceCache<T> {
    fn default() -> Self {
        Self { inner: Default::default() }
    }
}

impl<T> PrecedenceCache<T> {
    pub fn new() -> Self {
        PrecedenceCache { inner: FxHashMap::default() }
    }

    pub fn get(&self, position: usize, precedence: usize) -> Option<&T> {
        if let Some(v) = self.inner.get(&position) {
            let (precs, vals) = v;

            // Only return if a lower precedence (or the same) has been recorded
            return match precs.binary_search(&precedence) {
                Err(pos) if pos > 0 => Some(&vals[pos - 1]),
                Ok(pos) => Some(&vals[pos]),

                _ => None
            }
        }

        None
    }

    pub fn set(&mut self, position: usize, precedence: usize, v: T) {
        let (precs, vals) = self.inner.entry(position).or_default();

        if let Err(pos) = precs.binary_search(&precedence) {
            precs.insert(pos, precedence);
            vals.insert(pos, v);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PrecedenceCache;

    #[test]
    fn precedence_cache() {
        let mut cache = PrecedenceCache::<&str>::new();

        cache.set(0, 10, "Test 1");
        cache.set(0, 20, "Test 2");
        cache.set(0, 15, "Test 3");
        cache.set(0, 5, "Test 4");

        assert_eq!(cache.get(0, 10), Some(&"Test 1"));
        assert_eq!(cache.get(0, 14), Some(&"Test 1"));
        assert_eq!(cache.get(0, 15), Some(&"Test 3"));
        assert_eq!(cache.get(0, 7), Some(&"Test 4"));
        assert_eq!(cache.get(0, 2), None);
        assert_eq!(cache.get(0, 100), Some(&"Test 2"));
        assert_eq!(cache.get(1, 0), None);
    }
}