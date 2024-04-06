use std::{sync::Mutex, time::Instant};

use rustc_hash::FxHashMap;
use tabled::{settings::Style, Table, Tabled};

lazy_static! {
    pub static ref PROFILER: Mutex<FxHashMap<&'static str, (u128, usize)>> = Mutex::default();
}

pub struct ProfSpan {
    name: &'static str,
    start: Instant
}

impl ProfSpan {
    pub fn new(name: &'static str) -> Self {
        ProfSpan { start: Instant::now(), name }
    }

    pub fn end(&self) {
        let mut p_entry = PROFILER.lock().unwrap();
        let entry_data = p_entry.entry(self.name).or_insert((0, 0));

        entry_data.0 += self.start.elapsed().as_nanos();
        entry_data.1 += 1;
    }
}

#[derive(Tabled)]
struct ProfilerEntry {
    name: &'static str,
    total_time: u128,
    time: u128,
    count: usize,
    percentage: f32
}

pub fn clear_profiler() {
    PROFILER.lock().unwrap().clear();
}

pub fn print_profiler_results() {
    let mut entries = PROFILER.lock().unwrap().iter().map(|(a, b)| (*a, *b)).collect::<Vec<_>>();
    entries.sort_by_key(|(_, i)| *i);
    entries.reverse();

    let total_time = entries.iter().map(|(_, (i, _))| *i).sum::<u128>();

    let table = Table::new(
        entries.into_iter().map(|(name, (time, count))| ProfilerEntry {
            name,
            total_time: time / 1000000,
            count,
            time: time / count as u128,
            percentage: (time as f64 / total_time as f64) as f32,
        })
    ).with(Style::modern_rounded()).to_string();

    print!("{}", table);
}