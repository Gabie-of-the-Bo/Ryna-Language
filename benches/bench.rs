use std::{fs::read_to_string, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use ryna::context::standard_ctx;

pub fn execution_benchmarks(c: &mut Criterion) {
    let files = [
        "benches/sources/primality.ryna",
    
    ].iter().map(|path| {
        (path, read_to_string(path).expect("Unable to locate file"))
    }).collect::<Vec<_>>();

    for (path, file) in &files {
        let mut group = c.benchmark_group(path.split('/').last().unwrap());
        group.sample_size(10000);
        group.measurement_time(Duration::from_secs(60));
    
        let mut ctx = standard_ctx();
        ctx.optimize = true;
        let mut parsed = ctx.parse_without_precompiling(file).unwrap();
        ctx.precompile_module(&mut parsed).unwrap();
        let compiled = ctx.compiled_form(&parsed).unwrap().into_iter().map(|i| i.instruction).collect::<Vec<_>>();

        group.bench_function("Execution", |b| b.iter(|| {
            ctx.variables.clear();
            ctx.execute_compiled_code::<false>(&compiled, &[]).unwrap();
        }));

        group.finish();
    }

}

criterion_group!(benches, execution_benchmarks);
criterion_main!(benches);