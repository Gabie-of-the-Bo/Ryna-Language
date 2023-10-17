use std::{fs::read_to_string, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use nessa::context::standard_ctx;

pub fn execution_benchmarks(c: &mut Criterion) {
    let files = [
        "test/primality.nessa",
        "test/mapped_iterator.nessa",
        "test/dice.nessa",
        "test/ints.nessa",
        "test/random.nessa",
        "test/tuples.nessa",
        "test/array_access.nessa",
        "test/array_init.nessa",
        "test/list_comprehension.nessa",
        "test/map_array.nessa",
        "test/array_transform.nessa",
        "test/e_approximation.nessa",
        "test/basic_alias.nessa",
        "test/adt_list.nessa",
        "test/adt_generic_list.nessa",
        "test/adt_bin_tree.nessa",
        "test/numeric_interface.nessa",
        "test/parametric_interface.nessa",
        "test/peano_arithmetic.nessa"
    
    ].iter().map(|path| {
        (path, read_to_string(path).expect("Unable to locate file"))
    }).collect::<Vec<_>>();

    for (path, file) in &files {
        let mut group = c.benchmark_group(path.split("/").last().unwrap());
        group.sample_size(10);
        group.measurement_time(Duration::from_secs(15));
        group.throughput(criterion::Throughput::Bytes(file.len() as u64));
    
        let mut ctx = standard_ctx();

        group.bench_function("Parsing", |b| b.iter(|| {
            let mut ctx_cpy = ctx.clone();
            ctx_cpy.parse_without_precompiling(file).unwrap();
        }));

        let mut parsed = ctx.parse_without_precompiling(file).unwrap();

        group.sample_size(1000);

        group.bench_function("Compilation", |b| b.iter(|| {
            let mut ctx_cpy = ctx.clone();
            let mut parsed_cpy = parsed.clone();

            ctx_cpy.precompile_module(&mut parsed_cpy).unwrap();
            ctx_cpy.compiled_form(&parsed_cpy).unwrap();
        }));

        ctx.precompile_module(&mut parsed).unwrap();
        let compiled = ctx.compiled_form(&parsed).unwrap().into_iter().map(|i| i.instruction).collect();

        group.sample_size(10);

        group.bench_function("Execution", |b| b.iter(|| {
            let mut ctx_cpy = ctx.clone();
            ctx_cpy.execute_compiled_code::<false>(&compiled).unwrap();
        }));

        group.finish();
    }

}

criterion_group!(benches, execution_benchmarks);
criterion_main!(benches);