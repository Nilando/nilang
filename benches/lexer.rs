use criterion::*;
use nilang::{parser::{Lexer, Token, Ctrl}, SymbolMap};

fn lex_to_end(lexer: &mut Lexer, syms: &mut SymbolMap) {
    loop {
        match lexer.get_token(syms) {
            Ok(token) => if token.item == Token::Ctrl(Ctrl::End) {
                break;
            }
            Err(_) => assert!(false),
        }
    }
}

fn lexer_throughput(c: &mut Criterion) {
    let fibonacci_input = ("Fibonacci Example Input", &std::fs::read_to_string("./examples/fibonacci.nl").expect("error reading file"));
    let closure_input = ("Closure Example Input", &std::fs::read_to_string("./examples/closure_example.nl").expect("error reading file"));
    let fizzbuzz_input = ("Fizzbuzz Example Input", &std::fs::read_to_string("./examples/fizzbuzz.nl").expect("error reading file"));

    let mut group = c.benchmark_group("Lexer Throughput");
    for (name, input) in [fibonacci_input, closure_input, fizzbuzz_input].into_iter() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(format!(" {}", name), input, move |b, input| {
            b.iter(|| {
                let mut lexer = Lexer::new(input);
                let mut syms = SymbolMap::new();

                lex_to_end(&mut lexer, &mut syms)
            })
        });
    }
    group.finish();
}

criterion_group!(benches, lexer_throughput);
criterion_main!(benches);
