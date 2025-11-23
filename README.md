# Nilang
A scripting language interpreter designed for [Advent of Code](https://adventofcode.com/) 2025!
## Overview
Nilang is a dynamically-typed scripting language with a focus on simplicity and expressiveness. It features a custom bytecode compiler with an optimizing IR, register allocation, and a trace and sweep garbage-collected runtime.
## Features
- **Dynamic typing** - Variables can hold any type
- **First-class functions** - Functions as values, closures, partial functions
- **Lists and maps** - Built-in collection types
- **For/while loops** - Standard control flow
- **REPL** - Interactive development
- And much much more!
## Installation

### Quick Install (Recommended)
1. First, [install Rust and cargo](https://rust-lang.org/tools/install/)
2. Run the installation script:
```bash
curl -sSf https://raw.githubusercontent.com/Nilando/nilang/main/install.sh | bash
```

This will install nilang via cargo, download the standard library, and place it in `~/.nilang/std/`.

### Building from Source

```bash
git clone https://github.com/Nilando/nilang
cd nilang
cargo build --release
./install.sh
```

## Usage

Run a Nilang program:
```bash
nilang program.nl
```

Start the REPL:
```bash
nilang
```

Pipe input to the interpreter:
```bash
echo 'print(1 + 2);' | nilang -s
```

## Quick Start
#### 🏗️ Tutorial document is in progress! 🏗️

Check out the `examples/` directory for code samples:
- `fibonacci.nl` - Fibonacci sequence generator
- `fizzbuzz.nl` - FizzBuzz implementation
- `quicksort.nl` - Quicksort algorithm
- `game_of_life.nl` - Conway's Game of Life
- `eratosthenes_sieve.nl` - Prime number sieve
- And many more!

Try running an example:
```bash
nilang examples/fibonacci.nl
```
## Project Status
Nilang is currently in **early development** (v0.1.0), and may not be maintained/updated any further. The language is functional but may have rough edges. Future features of interest include FFI through standard C bindings and compilation to WebAssembly, though these may not be implemented in the near future.
### Known Limitations
- Error messages need improvement, particularly for parser errors
- Limited standard library
- Performance optimizations ongoing
- No FFI support
## Architecture
Nilang uses a multi-stage compilation pipeline:
1. **Lexer** → Tokens
2. **Parser** → AST
3. **IR Lowering** → Three-address code with SSA
4. **Optimization** → GVN, constant folding, DCE
5. **Codegen** → Register allocation and bytecode emission
6. **VM** → Bytecode interpretation with garbage collection
## Contributing
Issues and pull requests are welcome! This is a learning project and a work in progress.
## License
MIT License - see [LICENSE-MIT](LICENSE-MIT) for details.
## Author
Niland Schumacher
