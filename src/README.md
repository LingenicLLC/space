# Space Implementation

F* source files for the Space language.

## Modules

| Module | Description |
|--------|-------------|
| `Space.Types` | Core types: cell, discipline, universe_id |
| `Space.Stack` | Stack operations: push, pop, dup, swap, etc. |
| `Space.Stack.Properties` | Verified properties of stack operations |
| `Space.Discipline` | Discipline rules: copy, drop, self-destruct |
| `Space.Universe` | Universe: isolated memory region with stack |
| `Space.Arithmetic` | Arithmetic: add, sub, mul, div, mod (wrapping) |
| `Space.Bitwise` | Bitwise: and, or, xor, not, shift |
| `Space.Comparison` | Comparison: eq, lt, gt |
| `Space.World` | Runtime context managing multiple universes |
| `Space.Control` | Control flow: if-true, if-false, loops |
| `Space.Transfer` | Transfer values between universes |
| `Space.Instruction` | Instruction set and opcodes |
| `Space.Interpreter` | Machine state and execution primitives |
| `Space.Execute` | Primitive execution dispatch |
| `Space.Step` | Single-step interpreter execution |
| `Space.Memory` | Memory allocation and access |
| `Space.Borrow` | Borrowed pointers for cross-universe access |
| `Space.Borrow.Properties` | Verified borrow properties |
| `Space.BorrowOps` | Memory operations through borrows |
| `Space.Warp` | Warps for structured traversal |
| `Space.Warp.Properties` | Verified warp properties |
| `Space.WarpOps` | Memory operations through warps |
| `Space.Text.Types` | Text types: header, grapheme, complexity |
| `Space.Text.UTF8` | UTF-8 encoding/decoding |
| `Space.Text.Grapheme` | UAX #29 grapheme break algorithm |
| `Space.Text.Create` | Text creation and analysis |
| `Space.Text.Iter` | Text iteration for sequential access |
| `Space.Text.Ops` | Text operations: equal, compare, concat, slice |
| `Space.Text.Props` | Grapheme and text properties |
| `Space.Text.Warp` | Text warps for structured traversal |
| `Space.Text.Codepoint` | Code point access and iteration |
| `Space.Text.Properties` | Verified text properties |
| `Space.System` | System primitives: halt, emit, read |
| `Space.System.Universes` | System universes: data (default), return (linear) |
| `Space.Bytes` | Byte-level operations: fetch, store, length, copy |
| `Space.Universe.Properties` | Verified universe/discipline properties |
| `Space.Arithmetic.Properties` | Verified arithmetic properties |
| `Space.Text.UTF16` | UTF-16 encoding/decoding for interop |
| `Space.Text.Normalize` | Unicode normalization (NFC, NFD, NFKC, NFKD) |
| `Space.Text.Case` | Unicode case mapping (upper, lower, title) |

### Compiler Modules

| Module | Description |
|--------|-------------|
| `Space.Compiler.Token` | Token types for lexer |
| `Space.Compiler.Lexer` | Tokenizer: string → tokens |
| `Space.Compiler.AST` | Abstract syntax tree types |
| `Space.Compiler.Parser` | Parser: tokens → AST |
| `Space.Compiler.Bytecode` | Bytecode instruction set (46 opcodes) |
| `Space.Compiler.Codegen` | Code generator: AST → bytecode |
| `Space.Compiler.Decoder` | Bytecode decoder: bytecode → instructions |
| `Space.Compiler.Test` | 30 verification tests |

See [COMPILER.md](COMPILER.md) for detailed compiler documentation.

## Building

Requires F* and Z3 installed. See https://www.fstar-lang.org/

```bash
make verify           # Verify runtime modules (40)
make verify-compiler  # Verify runtime + compiler modules (46)
make extract          # Extract runtime to C
make extract-compiler # Extract compiler to C
make clean            # Clean build artifacts
```

## Status

Full Profile — 40 runtime modules + 8 compiler modules = **48 verified modules**.

- Runtime: Core, Text, UTF-16, Normalization, Case Mapping
- Compiler: Lexer, Parser, AST, Bytecode, Codegen, Decoder, Tests
- Tests: 30 verification tests pass (including Unicode and Decoder)
