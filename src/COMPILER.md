# Space Compiler

A verified compiler for the Space programming language, implemented entirely in F*.

## Architecture

```
.space source
    │
    ▼
┌─────────────┐
│   Lexer     │  Space.Compiler.Lexer
│  string →   │  - Tokenizes source code
│  tokens     │  - Handles strings, numbers, comments
└─────────────┘
    │
    ▼
┌─────────────┐
│   Parser    │  Space.Compiler.Parser
│  tokens →   │  - Recursive descent parser
│  AST        │  - Handles all control structures
└─────────────┘
    │
    ▼
┌─────────────┐
│   Codegen   │  Space.Compiler.Codegen
│  AST →      │  - Generates bytecode
│  bytecode   │  - Manages symbol tables
└─────────────┘
    │
    ▼
┌─────────────┐
│  Bytecode   │  Space.Compiler.Bytecode
│  Module     │  - 46 opcodes
└─────────────┘
    │
    ▼
┌─────────────┐
│  Decoder    │  Space.Compiler.Decoder
│  bytecode → │  - Decodes to instructions
│  instrs     │  - Feeds existing interpreter
└─────────────┘
    │
    ▼
┌─────────────┐
│ Interpreter │  Space.Interpreter + Space.Execute
│  instrs →   │  - Executes instructions
│  results    │  - Verified semantics
└─────────────┘
```

## Modules

| Module | LOC | Description |
|--------|-----|-------------|
| `Space.Compiler.Token` | ~250 | Token types, keywords, primitives |
| `Space.Compiler.Lexer` | ~430 | Tokenizer with UTF-8 and Unicode escapes |
| `Space.Compiler.AST` | ~140 | Abstract syntax tree types |
| `Space.Compiler.Parser` | ~450 | Recursive descent parser |
| `Space.Compiler.Bytecode` | ~240 | Bytecode format and opcodes |
| `Space.Compiler.Codegen` | ~390 | Code generator |
| `Space.Compiler.Decoder` | ~335 | Bytecode decoder |
| `Space.Compiler.Test` | ~355 | 30 verification tests |

## Bytecode Opcodes

### Stack (0x00-0x0F)
```
op_nop, op_dup, op_drop, op_swap, op_over, op_rot, op_nip, op_tuck, op_pick
```

### Arithmetic (0x10-0x1F)
```
op_add, op_sub, op_mul, op_divu, op_divs, op_mod, op_neg, op_min, op_max
```

### Bitwise (0x20-0x2F)
```
op_and, op_or, op_xor, op_not, op_shl, op_shr
```

### Comparison (0x30-0x3F)
```
op_eq, op_neq, op_lt, op_gt, op_ltu, op_gtu
```

### Memory (0x40-0x4F)
```
op_alloc_bytes, op_bytes_fetch, op_bytes_store, op_bytes_len, op_bytes_copy
```

### Control Flow (0x90-0x9F)
```
op_call, op_ret, op_jmp, op_jz, op_jnz, op_loop, op_exit
```

### Universe (0xA0-0xAF)
```
op_create_univ, op_end_univ, op_release_univ, op_transfer
```

### System (0xB0-0xBF)
```
op_halt, op_emit, op_read, op_emit_graph
```

## String Literals and Unicode

The lexer operates on UTF-8 bytes and supports full Unicode in string literals.

### Escape Sequences

| Escape | Meaning |
|--------|---------|
| `\n` | Newline (LF, 0x0A) |
| `\t` | Tab (0x09) |
| `\r` | Carriage return (0x0D) |
| `\\` | Backslash |
| `\"` | Double quote |
| `\0` | Null byte (0x00) |
| `\xXX` | Single byte (2 hex digits) |
| `\uXXXX` | Unicode BMP code point (4 hex digits) |
| `\UXXXXXXXX` | Full Unicode code point (8 hex digits) |

### Examples

```forth
"Hello, World!"          \ ASCII string
"Привет"                 \ Raw UTF-8 (Cyrillic)
"\u20AC100"              \ Euro sign: €100
"\U0001F600"             \ Emoji: 😀
"\x41\x42\x43"           \ Bytes: ABC
```

String literals are validated for proper UTF-8 encoding. Invalid UTF-8 sequences produce a compile error.

## Example

### Source Code
```forth
define-word factorial ( n -- n! )
    dup 1 less-than if-true
        drop 1
    if-else
        dup 1 subtract factorial multiply
    end-if
end-word
```

### Compilation
```fstar
let source = "define-word double ( n -- n ) dup add end-word"

match compile_string source with
| Inl bytecode_module -> (* success *)
| Inr (error_msg, location) -> (* error *)
```

### Bytecode Output
```
Function: double
  Offset: 0
  Length: 3
  Code: [0x01, 0x10, 0x91]  (* dup, add, ret *)
```

## Tests

30 verification tests cover:

| Category | Tests |
|----------|-------|
| Lexer | Tokens, strings, keywords, hex, comments |
| Parser | Words, constants, text, if, if-else, loop, while |
| Codegen | Simple words, multiple words, strings, full pipeline |
| Unicode | \xXX, \uXXXX, \UXXXXXXXX escapes, mixed escapes |
| Decoder | Simple bytecode, push, branches, comparisons, full pipeline |

All tests are verified by F*'s type system at compile time.

## Building

```bash
# Verify compiler modules
make verify-compiler

# Extract to C
make extract-compiler
krml *.krml -o spacec

# Run the compiler
./spacec compile program.space -o program.spacec
```

## Bytecode Format

```
┌─────────────────────────────────────┐
│ Magic: "SPACE" (5 bytes)            │
├─────────────────────────────────────┤
│ Version: u32                        │
├─────────────────────────────────────┤
│ String Table                        │
│  - count: u32                       │
│  - entries: [offset, length, data]  │
├─────────────────────────────────────┤
│ Function Table                      │
│  - count: u32                       │
│  - entries: [name_idx, offset, len] │
├─────────────────────────────────────┤
│ Code Section                        │
│  - length: u32                      │
│  - bytecode: [u8...]                │
└─────────────────────────────────────┘
```

## Verification Properties

The F* type system ensures:

1. **Termination** - All recursive functions terminate (fuel-based)
2. **Type safety** - No runtime type errors
3. **Memory safety** - No buffer overflows in bytecode generation
4. **Exhaustiveness** - All pattern matches are complete

## Future Work

- [x] Bytecode decoder (connects to existing interpreter)
- [ ] Forward reference resolution (backpatching)
- [ ] Optimization passes
- [ ] Debug info generation
- [ ] REPL mode
