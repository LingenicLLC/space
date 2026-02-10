# Space Implementation

F* verified implementation of the Space language — v0.8.3

## Status

**All 80 primitives fully implemented. Zero stubs.**

| Metric | Count |
|--------|-------|
| Runtime modules | 47 |
| Compiler modules | 8 |
| Total modules | 55 |
| Primitives | 80 |
| Verification | PASS |
| OCaml extraction | PASS |

## Primitives (80)

| Category | Count | Operations |
|----------|-------|------------|
| Stack | 8 | dup, drop, swap, over, rot, nip, tuck, pick |
| Arithmetic | 9 | add, sub, mul, div-u, div-s, mod, neg, min, max |
| Bitwise | 6 | and, or, xor, not, shl, shr |
| Comparison | 6 | eq, neq, lt-u, gt-u, lt-s, gt-s |
| Memory | 3 | fetch, store, alloc |
| Bytes | 5 | bytes-alloc, bytes-fetch, bytes-store, bytes-len, bytes-copy |
| Borrow | 8 | borrow-pointer, return-pointer, drop-pointer, fetch-borrowed, store-borrowed, fetch-and-end, store-and-end, offset-borrowed |
| Warp | 7 | warp-fetch, warp-store, warp-advance, warp-follow, warp-position, warp-restore, warp-null |
| Text | 11 | create-text, byte-length, grapheme-count, is-simple, grapheme-at, grapheme-first, grapheme-last, slice, concat, equal, compare |
| Text Warp | 5 | has-grapheme, current-grapheme, next-grapheme, grapheme-index, goto-grapheme |
| Grapheme | 3 | byte-length, is-ascii, code-points |
| Codepoint | 2 | code-point-count, code-point-at |
| I/O | 3 | emit, key, emit-grapheme |
| Normalize | 4 | nfc, nfd, nfkc, nfkd |
| Case | 3 | to-upper, to-lower, to-title |
| System | 1 | halt |

## Modules

### Core Runtime

| Module | Description |
|--------|-------------|
| `Space.Types` | Core types: cell, discipline, universe_id |
| `Space.Stack` | Stack operations with properties |
| `Space.Discipline` | Discipline rules: unrestricted, affine, linear |
| `Space.Memory` | Memory allocation and cell access |
| `Space.Universe` | Isolated memory region with stack |
| `Space.World` | Runtime context managing multiple universes |

### Operations

| Module | Description |
|--------|-------------|
| `Space.Arithmetic` | Wrapping arithmetic: add, sub, mul, div, mod |
| `Space.Bitwise` | Bitwise: and, or, xor, not, shift |
| `Space.Comparison` | Comparison: eq, lt, gt (signed/unsigned) |
| `Space.Control` | Control flow: branching |
| `Space.Transfer` | Transfer values between universes |

### Borrowing & Warps

| Module | Description |
|--------|-------------|
| `Space.Borrow` | Borrowed pointer state |
| `Space.BorrowOps` | Memory operations through borrows |
| `Space.Warp` | Warp state for structured traversal |
| `Space.WarpOps` | Memory operations through warps |

### Interpreter

| Module | Description |
|--------|-------------|
| `Space.Instruction` | 80 primitive opcodes |
| `Space.Execute` | Primitive execution (universe-level) |
| `Space.Interpreter` | Machine state: world, borrows, warps, text tables |
| `Space.Step` | Single-step execution (machine-level) |

### Text Processing

| Module | Description |
|--------|-------------|
| `Space.Text.Types` | Text header, grapheme, complexity levels |
| `Space.Text.UTF8` | UTF-8 encoding/decoding |
| `Space.Text.Grapheme` | UAX #29 grapheme cluster segmentation |
| `Space.Text.Create` | Text creation and validation |
| `Space.Text.Ops` | Operations: equal, compare, concat, slice |
| `Space.Text.Warp` | Text warp for grapheme iteration |
| `Space.Text.Normalize` | NFC, NFD, NFKC, NFKD normalization |
| `Space.Text.Case` | Upper, lower, title, casefold |

### Unicode Character Database

| Module | Entries | Description |
|--------|---------|-------------|
| `Space.Text.UCD.Types` | — | Type definitions |
| `Space.Text.UCD.Case` | 2,986 | Uppercase + lowercase + special casing |
| `Space.Text.UCD.CCC` | 922 | Combining character classes |
| `Space.Text.UCD.Decomp` | 2,061 | Canonical decompositions |
| `Space.Text.UCD.Comp` | 1,107 | Composition pairs + exclusions |

### Compiler

| Module | Description |
|--------|-------------|
| `Space.Compiler.Token` | Token types |
| `Space.Compiler.Lexer` | Tokenizer |
| `Space.Compiler.AST` | Abstract syntax tree |
| `Space.Compiler.Parser` | Parser |
| `Space.Compiler.Bytecode` | Bytecode format |
| `Space.Compiler.Codegen` | Code generator |
| `Space.Compiler.Decoder` | Bytecode decoder |
| `Space.Compiler.Test` | 30 verification tests |

## Building

Requirements:
- F* 2025.12.15 or later
- Z3 4.15.4
- Karamel (optional, for C extraction)

```bash
make verify           # Verify runtime (47 modules)
make verify-compiler  # Verify all (55 modules)
make extract-ocaml    # Extract to OCaml
make extract-c        # Extract to C (requires Karamel)
make clean            # Clean build artifacts
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      Space.Step                         │
│  step_prim routes to specialized handlers:              │
│  ├─ is_text_prim    → step_text_prim                   │
│  ├─ is_io_prim      → step_io_prim                     │
│  ├─ is_borrow_prim  → step_borrow_prim                 │
│  ├─ is_warp_prim    → step_warp_prim                   │
│  ├─ is_bytes_meta   → step_bytes_meta_prim             │
│  └─ else            → exec_prim (universe-only ops)    │
└─────────────────────────────────────────────────────────┘
                           │
┌─────────────────────────────────────────────────────────┐
│                   Space.Interpreter                     │
│  machine = {                                            │
│    mworld: world,           (* universes *)             │
│    borrows: borrow_state,   (* active borrows *)        │
│    warps: warp_table,       (* memory warps *)          │
│    texts: text_table,       (* allocated texts *)       │
│    text_warps: text_warp_table,                         │
│    graphemes: grapheme_table,                           │
│    bytes_meta: bytes_metadata,                          │
│    output: list nat,        (* I/O buffer *)            │
│    input: list nat,                                     │
│    inst_ptr, return_stack, running, error               │
│  }                                                      │
└─────────────────────────────────────────────────────────┘
```

## License

See LICENSE file.
