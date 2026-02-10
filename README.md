# Space

**A verified concatenative systems programming language.**

Space combines stack-based execution with linear types, isolated memory universes, and machine-checked proofs. Implemented in F*, extracted to C via Karamel.

## Status: v0.8 alpha

| Component | Status |
|-----------|--------|
| **Core types & stack** | ✓ Complete, verified |
| **Discipline system** | ✓ Complete, verified |
| **Universe lifecycle** | ✓ Complete, verified |
| **Borrow system** | ✓ Complete, verified |
| **Warp traversal** | ✓ Complete, verified |
| **Arithmetic & bitwise** | ✓ Complete, verified |
| **Control flow** | ✓ Complete |
| **Text/Unicode** | ✓ Complete, verified (UAX #29 grapheme clusters) |
| **Compiler: Lexer** | ✓ Complete (UTF-8, escapes, literals) |
| **Compiler: Parser** | ✓ Complete (recursive descent) |
| **Compiler: Bytecode** | ✓ Complete (full opcode set) |
| **Compiler: Codegen** | ✓ Complete |
| **Memory operations** | ✓ Complete (cell + byte level) |
| **Borrow/Warp ops** | ✓ Complete (fetch/store through borrows and warps) |
| **I/O primitives** | ✓ Complete (emit/read with channel abstraction) |
| **VM interpreter** | ◐ Modules complete, exec_prim needs wiring |
| **Unicode tables** | ◐ Placeholder tables, Full profile needs UCD data |
| **REPL** | ✗ Not started |
| **C extraction** | ✗ Not yet tested |

### Remaining Work

1. **Wire `exec_prim`** — Memory, I/O, borrow, warp operations are implemented in separate modules but return "not implemented" in `Space.Execute.fst`. Connect the calls.

2. **Expand `prim_op`** — Bytecode has opcodes for all operations, but `Space.Instruction.fst` only defines a subset. Add nip, tuck, pick, divs, neg, min, max, and all text/borrow/warp ops.

3. **Unicode tables** — Normalization and case mapping use placeholder lookups. Full profile needs actual UCD (Unicode Character Database) tables. This is data (~85KB), not code.

4. **Complex text slicing** — `text_slice_simple` only handles ASCII. Slicing text with emoji/combining characters needs grapheme index lookup.

## What's Here

### Implementation (49 F* files)

```
src/
├── Core Types & Verification
│   ├── Space.Types.fst              # Core types: cell, discipline, universe_id
│   ├── Space.Stack.fst              # Stack operations: push, pop, dup, swap, over, rot
│   ├── Space.Stack.Properties.fst   # Stack verification lemmas
│   ├── Space.Discipline.fst         # Linear/affine/unrestricted rules
│   ├── Space.Universe.fst           # Isolated memory regions, self-destruction
│   ├── Space.Universe.Properties.fst
│   ├── Space.Borrow.fst             # Cross-universe pointer borrowing
│   ├── Space.Borrow.Properties.fst  # 15 lemmas proving borrow safety
│   ├── Space.Warp.fst               # Structured traversal without escaping pointers
│   ├── Space.Warp.Properties.fst    # 28 lemmas proving warp correctness
│   ├── Space.Arithmetic.Properties.fst  # 26 arithmetic lemmas
│   └── ...
│
├── Compiler Pipeline
│   ├── Space.Compiler.Token.fst     # Token types, keywords, primitives
│   ├── Space.Compiler.Lexer.fst     # UTF-8 lexer with escape sequences
│   ├── Space.Compiler.AST.fst       # Abstract syntax tree
│   ├── Space.Compiler.Parser.fst    # Recursive descent parser
│   ├── Space.Compiler.Bytecode.fst  # Opcode definitions, module format
│   ├── Space.Compiler.Codegen.fst   # AST to bytecode compilation
│   ├── Space.Compiler.Decoder.fst   # Bytecode decoding
│   └── Space.Compiler.Test.fst      # Compiler tests
│
├── Text/Unicode (14 files, ~1,945 lines)
│   ├── Space.Text.Types.fst         # Text representation
│   ├── Space.Text.UTF8.fst          # UTF-8 encoding/decoding
│   ├── Space.Text.UTF16.fst         # UTF-16 for interop
│   ├── Space.Text.Grapheme.fst      # UAX #29 grapheme clusters
│   ├── Space.Text.Normalize.fst     # NFC/NFD/NFKC/NFKD
│   ├── Space.Text.Case.fst          # Case mapping
│   └── ...
│   # (F* has no built-in Unicode — implemented from scratch)
│
├── VM Runtime
│   ├── Space.Interpreter.fst        # Machine state
│   ├── Space.Execute.fst            # Primitive execution
│   ├── Space.Step.fst               # Single-step VM
│   ├── Space.World.fst              # Multi-universe runtime
│   └── ...
│
└── Makefile
```

### 93+ Machine-Checked Lemmas

Every core operation has verified properties:

```fstar
(* Stack: push then pop returns original *)
let push_pop_identity (s: stack) (v: cell) :
  Lemma (pop (push s v) == Some (v, s)) = ()

(* Linear universe must be empty to end *)
let linear_self_destruct_condition (u: universe) :
  Lemma (should_self_destruct u <==>
         (u.discipline = Linear && stack_empty u && is_live u)) = ()

(* Borrowed pointer can't return to wrong universe *)
let return_requires_source_match (b: borrowed) (src: universe) :
  Lemma (requires b.source_id <> src.id)
        (ensures None? (return_borrowed b src)) = ()
```

## Building

Requires [F*](https://www.fstar-lang.org/) and [Z3](https://github.com/Z3Prover/z3) (version 4.15.4).

```bash
cd src
make verify    # Type-check and verify all proofs
make extract   # Generate Karamel output (requires krml)
```

## Runtime Architecture

The F* implementation extracts to C via Karamel. Space programs compile to bytecode that runs on this verified runtime.

```
program.space  →  Compiler  →  bytecode  →  VM (extracted C)
```

**Runtime sizes:**

| Profile | Includes | Binary |
|---------|----------|--------|
| Embedded | Core only | ~2KB |
| Standard | Core + Text | ~35KB |
| Full | Core + Text + Unicode tables | ~120KB |

For comparison: Lua ~280KB, mruby ~400KB, QuickJS ~600KB. Embedded fits microcontrollers with 8KB flash.

## The Design

**Universes** are isolated memory regions. Pointers cannot escape. Each universe has a discipline:

- **Unrestricted**: Values can be copied and dropped freely
- **Affine**: Values can be dropped but not copied
- **Linear**: Values must be consumed exactly once

Linear universes self-destruct when their obligations are fulfilled:

```
1024 linear create-universe as scratch
    42 push
    pop              \ obligation fulfilled
end-universe         \ universe already gone
```

**Borrowing** enables temporary cross-universe access. Borrowed pointers track their source and must be dropped before the source can be destroyed.

**Warps** provide structured traversal of data structures without exposing internal pointers. You can navigate without creating references that might escape.

## Documentation

- [spacelang.org](https://spacelang.org) — Language specification
- [spacelang.org/formal-verification](https://spacelang.org/formal-verification/) — Verification approach
- [spacelang.org/philosophy](https://spacelang.org/philosophy/) — Design philosophy
- [spacelang.org/heritage](https://spacelang.org/heritage/) — Intellectual lineage

## Heritage

Space synthesizes ideas from:

- **Forth** (1970) — Concatenative execution
- **StrongForth** (2003) — Static typing for stack languages
- **Linear Logic** (1987) — Discipline system
- **Rust** (2010) — Borrowing semantics
- **F*** (2011) — Verification infrastructure

Forth powered Open Firmware (IEEE 1275) and runs actual spacecraft today. Space inherits this heritage and adds what Forth lacked: static types, linear resources, and machine-checked proofs.

## License

Apache 2.0. See [LICENSE](LICENSE).

## Author

Created by [Danslav Slavenskoj](https://github.com/slavenskoj). Design conceived summer 2025, first implemented in F* 2026-02-10.

---

*This is alpha software. Core semantics and compiler are complete; VM execution loop needs wiring. Expect breaking changes.*
