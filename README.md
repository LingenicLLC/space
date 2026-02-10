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
| **Bytecode decoder** | ✓ All 80 primitives decoded |
| **VM interpreter** | ✓ All 80 primitives wired |
| **Unicode tables** | ✓ Real UCD data (Unicode 15.0, 7325 lines) |
| **REPL** | ✗ Not started |
| **C extraction** | ✗ Not yet tested |

### Remaining Work

#### 1. Primitive wiring ✓ Complete

All 80 primitives wired in `Space.Step.fst`:
- Stack (8), Arithmetic (9), Bitwise (6), Comparison (6)
- Memory (8), Borrow (8), Warp (7), I/O (2)
- Text (11), Text warp (5), Grapheme (3), Codepoint (2)
- Normalization (4), Case (3), System (1 halt)

**Design decision:** UTF-16 primitives removed from instruction set — available as library functions in `Space.Text.UTF16.fst` when needed for interop, but not required as VM primitives.

#### 2. I/O architecture ✓

I/O uses machine-level input/output buffers (pure functional approach):
- `step_emit` appends to `m.output` buffer
- `step_key` reads from `m.input` buffer (returns -1 for EOF)

Platform-specific I/O happens at the host level — pre-populate `m.input`, read `m.output` after execution.

#### 3. Unicode tables ✓

UCD data tables implemented (7,325 lines, Unicode 15.0.0):
- Case mappings: 3,020 lines (including special casefolding: ß→ss, ligatures, Greek)
- Decomposition: 2,083 lines (canonical)
- Composition: 1,139 lines (canonical)
- Combining classes: 942 lines

#### 4. Dead code in Space.Execute.fst

The stubs in `exec_prim` (lines 289-345) are **dead code** — `step_prim` routes all operations to proper handlers before falling through. Can be cleaned up but causes no bugs.

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

Requires [F*](https://www.fstar-lang.org/) and [Z3](https://github.com/Z3Prover/z3) (version 4.13.3 or 4.15.4).

```bash
cd src
make verify         # Type-check and verify all proofs
make extract-ocaml  # Extract to OCaml
make extract-krml   # Extract to Karamel (.krml files)
make extract-c      # Generate C via Karamel (requires krml)
```

## Extraction Targets

F* can extract verified code to multiple languages:

| Target | Command | Status | Use Case |
|--------|---------|--------|----------|
| **OCaml** | `make extract-ocaml` | ✓ Working | REPL, prototyping, native performance |
| **F#** | `--codegen FSharp` | ✓ Available | .NET integration |
| **C** | `make extract-c` | ⚠ Requires Low* | Embedded systems |
| **Wasm** | via Karamel | ⚠ Requires Low* | Browser runtime |

### Low* and C Extraction

C extraction via Karamel requires the **Low\*** subset of F*:
- Machine integers (`UInt64.t`) instead of mathematical (`nat`)
- Mutable buffers instead of immutable lists
- Loops instead of recursion on data structures

Current Space implementation uses high-level F* for verification clarity. **Roadmap:**

1. **Now:** OCaml extraction (full F*, works today)
2. **Next:** Low* core modules (stack, memory, VM loop)
3. **Future:** Full C extraction for embedded targets

The verification proofs remain in high-level F*. Low* provides the extraction path — same semantics, different representation.

## Runtime Architecture

```
program.space  →  Compiler  →  bytecode  →  VM (extracted OCaml/C)
```

**Current:** OCaml extraction provides native-compiled runtime.
**Planned:** C extraction for embedded (requires Low* rewrite of core).

**Target runtime sizes (C extraction):**

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

## Applications

- [Applications of Verified Isolation](applications/APPLICATIONS.md) — What the combination enables
- [Parallel Universe Execution](applications/PARALLEL-UNIVERSES.md) — Safe parallelism from isolation
- [Unicode at the Systems Level](applications/UNICODE-SYSTEMS.md) — Grapheme-correct text on embedded systems

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

Created by [Danslav Slavenskoj](https://github.com/slavenskoj). Design conceived summer 2025, first implemented 2026-02-10.

---

*This is alpha software. Core semantics and compiler are complete; VM execution loop needs wiring. Expect breaking changes.*
