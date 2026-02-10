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
| **VM execution loop** | ✓ Complete |
| **Memory operations** | ◐ Defined, not wired up |
| **I/O primitives** | ◐ Defined, not wired up |
| **Text/Unicode** | ✓ Complete, verified |
| **Parser** | ✗ Missing |
| **REPL** | ✗ Missing |
| **C extraction** | ✗ Not yet tested |

## What's Here

### Verified Core (41 F* files, ~4,400 lines)

```
src/
├── Space.Types.fst              # Core types: cell, discipline, universe_id
├── Space.Stack.fst              # Stack operations: push, pop, dup, swap, over, rot
├── Space.Stack.Properties.fst   # Stack verification lemmas
├── Space.Discipline.fst         # Linear/affine/unrestricted rules
├── Space.Universe.fst           # Isolated memory regions, self-destruction
├── Space.Universe.Properties.fst
├── Space.Borrow.fst             # Cross-universe pointer borrowing
├── Space.Borrow.Properties.fst  # 15 lemmas proving borrow safety
├── Space.Warp.fst               # Structured traversal without escaping pointers
├── Space.Warp.Properties.fst    # 28 lemmas proving warp correctness
├── Space.Memory.fst             # Allocation, fetch, store
├── Space.Transfer.fst           # Cross-universe value transfer
├── Space.Instruction.fst        # Full instruction set
├── Space.Interpreter.fst        # Machine state
├── Space.Execute.fst            # Primitive execution
├── Space.Step.fst               # Single-step VM
├── Space.Control.fst            # Branching, loops
├── Space.World.fst              # Multi-universe runtime
├── Space.Arithmetic.fst         # Add, sub, mul, div, mod
├── Space.Arithmetic.Properties.fst  # 26 arithmetic lemmas
├── Space.Bitwise.fst            # And, or, xor, not, shift
├── Space.Comparison.fst         # Eq, neq, lt, gt
├── Space.Text.*.fst             # UTF-8/16, normalization, graphemes (14 files)
└── Makefile                     # Build configuration
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

Created by [Danslav Slavenskoj](https://github.com/slavenskoj). Design conceived summer 2025, first implemented 2026-02-10.

---

*This is alpha software. The core is verified; the tooling is incomplete. Expect breaking changes.*
