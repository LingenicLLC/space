# Space (F* Implementation)

> **FROZEN** — This implementation is no longer maintained.
> See `spark/` for the active SPARK/Ada implementation.

A stack-based language with verified semantics, implemented in F*.

## What is Space?

Space is a Forth-like language with:

- **Multiverse model** — isolated memory regions called universes
- **Discipline system** — linear, affine, and unrestricted value tracking
- **Borrowing** — safe temporary access to memory
- **Warps** — structured traversal of memory regions
- **Native Unicode** — grapheme-aware text with full normalization

## Implementation

The implementation is written in F* and verified with Z3. Extracts to OCaml/F#.

The C code generator has been removed — use the SPARK/Ada implementation for native binaries.

## Example

```
define-word hello ( -- )
  72 emit  101 emit  108 emit  108 emit  111 emit  10 emit
end-word

define-word main ( -- )
  hello
end-word
```

## Structure

```
src/
├── Space.*.fst          # F* runtime modules
└── Space.Compiler.*.fst # F* compiler modules (C backend stubbed)
```

## Requirements

- F* 2025.12.15+
- Z3 4.15.4
- OCaml with zarith
- GCC or Clang

## See Also

- `PROGRESS.md` — detailed module listing and architecture
- `VERSION` — version info and status
