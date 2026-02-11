# Space

**A verified concatenative systems programming language.**

Space combines stack-based execution with linear types, isolated memory universes, and machine-checked proofs. Implemented in F*, transpiles to C, which gcc compiles to native binaries.

## What is Space?

Space is a Forth-like language with:

- **Multiverse model** — isolated memory regions called universes
- **Discipline system** — linear, affine, and unrestricted value tracking
- **Borrowing** — safe temporary access to memory
- **Warps** — structured traversal of memory regions
- **Native Unicode** — grapheme-aware text with full normalization

## Implementation

This implementation is written in F* and verified with Z3. F* extracts to multiple targets:

```
Space source → F* compiler → C      → gcc/clang  → native binary
                           → OCaml  → ocamlopt   → native binary
                           → F#     → dotnet     → .NET assembly
```

## Language Design

### Core Concepts

**Universes** are isolated memory regions with their own stacks. Pointers cannot escape their universe of origin. Cross-universe access requires explicit borrowing or warping. This provides spatial memory safety by construction.

**Disciplines** govern value semantics:

- **Unrestricted:** Values can be copied and dropped freely (like integers)
- **Affine:** Values can be dropped but not copied (like file handles)
- **Linear:** Values must be consumed exactly once (like obligations)

**Borrowing** enables temporary cross-universe pointer access. Borrowed pointers track their source universe and must be dropped before the source can be destroyed.

**Warping** provides structured traversal of data structures without exposing internal pointers. A warp maintains a current position and saved positions, enabling tree traversal with automatic backtracking.

### Philosophical Foundation

The central insight is reframing resource management from "who frees this memory?" to "what must be true before this memory can cease to exist?" Linear universes self-destruct when their obligations are fulfilled — destruction is completion, not an action performed upon the universe.

This maps directly to linear logic (Girard, 1987), where premises are resources consumed by use. The three disciplines correspond to linear logic's modalities.

## Project Structure

```
├── src/                         # F* implementation
│   ├── Space.*.fst              # Runtime modules (47)
│   ├── Space.Compiler.*.fst     # Compiler modules (14)
│   └── runtime/                 # Generated C headers
│       ├── space_runtime.h      # Core primitives (~240 lines)
│       └── space_ucd_full.h     # Unicode 15.0 tables (~200KB)
├── runtime-cpp/                 # C++23 reference VM (for testing)
└── spacelang.org/               # Website
```

## Usage

```bash
cd src
make spacec                              # Build compiler
./spacec program.space > program.c       # Compile to C
gcc -O3 -I runtime program.c -o program  # Build native
./program                                # Run
```

## Requirements

- F* 2025.12.15+
- Z3 4.15.4
- OCaml with zarith
- GCC or Clang

## Unicode Support

Space provides full Unicode 15.0 support:
- UAX #29 grapheme cluster segmentation
- NFC/NFD/NFKC/NFKD normalization
- Full case mapping (1450 uppercase, 1433 lowercase mappings)
- O(1) grapheme access via lazy indexing

## See Also

- `VERSION` — version info and status
