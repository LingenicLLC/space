# Space Language

**A verified concatenative systems programming language.**

**Date:** 2026-02-10
**Version:** 0.8 alpha
**Status:** Implementation complete, tooling in progress

---

Space is a verified concatenative systems programming language. The implementation is substantially complete: 61 F* modules, full Unicode 15.0 support, transpiles to C, which gcc compiles to native binaries.

- **Multiverse model** — isolated memory regions called universes
- **Discipline system** — linear, affine, and unrestricted value tracking
- **Borrowing** — safe temporary access to memory
- **Warps** — structured traversal of memory regions
- **Native Unicode** — grapheme-aware text with full normalization

---

## 1. What Space Is

### Core Concepts

**Universes** are isolated memory regions with their own stacks. Pointers cannot escape their universe of origin. Cross-universe access requires explicit borrowing or warping. This provides spatial memory safety by construction.

**Disciplines** govern value semantics:

- **Unrestricted:** Values can be copied and dropped freely (like integers)
- **Affine:** Values can be dropped but not copied (like file handles)
- **Linear:** Values must be consumed exactly once (like obligations)

**Borrowing** enables temporary cross-universe pointer access. Borrowed pointers track their source universe and must be dropped before the source can be destroyed.

**Warping** provides structured traversal of data structures without exposing internal pointers. A warp maintains a current position and saved positions, enabling tree traversal with automatic backtracking.

Concepts reinforce each other:

- Concatenative execution makes resource flow explicit
- Linear types formalize that flow
- Universes isolate the flow spatially
- F* proves the flow is correct

---

## 2. Implementation Status

### 2.1 Codebase Metrics

```
F* Modules:           61 total
  Runtime modules:    47 (Space.*.fst)
  Compiler modules:   14 (Space.Compiler.*.fst)

Generated C:
  space_runtime.h:    ~240 lines (inline primitives)
  space_ucd_full.h:   ~200KB (Unicode 15.0 tables)
```

### 2.2 Compilation Pipeline

```
Space source → F* compiler → C      → gcc/clang  → native binary
                           → OCaml  → ocamlopt   → native binary
                           → F#     → dotnet     → .NET assembly
```

Three extraction targets provide deployment flexibility without sacrificing verification.

### 2.3 Unicode Implementation

Complete Unicode 15.0 support:

| Feature                          | Status   |
| -------------------------------- | -------- |
| UTF-8 encode/decode              | Complete |
| UAX #29 grapheme clustering      | Complete |
| NFC/NFD/NFKC/NFKD normalization  | Complete |
| Case mapping (upper/lower/title) | Complete |
| O(1) grapheme access             | Complete |

Tables: 1450 uppercase mappings, 1433 lowercase, 922 CCC entries, 2061 decompositions, 1026 compositions.

This is unusual for a systems language. Most treat strings as byte arrays and leave Unicode to libraries. Space has it verified from day one.

---

## 3. Verification

### 3.1 What's Proven

These properties are machine-checked by Z3, not just tested:

| Property              | Guarantee                              |
| --------------------- | -------------------------------------- |
| Universe isolation    | Pointers never escape their universe   |
| Discipline compliance | Linear values consumed exactly once    |
| Borrow safety         | Borrowed pointers don't outlive source |
| Warp containment      | Internal pointers stay inside warp     |
| UTF-8 validity        | All text is well-formed UTF-8          |
| Stack correctness     | Push-pop identity, size preservation   |

### 3.2 Trusted Computing Base

The TCB (what we must trust) is:

- F* type checker
- Z3 SMT solver
- C/OCaml/F# compilers
- Hardware

The Space implementation itself is verified. Bugs in Space code would be caught by F*.

### 3.3 Verification Methodology

F* uses refinement types — types annotated with predicates that must hold. Z3 proves these predicates automatically where possible. Where not, explicit lemmas guide the proof.

```fstar
let should_self_destruct (u: universe) : bool =
  u.discipline = Linear && stack_empty u && is_live u
```

The type system ensures this function is only called on valid universes.

---

## 4. Design Coherence

### 4.1 Why Concatenative + Linear Types Works

Stack-based execution models resource flow explicitly. Values are pushed, transformed, consumed. This is exactly what linear types formalize.

In an applicative language, you must trace variable bindings to track resource usage. In a concatenative language, you see it directly in the stack operations.

### 4.2 Why Universes Work

Universes solve the coordination problem. Traditional memory management requires global coordination — who's responsible for freeing what?

Universes are self-contained. Each manages its own memory. No global GC traversing the heap. No reference counting across boundaries. Linear universes clean up automatically when empty.

### 4.3 Why F* Works

F* was designed for exactly this: proving properties of systems code. HACL* (verified cryptography), miTLS (verified TLS), Project Everest — these run in the Windows kernel, Firefox, Python. The infrastructure is battle-tested.

---

## 5. Heritage

### 5.1 Intellectual Lineage

| Source              | Contribution                |
| ------------------- | --------------------------- |
| Forth (1970)        | Concatenative execution     |
| Linear Logic (1987) | Discipline system           |
| StrongForth (2003)  | Static typing for Forth     |
| Rust (2010)         | Borrowing semantics         |
| F* (2011)           | Verification infrastructure |

### 5.2 Forth in the Real World

Forth has proven real-world viability:

- **Open Firmware:** Sun, IBM AIX, PowerPC Mac (IEEE 1275)
- **Spacecraft:** Philae lander, NASA deep-space missions
- **Device Tree:** Lives on in every Linux ARM system

Space inherits this minimalism and adds what Forth lacked: static types, linear resources, machine-checked proofs.

## 6. Comparison

### 6.1 vs. Rust

Rust provides ownership-based safety without formal verification. It's practical and widely adopted.

Space's differentiator: machine-checked proofs. If it compiles, properties are guaranteed, not just likely. This matters when "likely" isn't good enough.

### 6.2 vs. Other Verified Languages

Dafny, Whiley, F* itself — verification-focused but not concatenative. Space occupies a unique niche.

### 6.3 vs. Other Forth Variants

StrongForth added types. ColorForth added color-coded semantics. Space adds verification — a stronger guarantee than either.

---

## 7. What Remains

### 7.1 Complete

- Core language semantics
- All 80 primitives
- Unicode 15.0 support
- F* verification
- C/OCaml/F# extraction
- Reference VM (C++23)

### 7.2 In Progress

- Parser for `.space` files
- REPL for interactive development
- Standard library
- Documentation

### 7.3 Future

- IDE integration (LSP)
- Package manager
- Community building
- Domain-specific libraries

---

