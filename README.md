# Space Language

**A verified concatenative systems programming language.**

**Date:** 2026-02-11
**Version:** 0.9 alpha
**Status:** SPARK/Ada implementation in progress

---

Space is a verified concatenative systems programming language that compiles to SPARK/Ada.

```
C       →  Assembly  →  machine code
Space   →  SPARK/Ada →  native binary
```

Space provides high-level abstractions — universes, disciplines, borrowing, warps — that compile down to verified SPARK/Ada code. GNATprove verifies the contracts. GNAT compiles to native binaries. The `spark/` directory contains the **runtime library** that Space programs compile against, like libc for C.

---

## 1. What Space Is

Space adds to the Forth model:

- **Universes** — isolated memory regions (spatial safety)
- **Disciplines** — linear, affine, unrestricted types (temporal safety)
- **Borrowing** — safe cross-universe access
- **Warps** — structured traversal without pointer escape
- **Native Unicode** — grapheme-aware text with normalization

### Core Concepts

**Universes** are isolated memory regions with their own stacks. Pointers cannot escape their universe of origin. Cross-universe access requires explicit borrowing or warping. This provides spatial memory safety by construction.

**Disciplines** govern value semantics:

- **Unrestricted:** Values can be copied and dropped freely (like integers)
- **Affine:** Values can be dropped but not copied (like file handles)
- **Linear:** Values must be consumed exactly once (like obligations)

**Borrowing** enables temporary cross-universe pointer access. Borrowed pointers track their source universe and must be dropped before the source can be destroyed.

**Warping** provides structured traversal of data structures without exposing internal pointers. A warp maintains a current position and saved positions, enabling tree traversal with automatic backtracking.

---

## 2. Architecture

### 2.1 Compilation Model

```
example.space  →  Space compiler  →  example.adb  →  GNATprove  →  GNAT  →  binary
                                          ↓
                                   spark/src/*.ads  (runtime library)
```

The Space compiler emits SPARK/Ada that calls into the runtime. GNATprove verifies contracts. GNAT compiles to native.

### 2.2 Directory Structure

```
spark/              # Runtime library (SPARK/Ada)
├── src/            # 49 source files — primitives with contracts
├── ucd/            # Unicode Character Database 15.0
├── tools/          # UCD table generator (Ada)
└── space.gpr       # GNAT project file

fstar/              # Frozen (historical F* prototype)
```

### 2.3 Runtime Library

```
Source files:     49 total
Primitives:       89 (with Pre/Post contracts)
Verification:     GNATprove
Compiler:         GNAT (GCC or LLVM backend)
```

### 2.4 Build

```bash
# Build runtime and compiler
gprbuild -P spark/space.gpr

# Verify with GNATprove
gnatprove -P spark/space.gpr --level=2
```

### 2.5 Unicode Support

Complete Unicode 15.0:

| Feature                          | Status   |
| -------------------------------- | -------- |
| UTF-8 encode/decode              | Complete |
| UAX #29 grapheme clustering      | Complete |
| NFC/NFD/NFKC/NFKD normalization  | Complete |
| Case mapping (upper/lower/title) | Complete |
| O(1) grapheme access             | Complete |

Tables: 1450 uppercase mappings, 1433 lowercase, 922 CCC entries, 2061 decompositions, 1026 compositions.

---

## 3. Verification

### 3.1 What's Proven

All primitives have Pre/Post contracts verified by GNATprove:

| Property              | Guarantee                              |
| --------------------- | -------------------------------------- |
| Universe isolation    | Pointers never escape their universe   |
| Discipline compliance | Linear values consumed exactly once    |
| Borrow safety         | Borrowed pointers don't outlive source |
| Warp containment      | Internal pointers stay inside warp     |
| UTF-8 validity        | All text is well-formed UTF-8          |
| Stack correctness     | Push-pop identity, size preservation   |

### 3.2 Example Contract

```ada
procedure Dup (S : in Out Stack)
  with Pre  => Size (S) >= 1 and Size (S) < Stack_Capacity,
       Post => Size (S) = Size (S)'Old + 1
               and Top (S) = Top (S)'Old;
```

### 3.3 Trusted Computing Base

- GNATprove (verifies contracts)
- GNAT (compiles to native)
- Hardware

The Space compiler is itself written in SPARK/Ada and verified by GNATprove — it's not part of the TCB.

---

## 4. Primitives (89)

### Core Runtime — Embedded Profile (60)

| Category | Count | Primitives |
|----------|-------|------------|
| Stack | 8 | `dup` `drop` `swap` `over` `rot` `nip` `tuck` `pick` |
| Arithmetic | 9 | `+` `-` `*` `/` `/s` `mod` `negate` `min` `max` |
| Bitwise | 6 | `and` `or` `xor` `not` `<<` `>>` |
| Comparison | 6 | `=` `<>` `<` `>` `<s` `>s` |
| Memory | 3 | `@` `!` `alloc` |
| Bytes | 5 | `bytes-alloc` `bytes-fetch` `bytes-store` `bytes-len` `bytes-copy` |
| Universe | 5 | `create-universe` `end-universe` `release-universe` `universe-push` `universe-pop` |
| Borrow | 8 | `borrow-pointer` `return-pointer` `drop-pointer` `fetch-borrowed` `store-borrowed` `fetch-and-end` `store-and-end` `offset-borrowed` |
| Warp | 7 | `warp-fetch` `warp-store` `warp-advance` `warp-follow` `warp-position` `warp-restore` `warp-null` |
| I/O | 3 | `emit` `key` `emit-grapheme` |

### Text Handling — Full Profile (+29)

| Category | Count | Primitives |
|----------|-------|------------|
| Text | 11 | `create-text` `byte-length` `grapheme-count` `is-simple` `grapheme-at` `grapheme-first` `grapheme-last` `slice` `concat` `equal` `compare` |
| Text Warp | 5 | `has-grapheme` `current-grapheme` `next-grapheme` `grapheme-index` `goto-grapheme` |
| Grapheme | 3 | `grapheme-byte-length` `grapheme-is-ascii` `grapheme-code-points` |
| Codepoint | 2 | `code-point-count` `code-point-at` |
| Normalize | 4 | `nfc` `nfd` `nfkc` `nfkd` |
| Case | 3 | `to-upper` `to-lower` `to-title` |
| System | 1 | `halt` |

---

## 5. Heritage

| Source              | Contribution                |
| ------------------- | --------------------------- |
| Forth (1970)        | Concatenative execution     |
| Linear Logic (1987) | Discipline system           |
| StrongForth (2003)  | Static typing for Forth     |
| Rust (2010)         | Borrowing semantics         |
| SPARK/Ada (1988)    | Compilation target          |

**Why SPARK/Ada as target?** Same reason C targets assembly: it's the right level of abstraction. SPARK provides verified systems programming. Space provides higher-level constructs (universes, disciplines) that compile down to it. No extraction gap — the verified code is the production code.

**Forth heritage:** Open Firmware (IEEE 1275), Philae lander, NASA deep-space missions. Space inherits the minimalism and adds what Forth lacked: static types, linear resources, machine-checked proofs.

---

## 6. Links

- Website: https://spacelang.org
- GitHub: https://github.com/LingenicLLC/space
