# Space Language — SPARK Implementation

A formally verified implementation of the Space programming language in SPARK/Ada.

## Overview

Space is a concatenative, stack-based language with:
- **Universe isolation** — memory safety without garbage collection
- **Discipline enforcement** — linear, affine, and unrestricted types
- **Automatic parallelism** — runtime exploits isolation for parallel execution
- **Formal verification** — all primitives have Pre/Post contracts

## Core Concepts

### Universes

Universes are isolated memory regions with their own stacks. Pointers cannot escape their universe of origin. Cross-universe access requires explicit borrowing or warping. This provides spatial memory safety by construction.

```
┌─────────────┐     ┌─────────────┐
│ Universe A  │     │ Universe B  │
│  ┌───────┐  │     │  ┌───────┐  │
│  │ Stack │  │     │  │ Stack │  │
│  └───────┘  │     │  └───────┘  │
│  ┌───────┐  │  ✗  │  ┌───────┐  │
│  │Memory │──┼──/──┼──│Memory │  │
│  └───────┘  │     │  └───────┘  │
└─────────────┘     └─────────────┘
     No pointer escape between universes
```

### Disciplines

Disciplines govern value semantics:

| Discipline | Copy | Drop | Use Case |
|------------|------|------|----------|
| **Unrestricted** | Yes | Yes | Integers, booleans |
| **Affine** | No | Yes | File handles, connections |
| **Linear** | No | No | Obligations, must-consume tokens |

### Borrowing

Borrowing enables temporary cross-universe pointer access. Borrowed pointers track their source universe and must be returned before the source can be destroyed.

```forth
borrow-pointer    \ Take temporary reference
fetch-borrowed    \ Read through borrowed pointer
return-pointer    \ Give it back (required before source destruction)
```

### Warping

Warping provides structured traversal of data structures without exposing internal pointers. A warp maintains a current position and saved positions, enabling tree traversal with automatic backtracking.

```forth
warp-follow       \ Descend into child structure
warp-position     \ Save current position
warp-restore      \ Return to saved position
```

## Quick Start

```bash
# Build runtime and compiler
gprbuild -P space.gpr

# Verify with GNATprove
gnatprove -P space.gpr --level=2

# Build and run Unicode table generator
cd tools && gprbuild -P gen_ucd.gpr && cd ..
./gen_ucd_main
```

## Directory Structure

```
spark/
├── README.md
├── IMPLEMENTATION_PLAN.md      # Roadmap
├── space.gpr                   # GNAT project file
│
├── tools/                      # Build tools (Ada)
│   ├── gen_ucd.gpr             # Project file
│   ├── gen_ucd.ads/adb         # Unicode table generator
│   └── gen_ucd_main.adb        # Main program
│
├── ucd/                        # Unicode Character Database 15.0
│   ├── UnicodeData.txt
│   ├── CaseFolding.txt
│   ├── SpecialCasing.txt
│   ├── CompositionExclusions.txt
│   ├── GraphemeBreakProperty.txt
│   └── emoji-data.txt
│
└── src/                        # Source files
    ├── Runtime
    │   ├── space-types.ads/adb         # Cell, Discipline, 128-bit Grapheme
    │   ├── space-stack.ads/adb         # Bounded stack operations
    │   ├── space-arithmetic.ads/adb    # Math primitives
    │   ├── space-bitwise.ads/adb       # Bit operations
    │   ├── space-comparison.ads/adb    # Comparisons
    │   ├── space-memory.ads/adb        # Memory access
    │   ├── space-bytes.ads/adb         # Byte-level operations
    │   ├── space-universe.ads/adb      # Universe lifecycle
    │   ├── space-borrow.ads/adb        # Borrowed pointers
    │   ├── space-warp.ads/adb          # Structured traversal
    │   ├── space-io.ads/adb            # I/O primitives
    │   ├── space-text.ads/adb          # Text handles
    │   ├── space-text-cursor.ads/adb   # Grapheme iteration (UAX #29)
    │   ├── space-text-grapheme.ads/adb # Grapheme properties
    │   ├── space-text-codepoint.ads/adb # Code point access
    │   ├── space-text-grapheme_break.ads/adb # UAX #29 segmentation
    │   ├── space-text-normalize.ads/adb # NFC/NFD/NFKC/NFKD
    │   ├── space-text-case.ads/adb     # Case mapping
    │   ├── space-parallel.ads/adb      # Ravenscar task pool
    │   └── space-parallel-data.ads/adb # SIMD operations
    │
    └── Compiler
        ├── space-compiler.ads
        ├── space-compiler-lexer.ads/adb
        ├── space-compiler-parser.ads/adb
        ├── space-compiler-ast.ads/adb
        ├── space-compiler-codegen.ads/adb
        └── space-compiler-main.adb
```

## Primitives (89 total)

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

### Text Handling — Full Profile (+28 = 89)

| Category | Count | Primitives |
|----------|-------|------------|
| Text | 11 | `create-text` `byte-length` `grapheme-count` `is-simple` `grapheme-at` `grapheme-first` `grapheme-last` `slice` `concat` `equal` `compare` |
| Text Cursor | 5 | `has-grapheme` `current-grapheme` `next-grapheme` `grapheme-index` `goto-grapheme` |
| Grapheme | 3 | `grapheme-byte-length` `grapheme-is-ascii` `grapheme-code-points` |
| Codepoint | 2 | `code-point-count` `code-point-at` |
| Normalize | 4 | `nfc` `nfd` `nfkc` `nfkd` |
| Case | 3 | `to-upper` `to-lower` `to-title` |
| System | 1 | `halt` |

## Profiles

| Profile | Primitives | Footprint | Target |
|---------|------------|-----------|--------|
| Embedded | 60 | ~2 KB | Microcontrollers |
| Standard | 77 | ~35 KB | General embedded (ASCII text) |
| Full | 89 | ~200 KB | Desktop/server (Unicode 15.0) |

## Text and Unicode

### 128-bit Grapheme Representation

Grapheme clusters are stored inline in 128 bits (2 stack cells), avoiding lookup tables:

```
Lo (64 bits): [len:4][zwj_flags:4][cp0:21][cp1:21][cp2_lo:14]
Hi (64 bits): [cp2_hi:7][cp3:21][cp4:21][reserved:15]
```

- Supports up to 5 base code points per grapheme
- ZWJ flags indicate joins between adjacent code points
- Handles emoji sequences like 👨‍👩‍👧‍👦 (family) inline

### UAX #29 Grapheme Segmentation

Full implementation of Unicode Text Segmentation:

| Rule | Description |
|------|-------------|
| GB1-GB5 | Break at start/end, keep CR×LF |
| GB6-GB8 | Hangul syllable sequences |
| GB9-GB9a | Extend and SpacingMark |
| GB9b | Prepend characters |
| GB11 | Extended_Pictographic + ZWJ sequences |
| GB12-GB13 | Regional Indicator pairs (flags) |
| GB999 | Default break |

### Unicode Tables

Generated from Unicode 15.0:

| Table | Entries |
|-------|---------|
| Uppercase mappings | 1,450 |
| Lowercase mappings | 1,433 |
| Combining classes | 922 |
| Composition pairs | 1,026 |
| Grapheme break properties | ~30,000 |
| Extended_Pictographic | ~3,500 |

## Automatic Parallelism

Parallelism is not exposed as primitives. The runtime automatically exploits universe isolation:

### Task Parallelism (Ravenscar)

```ada
pragma Profile (Ravenscar);

--  Fixed worker pool (static allocation)
Workers : array (1 .. Num_Workers) of Worker_Task;

--  Submit universe for parallel execution
Submit_Work (U, Callback);
Wait_All;  -- Barrier synchronization
```

- Deterministic scheduling
- No dynamic task creation
- Priority ceiling protocol for protected objects

### Data Parallelism (SIMD)

```ada
--  Auto-vectorized by GNAT with -O2/-O3
Par_Map (U, Start, Length, Op_Double'Access);
Par_Reduce (U, Start, Length, 0, Op_Add'Access, Sum);
Par_Fill (U, Start, Length, Value);
```

| Operation | Description |
|-----------|-------------|
| `Par_Map` | Apply unary op to each element |
| `Par_Map_Binary` | Element-wise binary op |
| `Par_Map_Scalar` | Broadcast scalar op |
| `Par_Reduce` | Associative reduction |
| `Par_Scan` | Prefix sum |
| `Par_Fill` | Memory fill |
| `Par_Copy` | Memory copy |
| `Par_Sum/Min/Max` | Specialized reductions |

### Scaling

Same 89 primitives run everywhere:

| Target | Task Parallel | Data Parallel |
|--------|---------------|---------------|
| Microcontroller | Sequential | None |
| Desktop | N cores | SIMD |
| Server | M cores | SIMD + GPU |

## Compiler

The compiler transforms `.space` source to SPARK/Ada:

```
.space → Lexer → Parser → AST → CodeGen → .ads/.adb → GNAT → binary
```

### Example

**Input** (`example.space`):
```forth
: square  dup * ;
: cube    dup square * ;
: main    3 cube ;
```

**Output** (SPARK/Ada):
```ada
procedure Word_square (S : in Out Stack) is
begin
   Dup (S);
   Mul (S);
end Word_square;

procedure Word_cube (S : in Out Stack) is
begin
   Dup (S);
   Word_square (S);
   Mul (S);
end Word_cube;

procedure Word_main (S : in Out Stack) is
begin
   Push (S, 3);
   Word_cube (S);
end Word_main;
```

### Supported Syntax

```forth
\ Line comment
( Block comment )

: name ... ;              \ Word definition
123 0xFF -42              \ Numbers
"hello"                   \ Strings

if ... then               \ Conditional
if ... else ... then      \ Conditional with else
begin ... while ... repeat \ Loop
10 0 do ... loop          \ Counted loop

universe{ ... }universe   \ Universe block
```

## Verification

All primitives have SPARK contracts:

```ada
procedure Dup (S : in Out Stack)
  with Pre  => Size (S) >= 1 and Size (S) < Stack_Capacity,
       Post => Size (S) = Size (S)'Old + 1
               and Top (S) = Top (S)'Old;
```

Verify with GNATprove:

```bash
gnatprove -P space.gpr --level=2 --timeout=10
```

## Implementation Status

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Core Runtime (41 primitives) | Complete |
| 2 | Universe Model (+20 primitives) | Complete |
| 3 | Text Handling (+28 primitives) | Complete |
| 4 | Parallel Execution | Complete |
| 5 | Compiler | Complete |
| 6 | Verification & Certification | Pending |

## Key Properties

- `SPARK_Mode => On` throughout
- All primitives have Pre/Post contracts
- No dynamic allocation after init
- Bounded stack and memory
- Deterministic execution
- Ravenscar-compliant tasking

## Requirements

- GNAT Pro or GNAT Community 2021+
- GNATprove (for verification)

No external dependencies. All tools are written in Ada.

## License

See LICENSE file.
