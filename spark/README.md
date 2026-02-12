# Space

A formally verifiable programming language. Proven safe at compile time. Compiles to machine code.

## Overview

Space is a concatenative language with linear resources, universe isolation, and machine-checked proofs. The compiler is written in SPARK and generates SPARK code verified by GNATprove.

```
1024 linear create-universe as scratch
    42 push
    borrow-pointer from parent
    fetch-borrowed
    drop-pointer
    pop
end-universe
```

## Toolchain

```
Space source
    │
    ▼
Space Compiler (written in SPARK)
    │
    ▼
Generated SPARK code with contracts
    │
    ▼
GNATprove (verifies via Z3/Alt-Ergo/CVC4)
    │
    ▼
GNAT → native binary (bare metal capable)
```

Space compiles to [SPARK](https://en.wikipedia.org/wiki/SPARK_(programming_language)), a safety-critical language used in aerospace and mission-critical industries. [GNATprove](https://docs.adacore.com/spark2014-docs/html/ug/en/gnatprove.html) proves your program is free of entire classes of bugs. Then [GNAT](https://en.wikipedia.org/wiki/GNAT) outputs machine code for Linux, macOS, Windows, or bare metal.

## Key Features

### Universes

Isolated memory regions with their own stack, their own discipline, and their own lifetime semantics.

```
1024 linear create-universe as scratch
    \ This is a self-contained world
    \ Nothing escapes. Nothing leaks in.
end-universe scratch
```

A universe isn't a memory pool. It's a bounded context of meaning. Pointers inside have significance. Outside, those same bits are meaningless.

### Self-Destructing Linear Universes

Linear universes self-destruct when their obligations are fulfilled — when the stack empties.

```
1024 linear create-universe as temp
    42 push
    pop        \ obligation fulfilled
end-universe   \ universe already gone — this acknowledges it
```

No explicit free. No destructor call. No reference count hitting zero. Completion itself is the cleanup.

### Disciplines

Substructural types applied at the container level:

| Discipline | Copy | Drop | Universe Behavior |
|------------|------|------|-------------------|
| **Unrestricted** | Yes | Yes | Immortal — never destroyed |
| **Affine** | No | Yes | Explicit release required |
| **Linear** | No | No | Self-destructs when empty |

### Warps

Address-free navigation through data structures. You enter a data structure and navigate, but you never hold a pointer to where you are.

```
ptr warp-into as w
    w warp-fetch      \ read current position
    1 w warp-advance  \ move forward
    w warp-follow     \ follow a reference
end-warp w            \ must close — warp is linear
```

Traversal without mapping. You can explore a data structure without creating a map of where you've been.

### O(1) Grapheme-Native Text

Space stores grapheme clusters — what users perceive as characters — with O(1) random access.

| Language | Unit | Random Access |
|----------|------|---------------|
| C | bytes | O(1) |
| Java | UTF-16 code units | O(1) |
| Python 3 | code points | O(1) |
| Rust | bytes | O(n) for chars |
| Swift | grapheme clusters | O(n) |
| **Space** | **grapheme clusters** | **O(1)** |

A family emoji (👨‍👩‍👧‍👦) is 25 bytes in UTF-8, but 1 grapheme. Space indexes by graphemes.

## What Gets Proven

- **No runtime errors** — No buffer overflows, no division by zero, no integer overflow
- **Contract compliance** — Preconditions imply postconditions
- **Data flow correctness** — No uninitialized reads, no data races
- **Universe isolation** — Pointers cannot escape their universe
- **Discipline compliance** — Linear values consumed exactly once
- **Borrow safety** — Borrowed pointers tracked and dropped correctly

## Novel Features

| Feature | Elsewhere | In Space |
|---------|-----------|----------|
| Self-destructing contexts | None | Linear universes |
| Address-free navigation | None | Warps |
| Grapheme text O(1) | Swift (O(n)) | O(1) random access |
| Linear types | Rust, Clean | At container level |
| Isolation | Erlang actors | Universes with disciplines |

## Heritage

Space synthesizes ideas from multiple traditions:

- **Forth** — Concatenative model, dual-stack architecture, minimalism
- **StrongForth** — Static typing for concatenative languages
- **Linear Logic (Girard)** — Disciplines (unrestricted, affine, linear)
- **Rust** — Borrowing, ownership-based memory safety
- **Ada/SPARK** — Contract-based verification, certification path
- **Clean** — Uniqueness types for safe mutation

## Future: On-Target Compilation

Some spacecraft run Forth — processors like the RTX2010 execute it directly. Space is designed for similar capability: a verified compiler small enough for embedded hardware.

The compiler's safety guarantees are all compile-time. A minimal Space → machine code compiler (20-50KB) could run on target hardware, enabling autonomous compilation in constrained environments.

## Project Structure

```
spark/
├── README.md
├── src/
│   ├── space_lexer.ads/adb
│   ├── space_parser.ads/adb
│   ├── space_ast.ads/adb
│   ├── space_codegen.ads/adb
│   └── ...
├── runtime/
│   ├── space_stack.ads/adb
│   ├── space_universe.ads/adb
│   └── ...
├── tests/
│   └── ...
├── examples/
│   └── ...
└── Analysis/
    └── FULL/
        ├── SPACE-UNIQUE-FEATURES.md
        ├── SPACE-TO-MACHINE-CODE.md
        ├── COLORFORTH-MODERNIZATION.md
        └── ...
```

## License

Apache 2.0

## Links

- **Website:** [spacelang.org](https://spacelang.org)
- **Formal Verification:** [spacelang.org/formal-verification](https://spacelang.org/formal-verification/)
- **Heritage:** [spacelang.org/heritage](https://spacelang.org/heritage/)
- **Philosophy:** [spacelang.org/philosophy](https://spacelang.org/philosophy/)

## Author

Created by [Danslav Slavenskoj](https://slavenskoj.com), [Lingenic LLC](https://lingenic.com).
