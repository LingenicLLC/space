# Space Language

**A verified concatenative systems programming language.**

**Date:** 2026-02-11
**Version:** 0.9 alpha
**Status:** SPARK/Ada **implementation in progress**

The core semantics were proven correct in F* using dependent types. The production implementation in SPARK reimplements those semantics with contracts verified by GNATprove, and extends them with parallel execution.

---

Space is a verified concatenative systems programming language that compiles to SPARK/Ada.

```
C       →  Assembly  →  machine code
Space   →  SPARK/Ada →  native binary
```

**Space provides high-level abstractions** — universes, disciplines, borrowing, warps — that compile down to verified SPARK/Ada code. GNATprove verifies the contracts. GNAT then compiles to **native binaries** or **bare metal**. 

---

## Space Concepts

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

| Discipline       | Copy | Drop | Use Case                         |
| ---------------- | ---- | ---- | -------------------------------- |
| **Unrestricted** | Yes  | Yes  | Integers, booleans               |
| **Affine**       | No   | Yes  | File handles, connections        |
| **Linear**       | No   | No   | Obligations, must-consume tokens |

### Warping

Warping provides structured traversal of data structures without exposing internal pointers. A warp maintains a current position and saved positions, enabling tree traversal with automatic backtracking.

```forth
warp-follow       \ Descend into child structure
warp-position     \ Save current position
warp-restore      \ Return to saved position
```

## 

- Website: https://spacelang.org
- GitHub: https://github.com/LingenicLLC/space
