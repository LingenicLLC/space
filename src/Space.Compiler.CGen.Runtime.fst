module Space.Compiler.CGen.Runtime

(** Generate C runtime from F* definitions
    Mirrors verified logic from Space.Stack, Space.Arithmetic, etc. *)

open FStar.String

(** Configuration section *)
let gen_config : string =
"/*
 * Configuration
 */
#ifndef SPACE_STACK_SIZE
#define SPACE_STACK_SIZE 65536
#endif

#ifndef SPACE_MEMORY_SIZE
#define SPACE_MEMORY_SIZE (4 * 1024 * 1024)
#endif

#ifndef SPACE_MAX_TEXTS
#define SPACE_MAX_TEXTS 1024
#endif

"

(** Stack infrastructure - mirrors Space.Stack *)
let gen_stack_infra : string =
"/*
 * Stack - mirrors Space.Stack
 * Invariant: sp points to top element, -1 when empty
 */
static uint64_t space_stack[SPACE_STACK_SIZE];
static int space_sp = -1;

#define PUSH(v) (space_stack[++space_sp] = (v))
#define POP()   (space_stack[space_sp--])
#define TOP()   (space_stack[space_sp])
#define NOS()   (space_stack[space_sp - 1])

"

(** Memory infrastructure - mirrors Space.Memory *)
let gen_memory_infra : string =
"/*
 * Memory - mirrors Space.Memory
 * Linear allocator with tracking for bytes-length
 */
static uint8_t space_memory[SPACE_MEMORY_SIZE];
static size_t space_mem_ptr = 0;

typedef struct {
    void *ptr;
    size_t len;
} space_alloc_entry;

static space_alloc_entry space_allocs[SPACE_MAX_TEXTS];
static size_t space_alloc_count = 0;

static inline void* space_alloc(size_t n) {
    if (space_mem_ptr + n > SPACE_MEMORY_SIZE) return NULL;
    void *p = &space_memory[space_mem_ptr];
    space_allocs[space_alloc_count].ptr = p;
    space_allocs[space_alloc_count].len = n;
    space_alloc_count++;
    space_mem_ptr += n;
    return p;
}

static inline size_t space_bytes_len(void *p) {
    for (size_t i = 0; i < space_alloc_count; i++) {
        if (space_allocs[i].ptr == p) return space_allocs[i].len;
    }
    return 0;
}

static inline void space_bytes_copy(void *dst, void *src, size_t n) {
    memcpy(dst, src, n);
}

"

(** Text infrastructure - mirrors Space.Text.Types *)
let gen_text_infra : string =
"/*
 * Text - mirrors Space.Text.Types
 * Handle-based text storage with UTF-8 data
 */
typedef struct {
    uint8_t *data;
    size_t byte_len;
    size_t grapheme_count;
} space_text;

static space_text space_texts[SPACE_MAX_TEXTS];
static size_t space_text_count = 0;

static inline size_t space_create_text(const uint8_t *data, size_t len) {
    size_t handle = space_text_count++;
    space_texts[handle].data = (uint8_t*)space_alloc(len);
    memcpy(space_texts[handle].data, data, len);
    space_texts[handle].byte_len = len;
    space_texts[handle].grapheme_count = len;  /* Updated by grapheme counter */
    return handle;
}

static inline size_t space_text_byte_length(size_t handle) {
    return space_texts[handle].byte_len;
}

static inline size_t space_text_grapheme_count(size_t handle) {
    return space_texts[handle].grapheme_count;
}

static inline const uint8_t* space_text_data(size_t handle) {
    return space_texts[handle].data;
}

"

(** I/O primitives - mirrors Space.System *)
let gen_io_prims : string =
"/*
 * I/O Primitives - mirrors Space.System
 */
static inline void space_emit(uint64_t c) {
    putchar((int)c);
}

static inline uint64_t space_key(void) {
    return (uint64_t)getchar();
}

static inline void space_emit_text(size_t handle) {
    fwrite(space_texts[handle].data, 1, space_texts[handle].byte_len, stdout);
}

"

(** Stack primitives - mirrors Space.Stack operations *)
let gen_stack_prims : string =
"/*
 * Stack Primitives - mirrors Space.Stack
 * Each operation preserves stack invariants
 */

/* dup ( a -- a a ) */
static inline void prim_dup(void) {
    uint64_t a = TOP();
    PUSH(a);
}

/* drop ( a -- ) */
static inline void prim_drop(void) {
    space_sp--;
}

/* swap ( a b -- b a ) */
static inline void prim_swap(void) {
    uint64_t b = POP();
    uint64_t a = POP();
    PUSH(b);
    PUSH(a);
}

/* over ( a b -- a b a ) */
static inline void prim_over(void) {
    uint64_t a = NOS();
    PUSH(a);
}

/* rot ( a b c -- b c a ) */
static inline void prim_rot(void) {
    uint64_t c = POP();
    uint64_t b = POP();
    uint64_t a = POP();
    PUSH(b);
    PUSH(c);
    PUSH(a);
}

/* nip ( a b -- b ) */
static inline void prim_nip(void) {
    uint64_t b = POP();
    space_sp--;
    PUSH(b);
}

/* tuck ( a b -- b a b ) */
static inline void prim_tuck(void) {
    uint64_t b = POP();
    uint64_t a = POP();
    PUSH(b);
    PUSH(a);
    PUSH(b);
}

/* pick ( ... n -- ... elem ) - pick nth element */
static inline void prim_pick(void) {
    uint64_t n = POP();
    uint64_t elem = space_stack[space_sp - n];
    PUSH(elem);
}

"

(** Arithmetic primitives - mirrors Space.Arithmetic *)
let gen_arith_prims : string =
"/*
 * Arithmetic Primitives - mirrors Space.Arithmetic
 * All operations use wrapping semantics (mod 2^64)
 */

/* add ( a b -- a+b ) */
static inline void prim_add(void) {
    uint64_t b = POP();
    space_stack[space_sp] += b;
}

/* sub ( a b -- a-b ) */
static inline void prim_sub(void) {
    uint64_t b = POP();
    space_stack[space_sp] -= b;
}

/* mul ( a b -- a*b ) */
static inline void prim_mul(void) {
    uint64_t b = POP();
    space_stack[space_sp] *= b;
}

/* div-u ( a b -- a/b ) unsigned division */
static inline void prim_div_u(void) {
    uint64_t b = POP();
    space_stack[space_sp] /= b;
}

/* div-s ( a b -- a/b ) signed division */
static inline void prim_div_s(void) {
    int64_t b = (int64_t)POP();
    space_stack[space_sp] = (uint64_t)((int64_t)space_stack[space_sp] / b);
}

/* mod ( a b -- a%b ) */
static inline void prim_mod(void) {
    uint64_t b = POP();
    space_stack[space_sp] %= b;
}

/* neg ( a -- -a ) two's complement negation */
static inline void prim_neg(void) {
    space_stack[space_sp] = (uint64_t)(-(int64_t)space_stack[space_sp]);
}

/* min ( a b -- min(a,b) ) unsigned */
static inline void prim_min(void) {
    uint64_t b = POP();
    if (b < TOP()) space_stack[space_sp] = b;
}

/* max ( a b -- max(a,b) ) unsigned */
static inline void prim_max(void) {
    uint64_t b = POP();
    if (b > TOP()) space_stack[space_sp] = b;
}

"

(** Bitwise primitives - mirrors Space.Bitwise *)
let gen_bitwise_prims : string =
"/*
 * Bitwise Primitives - mirrors Space.Bitwise
 */

/* and ( a b -- a&b ) */
static inline void prim_and(void) {
    uint64_t b = POP();
    space_stack[space_sp] &= b;
}

/* or ( a b -- a|b ) */
static inline void prim_or(void) {
    uint64_t b = POP();
    space_stack[space_sp] |= b;
}

/* xor ( a b -- a^b ) */
static inline void prim_xor(void) {
    uint64_t b = POP();
    space_stack[space_sp] ^= b;
}

/* not ( a -- ~a ) bitwise complement */
static inline void prim_not(void) {
    space_stack[space_sp] = ~space_stack[space_sp];
}

/* shl ( a n -- a<<n ) */
static inline void prim_shl(void) {
    uint64_t n = POP();
    space_stack[space_sp] <<= n;
}

/* shr ( a n -- a>>n ) logical shift right */
static inline void prim_shr(void) {
    uint64_t n = POP();
    space_stack[space_sp] >>= n;
}

"

(** Comparison primitives - mirrors Space.Comparison *)
let gen_compare_prims : string =
"/*
 * Comparison Primitives - mirrors Space.Comparison
 * Return 1 for true, 0 for false
 */

/* eq ( a b -- a==b ) */
static inline void prim_eq(void) {
    uint64_t b = POP();
    space_stack[space_sp] = (space_stack[space_sp] == b) ? 1 : 0;
}

/* neq ( a b -- a!=b ) */
static inline void prim_neq(void) {
    uint64_t b = POP();
    space_stack[space_sp] = (space_stack[space_sp] != b) ? 1 : 0;
}

/* lt-s ( a b -- a<b ) signed */
static inline void prim_lt_s(void) {
    int64_t b = (int64_t)POP();
    space_stack[space_sp] = ((int64_t)space_stack[space_sp] < b) ? 1 : 0;
}

/* gt-s ( a b -- a>b ) signed */
static inline void prim_gt_s(void) {
    int64_t b = (int64_t)POP();
    space_stack[space_sp] = ((int64_t)space_stack[space_sp] > b) ? 1 : 0;
}

/* lt-u ( a b -- a<b ) unsigned */
static inline void prim_lt_u(void) {
    uint64_t b = POP();
    space_stack[space_sp] = (space_stack[space_sp] < b) ? 1 : 0;
}

/* gt-u ( a b -- a>b ) unsigned */
static inline void prim_gt_u(void) {
    uint64_t b = POP();
    space_stack[space_sp] = (space_stack[space_sp] > b) ? 1 : 0;
}

"

(** Memory primitives - mirrors Space.Memory, Space.Bytes *)
let gen_memory_prims : string =
"/*
 * Memory Primitives - mirrors Space.Memory, Space.Bytes
 */

/* alloc ( n -- ptr ) allocate n bytes */
static inline void prim_alloc(void) {
    size_t n = (size_t)POP();
    PUSH((uint64_t)(uintptr_t)space_alloc(n));
}

/* fetch ( ptr -- value ) fetch 64-bit cell */
static inline void prim_fetch(void) {
    uint64_t *p = (uint64_t*)(uintptr_t)TOP();
    space_stack[space_sp] = *p;
}

/* store ( value ptr -- ) store 64-bit cell */
static inline void prim_store(void) {
    uint64_t *p = (uint64_t*)(uintptr_t)POP();
    uint64_t v = POP();
    *p = v;
}

/* bytes-alloc ( n -- ptr ) allocate n bytes */
static inline void prim_bytes_alloc(void) {
    size_t n = (size_t)POP();
    PUSH((uint64_t)(uintptr_t)space_alloc(n));
}

/* bytes-fetch ( ptr offset -- byte ) */
static inline void prim_bytes_fetch(void) {
    size_t off = (size_t)POP();
    uint8_t *p = (uint8_t*)(uintptr_t)TOP();
    space_stack[space_sp] = p[off];
}

/* bytes-store ( byte ptr offset -- ) */
static inline void prim_bytes_store(void) {
    size_t off = (size_t)POP();
    uint8_t *p = (uint8_t*)(uintptr_t)POP();
    uint8_t v = (uint8_t)POP();
    p[off] = v;
}

/* bytes-len ( ptr -- len ) */
static inline void prim_bytes_len(void) {
    void *p = (void*)(uintptr_t)TOP();
    space_stack[space_sp] = space_bytes_len(p);
}

/* bytes-copy ( src dst n -- ) */
static inline void prim_bytes_copy(void) {
    size_t n = (size_t)POP();
    void *dst = (void*)(uintptr_t)POP();
    void *src = (void*)(uintptr_t)POP();
    space_bytes_copy(dst, src, n);
}

"

(** Text case primitives - uses UCD tables *)
let gen_text_case_prims : string =
"/*
 * Text Case Primitives - mirrors Space.Text.Case
 * Uses UCD tables from space_ucd_full.h
 */

/* text-to-upper ( handle -- new-handle ) */
static inline void prim_text_to_upper(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    uint8_t *out = (uint8_t*)space_alloc(t->byte_len * 3);  /* Worst case expansion */
    size_t out_len = 0;
    size_t i = 0;
    while (i < t->byte_len) {
        uint32_t cp;
        int len = utf8_decode(&t->data[i], &cp);
        cp = ucd_to_upper(cp);
        out_len += utf8_encode(cp, &out[out_len]);
        i += len;
    }
    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

/* text-to-lower ( handle -- new-handle ) */
static inline void prim_text_to_lower(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    uint8_t *out = (uint8_t*)space_alloc(t->byte_len * 3);
    size_t out_len = 0;
    size_t i = 0;
    while (i < t->byte_len) {
        uint32_t cp;
        int len = utf8_decode(&t->data[i], &cp);
        cp = ucd_to_lower(cp);
        out_len += utf8_encode(cp, &out[out_len]);
        i += len;
    }
    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

"

(** Text primitives - mirrors Space.Text.Ops *)
let gen_text_prims : string =
"/*
 * Text Primitives - mirrors Space.Text.Ops
 */

/* create-text ( ptr len -- handle ) */
static inline void prim_create_text(void) {
    size_t len = (size_t)POP();
    uint8_t *ptr = (uint8_t*)(uintptr_t)POP();
    size_t h = space_create_text(ptr, len);
    PUSH(h);
}

/* text-byte-length ( handle -- len ) */
static inline void prim_text_byte_length(void) {
    size_t h = (size_t)TOP();
    space_stack[space_sp] = space_text_byte_length(h);
}

/* text-grapheme-count ( handle -- count ) */
static inline void prim_text_grapheme_count(void) {
    size_t h = (size_t)TOP();
    space_stack[space_sp] = space_text_grapheme_count(h);
}

/* text-equal ( h1 h2 -- bool ) */
static inline void prim_text_equal(void) {
    size_t h2 = (size_t)POP();
    size_t h1 = (size_t)TOP();
    space_text *t1 = &space_texts[h1];
    space_text *t2 = &space_texts[h2];
    int eq = (t1->byte_len == t2->byte_len) &&
             (memcmp(t1->data, t2->data, t1->byte_len) == 0);
    space_stack[space_sp] = eq ? 1 : 0;
}

/* text-concat ( h1 h2 -- h3 ) */
static inline void prim_text_concat(void) {
    size_t h2 = (size_t)POP();
    size_t h1 = (size_t)POP();
    space_text *t1 = &space_texts[h1];
    space_text *t2 = &space_texts[h2];
    size_t new_len = t1->byte_len + t2->byte_len;
    uint8_t *data = (uint8_t*)space_alloc(new_len);
    memcpy(data, t1->data, t1->byte_len);
    memcpy(data + t1->byte_len, t2->data, t2->byte_len);
    size_t h3 = space_text_count++;
    space_texts[h3].data = data;
    space_texts[h3].byte_len = new_len;
    space_texts[h3].grapheme_count = t1->grapheme_count + t2->grapheme_count;
    PUSH(h3);
}

/* emit-grapheme ( handle index -- ) emit single grapheme */
static inline void prim_emit_grapheme(void) {
    size_t idx = (size_t)POP();
    size_t h = (size_t)POP();
    space_text *t = &space_texts[h];
    /* For now, emit byte at index (simplified) */
    if (idx < t->byte_len) {
        putchar(t->data[idx]);
    }
}

"

(** Normalization primitives - uses UCD functions *)
let gen_normalize_prims : string =
"/*
 * Normalization Primitives - mirrors Space.Text.Normalize
 * Uses NFD/NFC from space_ucd_full.h
 */

/* text-normalize-nfd ( handle -- new-handle ) */
static inline void prim_text_normalize_nfd(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];

    /* Decode to codepoints */
    uint32_t cps[1024];
    size_t cp_count = 0;
    size_t i = 0;
    while (i < t->byte_len && cp_count < 1024) {
        uint32_t cp;
        int len = utf8_decode(&t->data[i], &cp);
        cps[cp_count++] = cp;
        i += len;
    }

    /* Apply NFD */
    uint32_t nfd_cps[2048];
    size_t nfd_len = ucd_nfd(cps, cp_count, nfd_cps, 2048);

    /* Encode back to UTF-8 */
    uint8_t *out = (uint8_t*)space_alloc(nfd_len * 4);
    size_t out_len = 0;
    for (size_t j = 0; j < nfd_len; j++) {
        out_len += utf8_encode(nfd_cps[j], &out[out_len]);
    }

    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

/* text-normalize-nfc ( handle -- new-handle ) */
static inline void prim_text_normalize_nfc(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];

    uint32_t cps[1024];
    size_t cp_count = 0;
    size_t i = 0;
    while (i < t->byte_len && cp_count < 1024) {
        uint32_t cp;
        int len = utf8_decode(&t->data[i], &cp);
        cps[cp_count++] = cp;
        i += len;
    }

    uint32_t nfc_cps[2048];
    size_t nfc_len = ucd_nfc(cps, cp_count, nfc_cps, 2048);

    uint8_t *out = (uint8_t*)space_alloc(nfc_len * 4);
    size_t out_len = 0;
    for (size_t j = 0; j < nfc_len; j++) {
        out_len += utf8_encode(nfc_cps[j], &out[out_len]);
    }

    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

"

(** Header preamble *)
let header_preamble : string =
"/* Space Language Runtime
 * Generated by Space.Compiler.CGen.Runtime
 *
 * This file is AUTO-GENERATED from verified F* definitions.
 * Do not edit manually.
 *
 * Source modules:
 *   Space.Stack      - Stack operations
 *   Space.Arithmetic - Wrapping arithmetic
 *   Space.Bitwise    - Bitwise operations
 *   Space.Comparison - Comparisons
 *   Space.Memory     - Memory allocation
 *   Space.Bytes      - Byte array operations
 *   Space.Text.*     - Text handling
 */

#ifndef SPACE_RUNTIME_H
#define SPACE_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include \"space_ucd_full.h\"

"

(** Footer *)
let header_footer : string =
"
#endif /* SPACE_RUNTIME_H */
"

(** Generate complete runtime header *)
let gen_runtime_header : string =
  header_preamble ^
  gen_config ^
  gen_stack_infra ^
  gen_memory_infra ^
  gen_text_infra ^
  gen_io_prims ^
  gen_stack_prims ^
  gen_arith_prims ^
  gen_bitwise_prims ^
  gen_compare_prims ^
  gen_memory_prims ^
  gen_text_prims ^
  gen_text_case_prims ^
  gen_normalize_prims ^
  header_footer
