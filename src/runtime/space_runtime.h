/* Space Language Runtime
 * Self-contained C runtime for compiled Space programs
 * No external dependencies
 */

#ifndef SPACE_RUNTIME_H
#define SPACE_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "space_ucd_full.h"

/*
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

/*
 * Stack
 */
static uint64_t space_stack[SPACE_STACK_SIZE];
static int space_sp = -1;

#define PUSH(v) (space_stack[++space_sp] = (v))
#define POP()   (space_stack[space_sp--])
#define TOP()   (space_stack[space_sp])
#define NOS()   (space_stack[space_sp - 1])

/*
 * Memory
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

/*
 * Text
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
    /* TODO: count graphemes properly */
    space_texts[handle].grapheme_count = len;
    return handle;
}

static inline size_t space_text_byte_length(size_t handle) {
    return space_texts[handle].byte_len;
}

static inline size_t space_text_grapheme_count(size_t handle) {
    return space_texts[handle].grapheme_count;
}

/*
 * I/O
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

/*
 * Stack Primitives (inline for performance)
 */
static inline void prim_dup(void)  { uint64_t a = TOP(); PUSH(a); }
static inline void prim_drop(void) { space_sp--; }
static inline void prim_swap(void) { uint64_t a = POP(); uint64_t b = POP(); PUSH(a); PUSH(b); }
static inline void prim_over(void) { uint64_t a = NOS(); PUSH(a); }
static inline void prim_rot(void)  { uint64_t c = POP(); uint64_t b = POP(); uint64_t a = POP(); PUSH(b); PUSH(c); PUSH(a); }
static inline void prim_nip(void)  { uint64_t a = POP(); space_sp--; PUSH(a); }
static inline void prim_tuck(void) { uint64_t a = POP(); uint64_t b = POP(); PUSH(a); PUSH(b); PUSH(a); }

/*
 * Arithmetic
 */
static inline void prim_add(void) { uint64_t b = POP(); space_stack[space_sp] += b; }
static inline void prim_sub(void) { uint64_t b = POP(); space_stack[space_sp] -= b; }
static inline void prim_mul(void) { uint64_t b = POP(); space_stack[space_sp] *= b; }
static inline void prim_div_u(void) { uint64_t b = POP(); space_stack[space_sp] /= b; }
static inline void prim_div_s(void) { int64_t b = (int64_t)POP(); space_stack[space_sp] = (uint64_t)((int64_t)space_stack[space_sp] / b); }
static inline void prim_mod(void) { uint64_t b = POP(); space_stack[space_sp] %= b; }
static inline void prim_neg(void) { space_stack[space_sp] = (uint64_t)(-(int64_t)space_stack[space_sp]); }
static inline void prim_min(void) { uint64_t b = POP(); if (b < TOP()) space_stack[space_sp] = b; }
static inline void prim_max(void) { uint64_t b = POP(); if (b > TOP()) space_stack[space_sp] = b; }

/*
 * Bitwise
 */
static inline void prim_and(void) { uint64_t b = POP(); space_stack[space_sp] &= b; }
static inline void prim_or(void)  { uint64_t b = POP(); space_stack[space_sp] |= b; }
static inline void prim_xor(void) { uint64_t b = POP(); space_stack[space_sp] ^= b; }
static inline void prim_not(void) { space_stack[space_sp] = ~space_stack[space_sp]; }
static inline void prim_shl(void) { uint64_t b = POP(); space_stack[space_sp] <<= b; }
static inline void prim_shr(void) { uint64_t b = POP(); space_stack[space_sp] >>= b; }

/*
 * Comparison
 */
static inline void prim_eq(void)  { uint64_t b = POP(); space_stack[space_sp] = (space_stack[space_sp] == b) ? 1 : 0; }
static inline void prim_neq(void) { uint64_t b = POP(); space_stack[space_sp] = (space_stack[space_sp] != b) ? 1 : 0; }
static inline void prim_lt_s(void) { int64_t b = (int64_t)POP(); space_stack[space_sp] = ((int64_t)space_stack[space_sp] < b) ? 1 : 0; }
static inline void prim_gt_s(void) { int64_t b = (int64_t)POP(); space_stack[space_sp] = ((int64_t)space_stack[space_sp] > b) ? 1 : 0; }
static inline void prim_lt_u(void) { uint64_t b = POP(); space_stack[space_sp] = (space_stack[space_sp] < b) ? 1 : 0; }
static inline void prim_gt_u(void) { uint64_t b = POP(); space_stack[space_sp] = (space_stack[space_sp] > b) ? 1 : 0; }

/*
 * Memory
 */
static inline void prim_alloc(void) {
    size_t n = (size_t)POP();
    PUSH((uint64_t)(uintptr_t)space_alloc(n));
}

static inline void prim_fetch(void) {
    uint64_t *p = (uint64_t*)(uintptr_t)TOP();
    space_stack[space_sp] = *p;
}

static inline void prim_store(void) {
    uint64_t v = POP();
    uint64_t *p = (uint64_t*)(uintptr_t)POP();
    *p = v;
}

static inline void prim_bytes_fetch(void) {
    size_t off = (size_t)POP();
    uint8_t *p = (uint8_t*)(uintptr_t)TOP();
    space_stack[space_sp] = p[off];
}

static inline void prim_bytes_store(void) {
    uint8_t v = (uint8_t)POP();
    size_t off = (size_t)POP();
    uint8_t *p = (uint8_t*)(uintptr_t)POP();
    p[off] = v;
}

/*
 * Text Case Mapping
 */
static inline void prim_text_to_upper(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    uint8_t *out = (uint8_t*)space_alloc(t->byte_len * 3);
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

#endif /* SPACE_RUNTIME_H */
