module Space.Compiler.CGen.C99

(** C99 backend for Space code generation
    Generates portable C99 code that mirrors verified logic.

    Target: Embedded systems, bare metal, maximum portability *)

open FStar.String
open Space.Compiler.CGen.Common

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

(** Universe infrastructure - mirrors Space.Universe *)
let gen_universe_infra : string =
"/*
 * Universe - mirrors Space.Universe
 * Isolated memory region with its own stack and discipline
 */

typedef enum {
    DISC_UNRESTRICTED = 0,  /* Can copy and drop freely */
    DISC_AFFINE = 1,        /* Cannot copy, can drop */
    DISC_LINEAR = 2         /* Cannot copy, cannot drop - must consume exactly once */
} space_discipline;

typedef enum {
    UNIVERSE_LIVE = 0,
    UNIVERSE_DESTROYED = 1
} space_universe_state;

#ifndef SPACE_MAX_UNIVERSES
#define SPACE_MAX_UNIVERSES 64
#endif

#ifndef SPACE_UNIVERSE_STACK_SIZE
#define SPACE_UNIVERSE_STACK_SIZE 1024
#endif

#ifndef SPACE_UNIVERSE_MEM_SIZE
#define SPACE_UNIVERSE_MEM_SIZE (64 * 1024)
#endif

typedef struct {
    uint32_t id;
    char name[64];
    space_discipline discipline;
    space_universe_state state;
    uint64_t stack[SPACE_UNIVERSE_STACK_SIZE];
    int sp;
    uint8_t memory[SPACE_UNIVERSE_MEM_SIZE];
    size_t mem_ptr;
    uint32_t capacity;
} space_universe;

static space_universe space_universes[SPACE_MAX_UNIVERSES];
static uint32_t space_universe_count = 0;
static uint32_t space_current_universe = 0;  /* 0 = global/parent */

static inline uint32_t space_create_universe(const char *name, uint32_t capacity, space_discipline disc) {
    if (space_universe_count >= SPACE_MAX_UNIVERSES) return 0;
    uint32_t id = ++space_universe_count;
    space_universe *u = &space_universes[id];
    u->id = id;
    strncpy(u->name, name, 63);
    u->name[63] = '\\0';
    u->discipline = disc;
    u->state = UNIVERSE_LIVE;
    u->sp = -1;
    u->mem_ptr = 0;
    u->capacity = capacity;
    return id;
}

static inline int space_universe_is_live(uint32_t id) {
    if (id == 0 || id > space_universe_count) return 0;
    return space_universes[id].state == UNIVERSE_LIVE;
}

static inline int space_universe_stack_empty(uint32_t id) {
    if (id == 0 || id > space_universe_count) return 1;
    return space_universes[id].sp < 0;
}

static inline int space_universe_should_destruct(uint32_t id) {
    if (id == 0 || id > space_universe_count) return 0;
    space_universe *u = &space_universes[id];
    return u->discipline == DISC_LINEAR && u->sp < 0 && u->state == UNIVERSE_LIVE;
}

static inline void space_universe_destroy(uint32_t id) {
    if (id == 0 || id > space_universe_count) return;
    space_universes[id].state = UNIVERSE_DESTROYED;
}

static inline int space_universe_push(uint32_t id, uint64_t v) {
    if (!space_universe_is_live(id)) return 0;
    space_universe *u = &space_universes[id];
    if (u->sp >= (int)u->capacity - 1) return 0;
    u->stack[++u->sp] = v;
    return 1;
}

static inline int space_universe_pop(uint32_t id, uint64_t *v) {
    if (!space_universe_is_live(id)) return 0;
    space_universe *u = &space_universes[id];
    if (u->sp < 0) return 0;
    *v = u->stack[u->sp--];
    return 1;
}

static inline void* space_universe_alloc(uint32_t id, size_t n) {
    if (!space_universe_is_live(id)) return NULL;
    space_universe *u = &space_universes[id];
    if (u->mem_ptr + n > SPACE_UNIVERSE_MEM_SIZE) return NULL;
    void *p = &u->memory[u->mem_ptr];
    u->mem_ptr += n;
    return p;
}

"

(** Borrowing infrastructure - mirrors Space.Borrow *)
let gen_borrow_infra : string =
"/*
 * Borrowing - mirrors Space.Borrow
 * Borrowed pointers for cross-universe access
 */

#ifndef SPACE_MAX_BORROWS
#define SPACE_MAX_BORROWS 256
#endif

typedef struct {
    uint64_t address;
    uint32_t source_id;
    int is_active;
} space_borrowed;

static space_borrowed space_borrows[SPACE_MAX_BORROWS];
static size_t space_borrow_count = 0;

static inline size_t space_borrow_pointer(uint32_t src_id, uint64_t addr) {
    if (space_borrow_count >= SPACE_MAX_BORROWS) return SIZE_MAX;
    size_t idx = space_borrow_count++;
    space_borrows[idx].address = addr;
    space_borrows[idx].source_id = src_id;
    space_borrows[idx].is_active = 1;
    return idx;
}

static inline int space_borrow_active(size_t idx) {
    if (idx >= space_borrow_count) return 0;
    return space_borrows[idx].is_active;
}

static inline void space_drop_borrow(size_t idx) {
    if (idx < space_borrow_count) {
        space_borrows[idx].is_active = 0;
    }
}

static inline uint64_t space_fetch_borrowed(size_t idx) {
    if (idx >= space_borrow_count || !space_borrows[idx].is_active) return 0;
    uint64_t *p = (uint64_t*)(uintptr_t)space_borrows[idx].address;
    return *p;
}

static inline void space_store_borrowed(size_t idx, uint64_t v) {
    if (idx >= space_borrow_count || !space_borrows[idx].is_active) return;
    uint64_t *p = (uint64_t*)(uintptr_t)space_borrows[idx].address;
    *p = v;
}

static inline int space_has_borrows_from(uint32_t src_id) {
    for (size_t i = 0; i < space_borrow_count; i++) {
        if (space_borrows[i].source_id == src_id && space_borrows[i].is_active) {
            return 1;
        }
    }
    return 0;
}

"

(** Warp infrastructure - mirrors Space.Warp *)
let gen_warp_infra : string =
"/*
 * Warps - mirrors Space.Warp
 * Structured traversal of universes
 */

#ifndef SPACE_MAX_WARPS
#define SPACE_MAX_WARPS 32
#endif

#ifndef SPACE_WARP_SAVE_DEPTH
#define SPACE_WARP_SAVE_DEPTH 16
#endif

typedef struct {
    uint32_t id;
    char name[64];
    uint32_t target_id;
    uint64_t position;
    uint64_t saved_positions[SPACE_WARP_SAVE_DEPTH];
    int save_count;
    int readonly;
    int active;
} space_warp;

static space_warp space_warps[SPACE_MAX_WARPS];
static uint32_t space_warp_count = 0;

static inline uint32_t space_warp_into(const char *name, uint32_t target_id, uint64_t pos, int readonly) {
    if (space_warp_count >= SPACE_MAX_WARPS) return 0;
    uint32_t id = ++space_warp_count;
    space_warp *w = &space_warps[id];
    w->id = id;
    strncpy(w->name, name, 63);
    w->name[63] = '\\0';
    w->target_id = target_id;
    w->position = pos;
    w->save_count = 0;
    w->readonly = readonly;
    w->active = 1;
    return id;
}

static inline void space_end_warp(uint32_t id) {
    if (id == 0 || id > space_warp_count) return;
    space_warps[id].active = 0;
}

static inline int space_warp_active(uint32_t id) {
    if (id == 0 || id > space_warp_count) return 0;
    return space_warps[id].active;
}

static inline uint64_t space_warp_position(uint32_t id) {
    if (id == 0 || id > space_warp_count) return 0;
    return space_warps[id].position;
}

static inline void space_warp_set_position(uint32_t id, uint64_t pos) {
    if (id == 0 || id > space_warp_count) return;
    space_warps[id].position = pos;
}

static inline void space_warp_advance(uint32_t id, uint64_t offset) {
    if (id == 0 || id > space_warp_count) return;
    space_warps[id].position += offset;
}

static inline uint64_t space_warp_fetch(uint32_t id) {
    if (id == 0 || id > space_warp_count || !space_warps[id].active) return 0;
    uint64_t *p = (uint64_t*)(uintptr_t)space_warps[id].position;
    return *p;
}

static inline void space_warp_store(uint32_t id, uint64_t v) {
    if (id == 0 || id > space_warp_count || !space_warps[id].active) return;
    if (space_warps[id].readonly) return;
    uint64_t *p = (uint64_t*)(uintptr_t)space_warps[id].position;
    *p = v;
}

static inline void space_warp_save(uint32_t id) {
    if (id == 0 || id > space_warp_count) return;
    space_warp *w = &space_warps[id];
    if (w->save_count < SPACE_WARP_SAVE_DEPTH) {
        w->saved_positions[w->save_count++] = w->position;
    }
}

static inline int space_warp_restore(uint32_t id, uint64_t pos) {
    if (id == 0 || id > space_warp_count) return 0;
    space_warp *w = &space_warps[id];
    for (int i = 0; i < w->save_count; i++) {
        if (w->saved_positions[i] == pos) {
            w->position = pos;
            return 1;
        }
    }
    return 0;
}

static inline int space_warp_is_null(uint32_t id) {
    if (id == 0 || id > space_warp_count) return 1;
    return space_warps[id].position == 0;
}

static inline int space_has_warps_to(uint32_t target_id) {
    for (uint32_t i = 1; i <= space_warp_count; i++) {
        if (space_warps[i].target_id == target_id && space_warps[i].active) {
            return 1;
        }
    }
    return 0;
}

"

(** Universe primitives for stack operations *)
let gen_universe_prims : string =
"/*
 * Universe Primitives - create/destroy/push/pop
 */

/* create-universe ( capacity discipline -- id ) */
static inline void prim_create_universe(void) {
    space_discipline disc = (space_discipline)POP();
    uint32_t cap = (uint32_t)POP();
    uint32_t id = space_create_universe(\"anon\", cap, disc);
    PUSH(id);
}

/* end-universe ( id -- ) check for self-destruct */
static inline void prim_end_universe(void) {
    uint32_t id = (uint32_t)POP();
    if (space_universe_should_destruct(id)) {
        space_universe_destroy(id);
    }
}

/* release-universe ( id -- ) explicit destroy for affine */
static inline void prim_release_universe(void) {
    uint32_t id = (uint32_t)POP();
    if (id > 0 && id <= space_universe_count) {
        if (space_universes[id].discipline == DISC_AFFINE) {
            space_universe_destroy(id);
        }
    }
}

/* universe-push ( value id -- ) */
static inline void prim_universe_push(void) {
    uint32_t id = (uint32_t)POP();
    uint64_t v = POP();
    space_universe_push(id, v);
}

/* universe-pop ( id -- value ) */
static inline void prim_universe_pop(void) {
    uint32_t id = (uint32_t)POP();
    uint64_t v = 0;
    space_universe_pop(id, &v);
    PUSH(v);
}

"

(** Borrow primitives *)
let gen_borrow_prims : string =
"/*
 * Borrow Primitives - borrow/drop/fetch/store
 */

/* borrow-pointer ( addr src-id -- borrow-idx ) */
static inline void prim_borrow_pointer(void) {
    uint32_t src = (uint32_t)POP();
    uint64_t addr = POP();
    size_t idx = space_borrow_pointer(src, addr);
    PUSH((uint64_t)idx);
}

/* drop-pointer ( borrow-idx -- ) */
static inline void prim_drop_pointer(void) {
    size_t idx = (size_t)POP();
    space_drop_borrow(idx);
}

/* return-pointer ( borrow-idx src-id -- ) return to source stack */
static inline void prim_return_pointer(void) {
    uint32_t src = (uint32_t)POP();
    size_t idx = (size_t)POP();
    if (space_borrow_active(idx)) {
        uint64_t addr = space_borrows[idx].address;
        space_drop_borrow(idx);
        space_universe_push(src, addr);
    }
}

/* fetch-borrowed ( borrow-idx -- value ) */
static inline void prim_fetch_borrowed(void) {
    size_t idx = (size_t)TOP();
    space_stack[space_sp] = space_fetch_borrowed(idx);
}

/* store-borrowed ( value borrow-idx -- ) */
static inline void prim_store_borrowed(void) {
    size_t idx = (size_t)POP();
    uint64_t v = POP();
    space_store_borrowed(idx, v);
}

/* fetch-and-end ( borrow-idx -- value ) fetch then drop borrow */
static inline void prim_fetch_and_end(void) {
    size_t idx = (size_t)TOP();
    space_stack[space_sp] = space_fetch_borrowed(idx);
    space_drop_borrow(idx);
}

/* store-and-end ( value borrow-idx -- ) store then drop borrow */
static inline void prim_store_and_end(void) {
    size_t idx = (size_t)POP();
    uint64_t v = POP();
    space_store_borrowed(idx, v);
    space_drop_borrow(idx);
}

/* offset-borrowed ( offset borrow-idx -- new-borrow-idx ) */
static inline void prim_offset_borrowed(void) {
    size_t idx = (size_t)POP();
    uint64_t offset = POP();
    if (idx < space_borrow_count && space_borrows[idx].is_active) {
        uint64_t new_addr = space_borrows[idx].address + offset;
        size_t new_idx = space_borrow_pointer(space_borrows[idx].source_id, new_addr);
        PUSH((uint64_t)new_idx);
    } else {
        PUSH(SIZE_MAX);
    }
}

"

(** Warp primitives *)
let gen_warp_prims : string =
"/*
 * Warp Primitives - warp-into/fetch/store/advance/etc
 */

/* warp-into ( pos target-id -- warp-id ) */
static inline void prim_warp_into(void) {
    uint32_t target = (uint32_t)POP();
    uint64_t pos = POP();
    uint32_t id = space_warp_into(\"anon\", target, pos, 0);
    PUSH(id);
}

/* warp-into-readonly ( pos target-id -- warp-id ) */
static inline void prim_warp_into_readonly(void) {
    uint32_t target = (uint32_t)POP();
    uint64_t pos = POP();
    uint32_t id = space_warp_into(\"anon\", target, pos, 1);
    PUSH(id);
}

/* end-warp ( warp-id -- ) */
static inline void prim_end_warp(void) {
    uint32_t id = (uint32_t)POP();
    space_end_warp(id);
}

/* warp-fetch ( warp-id -- value ) */
static inline void prim_warp_fetch(void) {
    uint32_t id = (uint32_t)TOP();
    space_stack[space_sp] = space_warp_fetch(id);
}

/* warp-store ( value warp-id -- ) */
static inline void prim_warp_store(void) {
    uint32_t id = (uint32_t)POP();
    uint64_t v = POP();
    space_warp_store(id, v);
}

/* warp-advance ( offset warp-id -- ) */
static inline void prim_warp_advance(void) {
    uint32_t id = (uint32_t)POP();
    uint64_t offset = POP();
    space_warp_advance(id, offset);
}

/* warp-position ( warp-id -- pos ) */
static inline void prim_warp_pos(void) {
    uint32_t id = (uint32_t)TOP();
    space_stack[space_sp] = space_warp_position(id);
}

/* warp-save ( warp-id -- ) save current position */
static inline void prim_warp_save(void) {
    uint32_t id = (uint32_t)POP();
    space_warp_save(id);
}

/* warp-restore ( pos warp-id -- success ) */
static inline void prim_warp_restore(void) {
    uint32_t id = (uint32_t)POP();
    uint64_t pos = POP();
    PUSH(space_warp_restore(id, pos) ? 1 : 0);
}

/* warp-null? ( warp-id -- bool ) */
static inline void prim_warp_null(void) {
    uint32_t id = (uint32_t)TOP();
    space_stack[space_sp] = space_warp_is_null(id) ? 1 : 0;
}

/* warp-follow ( warp-id -- value ) follow pointer at current position */
static inline void prim_warp_follow(void) {
    uint32_t id = (uint32_t)TOP();
    if (id > 0 && id <= space_warp_count && space_warps[id].active) {
        uint64_t *p = (uint64_t*)(uintptr_t)space_warps[id].position;
        uint64_t target = *p;
        space_warps[id].position = target;
        space_stack[space_sp] = target;
    } else {
        space_stack[space_sp] = 0;
    }
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

/* text-to-title ( handle -- new-handle ) */
static inline void prim_text_to_title(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    uint8_t *out = (uint8_t*)space_alloc(t->byte_len * 3);
    size_t out_len = 0;
    size_t i = 0;
    int at_word_start = 1;
    while (i < t->byte_len) {
        uint32_t cp;
        int len = utf8_decode(&t->data[i], &cp);
        if (at_word_start) {
            cp = ucd_to_title(cp);
            at_word_start = 0;
        } else {
            cp = ucd_to_lower(cp);
        }
        /* Check if this is a word boundary (space, punctuation) */
        if (cp == ' ' || cp == '\\t' || cp == '\\n' || cp == '-' || cp == '_') {
            at_word_start = 1;
        }
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
    /* Find grapheme at index and emit its bytes */
    size_t g = 0, i = 0;
    while (i < t->byte_len && g < idx) {
        int len = utf8_char_len(t->data[i]);
        i += len;
        g++;
    }
    if (i < t->byte_len) {
        int len = utf8_char_len(t->data[i]);
        fwrite(&t->data[i], 1, len, stdout);
    }
}

/* text-is-simple ( handle -- bool ) check if ASCII-only */
static inline void prim_text_is_simple(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    int simple = 1;
    for (size_t i = 0; i < t->byte_len; i++) {
        if (t->data[i] > 127) {
            simple = 0;
            break;
        }
    }
    space_stack[space_sp] = simple ? 1 : 0;
}

/* text-grapheme-at ( handle index -- grapheme-handle ) */
static inline void prim_text_grapheme_at(void) {
    size_t idx = (size_t)POP();
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    size_t g = 0, i = 0;
    while (i < t->byte_len && g < idx) {
        int len = utf8_char_len(t->data[i]);
        i += len;
        g++;
    }
    if (i < t->byte_len) {
        int len = utf8_char_len(t->data[i]);
        size_t new_h = space_create_text(&t->data[i], len);
        space_stack[space_sp] = new_h;
    } else {
        space_stack[space_sp] = SIZE_MAX;
    }
}

/* text-grapheme-first ( handle -- grapheme-handle ) */
static inline void prim_text_grapheme_first(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    if (t->byte_len > 0) {
        int len = utf8_char_len(t->data[0]);
        size_t new_h = space_create_text(t->data, len);
        space_stack[space_sp] = new_h;
    } else {
        space_stack[space_sp] = SIZE_MAX;
    }
}

/* text-grapheme-last ( handle -- grapheme-handle ) */
static inline void prim_text_grapheme_last(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    if (t->byte_len > 0) {
        /* Find start of last grapheme by scanning backwards */
        size_t i = t->byte_len - 1;
        while (i > 0 && (t->data[i] & 0xC0) == 0x80) {
            i--;
        }
        int len = t->byte_len - i;
        size_t new_h = space_create_text(&t->data[i], len);
        space_stack[space_sp] = new_h;
    } else {
        space_stack[space_sp] = SIZE_MAX;
    }
}

/* text-slice ( handle start end -- new-handle ) slice by grapheme indices */
static inline void prim_text_slice(void) {
    size_t end_idx = (size_t)POP();
    size_t start_idx = (size_t)POP();
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];

    /* Find byte offset for start */
    size_t g = 0, start_byte = 0;
    while (start_byte < t->byte_len && g < start_idx) {
        int len = utf8_char_len(t->data[start_byte]);
        start_byte += len;
        g++;
    }

    /* Find byte offset for end */
    size_t end_byte = start_byte;
    while (end_byte < t->byte_len && g < end_idx) {
        int len = utf8_char_len(t->data[end_byte]);
        end_byte += len;
        g++;
    }

    if (end_byte > start_byte) {
        size_t new_h = space_create_text(&t->data[start_byte], end_byte - start_byte);
        space_stack[space_sp] = new_h;
    } else {
        /* Empty slice */
        size_t new_h = space_create_text((uint8_t*)\"\", 0);
        space_stack[space_sp] = new_h;
    }
}

/* text-compare ( h1 h2 -- result ) lexicographic compare: -1, 0, 1 */
static inline void prim_text_compare(void) {
    size_t h2 = (size_t)POP();
    size_t h1 = (size_t)TOP();
    space_text *t1 = &space_texts[h1];
    space_text *t2 = &space_texts[h2];
    size_t min_len = t1->byte_len < t2->byte_len ? t1->byte_len : t2->byte_len;
    int cmp = memcmp(t1->data, t2->data, min_len);
    if (cmp == 0) {
        if (t1->byte_len < t2->byte_len) cmp = -1;
        else if (t1->byte_len > t2->byte_len) cmp = 1;
    } else {
        cmp = cmp < 0 ? -1 : 1;
    }
    space_stack[space_sp] = (uint64_t)(int64_t)cmp;
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

/* text-normalize-nfkd ( handle -- new-handle ) compatibility decomposition */
static inline void prim_text_normalize_nfkd(void) {
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

    uint32_t nfkd_cps[2048];
    size_t nfkd_len = ucd_nfkd(cps, cp_count, nfkd_cps, 2048);

    uint8_t *out = (uint8_t*)space_alloc(nfkd_len * 4);
    size_t out_len = 0;
    for (size_t j = 0; j < nfkd_len; j++) {
        out_len += utf8_encode(nfkd_cps[j], &out[out_len]);
    }

    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

/* text-normalize-nfkc ( handle -- new-handle ) compatibility composition */
static inline void prim_text_normalize_nfkc(void) {
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

    uint32_t nfkc_cps[2048];
    size_t nfkc_len = ucd_nfkc(cps, cp_count, nfkc_cps, 2048);

    uint8_t *out = (uint8_t*)space_alloc(nfkc_len * 4);
    size_t out_len = 0;
    for (size_t j = 0; j < nfkc_len; j++) {
        out_len += utf8_encode(nfkc_cps[j], &out[out_len]);
    }

    size_t new_h = space_text_count++;
    space_texts[new_h].data = out;
    space_texts[new_h].byte_len = out_len;
    space_texts[new_h].grapheme_count = t->grapheme_count;
    space_stack[space_sp] = new_h;
}

"

(** Text Warp primitives - grapheme-level warp traversal *)
let gen_text_warp_prims : string =
"/*
 * Text Warp Primitives - grapheme-level traversal
 * Warps over text handle, tracking grapheme position
 */

typedef struct {
    size_t text_handle;
    size_t byte_pos;
    size_t grapheme_pos;
    int active;
} space_text_warp;

static space_text_warp space_text_warps[SPACE_MAX_WARPS];
static size_t space_text_warp_count = 0;

/* text-warp-into ( text-handle -- twarp-id ) create text warp */
static inline void prim_text_warp_into(void) {
    size_t h = (size_t)POP();
    if (space_text_warp_count >= SPACE_MAX_WARPS) {
        PUSH(SIZE_MAX);
        return;
    }
    size_t id = space_text_warp_count++;
    space_text_warps[id].text_handle = h;
    space_text_warps[id].byte_pos = 0;
    space_text_warps[id].grapheme_pos = 0;
    space_text_warps[id].active = 1;
    PUSH(id);
}

/* warp-has-grapheme ( twarp-id -- bool ) */
static inline void prim_warp_has_grapheme(void) {
    size_t id = (size_t)TOP();
    if (id >= space_text_warp_count || !space_text_warps[id].active) {
        space_stack[space_sp] = 0;
        return;
    }
    space_text_warp *tw = &space_text_warps[id];
    space_text *t = &space_texts[tw->text_handle];
    space_stack[space_sp] = (tw->byte_pos < t->byte_len) ? 1 : 0;
}

/* warp-current-grapheme ( twarp-id -- grapheme-handle ) */
static inline void prim_warp_current_grapheme(void) {
    size_t id = (size_t)TOP();
    if (id >= space_text_warp_count || !space_text_warps[id].active) {
        space_stack[space_sp] = SIZE_MAX;
        return;
    }
    space_text_warp *tw = &space_text_warps[id];
    space_text *t = &space_texts[tw->text_handle];
    if (tw->byte_pos < t->byte_len) {
        int len = utf8_char_len(t->data[tw->byte_pos]);
        size_t new_h = space_create_text(&t->data[tw->byte_pos], len);
        space_stack[space_sp] = new_h;
    } else {
        space_stack[space_sp] = SIZE_MAX;
    }
}

/* warp-next-grapheme ( twarp-id -- ) advance to next grapheme */
static inline void prim_warp_next_grapheme(void) {
    size_t id = (size_t)POP();
    if (id >= space_text_warp_count || !space_text_warps[id].active) return;
    space_text_warp *tw = &space_text_warps[id];
    space_text *t = &space_texts[tw->text_handle];
    if (tw->byte_pos < t->byte_len) {
        int len = utf8_char_len(t->data[tw->byte_pos]);
        tw->byte_pos += len;
        tw->grapheme_pos++;
    }
}

/* warp-grapheme-index ( twarp-id -- index ) get current grapheme index */
static inline void prim_warp_grapheme_index(void) {
    size_t id = (size_t)TOP();
    if (id >= space_text_warp_count || !space_text_warps[id].active) {
        space_stack[space_sp] = SIZE_MAX;
        return;
    }
    space_stack[space_sp] = space_text_warps[id].grapheme_pos;
}

/* warp-goto-grapheme ( index twarp-id -- ) move to grapheme index */
static inline void prim_warp_goto_grapheme(void) {
    size_t id = (size_t)POP();
    size_t target = (size_t)POP();
    if (id >= space_text_warp_count || !space_text_warps[id].active) return;
    space_text_warp *tw = &space_text_warps[id];
    space_text *t = &space_texts[tw->text_handle];

    /* Reset to start */
    tw->byte_pos = 0;
    tw->grapheme_pos = 0;

    /* Advance to target */
    while (tw->grapheme_pos < target && tw->byte_pos < t->byte_len) {
        int len = utf8_char_len(t->data[tw->byte_pos]);
        tw->byte_pos += len;
        tw->grapheme_pos++;
    }
}

/* end-text-warp ( twarp-id -- ) */
static inline void prim_end_text_warp(void) {
    size_t id = (size_t)POP();
    if (id < space_text_warp_count) {
        space_text_warps[id].active = 0;
    }
}

"

(** Grapheme primitives - operations on single graphemes *)
let gen_grapheme_prims : string =
"/*
 * Grapheme Primitives - operations on grapheme handles
 */

/* grapheme-byte-length ( grapheme-handle -- len ) */
static inline void prim_grapheme_byte_length(void) {
    size_t h = (size_t)TOP();
    space_stack[space_sp] = space_texts[h].byte_len;
}

/* grapheme-is-ascii ( grapheme-handle -- bool ) */
static inline void prim_grapheme_is_ascii(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    int is_ascii = (t->byte_len == 1 && t->data[0] < 128) ? 1 : 0;
    space_stack[space_sp] = is_ascii;
}

/* grapheme-code-points ( grapheme-handle -- count ) count code points in grapheme */
static inline void prim_grapheme_code_points(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    size_t count = 0;
    size_t i = 0;
    while (i < t->byte_len) {
        int len = utf8_char_len(t->data[i]);
        i += len;
        count++;
    }
    space_stack[space_sp] = count;
}

"

(** Codepoint primitives - code point access *)
let gen_codepoint_prims : string =
"/*
 * Codepoint Primitives - access individual code points
 */

/* text-code-point-count ( handle -- count ) */
static inline void prim_text_code_point_count(void) {
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    size_t count = 0;
    size_t i = 0;
    while (i < t->byte_len) {
        int len = utf8_char_len(t->data[i]);
        i += len;
        count++;
    }
    space_stack[space_sp] = count;
}

/* text-code-point-at ( handle index -- codepoint ) */
static inline void prim_text_code_point_at(void) {
    size_t idx = (size_t)POP();
    size_t h = (size_t)TOP();
    space_text *t = &space_texts[h];
    size_t cp_idx = 0;
    size_t i = 0;
    while (i < t->byte_len && cp_idx < idx) {
        int len = utf8_char_len(t->data[i]);
        i += len;
        cp_idx++;
    }
    if (i < t->byte_len) {
        uint32_t cp;
        utf8_decode(&t->data[i], &cp);
        space_stack[space_sp] = cp;
    } else {
        space_stack[space_sp] = 0xFFFD; /* Replacement character */
    }
}

"

(** System primitives *)
let gen_system_prims : string =
"/*
 * System Primitives
 */

/* halt ( code -- ) halt with exit code */
static inline void prim_halt(void) {
    int code = (int)POP();
    exit(code);
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
 *   Space.Types      - Cell, discipline, identifiers
 *   Space.Stack      - Stack operations
 *   Space.Arithmetic - Wrapping arithmetic
 *   Space.Bitwise    - Bitwise operations
 *   Space.Comparison - Comparisons
 *   Space.Memory     - Memory allocation
 *   Space.Bytes      - Byte array operations
 *   Space.Universe   - Isolated memory regions
 *   Space.Borrow     - Cross-universe pointer access
 *   Space.Warp       - Structured traversal
 *   Space.Text.*     - Text handling with Unicode
 */

#ifndef SPACE_RUNTIME_H
#define SPACE_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
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
  gen_universe_infra ^
  gen_borrow_infra ^
  gen_warp_infra ^
  gen_text_infra ^
  gen_io_prims ^
  gen_stack_prims ^
  gen_arith_prims ^
  gen_bitwise_prims ^
  gen_compare_prims ^
  gen_memory_prims ^
  gen_universe_prims ^
  gen_borrow_prims ^
  gen_warp_prims ^
  gen_text_prims ^
  gen_text_case_prims ^
  gen_normalize_prims ^
  gen_text_warp_prims ^
  gen_grapheme_prims ^
  gen_codepoint_prims ^
  gen_system_prims ^
  header_footer
