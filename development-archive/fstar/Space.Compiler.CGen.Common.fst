module Space.Compiler.CGen.Common

(** Common definitions for code generation backends

    Each backend implements these primitives for its target language.
    The primitives are verified in Space.* modules; backends emit
    code that mirrors the verified logic. *)

(** Primitive categories *)
type prim_category =
  | Cat_Stack       (* dup, drop, swap, over, rot, nip, tuck, pick *)
  | Cat_Arithmetic  (* add, sub, mul, div-u, div-s, mod, neg, min, max *)
  | Cat_Bitwise     (* and, or, xor, not, shl, shr *)
  | Cat_Comparison  (* eq, neq, lt-u, gt-u, lt-s, gt-s *)
  | Cat_Memory      (* fetch, store, alloc *)
  | Cat_Bytes       (* bytes-alloc, bytes-fetch, bytes-store, bytes-len, bytes-copy *)
  | Cat_Universe    (* create-universe, end-universe, release-universe, push, pop *)
  | Cat_Borrow      (* borrow-pointer, return-pointer, drop-pointer, fetch-borrowed,
                       store-borrowed, fetch-and-end, store-and-end, offset-borrowed *)
  | Cat_Warp        (* warp-into, warp-fetch, warp-store, warp-advance, warp-follow,
                       warp-position, warp-restore, warp-null, end-warp *)
  | Cat_Text        (* create-text, byte-length, grapheme-count, is-simple, grapheme-at,
                       grapheme-first, grapheme-last, slice, concat, equal, compare *)
  | Cat_TextWarp    (* has-grapheme, current-grapheme, next-grapheme, grapheme-index, goto-grapheme *)
  | Cat_Grapheme    (* byte-length, is-ascii, code-points *)
  | Cat_Codepoint   (* code-point-count, code-point-at *)
  | Cat_IO          (* emit, key, emit-grapheme *)
  | Cat_Normalize   (* nfc, nfd, nfkc, nfkd *)
  | Cat_Case        (* to-upper, to-lower, to-title *)
  | Cat_System      (* halt *)

(** Stack effect notation: ( before -- after ) *)
type stack_effect = {
  inputs: list string;
  outputs: list string;
}

(** Primitive definition *)
type primitive = {
  name: string;
  category: prim_category;
  effect: stack_effect;
  description: string;
}

(** All 84 primitives *)
let primitives : list primitive = [
  (* Stack - 8 primitives *)
  { name = "dup";   category = Cat_Stack; effect = { inputs = ["a"]; outputs = ["a"; "a"] }; description = "Duplicate top" };
  { name = "drop";  category = Cat_Stack; effect = { inputs = ["a"]; outputs = [] }; description = "Discard top" };
  { name = "swap";  category = Cat_Stack; effect = { inputs = ["a"; "b"]; outputs = ["b"; "a"] }; description = "Swap top two" };
  { name = "over";  category = Cat_Stack; effect = { inputs = ["a"; "b"]; outputs = ["a"; "b"; "a"] }; description = "Copy second to top" };
  { name = "rot";   category = Cat_Stack; effect = { inputs = ["a"; "b"; "c"]; outputs = ["b"; "c"; "a"] }; description = "Rotate third to top" };
  { name = "nip";   category = Cat_Stack; effect = { inputs = ["a"; "b"]; outputs = ["b"] }; description = "Drop second" };
  { name = "tuck";  category = Cat_Stack; effect = { inputs = ["a"; "b"]; outputs = ["b"; "a"; "b"] }; description = "Copy top below second" };
  { name = "pick";  category = Cat_Stack; effect = { inputs = ["n"]; outputs = ["elem"] }; description = "Copy nth element" };

  (* Arithmetic - 9 primitives *)
  { name = "add";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a+b"] }; description = "Add" };
  { name = "sub";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a-b"] }; description = "Subtract" };
  { name = "mul";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a*b"] }; description = "Multiply" };
  { name = "div-u"; category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a/b"] }; description = "Unsigned divide" };
  { name = "div-s"; category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a/b"] }; description = "Signed divide" };
  { name = "mod";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["a%b"] }; description = "Modulo" };
  { name = "neg";   category = Cat_Arithmetic; effect = { inputs = ["a"]; outputs = ["-a"] }; description = "Negate" };
  { name = "min";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["min"] }; description = "Minimum" };
  { name = "max";   category = Cat_Arithmetic; effect = { inputs = ["a"; "b"]; outputs = ["max"] }; description = "Maximum" };

  (* Bitwise - 6 primitives *)
  { name = "and";   category = Cat_Bitwise; effect = { inputs = ["a"; "b"]; outputs = ["a&b"] }; description = "Bitwise AND" };
  { name = "or";    category = Cat_Bitwise; effect = { inputs = ["a"; "b"]; outputs = ["a|b"] }; description = "Bitwise OR" };
  { name = "xor";   category = Cat_Bitwise; effect = { inputs = ["a"; "b"]; outputs = ["a^b"] }; description = "Bitwise XOR" };
  { name = "not";   category = Cat_Bitwise; effect = { inputs = ["a"]; outputs = ["~a"] }; description = "Bitwise NOT" };
  { name = "shl";   category = Cat_Bitwise; effect = { inputs = ["a"; "n"]; outputs = ["a<<n"] }; description = "Shift left" };
  { name = "shr";   category = Cat_Bitwise; effect = { inputs = ["a"; "n"]; outputs = ["a>>n"] }; description = "Shift right" };

  (* Comparison - 6 primitives *)
  { name = "eq";    category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Equal" };
  { name = "neq";   category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Not equal" };
  { name = "lt-u";  category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Less than unsigned" };
  { name = "gt-u";  category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Greater than unsigned" };
  { name = "lt-s";  category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Less than signed" };
  { name = "gt-s";  category = Cat_Comparison; effect = { inputs = ["a"; "b"]; outputs = ["bool"] }; description = "Greater than signed" };

  (* Memory - 3 primitives *)
  { name = "fetch"; category = Cat_Memory; effect = { inputs = ["ptr"]; outputs = ["value"] }; description = "Fetch 64-bit cell" };
  { name = "store"; category = Cat_Memory; effect = { inputs = ["value"; "ptr"]; outputs = [] }; description = "Store 64-bit cell" };
  { name = "alloc"; category = Cat_Memory; effect = { inputs = ["n"]; outputs = ["ptr"] }; description = "Allocate n bytes" };

  (* Bytes - 5 primitives *)
  { name = "bytes-alloc"; category = Cat_Bytes; effect = { inputs = ["n"]; outputs = ["ptr"] }; description = "Allocate bytes" };
  { name = "bytes-fetch"; category = Cat_Bytes; effect = { inputs = ["ptr"; "off"]; outputs = ["byte"] }; description = "Fetch byte" };
  { name = "bytes-store"; category = Cat_Bytes; effect = { inputs = ["byte"; "ptr"; "off"]; outputs = [] }; description = "Store byte" };
  { name = "bytes-len";   category = Cat_Bytes; effect = { inputs = ["ptr"]; outputs = ["len"] }; description = "Get length" };
  { name = "bytes-copy";  category = Cat_Bytes; effect = { inputs = ["src"; "dst"; "n"]; outputs = [] }; description = "Copy bytes" };

  (* Universe - 5 primitives *)
  { name = "create-universe";  category = Cat_Universe; effect = { inputs = ["cap"; "disc"]; outputs = ["id"] }; description = "Create universe" };
  { name = "end-universe";     category = Cat_Universe; effect = { inputs = ["id"]; outputs = [] }; description = "End universe scope" };
  { name = "release-universe"; category = Cat_Universe; effect = { inputs = ["id"]; outputs = [] }; description = "Release affine universe" };
  { name = "universe-push";    category = Cat_Universe; effect = { inputs = ["val"; "id"]; outputs = [] }; description = "Push to universe" };
  { name = "universe-pop";     category = Cat_Universe; effect = { inputs = ["id"]; outputs = ["val"] }; description = "Pop from universe" };

  (* Borrow - 8 primitives *)
  { name = "borrow-pointer";  category = Cat_Borrow; effect = { inputs = ["addr"; "src"]; outputs = ["idx"] }; description = "Start borrow" };
  { name = "return-pointer";  category = Cat_Borrow; effect = { inputs = ["idx"; "src"]; outputs = [] }; description = "Return to source" };
  { name = "drop-pointer";    category = Cat_Borrow; effect = { inputs = ["idx"]; outputs = [] }; description = "End borrow" };
  { name = "fetch-borrowed";  category = Cat_Borrow; effect = { inputs = ["idx"]; outputs = ["val"] }; description = "Fetch via borrow" };
  { name = "store-borrowed";  category = Cat_Borrow; effect = { inputs = ["val"; "idx"]; outputs = [] }; description = "Store via borrow" };
  { name = "fetch-and-end";   category = Cat_Borrow; effect = { inputs = ["idx"]; outputs = ["val"] }; description = "Fetch then end" };
  { name = "store-and-end";   category = Cat_Borrow; effect = { inputs = ["val"; "idx"]; outputs = [] }; description = "Store then end" };
  { name = "offset-borrowed"; category = Cat_Borrow; effect = { inputs = ["off"; "idx"]; outputs = ["idx2"] }; description = "Offset borrow" };

  (* Warp - 7 primitives (plus warp-into, end-warp) *)
  { name = "warp-fetch";    category = Cat_Warp; effect = { inputs = ["id"]; outputs = ["val"] }; description = "Read at position" };
  { name = "warp-store";    category = Cat_Warp; effect = { inputs = ["val"; "id"]; outputs = [] }; description = "Write at position" };
  { name = "warp-advance";  category = Cat_Warp; effect = { inputs = ["off"; "id"]; outputs = [] }; description = "Advance position" };
  { name = "warp-follow";   category = Cat_Warp; effect = { inputs = ["id"]; outputs = ["val"] }; description = "Follow pointer" };
  { name = "warp-position"; category = Cat_Warp; effect = { inputs = ["id"]; outputs = ["pos"] }; description = "Get position" };
  { name = "warp-restore";  category = Cat_Warp; effect = { inputs = ["pos"; "id"]; outputs = ["ok"] }; description = "Restore position" };
  { name = "warp-null";     category = Cat_Warp; effect = { inputs = ["id"]; outputs = ["bool"] }; description = "Is null position" };

  (* Text - 11 primitives *)
  { name = "create-text";      category = Cat_Text; effect = { inputs = ["ptr"; "len"]; outputs = ["h"] }; description = "Create text" };
  { name = "byte-length";      category = Cat_Text; effect = { inputs = ["h"]; outputs = ["len"] }; description = "Byte length" };
  { name = "grapheme-count";   category = Cat_Text; effect = { inputs = ["h"]; outputs = ["n"] }; description = "Grapheme count" };
  { name = "is-simple";        category = Cat_Text; effect = { inputs = ["h"]; outputs = ["bool"] }; description = "Is ASCII-only" };
  { name = "grapheme-at";      category = Cat_Text; effect = { inputs = ["h"; "i"]; outputs = ["gh"] }; description = "Get grapheme" };
  { name = "grapheme-first";   category = Cat_Text; effect = { inputs = ["h"]; outputs = ["gh"] }; description = "First grapheme" };
  { name = "grapheme-last";    category = Cat_Text; effect = { inputs = ["h"]; outputs = ["gh"] }; description = "Last grapheme" };
  { name = "slice";            category = Cat_Text; effect = { inputs = ["h"; "s"; "e"]; outputs = ["h2"] }; description = "Slice text" };
  { name = "concat";           category = Cat_Text; effect = { inputs = ["h1"; "h2"]; outputs = ["h3"] }; description = "Concatenate" };
  { name = "equal";            category = Cat_Text; effect = { inputs = ["h1"; "h2"]; outputs = ["bool"] }; description = "Text equal" };
  { name = "compare";          category = Cat_Text; effect = { inputs = ["h1"; "h2"]; outputs = ["ord"] }; description = "Compare texts" };

  (* Text Warp - 5 primitives *)
  { name = "has-grapheme";     category = Cat_TextWarp; effect = { inputs = ["tw"]; outputs = ["bool"] }; description = "Has grapheme" };
  { name = "current-grapheme"; category = Cat_TextWarp; effect = { inputs = ["tw"]; outputs = ["gh"] }; description = "Current grapheme" };
  { name = "next-grapheme";    category = Cat_TextWarp; effect = { inputs = ["tw"]; outputs = [] }; description = "Advance grapheme" };
  { name = "grapheme-index";   category = Cat_TextWarp; effect = { inputs = ["tw"]; outputs = ["i"] }; description = "Current index" };
  { name = "goto-grapheme";    category = Cat_TextWarp; effect = { inputs = ["i"; "tw"]; outputs = [] }; description = "Jump to index" };

  (* Grapheme - 3 primitives *)
  { name = "grapheme-byte-length"; category = Cat_Grapheme; effect = { inputs = ["gh"]; outputs = ["len"] }; description = "Grapheme bytes" };
  { name = "grapheme-is-ascii";    category = Cat_Grapheme; effect = { inputs = ["gh"]; outputs = ["bool"] }; description = "Is ASCII" };
  { name = "grapheme-code-points"; category = Cat_Grapheme; effect = { inputs = ["gh"]; outputs = ["n"] }; description = "Code point count" };

  (* Codepoint - 2 primitives *)
  { name = "code-point-count"; category = Cat_Codepoint; effect = { inputs = ["h"]; outputs = ["n"] }; description = "Count code points" };
  { name = "code-point-at";    category = Cat_Codepoint; effect = { inputs = ["h"; "i"]; outputs = ["cp"] }; description = "Get code point" };

  (* I/O - 3 primitives *)
  { name = "emit";          category = Cat_IO; effect = { inputs = ["c"]; outputs = [] }; description = "Emit byte" };
  { name = "key";           category = Cat_IO; effect = { inputs = []; outputs = ["c"] }; description = "Read byte" };
  { name = "emit-grapheme"; category = Cat_IO; effect = { inputs = ["h"; "i"]; outputs = [] }; description = "Emit grapheme" };

  (* Normalize - 4 primitives *)
  { name = "nfc";  category = Cat_Normalize; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "NFC normalize" };
  { name = "nfd";  category = Cat_Normalize; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "NFD normalize" };
  { name = "nfkc"; category = Cat_Normalize; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "NFKC normalize" };
  { name = "nfkd"; category = Cat_Normalize; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "NFKD normalize" };

  (* Case - 3 primitives *)
  { name = "to-upper"; category = Cat_Case; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "To uppercase" };
  { name = "to-lower"; category = Cat_Case; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "To lowercase" };
  { name = "to-title"; category = Cat_Case; effect = { inputs = ["h"]; outputs = ["h2"] }; description = "To titlecase" };

  (* System - 1 primitive *)
  { name = "halt"; category = Cat_System; effect = { inputs = ["code"]; outputs = [] }; description = "Halt with code" }
]

(** Backend type *)
type backend =
  | Backend_C99
  | Backend_C23
  | Backend_Cpp
  | Backend_Forth

(** Parse backend from string *)
let backend_of_string (s: string) : option backend =
  match s with
  | "c99" | "c" -> Some Backend_C99
  | "c23" -> Some Backend_C23
  | "c++" | "cpp" -> Some Backend_Cpp
  | "forth" | "fth" -> Some Backend_Forth
  | _ -> None

(** Backend name *)
let backend_to_string (b: backend) : string =
  match b with
  | Backend_C99 -> "c99"
  | Backend_C23 -> "c23"
  | Backend_Cpp -> "c++"
  | Backend_Forth -> "forth"

(** Default backend *)
let default_backend : backend = Backend_C99
