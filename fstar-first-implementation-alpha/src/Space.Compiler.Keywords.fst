module Space.Compiler.Keywords

(** Unified keyword lookup for multiple languages.

    All keywords are hyphenated forms (define-word, definir-palabra, etc.)
    which are unlikely to collide with user-defined words. This means
    all languages can safely coexist in "all" mode - the reserved word
    list is simply larger, not ambiguous.
*)

open Space.Compiler.Token
open Space.Compiler.Keywords.En
open Space.Compiler.Keywords.Ja
open Space.Compiler.Keywords.Ko
open Space.Compiler.Keywords.Ru
open Space.Compiler.Keywords.Sa
open Space.Compiler.Keywords.Zh
open Space.Compiler.Keywords.ZhTrad
open Space.Compiler.Keywords.ZhClassical

(** Supported languages *)
type lang =
  | Lang_En          (* English - default *)
  | Lang_Ja          (* Japanese *)
  | Lang_Ko          (* Korean *)
  | Lang_Ru          (* Russian *)
  | Lang_Sa          (* Sanskrit *)
  | Lang_Zh          (* Chinese - Simplified *)
  | Lang_ZhTrad      (* Chinese - Traditional *)
  | Lang_ZhClassical (* Chinese - Classical/Literary *)
  | Lang_All         (* All languages combined *)

(** Parse language code from string *)
let lang_of_string (s: string) : option lang =
  if s = "en" then Some Lang_En
  else if s = "ja" then Some Lang_Ja
  else if s = "ko" then Some Lang_Ko
  else if s = "ru" then Some Lang_Ru
  else if s = "sa" then Some Lang_Sa             (* Sanskrit *)
  else if s = "zh" then Some Lang_Zh
  else if s = "zh-Hans" then Some Lang_Zh        (* BCP 47 *)
  else if s = "zh-Hant" then Some Lang_ZhTrad    (* BCP 47 *)
  else if s = "zh-trad" then Some Lang_ZhTrad
  else if s = "zh-classical" then Some Lang_ZhClassical
  else if s = "lzh" then Some Lang_ZhClassical   (* ISO 639-3 *)
  else if s = "all" then Some Lang_All
  else None

(** Lookup helper - search a keyword list *)
let rec lookup_in_list (s: string) (kws: list (string * token)) : option token =
  match kws with
  | [] -> None
  | (word, tok) :: rest ->
    if s = word then Some tok
    else lookup_in_list s rest

(** Lookup in English keywords *)
let lookup_en (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.En.keywords

(** Lookup in Japanese keywords *)
let lookup_ja (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.Ja.keywords

(** Lookup in Korean keywords *)
let lookup_ko (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.Ko.keywords

(** Lookup in Russian keywords *)
let lookup_ru (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.Ru.keywords

(** Lookup in Sanskrit keywords *)
let lookup_sa (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.Sa.keywords

(** Lookup in Chinese Simplified keywords *)
let lookup_zh (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.Zh.keywords

(** Lookup in Chinese Traditional keywords *)
let lookup_zh_trad (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.ZhTrad.keywords

(** Lookup in Classical Chinese keywords *)
let lookup_zh_classical (s: string) : option token =
  lookup_in_list s Space.Compiler.Keywords.ZhClassical.keywords

(** Lookup in all languages - for distinct script mode.
    Since scripts don't collide, we can search all. *)
let lookup_all (s: string) : option token =
  match lookup_en s with
  | Some tok -> Some tok
  | None ->
    match lookup_ja s with
    | Some tok -> Some tok
    | None ->
      match lookup_ko s with
      | Some tok -> Some tok
      | None ->
        match lookup_ru s with
        | Some tok -> Some tok
        | None ->
          match lookup_sa s with
          | Some tok -> Some tok
          | None ->
            match lookup_zh s with
            | Some tok -> Some tok
            | None ->
              match lookup_zh_trad s with
              | Some tok -> Some tok
              | None -> lookup_zh_classical s

(** Main lookup function with language selection *)
let lookup (lang: lang) (s: string) : option token =
  match lang with
  | Lang_En -> lookup_en s
  | Lang_Ja -> lookup_ja s
  | Lang_Ko -> lookup_ko s
  | Lang_Ru -> lookup_ru s
  | Lang_Sa -> lookup_sa s
  | Lang_Zh -> lookup_zh s
  | Lang_ZhTrad -> lookup_zh_trad s
  | Lang_ZhClassical -> lookup_zh_classical s
  | Lang_All -> lookup_all s

(** Default lookup uses all languages - distinct scripts don't collide *)
let lookup_default (s: string) : option token =
  lookup_all s

(** Classify a word using language-specific lookup *)
let classify_word_lang (lang: lang) (s: string) : token =
  match lookup lang s with
  | Some tok -> tok
  | None -> TOK_Identifier s

(** Classify a word using all-language lookup *)
let classify_word (s: string) : token =
  match lookup_default s with
  | Some tok -> tok
  | None -> TOK_Identifier s
