module Space.Compiler.Main

(** Main driver for the Space compiler
    Compiles .space source files to C code *)

open FStar.IO
open FStar.All
open Space.Compiler.Lexer
open Space.Compiler.Parser
open Space.Compiler.AST
open Space.Compiler.CGen

(** Print usage information *)
let print_usage () : ML unit =
  print_string "Usage: spacec [options] <input.space>\n";
  print_string "Options:\n";
  print_string "  --target c     Generate C code (default)\n";
  print_string "  --target bc    Generate bytecode\n";
  print_string "  -o <file>      Write output to file\n";
  print_string "  --help         Show this help\n"

(** Read entire file contents *)
let read_file (path: string) : ML (option string) =
  try
    let fd = open_read_file path in
    let rec read_all (acc: string) : ML string =
      let line = read_line fd in
      if input_line_is_eof fd then acc
      else read_all (acc ^ line ^ "\n")
    in
    let content = read_all "" in
    close_read_file fd;
    Some content
  with
  | _ -> None

(** Write string to file *)
let write_file (path: string) (content: string) : ML bool =
  try
    let fd = open_write_file path in
    write_string fd content;
    close_write_file fd;
    true
  with
  | _ -> false

(** Compile source string to C code *)
let compile_to_c (source: string) : either string string =
  match parse_string source with
  | Inr (msg, loc) ->
    Inr ("Parse error at line " ^ string_of_int loc.line ^
         ", column " ^ string_of_int loc.column ^ ": " ^ msg)
  | Inl program ->
    let c_code = gen_program program in
    Inl (code_to_string c_code)

(** Parse command line arguments *)
noeq type compiler_options = {
  input_file: option string;
  output_file: option string;
  target: string;
  show_help: bool;
}

let default_options : compiler_options = {
  input_file = None;
  output_file = None;
  target = "c";
  show_help = false;
}

let rec parse_args_aux (args: list string) (opts: compiler_options) : compiler_options =
  match args with
  | [] -> opts
  | "--help" :: rest -> parse_args_aux rest { opts with show_help = true }
  | "-h" :: rest -> parse_args_aux rest { opts with show_help = true }
  | "--target" :: t :: rest -> parse_args_aux rest { opts with target = t }
  | "-o" :: f :: rest -> parse_args_aux rest { opts with output_file = Some f }
  | arg :: rest ->
    if FStar.String.length arg > 0 then
      let first_char = FStar.String.sub arg 0 1 in
      if first_char = "-" then
        parse_args_aux rest opts  (* Skip unknown flags *)
      else
        parse_args_aux rest { opts with input_file = Some arg }
    else
      parse_args_aux rest opts

let parse_args (args: list string) : compiler_options =
  (* Skip first argument (program name) *)
  match args with
  | _ :: rest -> parse_args_aux rest default_options
  | [] -> default_options

(** Main entry point *)
let main () : ML unit =
  let args = FStar.List.Tot.Base.list_of_seq (FStar.All.get_args ()) in
  let opts = parse_args args in

  if opts.show_help then
    print_usage ()
  else
    match opts.input_file with
    | None ->
      print_string "Error: No input file specified\n";
      print_usage ()
    | Some input_path ->
      match read_file input_path with
      | None ->
        print_string ("Error: Cannot read file: " ^ input_path ^ "\n")
      | Some source ->
        if opts.target = "c" then
          match compile_to_c source with
          | Inr error ->
            print_string ("Error: " ^ error ^ "\n")
          | Inl c_code ->
            match opts.output_file with
            | None ->
              print_string c_code
            | Some output_path ->
              if write_file output_path c_code then
                print_string ("Wrote " ^ output_path ^ "\n")
              else
                print_string ("Error: Cannot write file: " ^ output_path ^ "\n")
        else
          print_string ("Error: Unknown target: " ^ opts.target ^ "\n")

(** Call main on module load when extracted *)
let _ = main ()
