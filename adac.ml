open Ast
open Parser
open Lexer
open Lexing
open Printf
open Typeur_programme

let parse_only = ref false
let type_only = ref false
let file = ref ""
let speclist = [
  ("--parse-only", Arg.Set parse_only, "Parse only.");
  ("--type-only", Arg.Set type_only, "Parse and type only.")
]
let msg = "adac: available parameters."
let () = Arg.parse speclist (fun anon -> file := anon) msg

let f = open_in (!file)
let buf = Lexing.from_channel f
let () = buf.lex_curr_p <- {buf.lex_curr_p with pos_fname = !file}

let ex =
  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "File \"%s\", line %d, character %d" (!file)
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    in
    try (let program = Parser.fichier Lexer.token buf in
         if !parse_only then
           exit(0)
         else
          begin
            let ok, program = typeur !file program in
            if ok then exit(0) else exit(1)
          end )
    with
    | Parser.Error ->
      fprintf stderr "%a: syntax error \n" print_position buf;
      exit 1
    | Ast.Error msg ->
      fprintf stderr "%a: syntax error \n %s" print_position buf msg;
      exit 1
    | Lexer.LexingError c ->
      let msg = if c == 'i' then
          "32 bit integer size overflow.\n" else "" in
      fprintf stderr "%a: lexing error \n %s" print_position buf msg;
      exit 1
