open Ast
open Parser
open Lexer
open Lexing
open Printf
open Ast_printer
open Typeur_programme

let parse_only = ref false
let type_only = ref false
let file = ref ""
let speclist = [
  ("--parse-only", Arg.Set parse_only, "N'effectue que le parsing");
  ("--type-only", Arg.Set type_only, "N'effectue que le typage.")
]
let msg = "0b101010: Options disponibles."
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
            print_program program;
            if ok then exit(0) else exit(1)
          end )
    with
      | Parser.Error ->
        fprintf stderr "%a: syntax error \n" print_position buf;

        exit 1
      | Lexer.LexingError _ ->
        fprintf stderr "%a: lexing error \n" print_position buf;
        exit 1


(**
        let keywords = Hashtbl.create 97
        let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
        ["access",ACCESS;"and",AND;"begin",BEGIN;"else",ELSE;"elsif",ELSIF;"end",END;
        "false",FALSE;"for",FOR;"function",FUNCTION;"if",IF;"in",IN;"is",IS;
        "loop",LOOP;"new",NEW;"not",NOT;"null",NULL;"or",OR;"out",OUT;"with",WITH;
        "procedure",PROCEDURE;"record",RECORD;"rem",REM;"return",RETURN;"use",USE;
        "reverse",REVERSE;"then", THEN;"true",TRUE;"type",TYPE;"while",WHILE]

        let str_token = function
          | INT i -> "int "+(string_of_int i)
          | CHR c -> "char "^c
          | IDENT i -> i
          | ACCESS | AND | BEGIN | ELSE | ELSIF | END | FALSE | FOR | FUNCTION | IF | IN | IS | LOOP | NEW | NOT
          | NULL | OR | OUT | WITH | PROCEDURE | RECORD | REM | RETURN | USE | REVERSE | THEN | TRUE | TYPE | WHILE
          |  EOF
          |  LP RP
          |  PLUS MINUS TIMES DIV
          |  EQ NEQ
          |  LET GET GT LT
          |  COLON SEMICOLON ASSIGN COMMA DOT


          | OR
          | AND
          | NOT
          | EQ NEQ
          | LET LT GET GT
          | PLUS MINUS
          | TIMES DIV REM
          | DOT**)
