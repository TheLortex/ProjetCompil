{
  open Parser
  open Lexing

  let keywords = Hashtbl.create 97
  let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
  ["access",ACCESS;"and",AND;"begin",BEGIN;"else",ELSE;"elsif",ELSIF;"end",END;
  "false",FALSE;"for",FOR;"function",FUNCTION;"if",IF;"in",IN;"is",IS;
  "loop",LOOP;"new",NEW;"not",NOT;"null",NULL;"or",OR;"out",OUT;"with",WITH;
  "procedure",PROCEDURE;"record",RECORD;"rem",REM;"return",RETURN;"use",USE;
  "reverse",REVERSE;"then", THEN;"true",TRUE;"type",TYPE;"while",WHILE]


  exception LexingError of char

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let ident  = letter (letter|digit|'_')*
let identchr  = ident '\'' ident

rule token = parse
  | "--"    {comment lexbuf}
  | '.'     {DOT}
  | ".."    {DOTDOT}
  | ','     {COMMA}
  | ":="    {ASSIGN}
  | ':'     {COLON}
  | ';'     {SEMICOLON}
  | '+'     {PLUS}
  | '-'     {MINUS}
  | '*'     {TIMES}
  | '/'     {DIV}
  | '='     {EQ}
  | "/="    {NEQ}
  | '('     {LP}
  | ')'     {RP}
  | "<="    {LET}
  | ">="    {GET}
  | '<'     {LT}
  | '>'     {GT}
  | eof     {EOF}
  | '\n'    {next_line lexbuf; token lexbuf}
  | [' ' '\t']    {token lexbuf}
  | digit+ as s {
    try
      let i = int_of_string s in
      let borne = -Int32.to_int Int32.min_int in
      if i > borne || i < -borne then
        (print_int i;raise (LexingError 'i'))
      else
        INT (int_of_string s)
    with
      | Failure _ -> raise (LexingError 'i')}
  | identchr as s {let s = String.lowercase s in
  if s = "character\'val" then CHARACTVAL else raise (LexingError '\'')}
  | '\''(_ as c) '\'' {CHR c}
  | ident as s { let s = String.lowercase s in
                 try Hashtbl.find keywords s with Not_found -> IDENT s}
  | _ as c       {raise (LexingError c)}

and comment = parse
  | eof {EOF}
  | '\n' {next_line lexbuf; token lexbuf}
  | _ {comment lexbuf}
