{
  let keywords = Hashtbl.create 97
  let () = List.iter (fun (s,t) -> Hashtbl.add keywords s t)
  ["access",ACCESS;"and",AND;"begin",BEGIN;"else",ELSE;"elsif",ELSIF;"end",END;
  "false",FALSE;"for",FOR;"function",FUNCTION;"if",IF;"in",IN;"is",IS;
  "loop",LOOP;"new",NEW;"not",NOT;"null",NULL;"or",OR;"out",OUT;"with",WITH;
  "procedure",PROCEDURE;"record",RECORD;"rem",REM;"return",RETURN;"use",USE;
  "reverse",REVERSE;"then", THEN;"true",TRUE;"type",TYPE;"while",WHILE]
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let ident  = letter [letter digit '_']* 

rule token = parse
  | "--"    {comment lexbuf}
  | '.'     {DOT}
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
  | [' ' '\t' '\n']    {token lexbuf}
  | digit+ as s {INT (int_of_string s)}
  | '\''(_ as c) '\'' {CHR c}
  | ident as s { let s = String.lowercase s in
                 try Hashtbl.find keywords s with Not_found -> IDENT s
  | _       {failwith "Caractère illégal. (GO BACK TO YOUR COUNTRY)"}

and comment = parse
  | '\n' -> {token lexbuf}
  | _ -> {comment lexbuf}
