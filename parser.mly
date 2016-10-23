%{
  open Ast
%}

%token <int> INT
%token <char> CHR
%token <string> IDENT
%token ACCESS,AND,BEGIN,ELSE,ELSIF,END,FALSE,FOR,FUNCTION,IF,IN,IS,LOOP,NEW,NOT
%token NULL,OR,OUT,WITH,PROCEDURE,RECORD,REM,RETURN,USE,REVERSE,THEN,TRUE,TYPE,WHILE
%token EOF
%token LP RP
%token PLUS MINUS TIMES DIV
%token EQ NEQ
%token LET GET GT LT
%token COLON SEMICOLON ASSIGN COMMA DOT


%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ
%nonassoc LET LT GET GT
%left PLUS MINUS
%left TIMES DIV REM
%left DOT



%start fichier
%type <Ast.fichier> fichier



%%

fichier:
| WITH i1 = IDENT DOT i2 = IDENT SEMICOLON USE i1_ = IDENT DOT i2_ = IDENT SEMICOLON PROCEDURE i3 = IDENT IS d = decls BEGIN i = instrs_ END i4=IDENT SEMICOLON EOF
  {
    if (String.lowercase i1) = "ada" && (String.lowercase i2) = "text_io"
    && (String.lowercase i2) = "ada" && (String.lowercase i2) = "text_io"
    && (String.lowercase i3) = (String.lowercase i4) then
      (i3,d,i)
    else failwith "Erreur fichier."
  }
| WITH i1 = IDENT DOT i2 = IDENT SEMICOLON
    USE i1_ = IDENT DOT i2_ = IDENT SEMICOLON
    PROCEDURE i3 = IDENT IS d = decls
    BEGIN i = instrs_ END EOF
    {
      if (String.lowercase i1) = "ada" && (String.lowercase i2) = "text_io"
      && (String.lowercase i2) = "ada" && (String.lowercase i2) = "text_io"
      then
        (i3,d,i)
      else failwith "Erreur fichier."
    }
;

decls:
  | i = decl {[i]}
  | i = decl l = decls {i::l}
  | {[]}
;

decl:
    | TYPE i = IDENT SEMICOLON {DeclType i}
    | TYPE i = IDENT IS ACCESS i2 = IDENT SEMICOLON {DeclTypeAccess (i,i2)}
    | TYPE i = IDENT IS RECORD c = champs_ END RECORD SEMICOLON
      {DeclTypeRecord (i,c)}
    | i = idents_ COLON t = typ SEMICOLON {Decl (i,t,None)}
    | i = idents_ COLON t = typ ASSIGN e = expr SEMICOLON {Decl (i,t,(Some e))}
    | PROCEDURE i = IDENT p = params IS d = decls
      BEGIN ins = instrs_ END i2 = IDENT SEMICOLON {
        if (String.lowercase i) = (String.lowercase i2) then
          DeclProcedure (i,p,d,ins)
        else failwith "Erreur nommage"
      }
    | PROCEDURE i = IDENT p = params IS d = decls
      BEGIN ins = instrs_ END SEMICOLON {DeclProcedure (i,p,d,ins)}
    | FUNCTION i = IDENT p = params RETURN t=typ IS d = decls
      BEGIN ins = instrs_ END i2 = IDENT SEMICOLON {
          if (String.lowercase i) = (String.lowercase i2) then
            DeclFunction (i,p,t,d,ins)
          else failwith "Erreur nommage"
        }
    | FUNCTION i = IDENT p = params RETURN t=typ IS d = decls
      BEGIN ins = instrs_ END SEMICOLON {DeclFunction (i,p,t,d,ins)}
;

champs_:
  | i = champs {[i]}
  | i = champs l = champs_  {i::l}
;

champs:
  | i = idents_ COLON t = typ SEMICOLON {(i,t)}
;

typ:
  | ACCESS i = IDENT {TAccess i}
  | i = IDENT {TIdent i}
;

params:
  | {[]}
  | LP p = param_ RP {p}
;

param_:
  | i = param {[i]}
  | i = param l = param_ {i::l}
;

param:
  | i=idents_ COLON m=mode t = typ SEMICOLON {(i,Some m,t)}
  | i=idents_ COLON t = typ SEMICOLON {(i,None,t)}
;

mode:
  | IN OUT {ModeInOut}
  | IN {ModeIn}
;

exprs_:
  | e = expr COMMA {[e]}
  | e = expr l = exprs_  COMMA {e::l}
;

expr:
  | i = INT {EInt i}
  | c = CHR {EChar c}
  | TRUE {ETrue}
  | FALSE {EFalse}
  | NULL {ENull}
  | a = acces {EAccess a}
  | e1 = expr o = operateur e2 = expr {EOp(e1,o,e2)}
  | NOT e = expr {ENot e}
  | MINUS e = expr {EMinus e}
  | NEW i = IDENT {ENew i}
  | i=IDENT LP e=exprs_ RP {EEval (i,e)}
;

instr:
  | a = acces ASSIGN e = expr SEMICOLON {IAssign (a,e)}
  | i = IDENT SEMICOLON {IEval (i,[])}
  | i = IDENT LP e = exprs_ RP SEMICOLON {IEval (i,e)}
  | RETURN SEMICOLON {IReturn None}
  | RETURN e=expr SEMICOLON {IReturn (Some e)}
  | BEGIN i=instrs_ END SEMICOLON {IScope i}
  | IF e = expr THEN i=instrs_
    eif = elsifs
    ELSE i3=instrs_
    END IF SEMICOLON {IConditional (e,i,eif,Some i3)}
  | IF e = expr THEN i=instrs_
      eif = elsifs
      END IF SEMICOLON {IConditional (e,i,eif,None)}
  | FOR i = IDENT IN REVERSE e1 = expr DOT DOT e2 = expr
    LOOP ins=instrs_ END LOOP SEMICOLON
      {IFor (i,true,e1,e2,ins)}
  | FOR i = IDENT IN e1 = expr DOT DOT e2 = expr
    LOOP ins=instrs_ END LOOP SEMICOLON
      {IFor (i,false,e1,e2,ins)}
  | WHILE e = expr LOOP ins = instrs_ END LOOP SEMICOLON
    {IWhile (e,ins)}
;
instrs_:
  | i = instr {[i]}
  | i = instr l = instrs_ {i::l}

;
operateur:
  | EQ {OpEq}
  | NEQ {OpNeq}
  | LT {OpLt}
  | GT {OpGt}
  | LET {OpLet}
  | GET {OpGet}
  | PLUS {OpPlus}
  | MINUS {OpMinus}
  | TIMES {OpTimes}
  | DIV {OpDiv}
  | REM {OpRem}
  | AND THEN {OpAndThen}
  | AND {OpAnd}
  | OR ELSE {OpOrElse}
  | OR {OpOr}
;
idents_:
  | i = IDENT COMMA {[i]}
  | i = IDENT COMMA l = idents_ {i::l}

;
acces:
  | i = IDENT {None,i}
  | e = expr DOT i = IDENT {Some e,i}

;
elsifs:
  | {[]}
  | l=elsifs i=elsif {i::l}
;
elsif:
  | ELSIF e = expr THEN i = instrs_ {e,i}
