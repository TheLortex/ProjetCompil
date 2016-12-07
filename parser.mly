%{
  open Ast

  let to_texpr (ex,b,e) = {expr=ex; lb=b; le=e; typ=TypeNone}
  let to_tdecl (d,b,e) = {decl=d; lb=b; le=e; env=empty}
  let to_tinstr (i,b,e) = {instr=i; lb=b; le=e; typ=TypeNone}
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
%token COLON SEMICOLON ASSIGN COMMA DOT DOTDOT
%token CHARACTVAL

%left OR
%nonassoc ELSE
%left AND
%nonassoc THEN
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
| WITH i1 = IDENT DOT i2 = IDENT SEMICOLON USE i1_ = IDENT DOT i2_ = IDENT SEMICOLON PROCEDURE i3 = IDENT IS d = list(decl) BEGIN i = instrs_ END i4=IDENT SEMICOLON EOF
  {
    if (String.lowercase i1) = "ada" && (String.lowercase i2) = "text_io"
    && (String.lowercase i1_) = "ada" && (String.lowercase i2_) = "text_io"
    && (String.lowercase i3) = (String.lowercase i4) then
      (i3,d,i)

    else  (raise (Error "Mauvaise syntaxe de début de fichier.\n"))
  }
| WITH i1 = IDENT DOT i2 = IDENT SEMICOLON
    USE i1_ = IDENT DOT i2_ = IDENT SEMICOLON
    PROCEDURE i3 = IDENT IS d = list(decl)
    BEGIN i = instrs_ END SEMICOLON EOF
    {
      if (String.lowercase i1) = "ada" && (String.lowercase i2) = "text_io"
      && (String.lowercase i1_) = "ada" && (String.lowercase i2_) = "text_io"
      then
        (i3,d,i)
      else (raise (Error "Mauvaise syntaxe de début de fichier.\n"))
    }
;

decl:
    | TYPE i = IDENT SEMICOLON {to_tdecl (DeclType i,$startpos,$endpos)}
    | TYPE i = IDENT IS ACCESS i2 = IDENT SEMICOLON {to_tdecl (DeclTypeAccess (i,i2),$startpos,$endpos)}
    | TYPE i = IDENT IS RECORD c = champs_ END RECORD SEMICOLON
      {to_tdecl (DeclTypeRecord (i,c),$startpos,$endpos)}
    | i = idents_ COLON t = typ SEMICOLON {to_tdecl (Decl (i,t,None),$startpos,$endpos)}
    | i = idents_ COLON t = typ ASSIGN e = expr SEMICOLON {to_tdecl (Decl (i,t,(Some e)),$startpos,$endpos)}
    | PROCEDURE i = IDENT p = params IS d = list(decl)
      BEGIN ins = instrs_ END i2 = IDENT SEMICOLON {
        if (String.lowercase i) = (String.lowercase i2) then
          to_tdecl (DeclProcedure (i,p,d,ins),$startpos,$endpos)
        else (raise (Error ("Erreur de nommage: "^i^" != "^i2^"\n")))
      }
    | PROCEDURE i = IDENT p = params IS d = list(decl)
      BEGIN ins = instrs_ END SEMICOLON {to_tdecl (DeclProcedure (i,p,d,ins),$startpos,$endpos)}
    | FUNCTION i = IDENT p = params RETURN t=typ IS d = list(decl)
      BEGIN ins = instrs_ END i2 = IDENT SEMICOLON {
          if (String.lowercase i) = (String.lowercase i2) then
            to_tdecl (DeclFunction (i,p,t,d,ins),$startpos,$endpos)
          else (raise (Error ("Erreur de nommage: "^i^" != "^i2^"\n")))
        }
    | FUNCTION i = IDENT p = params RETURN t=typ IS d = list(decl)
      BEGIN ins = instrs_ END SEMICOLON {to_tdecl (DeclFunction (i,p,t,d,ins),$startpos,$endpos)}
;
idents_:
  | i = separated_nonempty_list(COMMA, IDENT) {i}

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
  | LP p = separated_nonempty_list(SEMICOLON, pr = param {pr}) RP {p}
;
param:
  | i=idents_ COLON m=option(mode) t = typ {(i,m,t)}
;

mode:
  | IN OUT {ModeInOut}
  | IN {ModeIn}
;

exprs_:
  | e = separated_nonempty_list(COMMA,x = expr {x}) {e}
;

expr:
  | i = INT {to_texpr (EInt i,$startpos,$endpos)}
  | c = CHR {to_texpr (EChar c,$startpos,$endpos)}
  | TRUE {to_texpr (ETrue,$startpos,$endpos)}
  | FALSE {to_texpr (EFalse,$startpos,$endpos)}
  | NULL {to_texpr (ENull,$startpos,$endpos)}
  | a = acces {to_texpr (EAccess a,$startpos,$endpos)}
  | e1 = expr o = operateur e2 = expr {to_texpr (EOp(e1,o,e2),$startpos,$endpos)}
  | NOT e = expr {to_texpr (ENot e,$startpos,$endpos)}
  | NEW i = IDENT {to_texpr (ENew i,$startpos,$endpos)}
  | MINUS e = expr {to_texpr (EMinus e,$startpos,$endpos)}
  | i=IDENT LP e=exprs_ RP {to_texpr (EEval (i,e),$startpos,$endpos)}
  | LP e=expr RP {e}
  | CHARACTVAL LP e=expr RP {to_texpr (EChr e,$startpos,$endpos)}
;

instr:
  | a = acces ASSIGN e = expr SEMICOLON {to_tinstr (IAssign (a,e),$startpos,$endpos)}
  | i = IDENT SEMICOLON {to_tinstr (IEval (i,[]),$startpos,$endpos)}
  | i = IDENT LP e = exprs_ RP SEMICOLON {to_tinstr (IEval (i,e),$startpos,$endpos)}
  | RETURN SEMICOLON {to_tinstr (IReturn None,$startpos,$endpos)}
  | RETURN e=expr SEMICOLON {to_tinstr (IReturn (Some e),$startpos,$endpos)}
  | BEGIN i=instrs_ END SEMICOLON {to_tinstr (IScope i,$startpos,$endpos)}
  | IF e = expr THEN i=instrs_
    eif = elsifs
    ELSE i3=instrs_
    END IF SEMICOLON {to_tinstr (IConditional (e,i,eif,Some i3),$startpos,$endpos)}
  | IF e = expr THEN i=instrs_
      eif = elsifs
      END IF SEMICOLON {to_tinstr (IConditional (e,i,eif,None),$startpos,$endpos)}
  | FOR i = IDENT IN REVERSE e1 = expr DOTDOT e2 = expr
    LOOP ins=instrs_ END LOOP SEMICOLON
      {to_tinstr (IFor (i,true,e1,e2,ins),$startpos,$endpos)}
  | FOR i = IDENT IN e1 = expr DOTDOT e2 = expr
    LOOP ins=instrs_ END LOOP SEMICOLON
      {to_tinstr (IFor (i,false,e1,e2,ins),$startpos,$endpos)}
  | WHILE e = expr LOOP ins = instrs_ END LOOP SEMICOLON
    {to_tinstr (IWhile (e,ins),$startpos,$endpos)}
;
instrs_:
  | i = nonempty_list(e = instr {e}) {i}

;
%inline operateur:
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
  | AND {OpAnd}
  | AND THEN {OpAndThen}
  | OR {OpOr}
  | OR ELSE {OpOrElse}
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
