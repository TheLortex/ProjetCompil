type loc = Lexing.position

exception Error of string
type ident = string

type mode =
  | ModeIn (*Readonly*)
  | ModeInOut (*rw*)
  | ModeNone

type ident_level = int * string
module Ident_level =
struct
  type t = ident_level
  let compare (l1,s1) (l2,s2) =
    (if l1 = l2 then String.compare s1 s2 else (l1-l2))
end


module Smap = Map.Make(String)
module Lmap = Map.Make(Ident_level)

type typ =
  | Tint
  | Tchar
  | Tbool
  | TypeNull
  | TRecord of ident_level
  | TAccessRecord of ident_level
  | TypeNone
  | TypeError
  | TFunction of func
  | TRecordDef of recd
  | TType of ident_level

and func = typ * (tparam list) * int
and tparam = ident * (mode option) * typ
and recd = typ Smap.t
and vard = {
  typ: typ;
  mode: mode;
  level: int;
  offset: int;
}

type env = {
  vars: vard Smap.t;
  types: typ Lmap.t;
  idents: ident list;
  records_to_check: ident list;
  current_offset: int;
}

let empty =
  {types = Lmap.empty;
   vars = Smap.empty;
   idents = [];
   records_to_check = [];
   current_offset = 0;}

type status = int Smap.t

type texpr = {
  expr: expr;
  lb: loc;
  le: loc;
  typ: typ
}
and expr =
  | EInt of int
  | EChar of char
  | ETrue
  | EFalse
  | ENull
  | EAccess of acces
  | EOp of texpr * operateur * texpr
  | ENot of texpr
  | EMinus of texpr
  | ENew of ident
  | EEval of ident * (texpr list)
  | EChr of texpr
and acces = (texpr option) * ident
and operateur =
  | OpEq | OpNeq
  | OpLt | OpLet
  | OpGt | OpGet
  | OpPlus | OpMinus
  | OpTimes | OpDiv
  | OpRem
  | OpAnd | OpAndThen
  | OpOr | OpOrElse



type tinstr = {
  instr: instr;
  lb: loc;
  le: loc;
  typ: typ
}
and instr =
  | IAssign of acces * texpr
  | IEval of ident * (texpr list)
  | IReturn of texpr option
  | IScope of (tinstr list)
  | IConditional of
      texpr * (tinstr list) *
      ((texpr * (tinstr list)) list) *
      ((tinstr list) option)
  | IFor of ident * bool * texpr * texpr * (tinstr list)
  | IWhile of texpr * (tinstr list)

type fichier = ident * (tdecl list) * (tinstr list)
and tdecl = {
  decl: decl;
  lb: loc;
  le: loc;
  env: env
  }
and decl =
  | DeclType of ident
  | DeclTypeAccess of ident * ident
  | DeclTypeRecord of ident * (champs list)
  | Decl of (ident list) * typ_ * (texpr option)
  | DeclProcedure of ident * (param list) * (tdecl list) * (tinstr list)
  | DeclFunction of ident * (param list) * typ_ * (tdecl list) * (tinstr list)
and champs = (ident list) * typ_
and typ_ =
  | TIdent of ident
  | TAccess of ident
and param = (ident list) * (mode option)* typ_

let rec p_typ = function
  | Tint -> "int"
  | Tchar -> "char"
  | Tbool -> "bool"
  | TypeNull -> "null"
  | TRecord (i,r) -> "record("^r^")"
  | TAccessRecord (i,r) -> "access("^r^")"
  | TypeError -> "ERR"
  | TypeNone -> "none"
  | TType (i,t) -> "type "^t
  | _ -> "def"

let p_op = function
  | OpAnd -> "and"
  | OpAndThen -> "and then"
  | OpOr -> "or"
  | OpOrElse -> "or else"
  | OpDiv -> "/"
  | OpMinus -> "-"
  | OpPlus -> "+"
  | OpTimes -> "*"
  | OpRem -> "rem"
  | OpEq -> "="
  | OpNeq -> "!="
  | OpGt -> ">"
  | OpGet -> ">="
  | OpLt -> "<"
  | OpLet -> "<="
