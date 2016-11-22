type loc = Lexing.position


type ident = string

type mode =
  | ModeIn
  | ModeInOut
  
type typ =
  | Tint
  | Tchar
  | Tbool
  | TypeNull
  | TRecord of string
  | TAccessRecord of string
  | TypeError
  | TypeNone
  | TType of typ
  | TFunction of func
and func = typ * (tparam list)
and tparam = ident * (mode option) * typ



module Smap = Map.Make(String)
type recd = typ Smap.t

type env = {
  vars: (typ * mode) Smap.t;
  types: typ Smap.t;
  records: recd Smap.t;
  functions: func Smap.t;
  idents: ident list;
}

let empty = {vars = Smap.empty;types = Smap.empty; records = Smap.empty; functions = Smap.empty; idents = []}

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