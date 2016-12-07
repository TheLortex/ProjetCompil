type loc = Lexing.position

exception Error of string
type ident = string

type mode =
  | ModeIn (*Readonly*)
  | ModeInOut (*rw*)
  | ModeNone

module Smap = Map.Make(String)

type typ =
  | Tint
  | Tchar
  | Tbool
  | TypeNull
  | TRecord of string
  | TRecordDef of recd
  | TAccessRecord of string
  | TypeError
  | TypeNone
  | TFunction of func
  | TType of typ

and func = typ * (tparam list)
and tparam = ident * (mode option) * typ
and recd = typ Smap.t

type env = {
  vars: (typ * mode) Smap.t;
  records: (recd * int) Smap.t;
  idents: ident list;
  records_to_check: ident list
}

let empty = {records = Smap.empty; vars = Smap.empty; idents = []; records_to_check = []}

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
