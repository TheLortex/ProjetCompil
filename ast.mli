type  fichier = ident * (decl list) * (instr list)

and decl =
  | DeclType of ident
  | DeclTypeAccess of ident * ident
  | DeclTypeRecord of ident * (champs list)
  | Decl of (ident list) * typ * (expr option)
  | DeclProcedure of ident * (param list) * (decl list) * (instr list)
  | DeclFunction of ident * (param list) * typ * (decl list) * (instr list)

and champs = ident list * typ

and typ =
  | TIdent of ident
  | TAccess of ident

and param = (ident list) * (mode option)* typ

and mode =
  | ModeIn
  | ModeInOut

and expr =
  | EInt of int
  | EChar of char
  | ETrue
  | EFalse
  | ENull
  | EAccess of acces
  | EOp of expr * operateur * expr
  | ENot of expr
  | ENew of ident
  | EEval of ident * (expr list)
  | EChr of expr

and instr =
  | IAssign of acces * expr
  | IEval of ident * (expr list)
  | IReturn of expr option
  | IScope of (instr list)
  | IConditional of
    expr * (instr list) *
    ((expr * (instr list)) list) *
    ((instr list) option)
  | IFor of ident * bool * expr * expr * (instr list)
  | IWhile of expr * (instr list)

and operateur =
  | OpEq | OpNeq
  | OpLt | OpLet
  | OpGt | OpGet
  | OpPlus | OpMinus
  | OpTimes | OpDiv
  | OpRem
  | OpAnd | OpAndThen
  | OpOr | OpOrElse

and acces = (expr option) * ident
and ident = string
