open Typeur
open Ast
open Ast_printer

let rec type_expr env (texpr : texpr) =
  let lb = texpr.lb and le = texpr.le in
  match texpr.expr with
  | EInt _ ->
    {texpr with typ = Tint}

  | EChar _ ->
    {texpr with typ = Tchar}

  | ETrue | EFalse ->
    {texpr with typ = Tbool}

  | ENull ->
    {texpr with typ = TypeNull}

  | EAccess (None, ident)-> (*Accès d'une variable.*)
    {texpr with typ = find_var env ident lb le}

  | EAccess (Some e, ident) -> (*Accès d'un type enregistrement.*)
    let nexpr = type_expr env e in
       {texpr with
        expr = EAccess(Some nexpr, ident);
        typ = (match nexpr.typ with
            | TAccessRecord x ->
              find_record_field env x ident lb le
            | TRecord x ->
              find_record_field env x ident lb le
            | t ->
              message_erreur lb le
                ("the left value isn't of type record : "^
                 (p_typ t)^".");
              TypeError
          )
       }
  | EOp (e1, op, e2) when List.mem op [OpPlus; OpMinus; OpTimes; OpDiv; OpRem]->
    let nexpr1 = type_expr env e1 and nexpr2 = type_expr env e2 in
    {texpr with
     expr = EOp(nexpr1,op,nexpr2);
     typ = (
       if nexpr1.typ = Tint && nexpr2.typ = Tint then
        Tint
       else
        begin
          if nexpr1.typ != TypeError && nexpr2.typ != TypeError then
            message_erreur lb le
              ("type mismatch:  "^(p_typ nexpr1.typ)^" "^(p_op op)^" "^
               (p_typ nexpr2.typ)^" whereas int "^(p_op op)^
               " int was expected.");
            TypeError
        end
     )
    }
  | EOp (e1, op, e2) when List.mem op [OpEq; OpNeq] ->
    let nexpr1 = type_expr env e1 and nexpr2 = type_expr env e2 in
    {texpr with
     expr = EOp(nexpr1,op,nexpr2);
     typ = (if teq nexpr1.typ nexpr2.typ then
              Tbool
            else
              begin
                if nexpr1.typ != TypeError && nexpr2.typ != TypeError then
                  message_erreur lb le
                    ("type mismatch for operator"^(p_op op)^
                      ": "^(p_typ nexpr1.typ)^" != "^(p_typ nexpr2.typ)^".");
                TypeError
              end
           )
    }
  | EOp (e1, op, e2) when List.mem op [OpLt; OpLet; OpGt; OpGet] ->
    let nexpr1 = type_expr env e1 and nexpr2 = type_expr env e2 in
    {texpr with
     expr = EOp(nexpr1,op,nexpr2);
     typ = (
       if nexpr1.typ = Tint && nexpr2.typ = Tint then
         Tbool
       else
         begin
           if nexpr1.typ != TypeError && nexpr2.typ != TypeError then
             message_erreur lb le
               ("type mismatch for operator"^(p_op op)^":  "^
                (p_typ nexpr1.typ)^" "^(p_op op)^" "^(p_typ nexpr2.typ)^
                " whereas int "^(p_op op)^" int was expected.");
           TypeError
         end
     )
    }
  | EOp (e1, op, e2) when List.mem op [OpOr; OpOrElse; OpAnd; OpAndThen] ->
    let nexpr1 = type_expr env e1 and nexpr2 = type_expr env e2 in
    {texpr with
     expr = EOp(nexpr1,op,nexpr2);
     typ = (
      if nexpr1.typ = Tbool && nexpr2.typ = Tbool then
        Tbool
      else
        begin
          if nexpr1.typ != TypeError && nexpr2.typ != TypeError then
            message_erreur lb le
              ("type mismatch for operator "^(p_op op)^":  "^
               (p_typ nexpr1.typ)^" "^(p_op op)^" "^(p_typ nexpr2.typ)^
               " whereas bool "^(p_op op)^" bool was expected.");
          TypeError
        end
     )
    }
  | ENot e ->
    let nexpr = type_expr env e in
    {texpr with
     expr = ENot(nexpr);
     typ =
       (if nexpr.typ = Tbool then
          Tbool
        else
          begin
            if nexpr.typ != TypeError then
              message_erreur lb le
                ("type mismatch for operator not: bool != "^(p_typ nexpr.typ));
            TypeError
          end
       )
    }
  | EMinus e ->
    let nexpr = type_expr env e in
    {texpr with
     expr = EMinus(nexpr);
     typ =
       (if nexpr.typ = Tint then
          Tint
        else
          begin
            if nexpr.typ != TypeError then
              message_erreur lb le
                ("type mismatch for operator - : int != "^(p_typ nexpr.typ));
            TypeError
          end)}
  | ENew i ->
    {texpr with
     typ =
       (let res,_,level = find_record env i in
        if res then
          TAccessRecord (level,i)
        else
          begin
            message_erreur lb le ("the record "^i^" is not declared.");
            TypeError
          end
       )
    }
  (*Appel de fonction ou de procédure*)
  | EEval (ident,exprs) ->
    let (return, params) = find_function env ident lb le in
    let n_exprs = List.map (fun expr -> type_expr env expr) exprs in
    {texpr with
      expr = EEval(ident,n_exprs);
      typ = (if check_type env lb le params n_exprs then return else TypeError)
    }
  | EChr e ->
    let nexpr = type_expr env e in
    {texpr with
     expr = EChr(nexpr);
     typ = (
      if nexpr.typ = Tint then
        Tchar
      else
        begin
          if nexpr.typ != TypeError then
            message_erreur lb le
              ("type mismatch: int != "^(p_typ nexpr.typ));
          TypeError
        end)}
  | _ -> texpr
