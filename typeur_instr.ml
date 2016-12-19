open Typeur_expr
open Typeur
open Ast

let rec type_instr ret env niveau (tinstr : tinstr) =
  let lb = tinstr.lb and le = tinstr.le in
  match tinstr.instr with
  | IAssign ((None, ident),expr) ->
    let texpr = type_expr env expr in
    {tinstr with
     instr = IAssign((None, ident),texpr);
     typ = (
       let tvar = find_var env ident lb le in
       if teq tvar texpr.typ then
         begin
           if find_var_mode env ident lb le = ModeInOut then
             TypeNone
           else
             (message_erreur lb le ("parameter "^ident^" cannot be modified.");
               TypeError)
         end
      else
        begin
          if texpr.typ != TypeError && tvar != TypeError then
            message_erreur lb le ("type mismatch for "^ident^": "^(p_typ tvar)^
                                  " := "^(p_typ texpr.typ));
          TypeError
        end
     )}
  | IAssign ((Some expr_record, ident),expr_value) ->
    let texpr_value = type_expr env expr_value
    and texpr_record = type_expr env expr_record in
    {tinstr with
     instr = IAssign((Some texpr_record, ident),texpr_value);
     typ =
       (match texpr_record.typ with
       | TRecord (level,r) when est_valeur_gauche env texpr_record lb le ->
         let trecordfield =
           (find_record_field env (level,r) ident tinstr.lb tinstr.le) in
         if teq trecordfield texpr_value.typ then
           begin
             match expr_record.expr with
             | EAccess (None,ident) -> (
                   if find_var_mode env ident lb le = ModeInOut then
                     TypeNone
                   else
                     (message_erreur lb le
                        (ident^" cannot be modified.");
                      TypeError))
             | _ -> TypeNone
           end
         else
           begin
             if texpr_value.typ != TypeError && trecordfield != TypeError then
               message_erreur lb le
                 ("type mismatch for "^r^"."^ident^": "^(p_typ trecordfield)^
                  " := "^(p_typ texpr_value.typ));
             TypeError
           end

       | TAccessRecord (level,r) ->
         let trecordfield =
           (find_record_field env (level,r) ident tinstr.lb tinstr.le) in
         if teq trecordfield texpr_value.typ then
           TypeNone
         else
           begin
             if texpr_value.typ != TypeError && trecordfield != TypeError then
               message_erreur lb le
                 ("type mismatch for "^r^"."^ident^": "^(p_typ trecordfield)^
                  " := "^(p_typ texpr_value.typ));
             TypeError
           end

       | TRecord _ ->
         message_erreur lb le
           ("the left expression of the access isn't a left-value.");
         TypeError
       | _ ->
         message_erreur lb le
           ("the left expression isn't of type record: "^
            (p_typ texpr_record.typ));
         TypeError
       )
    }
  | IEval (ident,lexprs) ->
    let (return, params,_) = find_function env ident tinstr.lb tinstr.le in
    let ntexprs = List.map (fun expr -> type_expr env expr) lexprs in
    {tinstr with
     instr = IEval(ident,ntexprs);
     typ =
       (if check_type env lb le params ntexprs then
        begin
          if teq return TypeNone then
            TypeNone
          else
            (message_erreur lb le
               "invalid use of a function in a procedure.";
             TypeError)
        end
        else TypeError
       )
    }
  | IReturn None ->
    {tinstr with
     typ =
       (if teq ret TypeNone then
          TypeNone
        else
        begin
          message_erreur lb le
            ("the return must be of type "^(p_typ ret));
          TypeError
        end
       )
    }
  | IReturn (Some e) ->
    let expr = type_expr env e in
    {tinstr with
     instr = IReturn (Some expr);
     typ =
       (if teq ret expr.typ then
          ret
        else begin
          message_erreur lb le
            ("return value of type "^(p_typ expr.typ)^
             " but the function must return "^(p_typ ret)^".");
          TypeError
        end
       )
    }
  | IScope linstr ->
    let m,err = type_list_instr ret env niveau linstr in
    {tinstr with
     instr = IScope m;
     typ = err}
  | IConditional (expr_cdn, linstr_then, linstrs_elseif, linstrs_else) ->
    let texpr_cdn = type_expr env expr_cdn in
    let errflag = ref (
        if teq texpr_cdn.typ Tbool then TypeNone else
          (if texpr_cdn.typ != TypeError then
             message_erreur lb le
               ("type mismatch for the condition: "^
                (p_typ texpr_cdn.typ)^" != bool.");
           TypeError))
    in
    let nlinstr_then, nerr = type_list_instr ret env niveau linstr_then in
    let () =
      if teq nerr TypeError then
        (errflag := TypeError)
    in
    let nlinstrs_elseif =
      List.map
        (fun (expr,lst) ->
           let m,r = type_list_instr ret env niveau lst
           and nexpr = type_expr env expr in
           if teq r TypeError then errflag := TypeError; nexpr, m)
        linstrs_elseif
    in
    let nlinstrs_else =
      match linstrs_else with
      | Some tlist -> begin
        let m,r = type_list_instr ret env niveau tlist in
        if teq r TypeError then (errflag := TypeError);Some m
      end
      | _ -> None
    in
    {tinstr with
     instr =
       IConditional (texpr_cdn, nlinstr_then, nlinstrs_elseif, nlinstrs_else);
     typ = !errflag
    }
  | IFor (x,reverse,e1, e2, instrs) ->
    let env = {env with vars = Smap.remove x env.vars; idents = List.filter (fun s -> s != x) env.idents} in
    let ne1 = type_expr env e1 and ne2 = type_expr env e2 in
    let env = {env with current_offset = env.current_offset - 8} in
    let env, ok = add_var lb le env x Tint ModeIn (niveau+1) false in
    let m,e = type_list_instr ret env niveau instrs in
    let errflag =
      if teq ne1.typ Tint && teq ne2.typ Tint && e != TypeError && ok then
        TypeNoneWithEnv env
      else
        (if(not(teq ne1.typ Tint && teq ne2.typ Tint) &&
            ne1.typ != TypeError && ne2.typ != TypeError) then
           message_erreur lb le
             ("type mismatch in the for loop: "^
              (p_typ ne1.typ)^" != int or "^(p_typ ne2.typ)^" != int.");
         TypeError)
    in
    {tinstr with
     instr = IFor(x,reverse,ne1,ne2,m);
     typ = errflag}
  | IWhile (texpr, instrs) ->
    let ne = type_expr env texpr in
    let m,e = type_list_instr ret env niveau instrs in
    let errflag =
      if teq ne.typ Tbool && not(teq e TypeError) then
        TypeNone
      else
        (if not(teq ne.typ Tbool) && ne.typ != TypeError then
           message_erreur lb le
             ("type mismatch for the while loop: "^(p_typ ne.typ)^" != bool.");
         TypeError)
    in
    {tinstr with
     instr = IWhile(texpr, instrs);
     typ = errflag}

and type_list_instr ret env niveau linstr =
  let m = List.map (fun instr -> type_instr ret env niveau instr) linstr in
  let err = List.fold_left
      (fun a b -> if teq b.typ TypeError || teq a TypeError then
          TypeError
        else
          TypeNone)
      TypeNone m in
  m,err
