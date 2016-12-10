open Typeur_expr
open Typeur
open Ast
open Ast_printer

let rec type_instr ret env (tinstr : tinstr) =
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
             (message_erreur lb le ("Affectation illégale du paramètre "^ident);
               TypeError)
         end
      else
        begin
          if texpr.typ != TypeError && tvar != TypeError then
          message_erreur lb le ("Incohérence des types pour "^ident^": "^(p_typ tvar)^" := "^(p_typ texpr.typ));
          TypeError
        end
     )}
  | IAssign ((Some e, ident),expr) ->
    let texpr = type_expr env expr and ne = type_expr env e in
    {tinstr with
     instr = IAssign((Some ne, ident),texpr);
     typ = (match ne.typ with
         | TRecord (level,r) when est_valeur_gauche env ne lb le ->
           let trecordfield = (find_record_field env (level,r) ident tinstr.lb tinstr.le) in
           if teq trecordfield texpr.typ then
             begin
               match e.expr with (*TODO: Vérifier que c'est correct*)
               | EAccess (None,ident) -> (
                     if find_var_mode env ident lb le = ModeInOut then
                       TypeNone
                     else
                       (message_erreur lb le ("Affectation illégale du paramètre "^ident);
                        TypeError))
               | _ -> TypeNone
             end
           else
             begin
               if texpr.typ != TypeError && trecordfield != TypeError then
               message_erreur lb le ("Incohérence des types pour "^r^"."^ident^": "^(p_typ trecordfield)^" := "^(p_typ texpr.typ));
               TypeError
             end
         | TAccessRecord (level,r) ->
           let trecordfield = (find_record_field env (level,r) ident tinstr.lb tinstr.le) in
           if teq trecordfield texpr.typ then
             TypeNone
           else
             begin
               if texpr.typ != TypeError && trecordfield != TypeError then
               message_erreur lb le ("Incohérence des types pour "^r^"."^ident^": "^(p_typ trecordfield)^" := "^(p_typ texpr.typ));
               TypeError
             end
         | TRecord _ ->
           message_erreur lb le ("L'expression n'est pas pas une valeur gauche.le");TypeError
         | _ -> message_erreur lb le ("L'expression n'est pas de type enregistrement : "^p_typ ne.typ);TypeError
       )}
  | IEval (ident,lexprs) ->
    let (return, params) = find_function env ident tinstr.lb tinstr.le in
    let ntexprs = List.map (fun expr -> type_expr env expr) lexprs in
    {tinstr with
     instr = IEval(ident,ntexprs);
     typ = (if check_type env lb le params ntexprs then
              begin
                if teq return TypeNone then
                  TypeNone
                else
                  (message_erreur lb le "Utilisation invalide d'une fonction dans une procédure."; TypeError)
              end
            else TypeError)}
  | IReturn None -> {tinstr with
                     typ = (if teq ret TypeNone then
                              TypeNone
                            else
                              begin
                                message_erreur lb le ("Return; alors que la fonction renvoie "^(p_typ ret));
                                TypeError
                              end)}
  | IReturn (Some e) ->
    let expr = type_expr env e in
    {tinstr with
     instr = IReturn (Some expr);
     typ = (if teq ret expr.typ then
              ret
            else begin
              message_erreur lb le ("Return "^(p_typ expr.typ)^"; alors que la fonction renvoie "^(p_typ ret)^".");
              TypeError
            end)}
  | IScope linstr -> let m,err = type_list_instr ret env linstr in
    {tinstr with
     instr = IScope m;
     typ = err}
  | IConditional (expr_cdn, linstr_then, linstrs_elseif, linstrs_else) ->
    let nexpr_cdn = type_expr env expr_cdn in
    let errflag = ref (if teq nexpr_cdn.typ Tbool then TypeNone else (if nexpr_cdn.typ != TypeError then message_erreur lb le ("Incohérence des types pour la condition: "^(p_typ nexpr_cdn.typ)^" != bool.");TypeError)) in
    let nlinstr_then,nerr = type_list_instr ret env linstr_then in
    let () = if teq nerr TypeError then (errflag := TypeError) in
    let nlinstrs_elseif = List.map (fun (expr,lst) ->
        let m,r = type_list_instr ret env lst and nexpr = type_expr env expr in
        if teq r TypeError then errflag := TypeError;nexpr,m) linstrs_elseif in
    let nlinstrs_else =
      match linstrs_else with
      | Some tlist -> begin
        let m,r = type_list_instr ret env tlist in
        if teq r TypeError then (errflag := TypeError);Some m
      end
      | _ -> None
    in
    {tinstr with
     instr = IConditional (nexpr_cdn, nlinstr_then, nlinstrs_elseif, nlinstrs_else);
     typ = !errflag
    }
  | IFor (x,reverse,e1, e2, instrs) ->
    let env = {env with vars = Smap.remove x env.vars} in
    let ne1 = type_expr env e1 and ne2 = type_expr env e2 in
    let env, ok = {env with vars = Smap.add x (Tint,ModeIn) env.vars}, true in
    let m,e = type_list_instr ret env instrs in
    let errflag = if teq ne1.typ Tint && teq ne2.typ Tint && e != TypeError && ok then TypeNone else (if(not(teq ne1.typ Tint && teq ne2.typ Tint) && ne1.typ != TypeError && ne2.typ != TypeError) then message_erreur lb le ("Incohérence des types pour for: "^(p_typ ne1.typ)^" != int ou "^(p_typ ne2.typ)^" != int.");TypeError) in
    {tinstr with
     instr = IFor(x,reverse,ne1,ne2,m);
     typ = errflag}
  | IWhile (texpr, instrs) ->
    let ne = type_expr env texpr in
    let m,e = type_list_instr ret env instrs in
    let errflag = if teq ne.typ Tbool && not(teq e TypeError) then TypeNone else (if not(teq ne.typ Tbool) && ne.typ != TypeError then message_erreur lb le ("Incohérence des types pour while: "^(p_typ ne.typ)^" != bool.");TypeError) in
    {tinstr with
     instr = IWhile(texpr, instrs);
     typ = errflag}

and type_list_instr ret env linstr =
  let m = List.map (fun instr -> type_instr ret env instr) linstr in
  let err = List.fold_left (fun a b -> if teq b.typ TypeError || teq a TypeError then TypeError else TypeNone) TypeNone m in
  m,err
