open Typeur_expr
open Typeur_instr
open Typeur
open Ast
open Ast_printer



let get_type env lb le = function
  | TAccess i -> TAccessRecord i
  | TIdent i -> begin
      match String.lowercase i with
      | "integer" -> Tint
      | "boolean" -> Tbool
      | "character" -> Tchar
      | "none" -> TypeNone
      | r ->
        match find_type env r lb le with
        | TRecordDef recd ->
          begin
          if Smap.is_empty recd then
            (message_erreur lb le ("L'enregistrement "^r^" est vide.");TypeError)
          else
            TRecord r
          end
        | TAccessRecord q -> TAccessRecord q
        | f -> (message_erreur lb le (r^" ne désigne pas un type : "^(p_typ f));TypeError)

    end


let check_records lb le env =
  let aux_check ok ident =
    let t,_ = Smap.find ident env.vars in (*Pas de try car ident inclus dans env.idents*)
    match t with
    | TType (TRecordDef recd) -> if Smap.is_empty recd then (message_erreur lb le ("Le type enregistrement "^ident^" est déclaré mais n'a pas été défini avant de passer à un niveau de déclarations supérieur.");false) else ok
    | _ -> ok in
  List.fold_left aux_check true env.records_to_check

let rec type_decl env tdecl =
  let lb = tdecl.lb and le = tdecl.le in
  match tdecl.decl with
  | DeclType x -> let nenv, ok = add_record lb le env x in
    {nenv with records_to_check = x::nenv.records_to_check}, ok, tdecl
  | DeclTypeAccess (x, y) ->
    let nenv, ok = add_type lb le env x (TAccessRecord y) in
      nenv, (if not(teq (find_type env y lb le) TypeError) then
                                         ok
                                       else
                                         false), tdecl
  | DeclTypeRecord (x, champs) ->
    let aux_add_field (env_, ok) (ident_lst, typ_) =
      let tident = get_type env_ lb le typ_ in
      let nenv_, result = List.fold_left (fun (nenv_,ok) ident ->
                        let nenv,ok_ = add_record_field nenv_ x ident tident lb le in
                        nenv,ok_ && ok) (env_,true) ident_lst in
      nenv_, result && ok
    in
    let r,p = find_record env x in
    if r && Smap.is_empty p then
      (let nenv, nok = List.fold_left aux_add_field (env,true) champs in
      nenv, nok, tdecl)
    else begin
      if r then
        (message_erreur lb le ("L'enregistrement "^x^" a été défini auparavant.");env, false, tdecl)
      else
        begin
          let env_, ok_ = (if r then(env, true) else (add_record lb le env x)) in
          let nenv, nok = List.fold_left aux_add_field (env_,true) champs in
          nenv, ok_ && nok, tdecl
        end
    end
  | Decl (xlist, typ_, None) ->
    let vtype = get_type env lb le typ_  in
    if not(teq vtype TypeError) then
      let env, ok = List.fold_left (fun (env_,ok_) x ->
          let env_,ok = add_var lb le env_ x vtype ModeInOut in
          env_, ok && ok_) (env,true) xlist in
      env, ok, tdecl
    else
      env, false, tdecl
  | Decl (xlist, typ_, Some expr) ->
    let nexpr = type_expr env expr and vtype = get_type env lb le typ_ in
    if teq nexpr.typ vtype then
      let env, ok = List.fold_left (fun (env_,ok_) x ->
        let env_, ok = add_var lb le env_ x vtype ModeInOut in
        env_, ok && ok_) (env,true) xlist in
      env,ok, {tdecl with decl = Decl (xlist,typ_,Some nexpr)}
    else
      begin
        message_erreur lb le ("Incohérence des types dans la définition de "^(
            List.fold_left (fun s x -> s^","^x) "" xlist)^": "^(p_typ vtype)^" := "^(p_typ nexpr.typ));
        env, false, {tdecl with decl = Decl (xlist,typ_,Some nexpr)}
      end
  | DeclProcedure (i,params,decls,instrs) ->
    type_decl env {tdecl with decl = DeclFunction (i,params,TIdent "none",decls,instrs)}
  | DeclFunction  (i,params,rettyp_,decls,instrs) ->
    let chk_records = check_records lb le env in
    (*Vérification de la présence d'un return en dernière instruction.*)
    let rec check_return lst =
      if rettyp_ = TIdent "none" then true else
      begin
        match lst with
        | [] -> false
        | w::q ->
          begin
            match w.instr with
            | IReturn _ -> true
            | IScope lst -> check_return lst || check_return q
            | IConditional(_,thenlist,elseiflist,Some elselist) ->
              let _,elseiflist = List.split elseiflist in
              (check_return thenlist && (List.for_all check_return elseiflist) && check_return elselist) || check_return q
            | IConditional(_,thenlist,elseiflist,None) ->
              let _,elseiflist = List.split elseiflist in
              (check_return thenlist && (List.for_all check_return elseiflist)) || check_return q
            | IFor (_,_,_,_,lst) -> check_return lst || check_return q
            | IWhile (_,lst) -> check_return lst || check_return q
            | _ -> check_return q
          end
      end in
    let ret = get_type env lb le rettyp_ in
    let tparams =
      List.flatten (
        List.map (fun (idlist,mode,typ_) ->
            let ntyp = get_type env lb le typ_ in
            List.map (fun id -> id,mode,ntyp) idlist
          ) params
      ) in
    let nenv = {env with idents = []; records_to_check = []} in (*Vérification de l'unicité des identifiants*)
    let nenv, ok = (add_function ~addid:(false) lb le nenv i (ret, tparams)) in
    let nenv, ok = List.fold_left (fun (ev,ek) (i,m,t) ->
        match m with
        | Some m_ ->
          let ev, ek_ = add_var lb le ev i t m_ in ev, ek_ && ek
        | None ->
          let ev, ek_ = add_var lb le ev i t ModeIn in ev, ek_ && ek) (nenv,ok) tparams in
    let nenv, err_d, ndecl = type_decl_list nenv decls in
    let chk_records = chk_records && check_records lb le nenv in
    let ninstr, err_i = type_list_instr ret nenv instrs in
    let err_r = (if check_return instrs then
                   ok
                 else
                   (message_erreur lb le "L'execution d'une fonction doit nécessairement terminer sur une instruction return.";
                    false)
                ) in

    let nenv, nerr = add_function lb le env i (ret,tparams) in
    let nenv = {nenv with records_to_check = []} in
    nenv, chk_records && err_d && err_i != TypeError && err_r && nerr, {tdecl with
                                               env = nenv;
                                               decl = DeclFunction (i,params,rettyp_,ndecl,ninstr)}

and type_decl_list env ldecl =
  let aux_iter_decls (env, err, lst) decl =
    let nenv,nerr,ndecl = type_decl env decl in
    (nenv, nerr && err, ndecl::lst)
  in
  List.fold_left aux_iter_decls (env,true,[]) ldecl
