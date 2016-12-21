open Typeur_expr
open Typeur_instr
open Typeur
open Ast

let get_type env lb le i =
  let id = String.lowercase (match i with |TIdent x |TAccess x -> x) in
  begin
      match find_type env id lb le with
      | TRecord ident ->
        begin
          match i with
          | TIdent x ->
            if (let _,recd,_ = find_record env x in Smap.is_empty recd.recd) then
              (TypeError)
            else
              (TRecord ident)
          | TAccess x -> (TAccessRecord ident)
        end
      | t -> t
    end

let check_records lb le env =
  let aux_check ok ident =
    try
      let t = (Smap.find ident env.vars).typ in
      match t with
      | TType (niveau,ident) ->
        (match get_custom_type env (niveau,ident) with
         | TRecordDef recd ->
           if Smap.is_empty recd.recd then
             (message_erreur lb le
                ("record "^ident^" is declared but hasn't been defined before going to another declaration level.");
              false)
           else ok
        | _ -> ok)
      | _ -> ok
    with
    | Not_found ->
      message_erreur lb le
        ("record "^ident^" not found.");false
  in
  List.fold_left aux_check true env.records_to_check

let rec type_decl env tdecl niveau =
  let lb = tdecl.lb and le = tdecl.le in
  match tdecl.decl with
  | DeclType x ->
    let nenv, ok = add_type lb le env x (TRecordDef {recd=Smap.empty;rcurrent_offset = 0}) niveau in
    if ok then
      {nenv with
       records_to_check = x::nenv.records_to_check
      }, ok, tdecl
    else nenv, ok, tdecl
  | DeclTypeAccess (x, y) ->
    if x = y then
      (message_erreur lb le (x^" can't access itself."); env, false, tdecl)
    else
      begin
        let ok1,_,lvl = find_record env y in
        if not(ok1) then
          (message_erreur lb le
             ("record "^y^" is not declared."));
        let nenv, ok2 = add_type lb le env x (TAccessRecord (lvl,y)) niveau in
        nenv, ok1&&ok2, tdecl
      end
  | DeclTypeRecord (x, champs) ->
    let verifier_champ env (_,typ_) = match typ_ with
      | TIdent i ->
        begin
          match find_type env i lb le with
          | TypeError -> false
          | TRecord (lvl, ident) ->
            if is_record_defined lb le env ident then true else
              (message_erreur lb le
                 ("invalid use of record "^ident^
                  " that is declared but not defined.");
               false)
          | t -> true
        end
      | TAccess i ->
        begin
          match find_type env i lb le with
          | TypeError -> false
          | _ -> true
        end
    in
    let aux_add_field (env_, ok) (ident_lst, typ_) =
      let tident = get_type env_ lb le typ_ in
      let nenv_, result =
        List.fold_left
          (fun (nenv_,ok) ident ->
             let nenv,ok_ =
               add_record_field lb le nenv_ x ident tident niveau in
             nenv,ok_ && ok)
          (env_,true)
          ident_lst
      in
      nenv_, result && ok
    in
    let r,p,_ = find_record env x in
    if r && Smap.is_empty p.recd then(*Déclaré non défini.*)
      let ok0 = List.for_all (verifier_champ env) champs in
      (let nenv, nok =
         List.fold_left aux_add_field (env,true) champs in
       nenv, nok&&ok0,tdecl)
    else begin
      if (r && List.mem x env.idents) then (*Déclaré et défini*)
        (message_erreur lb le
           ("record "^x^" is already defined.");
         env, false, tdecl)
      else (*Non déclaré*)
        begin
          let env_, ok_ =
            add_type lb le env x (TRecordDef {recd=Smap.empty;rcurrent_offset=0}) niveau in
          let ok0 =
            List.for_all (verifier_champ env_) champs in
          let nenv, nok =
            List.fold_left aux_add_field (env_,true) champs in
          (nenv, ok_ && nok && ok0, tdecl)
        end
    end
  | Decl (xlist, typ_, None) ->
    let autoshadow = List.exists (fun x -> x = match typ_ with
      |TIdent i |TAccess i -> i ) xlist in
    if autoshadow then
      (message_erreur lb le
         ("type "^(match typ_ with |TIdent i |TAccess i -> i)^
          " is shadowed by a variable with the same identifier."));
    let vtype = get_type env lb le typ_  in
    if not(teq vtype TypeError) then
      let env, ok = List.fold_left (fun (env_,ok_) x ->
          let env_,ok = add_var lb le env_ x vtype ModeVar niveau false in
          env_, ok && ok_) (env,not(autoshadow)) xlist in
      env, ok, tdecl
    else
      env, false, tdecl
  | Decl (xlist, typ_, Some expr) ->
    let autoshadow = List.exists (fun x -> x = match typ_ with
      |TIdent i |TAccess i -> i ) xlist in
    if autoshadow then
      (message_erreur lb le
         ("type "^(match typ_ with |TIdent i |TAccess i -> i)^
          " is shadowed by a variable with the same identifier."));
    let nexpr = type_expr env expr and vtype = get_type env lb le typ_ in
    if teq nexpr.typ vtype then
      let env, ok = List.fold_left (fun (env_,ok_) x ->
        let env_, ok = add_var lb le env_ x vtype ModeVar niveau false in
        env_, ok && ok_) (env,not(autoshadow)) xlist in
      env,ok, {tdecl with decl = Decl (xlist,typ_,Some nexpr)}
    else
      begin
        message_erreur lb le ("type mismatch in "^(
            List.fold_left
              (fun s x -> s^","^x) "" xlist)^
                              ": "^(p_typ vtype)^
                              " := "^(p_typ nexpr.typ));
        env, false, {tdecl with decl = Decl (xlist,typ_,Some nexpr)}
      end
  | DeclProcedure (i,params,decls,instrs) ->
    type_decl
      env
      {tdecl with decl = DeclFunction (i,params,TIdent "none",decls,instrs)}
      niveau
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
              (check_return thenlist &&
               (List.for_all check_return elseiflist) &&
               check_return elselist)
              || check_return q
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
    (*Vérification de l'unicité des identifiants*)
    let nenv = {env with idents = []; records_to_check = []} in
    let nenv, ok = (add_function ~addid:(false) lb le nenv niveau i (ret, tparams)) in
    let nenv, ok = List.fold_left (fun (ev,ek) (i,m,t) ->
        match m with
        | Some m_ ->
          let ev, ek_ =
            add_var lb le ev i t m_ (niveau+1) true in ev, ek_ && ek
        | None ->
          let ev, ek_ =
            add_var lb le ev i t ModeIn (niveau+1) true in ev, ek_ && ek) (nenv,ok) tparams in
    let fenv, err_d, ndecl = type_decl_list nenv decls (niveau+1) in
    let chk_records = chk_records && check_records lb le fenv in
    let ninstr, err_i = type_list_instr ret fenv niveau instrs in
    let err_r =
      (if check_return instrs then
         ok
       else
         (message_erreur lb le
            "the execution of a function must end on a return instruction";
          false)
      ) in

    let nenv, nerr = add_function lb le env niveau i (ret,tparams) in
    let nenv = {nenv with records_to_check = []} in
    nenv,
    chk_records && err_d && err_i != TypeError && err_r && nerr,
    {tdecl with
     env = fenv;
     decl = DeclFunction (i,params,rettyp_,ndecl,ninstr)
    }

and type_decl_list env ldecl niveau =
  let aux_iter_decls (env, err, lst) decl =
    let nenv,nerr,ndecl = type_decl env decl niveau in
    (nenv, nerr && err, ndecl::lst)
  in
  List.fold_left aux_iter_decls (env,true,[]) ldecl
