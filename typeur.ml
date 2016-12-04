open Ast
open Printf
open Lexing
open Ast_printer


(* Vérifie l'égalité de types *)
let teq t1 t2 = match t1,t2 with
  | (TAccessRecord _, TypeNull) -> true
  | TypeNull,(TAccessRecord _) -> true
  | t1,t2 when t1 = t2 -> true
  | t1,t2 -> false

let message_erreur lb le message =
  let print_position pos1 pos2 =
    fprintf stderr "File \"%s\", line %d, characters %d-%d" pos1.pos_fname
      pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol + 1) (pos2.pos_cnum - pos2.pos_bol + 1)
  in
  print_position lb le;
  fprintf stderr ":\n %s \n" message

  let rec est_valeur_gauche expr = match expr.expr with
    | EAccess (None, ident) -> true
    | EAccess (Some e, ident)-> begin
        match e.typ with
        | TRecord _ -> est_valeur_gauche e
        | TAccessRecord _ -> true
        | _ -> false
      end
    | _ -> false

let find_var_mode env ident lb le  =
  try
    let _,m = Smap.find ident env.vars in m
  with
  | Not_found -> message_erreur lb le ("Variable "^ident^" non déclarée.\n"); ModeIn


let rec get_expr_mode env texpr lb le = match texpr.expr with
  | EAccess (None, ident) -> find_var_mode env ident lb le
  | EAccess (Some e, ident) -> get_expr_mode env e lb le
  | _ -> ModeNone

let rec check_type env lb le (l1 : tparam list) (l2 : texpr list) = match l1,l2 with
  | [],[] -> true
  | _,[] -> false
  | [],_ -> false
  | ((x,mode,t1)::q1),(t2::q2) when teq t1 t2.typ ->
    if mode = Some ModeInOut then
      begin
        let expr_mode = get_expr_mode env t2 lb le in
        if est_valeur_gauche t2 && expr_mode != ModeIn then
          check_type env lb le q1 q2
        else begin
          if not(est_valeur_gauche t2) then
            (message_erreur lb le ("Le paramètre "^x^" est déclaré in out mais l'expression donnée n'est pas une valeur gauche.");(let _ = check_type env lb le q1 q2 in ());false)
          else
            (message_erreur lb le ("Le paramètre "^x^" est déclaré in out mais l'expression donnée est déclarée en in.");(let _ = check_type env lb le q1 q2 in ());false)
        end
      end
    else
      check_type env lb le q1 q2
  | ((x,_,t1)::q1),(t2::q2) ->
    if t2.typ != TypeError && t1 != TypeError then message_erreur lb le ("Incohérence des types pour le paramètre "^x^" : "^(p_typ t1)^" != "^(p_typ t2.typ));(let _ = check_type env lb le q1 q2 in ());false



let add_ident lb le env i =
  if List.mem i env.idents then
    (message_erreur lb le ("L'identifiant "^i^" a déjà été défini auparavant."); env,false)
  else
    {env with idents = (i::env.idents)}, true


let find_var env ident lb le  =
  try
    let t,_ = Smap.find ident env.vars in
    begin
      match t with
      | TFunction (ret,[]) -> ret
      | TFunction (ret, p) -> message_erreur lb le ("La fonction "^ident^" attend des paramètres."); TypeError
      | TType _ -> message_erreur lb le (ident^" est un type.");TypeError
      | _ -> t
    end
  with
  | Not_found -> message_erreur lb le ("Variable "^ident^" non déclarée.\n"); TypeError

let find_record_field env ident x lb le =
  try
    begin
      match Smap.find ident env.vars with
      | (TType (TRecordDef r)),_ ->
        begin
          try
            Smap.find x r
          with
          | Not_found -> TypeError
        end
      | _,_ -> TypeError
    end
  with
  | Not_found -> message_erreur lb le ("Champ "^ident^"."^x^" non déclaré.\n"); TypeError

let find_function env ident lb le =
  try
    begin
      match Smap.find ident env.vars with
      | TFunction f,_ -> f
      | _,_ -> TypeError, []
    end
  with
  | Not_found -> message_erreur lb le ("Fonction "^ident^" non déclarée.\n"); TypeError, []

let find_record env ident =
  try
    begin
      match Smap.find ident env.vars with
      | TType (TRecordDef recd),_-> true, recd
      | _ -> false, Smap.empty
    end
  with
  | Not_found -> false, Smap.empty

let find_type env ident lb le =
  try
    begin
      match Smap.find ident env.vars with
      | TType t,_ -> t
      | _ -> TypeError
    end
  with
  | Not_found ->
    begin
      match ident with
      | "integer" -> Tint
      | "boolean" -> Tbool
      | "character" -> Tchar
      | "none" -> TypeNone
      | _ -> (message_erreur lb le (ident^" ne désigne pas un type.");TypeError)
    end

let add_var lb le env ident typ mode =
  let env = {env with vars = Smap.add ident (typ,mode) env.vars} in
  add_ident lb le env ident

let add_type lb le env ident t =
  let typedef = TType t in
  let env = {env with vars = Smap.add ident (typedef, ModeNone) env.vars} in
  add_ident lb le env ident

let add_record lb le env ident =
  let env = {env with vars = Smap.add ident (TType (TRecordDef Smap.empty), ModeNone) env.vars} in
  add_ident lb le env ident

let add_record_field env ident x typ lb le =
  let res, record = find_record env ident in
  if res then
    begin
      if Smap.mem x record then (message_erreur lb le ("Champs "^x^" déjà déclaré dans l'enregistement "^ident^".");env, false) else (
        {env with vars = Smap.add ident (TType (TRecordDef (Smap.add x typ record)), ModeNone) env.vars}, true)
    end
  else
    (message_erreur lb le ("Enregistrement "^ident^" non déclaré.\n"); env,false)

let add_function ?(addid=true) lb le env ident (ret,params) =
  let env = {env with vars = Smap.add ident (TFunction (ret,params), ModeNone) env.vars} in
  let nenv_,nok = (if addid then add_ident lb le env ident else env,true)
  in nenv_, nok
