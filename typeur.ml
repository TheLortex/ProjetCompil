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
  | _ -> ModeIn

let rec check_type env lb le (l1 : tparam list) (l2 : texpr list) = match l1,l2 with
  | [],[] -> true
  | _,[] -> false
  | [],_ -> false
  | ((x,mode,t1)::q1),(t2::q2) when teq t1 t2.typ ->
    if mode = Some ModeInOut then
      begin
        let expr_mode = get_expr_mode env t2 lb le in
        if est_valeur_gauche t2 && expr_mode == ModeInOut then
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
  | ((x,_,t1)::q1),(t2::q2) -> message_erreur lb le ("Incohérence des types pour le paramètre "^x^" : "^(p_typ t1)^" != "^(p_typ t2.typ));(let _ = check_type env lb le q1 q2 in ());false



let add_ident lb le env i =
  if List.mem i env.idents then
    (message_erreur lb le ("L'identifiant "^i^" a déjà été défini auparavant."); env,false)
  else
    {env with idents = (i::env.idents)}, true


let find_var env ident lb le  =
  try
    let t,_ = Smap.find ident env.vars in t
  with
  | Not_found ->
    begin
      try
        let (ret,params) = Smap.find ident env.functions in
        begin
          match params with
          | [] -> ret
          | _ -> message_erreur lb le ("La fonction "^ident^" a des paramètres."); TypeError
        end
      with
      | Not_found -> message_erreur lb le ("Variable "^ident^" non déclarée.\n"); TypeError
    end



let find_record_field env ident x lb le =
  try
    Smap.find x (Smap.find ident env.records)
  with
  | Not_found -> message_erreur lb le ("Champ "^ident^"."^x^" non déclaré.\n"); TypeError

let find_function env ident lb le =
  try
    Smap.find ident env.functions
  with
  | Not_found -> message_erreur lb le ("Fonction "^ident^" non déclarée.\n"); TypeError, []
let find_record env ident = Smap.exists (fun key _ -> key = ident) env.records
let find_type env ident lb le =
  try
    Smap.find ident env.types
  with
  | Not_found ->
    if find_record env ident then
      (TRecord ident)
    else
      (message_erreur lb le ("Type "^ident^" non déclaré.\n"); TypeError)

let add_var lb le env ident typ mode =
  let env = {env with vars = Smap.add ident (typ,mode) env.vars} in
  add_ident lb le env ident

let add_type lb le env ident t =
  let env = {env with types = Smap.add ident t env.types} in
  add_ident lb le env ident

let add_record lb le env ident =
  let env = {env with records = Smap.add ident (Smap.empty) env.records} in
  add_ident lb le env ident

let add_record_field env ident x typ lb le =
  try
    let record = Smap.find ident env.records in
    begin
      if Smap.mem x record then (env, false) else (
        {env with records = Smap.add ident (Smap.add x typ record) env.records}, true)
    end
  with
  | Not_found -> message_erreur lb le ("Enregistrement "^ident^" non déclaré.\n"); env,false
let add_function lb le env ident (ret,params) =
  let env = {env with functions = Smap.add ident (ret,params) env.functions} in
  let nenv_,nok = add_ident lb le env ident
  in nenv_, nok
