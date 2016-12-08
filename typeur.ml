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

let find_var_mode env ident lb le  =
  try
    let _,m = Smap.find ident env.vars in m
  with
  | Not_found -> message_erreur lb le ("Variable "^ident^" non déclarée.\n"); ModeIn

let rec est_valeur_gauche env expr lb le = match expr.expr with
  | EAccess (None, ident) -> (*ident peut être un appel de fonction sans paramètre. *)
    begin
      match find_var_mode env ident lb le with
        | ModeIn | ModeNone -> false
        | ModeInOut -> true
    end
  | EAccess (Some e, ident)-> begin
      match e.typ with
      | TRecord _ -> est_valeur_gauche env e lb le
      | TAccessRecord _ -> true
      | _ -> false
    end
  | _ -> false


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
        if est_valeur_gauche env t2 lb le && expr_mode != ModeIn then
          check_type env lb le q1 q2
        else begin
          if not(est_valeur_gauche env t2 lb le) then
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
      | _ -> TypeError
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
      match Smap.find ident env.records with
      | recd,lvl-> true, recd, lvl
    end
  with
  | Not_found -> false, Smap.empty, 0

let is_record_defined env x lb le =
  try
    begin
      match Smap.find x env.vars with
      | TType(TRecordDef recd),_ -> if Smap.is_empty recd then (message_erreur lb le ("Type enregistrement"^x^" utilisé avant sa définition complète."); false)
        else true
      | _ -> false
    end
  with
  | Not_found -> false

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
      try
        match Smap.find ident env.records with
        | i,l -> TRecord ((string_of_int l)^" "^ident)
      with Not_found ->
        begin
          match ident with
          | "integer" -> Tint
          | "boolean" -> Tbool
          | "character" -> Tchar
          | "none" -> TypeNone
          | _ -> (message_erreur lb le (ident^" ne désigne pas un type.");TypeError)
        end
    end
let add_var lb le env ident typ mode = (*TODO: Raise error*)
  let env = {env with vars = Smap.add ident (typ,mode) env.vars} in
        add_ident lb le env ident

let add_type lb le env ident t =
  let typedef = TType t in
  let env = {env with vars = Smap.add ident (typedef, ModeNone) env.vars} in
  add_ident lb le env ident

let add_record lb le env ident niveau =
  let env = {env with
             vars = Smap.add ((string_of_int niveau)^" "^ident) (TType (TRecordDef Smap.empty), ModeNone) env.vars;
             records = Smap.add ident (Smap.empty,niveau) env.records} in
  add_ident lb le env ident

let add_record_field env ident x typ lb le niveau =
  let res, record, _ = find_record env ident in
  if res then
    begin
      if Smap.mem x record then (message_erreur lb le ("Champs "^x^" déjà déclaré dans l'enregistement "^ident^".");env, false) else (
        let nvars = Smap.add ((string_of_int niveau)^" "^ident) (TType (TRecordDef (Smap.add x typ record)), ModeNone) env.vars in
        let nvars = Smap.add ident (TType (TRecord ((string_of_int niveau)^" "^ident)), ModeNone) nvars in
        {env with
         vars = nvars;
         records = Smap.add ident ((Smap.add x typ record), niveau) env.records}, true)
    end
  else
    (message_erreur lb le ("Enregistrement "^ident^" non déclaré.\n"); env,false)

let add_function ?(addid=true) lb le env ident (ret,params) =
  let env = {env with vars = Smap.add ident (TFunction (ret,params), ModeNone) env.vars} in
  let nenv_,nok = (if addid then add_ident lb le env ident else env,true)
  in nenv_, nok
