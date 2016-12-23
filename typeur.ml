open Ast
open Printf
open Lexing
open Compile

let teq t1 t2 = match t1,t2 with
  | (TAccessRecord _, TypeNull) -> true
  | TypeNull,(TAccessRecord _) -> true
  | t1,t2 when t1 = t2 -> true
  | _,_ -> false




let message_erreur lb le message =
  let print_position pos1 pos2 =
    fprintf stderr "File \"%s\", line %d, characters %d-%d"
      pos1.pos_fname
      pos1.pos_lnum
      (pos1.pos_cnum - pos1.pos_bol + 1)
      (pos2.pos_cnum - pos2.pos_bol + 1)
  in
  print_position lb le;
  fprintf stderr ":\n %s \n" message

let find_var_mode env ident lb le  =
  try
    (Smap.find ident env.vars).mode
  with
  | Not_found ->
    message_erreur lb le
      ("variable "^ident^" is not declared.\n");
    ModeIn

let rec est_valeur_gauche env expr lb le = match expr.expr with
  | EAccess (None, ident) ->
    begin
      match find_var_mode env ident lb le with
        | ModeIn | ModeNone -> false
        | ModeInOut | ModeVar -> true
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

(*Vérifie la compatibilité des paramètres.*)
let check_type env lb le l1 l2 =
  let rec typing_correctness inout_vars (l1 : tparam list) (l2 : texpr list)  =
  match l1,l2 with
  | [],[] -> true
  | _,[] -> false
  | [],_ -> false
  | ((x,mode,t1)::q1),(t2::q2) when teq t1 t2.typ ->
    if mode = Some ModeInOut then
      begin
        let expr_mode = get_expr_mode env t2 lb le in
        if est_valeur_gauche env t2 lb le && expr_mode != ModeIn then
          begin
            if List.mem t2.expr inout_vars then
              (message_erreur lb le ("writable actual for "^x^" is overlapped");
               let _ = typing_correctness inout_vars q1 q2 in ();
               false)
            else
              typing_correctness (t2.expr::inout_vars) q1 q2
          end
        else begin
          if not(est_valeur_gauche env t2 lb le) then
            (message_erreur lb le
               ("parameter "^x^
          " is declared 'in out' but the given expression isn't a left-value.");
             (let _ = typing_correctness inout_vars q1 q2 in ());
             false)
          else
            (message_erreur lb le
               ("parameter "^x^
                " is declared in out but the given expression is declared in.");
             (let _ = typing_correctness inout_vars q1 q2 in ());
             false)
        end
      end
    else
      typing_correctness inout_vars q1 q2
  | ((x,_,t1)::q1),(t2::q2) ->
    if t2.typ != TypeError && t1 != TypeError then
      message_erreur lb le
        ("type mismatch for parameter "^x^": "^
         (p_typ t1)^" != "^(p_typ t2.typ));
    (let _ = typing_correctness inout_vars q1 q2 in ());
    false
  in
  typing_correctness [] l1 l2

let add_ident lb le env i =
  if List.mem i env.idents then
    (message_erreur lb le ("identifier "^i^" is already in use in this scope.");
     env,false)
  else
    {env with idents = (i::env.idents)}, true

let is_function env ident =
  try
    let t = (Smap.find ident env.vars).typ in
    begin
      match t with
      | TFunction (ret,[],_) -> true
      | TFunction (ret, p,_) -> false
      | TType _ -> false
      | _ -> false
    end
  with
  | Not_found -> false



let find_var env ident lb le  =
  try
    let t = (Smap.find ident env.vars).typ in
    begin
      match t with
      | TFunction (ret,[],_) -> ret
      | TFunction (ret, p,_) ->
        message_erreur lb le
          ("function "^ident^" requires parameters."); TypeError
      | TType _ -> message_erreur lb le (ident^" is a type.");TypeError
      | _ -> t
    end
  with
  | Not_found ->
    message_erreur lb le
      ("variable "^ident^" is not declared.");
    TypeError

let rec find_record_field env (level,ident) x lb le =
  try
    begin
      match Lmap.find (level,ident) env.types with
      | TRecordDef r ->
        begin
          try
            (Smap.find x r.recd).rtyp
          with
          | Not_found -> TypeError
        end
      | TAccessRecord level_ident -> find_record_field env level_ident x lb le
      | _ -> TypeError
    end
  with
  | Not_found ->
    message_erreur lb le
      ("field "^ident^"."^x^" is not declared.");
    TypeError

let find_function env ident lb le =
  try
    begin
      match (Smap.find ident env.vars).typ with
      | TFunction f -> f
      | _ -> TypeError, [], 0
    end
  with
  | Not_found -> message_erreur lb le ("function "^ident^" is not declared.");
    TypeError, [], 0

let get_record_def env (level,ident) =
  try
    begin
      match Lmap.find (level,ident) env.types with
      | TRecordDef r -> r
      | _ -> {recd=Smap.empty; rcurrent_offset=0}
    end
  with
  | Not_found -> {recd=Smap.empty; rcurrent_offset=0}

  (* Taille du type en octets *)
let rec type_size env = function
  | Tchar -> 8 (*TODO: Optimize*)
  | Tbool -> 8
  | Tint -> 8
  | TypeNull -> 8
  | TAccessRecord _ -> 8
  | TRecord id ->
    let recd = get_record_def env id in
    let size_field _ tfield total = total + type_size env tfield.rtyp in
    Smap.fold size_field recd.recd 0
  | _ -> 0

let find_record env ident =
  try
    begin
      match (Smap.find ident env.vars).typ with
      | TType (lvl,ident) -> true, get_record_def env (lvl,ident), lvl
      | _ -> false, {recd=Smap.empty; rcurrent_offset=0}, 0
    end
  with
  | Not_found -> false, {recd=Smap.empty; rcurrent_offset=0}, 0

let is_record_defined lb le env ident =
  let result, def, _ = find_record env ident in
  if result then not(Smap.is_empty def.recd)
  else
    (message_erreur lb le
       ("record "^ident^" is not declared."); false)

let get_custom_type env (lvl,ident) = Lmap.find (lvl,ident) env.types

let find_type env type_ident lb le =
  try
    begin
      match (Smap.find type_ident env.vars).typ with
      | TType t -> (match get_custom_type env t with
        | TRecordDef _ -> TRecord t
        | x -> x)
      | _ ->
        (message_erreur lb le
           (type_ident^" does not refer to a type.");
         TypeError)
    end
  with
  | Not_found ->
    begin
      match type_ident with
      | "integer" -> Tint
      | "boolean" -> Tbool
      | "character" -> Tchar
      | "none" -> TypeNone
      | _ ->
        (message_erreur lb le
           (type_ident^" does not refer to a type.");
         TypeError)
    end

let add_var lb le env ident typ mode niveau isparam =
  let env =
    if isparam then (*On passe en paramètre l'adresse de la variable.*)
      let ts = type_size env typ in
      {env with vars = Smap.add ident{typ = typ;mode = mode; level = niveau; offset = env.param_offset + (match mode with | ModeInOut -> 8 | _ -> ts)-8; uid="";} env.vars;
                param_offset = env.param_offset + (match mode with | ModeInOut -> 8 | _ -> ts)}
    else
      {env with vars = Smap.add ident{typ = typ;mode = mode; level = niveau; offset = env.current_offset;uid="";} env.vars;
                current_offset = env.current_offset - type_size env typ}

  in
  add_ident lb le env ident

let add_type lb le env ident typ niveau =
  let typedef = TType (niveau,ident) in
  let env = {env with vars = Smap.add ident {typ = typedef; mode = ModeNone; level = 0; offset = 0;uid="";} env.vars;
                      types = Lmap.add (niveau,ident) typ env.types} in
  add_ident lb le env ident

let add_record_field lb le env ident champ typ niveau =
  let res, def, _ = find_record env ident in
  if res then
    begin
      if Smap.mem champ def.recd then
        (message_erreur lb le
           ("field "^champ^" is already declared in the record "^ident^".");
         env, false)
      else (
        let cf = def.rcurrent_offset and size = type_size env typ in
        {env with
         types =
           Lmap.add
             (niveau,ident)
             (TRecordDef {recd=(Smap.add champ {rtyp = typ; roffset = cf} def.recd); rcurrent_offset = cf+size;})
             env.types
        }, true)
    end
  else
    (message_erreur lb le
       ("record "^ident^" is not declared.");
     env,false)

let add_function ?(addid=true) lb le env niveau ident uid (ret,params) =
  let env       =
    {env with
     vars = Smap.add ident {typ = TFunction (ret,params,niveau); mode = ModeNone; level = niveau; offset = 0;uid=uid;} env.vars} in
  let env, nok  =
    (if addid then add_ident lb le env ident else env,true)
  in env, nok
