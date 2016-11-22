open Typeur_decl
open Typeur_instr
open Typeur
open Ast

open Lexing

let noloc =  {pos_fname = ""; pos_bol = 0; pos_lnum = 0; pos_cnum = 0}

let typeur (i, decls, instrs) =
  let env = empty in
  let env, _ = add_function noloc noloc env "put" (TypeNone, [("e",None,Tchar)]) in
  let env, _ = add_function noloc noloc env "new_line" (TypeNone, []) in
  let (nenv, d_ok, ndecls) = type_decl_list env decls in
  let (ninstrs, i_errs) = type_list_instr TypeNone nenv instrs in
  if not(d_ok) then
    print_string "Erreur lors du typage des déclarations.\n"
  ;if i_errs = TypeError then print_string "Erreur lors du typage des instructions.\n"
  ;(i, ndecls, ninstrs)
