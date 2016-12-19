open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile_instr
open Compile
open Lexing

let compile_decl niveau =
  let noloc = {pos_bol = 0;
               pos_lnum = 1;
               pos_cnum = 0;
               pos_fname = "";} in
  let rec locals_size decl_env = function
    | [] -> 8
    | p::q ->
      begin(
        match p.decl with
        | Decl (ilist,TIdent t,v) ->
          locals_size decl_env q + (List.length ilist)*(type_size decl_env (find_type decl_env t noloc noloc))
        | Decl (ilist,TAccess t,v) ->
          locals_size decl_env q + (List.length ilist)*8
        | _ ->
          locals_size decl_env q)
      end
in
let rec comprec niveau tdecl =
  let decl_env = tdecl.env in
  match tdecl.decl with
    | DeclProcedure (ident, params, decls, instrs) ->
      let fs = locals_size decl_env decls in
      List.fold_left (fun t d -> t ++ comprec (niveau+1) d) nop decls ++
      (if niveau == 0 then (glabel "main") else (label (ident^" "^(string_of_int niveau)))) ++ (* def de la fonction *)
      subq (imm fs) (reg rsp) ++ (*Ajout des variables locales sur la frame*)
      (if niveau == 0 then nop else movq (reg rbp) (ind ~ofs:(fs-8) rsp)) ++ (*Sauvegarde du rbp appelant*)
      leaq (ind ~ofs:(fs-8) rsp) rbp ++ (*Positionnement du pointeur de frame*)
      compile_instrs (niveau+1) decl_env instrs ++ (*Compilation des instructions*)
       (*Désallouage de la pile*)
      (if niveau == 0 then (addq (imm fs) (reg rsp) ++ movq (imm 0) (reg rax)) else (movq (reg rbp) (reg rsp) ++ popq rbp)) ++ 
      ret
    | _ -> nop
  in
  comprec niveau
