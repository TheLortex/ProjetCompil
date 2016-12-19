open Typeur_decl
open Typeur_instr
open Typeur
open Ast

open Lexing
open Printf

let typeur fname (i, decls, instrs) =
  let noloc = {pos_fname = fname; pos_bol = 0; pos_lnum = 1; pos_cnum = 0} in
  let env = empty in
  let env, _ =
    add_function noloc noloc env 0 "put" (TypeNone, [("e",None,Tchar)]) in
  let env, _ =
    add_function noloc noloc env 0 "new_line" (TypeNone, []) in

  let (fenv, ok, ntree) =
    type_decl env {decl = DeclProcedure(i,[],decls,instrs);
                   lb = noloc;
                   le = noloc;
                   env = empty} 0 in
  match ntree.decl with
    | DeclFunction(i,_,_,ndecls,ninstrs)  ->
            begin
              if not(ok) then
                fprintf stderr "fatal error: program can't be typed.\n";
              ok,(i, ndecls, ninstrs),ntree.env
            end
    | _ -> failwith "wat"
