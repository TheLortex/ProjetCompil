open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile_instr
open Compile
open Lexing

let compile_decl niveau = print_string ("decl("^(string_of_int niveau)^")\n");
  let noloc = {pos_bol = 0;
               pos_lnum = 1;
               pos_cnum = 0;
               pos_fname = "";} in
  let rec locals_init niveau decl_env = function
    | [] -> nop
    | p::q ->
      begin(
        match p.decl with
        | Decl (ilist,TIdent t,Some expr) ->print_string ("init de "^(List.hd ilist)^" niveau "^(string_of_int niveau)^"\n");
          compile_expr false (niveau+1) decl_env expr ++
          (match expr.typ with
           | TRecord i ->
             let ts = type_size decl_env (TRecord i) in
             let c = ref(nop) in
             for i = 0 to (ts/8-1) do
               c := !c ++ popq rax ++
                    (List.fold_left
                       (fun s x ->
                          s ++
                          movq (reg rax) (ind ~ofs:((get_var_offset decl_env x) - 8*i) rbp)
                       ) nop ilist)
             done; !c;
           | _ ->
                popq rax ++
                (List.fold_left
                   (fun s x ->
                      s ++
                      movq (reg rax) (ind ~ofs:(get_var_offset decl_env x) rbp)
                   ) nop ilist)) ++
          locals_init niveau decl_env q
        | Decl (ilist,TAccess t,Some expr) -> print_string ("init de "^(List.hd ilist)^" niveau "^(string_of_int niveau)^"\n");
          compile_expr true (niveau+1) decl_env expr ++
          popq rax ++
          (List.fold_left
             (fun s x ->
                s ++
                movq (reg rax) (ind ~ofs:(get_var_offset decl_env x) rbp)
             ) nop ilist) ++
          locals_init niveau decl_env q
        | _ -> locals_init niveau decl_env q)
      end
  in
let rec comprec niveau tdecl = print_int niveau;
  let decl_env = tdecl.env in
  (*Smap.iter (fun x _ -> print_string ("."^x^"\n")) decl_env.vars;*)
  match tdecl.decl with
  | DeclProcedure (ident, params,decls, instrs) | DeclFunction (ident, params, _,decls, instrs) ->
    print_string ("compilation de la procédure "^ident^" au niveau "^(string_of_int niveau)^"\n");
    let fs = -tdecl.env.current_offset in
    List.fold_left (fun t d -> t ++ comprec (niveau+1) d) nop decls ++
    (if niveau == 0 then (glabel "main") else (label (ident^"_"^(string_of_int niveau)))) ++ (* def de la fonction *)
    subq (imm fs) (reg rsp) ++ (*Ajout des variables locales sur la frame*)
    (if niveau == 0 then nop else movq (reg rbp) (ind ~ofs:(fs-8) rsp)) ++ (*Sauvegarde du rbp appelant*)
    leaq (ind ~ofs:(fs-8) rsp) rbp ++ (*Positionnement du pointeur de frame*)
    locals_init niveau decl_env decls ++
    compile_instrs (niveau+1) decl_env instrs ++ (*Compilation des instructions*)
     (*Désallouage de la pile*)
    (if niveau == 0 then (addq (imm fs) (reg rsp) ++ movq (imm 0) (reg rax)) else (movq (reg rbp) (reg rsp) ++ popq rbp)) ++
    ret
  | _ -> nop
  in
  comprec niveau
