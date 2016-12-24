open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile_instr
open Compile
open Lexing

let compile_decl niveau =
  let rec locals_init niveau decl_env = function
    | [] -> nop
    | p::q ->
      begin(
        match p.decl with
        | Decl (ilist,TIdent t,Some expr) ->
          compile_expr false (niveau+1) decl_env expr ++
          (match expr.typ with
           | TRecord i ->
             let ts = type_size decl_env (TRecord i) in
             let c = ref(nop) in
             for i = (ts/8-1) downto 0 do
               c := !c ++ popq rax ++
                    (List.fold_left
                       (fun s x ->
                          s ++
                          movq (reg rax) (ind ~ofs:((get_var_offset decl_env x) - 8*i) rbp)
                       ) nop ilist)
             done; !c;
           | _ ->
                (List.fold_left
                   (fun s x ->
                      s ++
                      movq (reg rax) (ind ~ofs:(get_var_offset decl_env x) rbp)
                   ) nop ilist)) ++
          locals_init niveau decl_env q
        | Decl (ilist,TAccess t,Some expr) ->
          compile_expr true (niveau+1) decl_env expr ++
          (List.fold_left
             (fun s x ->
                s ++
                movq (reg rax) (ind ~ofs:(get_var_offset decl_env x) rbp)
             ) nop ilist) ++
          locals_init niveau decl_env q
        | _ -> locals_init niveau decl_env q)
      end
  in
let rec comprec niveau tdecl =
  let decl_env = tdecl.env in
  (*Smap.iter (fun x _ -> print_string ("."^x^"\n")) decl_env.vars;*)
  match tdecl.decl with
  | DeclProcedure (ident, params,decls, instrs) | DeclFunction (ident, params, _,decls, instrs) ->
    let fs = -tdecl.env.current_offset in
    List.fold_left (fun t d -> t ++ comprec (niveau+1) d) nop decls ++
    (if niveau == 0 then (glabel "main") else (label (try Smap.find ident decl_env.vars with |Not_found -> failwith ident).uid )) ++ (* def de la fonction *)
    subq (imm fs) (reg rsp) ++ (*Ajout des variables locales sur la frame*)
    (if niveau == 0 then nop else movq (reg rbp) (ind ~ofs:(fs-8) rsp)) ++ (*Sauvegarde du rbp appelant*)
    leaq (ind ~ofs:(fs-8) rsp) rbp ++ (*Positionnement du pointeur de frame*)
    locals_init niveau decl_env (List.rev decls) ++
    compile_instrs (niveau+1) decl_env instrs ++ (*Compilation des instructions*)
     (*Désallouage de la pile*)
    (if niveau == 0 then (addq (imm fs) (reg rsp) ++ movq (imm 0) (reg rax)) else (movq (reg rbp) (reg rsp) ++ popq rbp)) ++
    ret
  | _ -> nop
  in
  comprec niveau
