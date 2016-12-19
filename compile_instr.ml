open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile

let rec compile_instr niveau decl_env =
  let rec comprec = function
    | IEval (i, lexpr) ->
      let ret_typ, params, level = match (Smap.find i decl_env.vars).typ with
        | TFunction (r,p,l) -> r,p,l
      in
      let rec frame_size = function
        | [] -> 0
        | (_,_,p)::q -> type_size decl_env p + frame_size q
      in
      List.fold_left (*Empilement des paramètres*)
        (fun acc e -> acc ++ compile_expr niveau decl_env e.expr) nop lexpr ++
      movq (reg rbp) (reg rsi) ++
      iter (niveau - level - 1) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ (*Empilement du pointeur vers le tableau d'activation de la fonction*)
      pushq (reg rsi) ++
      call (i^"_"^(string_of_int level)) ++ (*Appel de la procédure*)
      addq (imm (8+frame_size params)) (reg rsp) (*Dépilage*)


    | _ -> nop
  in
  comprec
and compile_instrs niveau decl_env = function
  | [] -> nop
  | p::q -> (compile_instr niveau decl_env p.instr) ++ compile_instrs niveau decl_env q
