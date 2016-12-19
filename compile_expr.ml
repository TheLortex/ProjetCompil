open X86_64
open Ast
open Typeur
open Printf
open Char
open Compile

let compile_expr valeur_gauche niveau decl_env = print_string ("decl("^(string_of_int niveau)^")\n");
  let rec comprec texpr = match texpr.expr with
    | EInt i ->  movq (imm i) (reg rax) ++ pushq (reg rax)
    | EChar c -> movq (imm (Char.code c)) (reg rax) ++ pushq (reg rax)
    | ETrue -> movq (imm 1) (reg rax) ++ pushq (reg rax)
    | EFalse -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | ENull -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | EAccess (None, ident) ->
      (try
      let vardecl = Smap.find ident decl_env.vars in
      if valeur_gauche then
        (
          movq (reg rbp) (reg rsi) ++
          iter (niveau - vardecl.level) (leaq (ind ~ofs:16 rsi) rsi) ++
          pushq (ind ~ofs:vardecl.offset rsi)
        )
      else
        (
          movq (reg rbp) (reg rsi) ++
          iter (print_int niveau; print_string "_";print_int vardecl.level;print_string (ident^"\n");niveau - vardecl.level) (leaq (ind ~ofs:16 rsi) rsi) ++
          leaq (ind ~ofs:vardecl.offset rsi) rsi ++
          pushq (ind rsi)
        )
       with | Not_found -> failwith ("Access de "^ident^" non trouvé"))
    | EAccess (Some expr, ident) -> nop
    | EOp (expr1, operateur, expr2) ->
      comprec expr1 ++ comprec expr2 ++ popq rbx ++ popq rax ++
      begin
      match operateur with
      | OpDiv   -> cqto ++
                   idivq (reg rbx) ++
                   pushq (reg rax)
      | OpMinus -> subq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpPlus  -> addq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpTimes -> imulq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpRem   -> cqto ++
                   idivq (reg rbx) ++
                   pushq (reg rdx)
      | OpEq    -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   sete (reg cl) ++
                   pushq (reg rcx)
      | OpNeq   -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setne (reg cl) ++
                   pushq (reg rcx)
      | OpGt    -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setg (reg cl) ++
                   pushq (reg rcx)
      | OpGet   -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setge (reg cl) ++
                   pushq (reg rcx)
      | OpLt    -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setl (reg cl) ++
                   pushq (reg rcx)
      | OpLet   -> xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setle (reg cl) ++
                   pushq (reg rcx)
      end
    | ENot expr -> comprec expr ++
                   cmpq (imm 0) (ind rsp) ++
                   sete (ind rsp)
    | EMinus expr -> comprec expr ++
                     popq rax ++
                     negq (reg rax) ++
                     pushq (reg rax)
    | ENew ident -> nop
    | EEval (ident, lparams) ->
      let ret_typ, params, level = match (Smap.find ident decl_env.vars).typ with
        | TFunction (r,p,l) -> r,p,l
      in
      let rec frame_size = function
        | [] -> 0
        | (_,_,p)::q -> type_size decl_env p + frame_size q
      in
      List.fold_left (*Empilement des paramètres*)
        (fun acc e -> acc ++ comprec e) nop lparams ++
      movq (reg rbp) (reg rsi) ++
      (print_int niveau; print_string "_";print_int level;print_string "\n";iter (niveau - level) (movq (ind ~ofs:16 rsi) (reg rsi))) ++ (*Empilement du pointeur vers le tableau d'activation de la fonction*)
      pushq (reg rsi) ++
      call (ident^"_"^(string_of_int level)) ++ (*Appel de la procédure et retour dans rax*)
      addq (imm (8+frame_size params)) (reg rsp) ++ (*Dépilage*)
      pushq (reg rax)
    | EChr expr -> comprec expr
  in
  comprec
