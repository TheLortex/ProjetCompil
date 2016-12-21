open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile

let rec compile_instr niveau decl_env = print_string ("instr("^(string_of_int niveau)^")\n");
  let rec comprec tinstr =
    match tinstr.instr with
    | IEval (i, lexpr) ->print_string "eval\n";
      let ret_typ, params, level =
        match (try (Smap.find i decl_env.vars).typ with |Not_found -> print_string ("\nb "^i^"\n");TypeNone) with
        | TFunction (r,p,l) ->print_string (i^" found.\n"); r,p,l
      in
      let rec frame_size = function
        | [] -> 0
        | (_,Some ModeInOut,p)::q -> 8 + frame_size q
        | (_,_,p)::q -> type_size decl_env p + frame_size q
      in
      List.fold_left (*Empilement des paramètres*)
        (fun acc (e,(_,m,_)) -> acc ++ compile_expr (match m with |Some ModeInOut -> true | _ -> false) niveau decl_env e) nop (List.combine lexpr params) ++
      movq (reg rbp) (reg rsi) ++
      iter (niveau - level) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ (*Empilement du pointeur vers le tableau d'activation de la fonction*)
      pushq (reg rsi) ++
      call (i^"_"^(string_of_int level)) ++ (*Appel de la procédure*)
      addq (imm (8+frame_size params)) (reg rsp) (*Dépilage*)
    | IConditional(texpr,instrsthen,[],q) ->print_string "conditionalend\n";
      let i1 = ("if_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) and e1 = ("if_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) in
      compile_expr false niveau decl_env texpr ++
      popq rax ++
      cmpq (imm 1) (reg rax) ++
      je i1 ++
      begin
        match q with
        | None -> nop
        | Some instrs -> compile_instrs niveau decl_env instrs
      end ++
      jmp e1 ++
      label i1 ++
      compile_instrs niveau decl_env instrsthen ++
      label e1
    | IConditional(texpr,instrsthen,elsifs,q) -> print_string "conditional\n";
      let i1 = ("if_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) and e1 = ("if_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) in
      compile_expr false niveau decl_env texpr ++
      popq rax ++
      cmpq (imm 1) (reg rax) ++
      je i1 ++
      begin
        let nexpr, ninstr = List.hd elsifs in
        comprec {tinstr with instr = (IConditional(nexpr,ninstr,List.tl elsifs,q))}
      end ++
      jmp e1 ++
      label i1 ++
      compile_instrs niveau decl_env instrsthen ++
      label e1
    | IFor(x,reverse,expr1,expr2,instrs) ->print_string "for\n";
      let decl_env = match tinstr.typ with | TypeNoneWithEnv e -> e in
      let forlabel = "for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ())) in
      let forlabel2 = "for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ())) in
      compile_expr false niveau decl_env expr2 ++
      compile_expr false niveau decl_env expr1 ++
      jmp forlabel2 ++
      label forlabel ++
      compile_instrs niveau decl_env instrs ++
      addq (imm 1) (ind rsp) ++
      label forlabel2 ++
      movq (ind ~ofs:8 rsp) (reg rax) ++
      cmpq (reg rax) (ind rsp) ++
      jle  forlabel ++
      addq (imm 16) (reg rsp)

    | IReturn None -> (movq (reg rbp) (reg rsp) ++ popq rbp) ++ ret
    | IAssign (access, texpr) ->
      let ts = type_size decl_env texpr.typ in
      compile_expr false niveau decl_env texpr ++ (*Résultat de l'expression sur la pile*)
      compile_expr true niveau decl_env {expr = (EAccess access); typ = texpr.typ; lb=noloc;le=noloc;} ++ (*Adresse de sauvegarde sur la pile*)
      popq rsi ++ (*On balance l'adresse dans rsi*)
      (let c = ref(nop) in (*On push le contenu*)
       for i = 1 to (ts/8) do
         c := !c ++ (popq rax) ++ movq (reg rax) (ind ~ofs:(-ts+8*i) rsi)
       done; !c)

    | _ -> nop
  in
  comprec
and compile_instrs niveau decl_env = function
  | [] -> nop
  | p::q -> (compile_instr niveau decl_env p) ++ compile_instrs niveau decl_env q
