open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile

let rec compile_instr niveau decl_env =
  let rec comprec tinstr =
    match tinstr.instr with
    | IEval (i, lexpr) ->
      let ret_typ, params, level =
        match (try (Smap.find i decl_env.vars).typ with |Not_found -> print_string ("\nb "^i^"\n");TypeNone) with
        | TFunction (r,p,l) ->r,p,l
        | _ -> failwith "compile_instr.ml:15"
      in
      let rec frame_size = function
        | [] -> 0
        | (_,Some ModeInOut,p)::q -> 8 + frame_size q
        | (_,_,p)::q -> type_size decl_env p + frame_size q
      in
      List.fold_left (*Empilement des paramètres*)
        (fun acc (e,(_,m,_)) -> compile_expr (match m with |Some ModeInOut -> true | _ -> false) niveau decl_env e ++ acc) nop (List.combine lexpr params) ++
      movq (reg rbp) (reg rsi) ++
      iter (niveau - level) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ (*Empilement du pointeur vers le tableau d'activation de la fonction*)
      pushq (reg rsi) ++
      call (i^"_"^(string_of_int level)) ++ (*Appel de la procédure*)
      addq (imm (8+frame_size params)) (reg rsp) (*Dépilage*)
    | IConditional(texpr,instrsthen,[],q) ->
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
    | IConditional(texpr,instrsthen,elsifs,q) ->
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
    | IFor(x,reverse,expr1,expr2,instrs) ->
      let decl_env = match tinstr.typ with | TypeNoneWithEnv e -> e | _ -> failwith "" in
      let forlabel = "for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ())) in
      let forlabel2 = "for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ())) in
      (if reverse
       then (compile_expr false niveau decl_env expr1 ++
             compile_expr false niveau decl_env expr2)
       else (compile_expr false niveau decl_env expr2 ++
             compile_expr false niveau decl_env expr1)) ++
      jmp forlabel2 ++
      label forlabel ++
      compile_instrs niveau decl_env instrs ++
      (if reverse then decq (ind rsp) else incq (ind rsp)) ++
      label forlabel2 ++
      movq (ind ~ofs:8 rsp) (reg rax) ++
      cmpq (reg rax) (ind rsp) ++
      (if reverse then jge else jle)  forlabel ++
      addq (imm 16) (reg rsp)

    | IReturn None -> movq (reg rbp) (reg rsp) ++ popq rbp ++ ret
    | IReturn (Some expr) ->
      compile_expr false niveau decl_env expr ++
      (match expr.typ with
       | TRecord (i,l) ->
         let ts = type_size decl_env (TRecord (i,l) ) in
           compile_expr false niveau decl_env {expr = (ENew l); typ = TAccessRecord (i,l); lb = noloc; le=noloc;} ++
           popq rsi ++
           (let c = ref(nop) in (*On push le contenu*)
            for i = 1 to (ts/8) do
              c := !c ++ (popq rax) ++ movq (reg rax) (ind ~ofs:(-ts+8*i) rsi)
            done; !c) ++
           movq (reg rbp) (reg rsp) ++
           popq rbp ++
           movq (reg rsi) (reg rax)++
           ret
       | _ -> popq rax) ++ movq (reg rbp) (reg rsp) ++ popq rbp ++ ret
    | IAssign (access, texpr) ->
      let ts = type_size decl_env texpr.typ in
      compile_expr false niveau decl_env texpr ++ (*Résultat de l'expression sur la pile*)
      compile_expr true niveau decl_env {expr = (EAccess access); typ = texpr.typ; lb=noloc;le=noloc;} ++ (*Adresse de sauvegarde sur la pile*)
      popq rsi ++ (*On balance l'adresse dans rsi*)
      (let c = ref(nop) in (*On push le contenu*)
       for i = 1 to (ts/8) do
         c := !c ++ (popq rax) ++ movq (reg rax) (ind ~ofs:(-ts+8*i) rsi)
       done; !c)
    | IScope linstr ->
      compile_instrs niveau decl_env linstr
    | IWhile (texpr, linstr) ->
      let l1 = ("for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) and
      l2 = ("for_"^(string_of_int niveau)^"_"^(string_of_int (uuid ()))) in
      label l1 ++
      compile_expr false niveau decl_env texpr ++
      popq rax ++
      cmpq (imm 0) (reg rax) ++
      je l2 ++
      compile_instrs niveau decl_env linstr ++
      jmp l1 ++
      label l2
  in
  comprec
and compile_instrs niveau decl_env = function
  | [] -> nop
  | p::q -> (compile_instr niveau decl_env p) ++ compile_instrs niveau decl_env q
