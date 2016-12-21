open X86_64
open Ast
open Typeur
open Printf
open Char
open Compile

let rec compile_expr valeur_gauche niveau decl_env = print_string ("decl("^(string_of_int niveau)^")\n");
  let rec comprec texpr = match texpr.expr with
    | EInt i ->  movq (imm i) (reg rax) ++ pushq (reg rax)
    | EChar c -> movq (imm (Char.code c)) (reg rax) ++ pushq (reg rax)
    | ETrue -> movq (imm 1) (reg rax) ++ pushq (reg rax)
    | EFalse -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | ENull -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | EAccess (None, ident) ->
      (try
      let vardecl = Smap.find ident decl_env.vars in
      if valeur_gauche then (*On veut l'adresse*)
        (
          movq (reg rbp) (reg rsi) ++
          iter (niveau - vardecl.level) (leaq (ind ~ofs:16 rsi) rsi) ++
          begin
            match vardecl.mode with
            | ModeInOut  -> leaq (ind ~ofs:vardecl.offset rsi) rsi ++
                                    pushq (ind rsi)
            | ModeIn | ModeNone | ModeVar ->
              (match texpr.typ with
               | TAccessRecord _ -> leaq (ind ~ofs:vardecl.offset rsi) rsi ++
                             pushq (reg rsi)
               | _ -> addq (imm vardecl.offset) (reg rsi) ++ pushq (reg rsi)

              )

          end
        )
      else
        (
          movq (reg rbp) (reg rsi) ++
          iter (print_int niveau; print_string "_";print_int vardecl.level;print_string (ident^"\n");niveau - vardecl.level) (leaq (ind ~ofs:16 rsi) rsi) ++
          leaq (ind ~ofs:vardecl.offset rsi) rsi ++
          begin
            match vardecl.mode with
            | ModeInOut ->
              movq (ind rsi) (reg rsi) ++ (*rsi pointe sur la variable en mémoire*)
              begin
                match vardecl.typ with
                | TRecord i ->
                  let ts = type_size decl_env (TRecord i) in
                  let c = ref(nop) in
                  for i = 0 to (ts/8-1) do
                    c := !c ++ pushq (ind ~ofs:(-8*i) rsi)
                  done; !c
                | _ -> pushq (ind rsi)

              end
            | ModeIn | ModeNone | ModeVar  ->
              begin
                match vardecl.typ with
                | TRecord i ->
                  let ts = type_size decl_env (TRecord i) in
                  let c = ref(nop) in
                  for i = 0 to (ts/8-1) do
                    c := !c ++ pushq (ind ~ofs:(-8*i) rsi)
                  done; !c
                | _ -> pushq (ind rsi)
              end
          end
        )
       with | Not_found -> failwith ("Access de "^ident^" non trouvé"))
    | EAccess (Some expr, ident) ->
      comprec expr ++
      if valeur_gauche then (*Calculer l'adresse*)
        begin
        match expr.typ with
        | TRecord i ->
          let rdef = get_record_def decl_env i in
          let varoffset = (Smap.find ident rdef.recd).roffset in
          popq rax ++
          subq (imm varoffset) (reg rax) ++
          pushq (reg rax)
        | TAccessRecord i ->
          let rdef = get_record_def decl_env i in
          let varoffset = (Smap.find ident rdef.recd).roffset in
          popq rax ++
          movq (ind rax) (reg rax) ++
          subq (imm varoffset) (reg rax) ++
          pushq (reg rax)
        end
      else
        begin
        match expr.typ with
        | TRecord i -> (*Le record entier a été empilé *)
          let rdef = get_record_def decl_env i in
          let varoffset = (Smap.find ident rdef.recd).roffset in
          let ts = type_size decl_env (Smap.find ident rdef.recd).rtyp in
          let rs = type_size decl_env (TRecord i) in
          let decalage_rsp = rs-ts in
          (let c = ref(nop) in (*On push le contenu*)
           for i = (ts/8-1) downto 0 do
             c := !c ++ movq (ind ~ofs:(rs-8-varoffset-8*i) rsp) (reg rax) ++ movq (reg rax) (ind ~ofs:(rs-8*(i+1)) rsp)
           done; !c) ++ addq (imm decalage_rsp) (reg rsp)
        | TAccessRecord i -> (*L'adresse du record a été empilée*)
          let rdef = get_record_def decl_env i in
          let varoffset = (Smap.find ident rdef.recd).roffset in
          let ts = type_size decl_env (Smap.find ident rdef.recd).rtyp in
          popq rsi ++
          leaq (ind ~ofs:(-varoffset) rsi) rsi ++ (*On se met au bon endroit*)
          (let c = ref(nop) in (*On push le contenu*)
          for i = 0 to (ts/8-1) do
            c := !c ++ pushq (ind ~ofs:(-8*i) rsi)
          done; !c)

        end



    | EOp (expr1, operateur, expr2) ->
      comprec expr1 ++ comprec expr2 ++
      begin
      match operateur with
      | OpDiv   -> popq rbx ++ popq rax ++ cqto ++
                   idivq (reg rbx) ++
                   pushq (reg rax)
      | OpMinus -> popq rbx ++ popq rax ++ subq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpPlus  -> popq rbx ++ popq rax ++ addq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpTimes -> popq rbx ++ popq rax ++ imulq (reg rbx) (reg rax) ++
                   pushq (reg rax)
      | OpRem   -> popq rbx ++ popq rax ++ cqto ++
                   idivq (reg rbx) ++
                   pushq (reg rdx)
      | OpEq    ->
        begin
          match expr1.typ with
          | TRecord i ->
            let ts = type_size decl_env (TRecord i) in
            xorq (reg rcx) (reg rcx) ++ xorq (reg rdx) (reg rdx) ++
            (let c = ref(nop) in (*On push le contenu*)
             for i = 0 to (ts/8-1) do
               c := !c ++ popq rax ++ cmpq (reg rax) (ind ~ofs:(ts-8) rsp) ++
                    sete (reg cl) ++ addq (reg rcx) (reg rdx)
             done; !c) ++ addq (imm ts) (reg rsp) ++
            cmpq (imm (ts/8)) (reg rdx) ++
            sete (reg cl) ++
            pushq (reg rcx)
          | _ -> popq rbx ++ popq rax ++
                 xorq (reg rcx) (reg rcx) ++
                 cmpq (reg rbx) (reg rax) ++
                 sete (reg cl) ++
                 pushq (reg rcx)
        end
      | OpNeq   ->
        begin
          match expr1.typ with
          | TRecord i ->
            let ts = type_size decl_env (TRecord i) in
            xorq (reg rcx) (reg rcx) ++ xorq (reg rdx) (reg rdx) ++
            (let c = ref(nop) in (*On push le contenu*)
             for i = 0 to (ts/8-1) do
               c := !c ++ popq rax ++ cmpq (reg rax) (ind ~ofs:(ts-8) rsp) ++
                    sete (reg cl) ++ addq (reg rcx) (reg rdx)
             done; !c) ++ addq (imm ts) (reg rsp) ++
            cmpq (imm (ts/8)) (reg rdx) ++
            setne (reg cl) ++
            pushq (reg rcx)
          | _ -> popq rbx ++ popq rax ++
                 xorq (reg rcx) (reg rcx) ++
                 cmpq (reg rbx) (reg rax) ++
                 setne (reg cl) ++
                 pushq (reg rcx)
        end
      | OpGt    -> popq rbx ++ popq rax ++ xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setg (reg cl) ++
                   pushq (reg rcx)
      | OpGet   -> popq rbx ++ popq rax ++ xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setge (reg cl) ++
                   pushq (reg rcx)
      | OpLt    -> popq rbx ++ popq rax ++ xorq (reg rcx) (reg rcx) ++
                   cmpq (reg rbx) (reg rax) ++
                   setl (reg cl) ++
                   pushq (reg rcx)
      | OpLet   -> popq rbx ++ popq rax ++ xorq (reg rcx) (reg rcx) ++
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
    | ENew ident -> let x = match texpr.typ with |TAccessRecord i -> i in (*TODO: Penser à free*)
      let ts = type_size decl_env (TRecord x) in
      movl (imm ts) (reg eax) ++
      movl (reg eax) (reg edi) ++
      call "sbrk" ++
      addq (imm (ts-8)) (reg rax) ++
      pushq (reg rax)

    | EEval (ident, lparams) ->
      let ret_typ, params, level = match (Smap.find ident decl_env.vars).typ with
        | TFunction (r,p,l) -> r,p,l
      in
      let rec frame_size = function
        | [] -> 0
        | (_,Some ModeInOut,p)::q -> 8 + frame_size q
        | (_,_,p)::q -> type_size decl_env p + frame_size q
      in
      List.fold_left (*Empilement des paramètres*)
        (fun acc (e,(_,m,_)) -> acc ++ compile_expr (match m with |Some ModeInOut -> true | _ -> false) niveau decl_env e) nop (List.combine lparams params) ++
      movq (reg rbp) (reg rsi) ++
      (print_int niveau; print_string "_";print_int level;print_string "\n";iter (niveau - level) (movq (ind ~ofs:16 rsi) (reg rsi))) ++ (*Empilement du pointeur vers le tableau d'activation de la fonction*)
      pushq (reg rsi) ++
      call (ident^"_"^(string_of_int level)) ++ (*Appel de la procédure et retour dans rax*)
      addq (imm (8+frame_size params)) (reg rsp) ++ (*Dépilage*)
      pushq (reg rax)
    | EChr expr -> comprec expr
  in
  comprec
