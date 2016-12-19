open X86_64
open Ast

let rec iter n code =
  if n = 0 then nop else code ++ iter (n - 1) code


let c = ref(0) 
let uuid () = c := !c+1; !c

let get_var_offset decl_env x =
  (Smap.find x decl_env.vars).offset
