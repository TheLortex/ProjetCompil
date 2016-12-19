open X86_64
open Ast
open Typeur
open Printf
open Char

let compile_expr niveau decl_env =
  let rec comprec = function
    | EInt i ->  movq (imm i) (reg rax) ++ pushq (reg rax)
    | EChar c -> movq (imm (Char.code c)) (reg rax) ++ pushq (reg rax)
    | ETrue -> movq (imm 1) (reg rax) ++ pushq (reg rax)
    | EFalse -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | ENull -> movq (imm 0) (reg rax) ++ pushq (reg rax)
    | EAccess (None, ident) -> nop
    | EAccess (Some expr, ident) -> nop
    | EOp (expr1, operateur, expr2) -> nop
    | ENot expr -> nop
    | EMinus expr -> nop
    | ENew ident -> nop
    | EEval (ident, lparams) -> nop
    | EChr expr -> nop
  in
  comprec
