open X86_64
open Ast
open Typeur
open Printf
open Compile_expr
open Compile_instr
open Compile_decl
open Compile
open Lexing
open Filename

let compile fenv filepath (_, decls, instrs) =
  let assembly_text =
    compile_decl 0 {decl = DeclProcedure("main",[],decls,instrs);
                    lb = noloc;
                    le = noloc;
                    env = fenv} ++
    label "put_0" ++
    movq (ind ~ofs:16 rsp) (reg rsi) ++
    movq (ilab ".char") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf" ++
    ret ++
    label "new_line_0" ++
    movq (ilab ".ligne") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf" ++
    ret
  in
  let  assembly_data = label ".ligne" ++ string "\n" ++
                       label ".char" ++ string "%c"
  in
  print_in_file ((Filename.chop_extension filepath)^".s") {text = assembly_text; data = assembly_data}
