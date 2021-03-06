all: adac.ml ast.ml lexer.mll parser.mly typeur_decl.ml typeur_expr.ml typeur_instr.ml typeur_programme.ml typeur.ml compile_expr.ml compile_decl.ml compile_instr.ml
	ocamlbuild adac.byte -use-menhir;mv adac.byte adac

debug: adac.ml ast.ml lexer.mll parser.mly typeur_decl.ml typeur_expr.ml typeur_instr.ml typeur_programme.ml typeur.ml compile_expr.ml compile_decl.ml compile_instr.ml
	ocamlbuild adac.byte -use-menhir -ocamlopt "ocamlopt -g" -cflag -g;mv adac.byte adac

dot: adac_dot.ml ast_printer.ml ast.ml lexer.mll parser.mly typeur_decl.ml typeur_expr.ml typeur_instr.ml typeur_programme.ml typeur.ml compile_expr.ml compile_decl.ml compile_instr.ml
	ocamlbuild adac_dot.byte -use-menhir -use-ocamlfind -pkg dot;mv adac_dot.byte adac

clean:
	rm -rf _build/; rm -f adac
