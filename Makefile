all: adac.ml ast.ml lexer.mll parser.mly typeur_decl.ml typeur_expr.ml typeur_instr.ml typeur_programme.ml typeur.ml
	ocamlbuild adac.byte -use-menhir;mv adac.byte adac

dot: adac_dot.ml ast_printer.ml ast.ml lexer.mll parser.mly typeur_decl.ml typeur_expr.ml typeur_instr.ml typeur_programme.ml typeur.ml
	ocamlbuild adac_dot.byte -use-menhir -use-ocamlfind -pkg dot;mv adac_dot.byte adac

clean:
	rm -r _build/; rm adac
