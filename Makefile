default:
	ocamllex lexer.mll
	menhir parser.mly
	ocamlopt parser.mli parser.ml lexer.ml main.ml

clean:
	rm -f lexer.o main.cmi main.cmx main.o parser.cmi parser.mli parser.o lexer.cmx lexer.cmi a.out parser.cmx