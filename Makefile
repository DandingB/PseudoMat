default:
	dune build
	dune exec ./Main.exe test.ps


clean:
	dune clean
	rm -f lexer.o main.cmi main.cmx main.o parser.cmi parser.mli parser.o lexer.cmx lexer.cmi a.out parser.cmx lexer.ml parser.ml