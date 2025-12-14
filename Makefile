all:
	dune build
	mv pyretc.exe pyretc

clean:
	dune clean

dependencies:
	opam install menhir dune ppx_deriving ocaml 
