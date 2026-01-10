all:
	dune build
	@mv pyretc.exe pyretc

clean:
	dune clean

test: all
	./pyretc mainTest.arr
	@echo "From MY COMPILER : "	
	gcc -no-pie mainTest.s -o mainTest
	./mainTest
	@echo "From Pyret : "
	pyret -q -e none mainTest.arr
	

dependencies:
	opam install menhir dune ppx_deriving ocaml 
