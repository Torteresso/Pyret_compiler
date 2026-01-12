all:
	dune build
	@mv pyretc.exe pyretc

clean:
	dune clean

test: all
	./pyretc -v mainTest.arr
	@echo "From Pyret : "
	pyret -q -e none mainTest.arr
	@echo "From MY COMPILER : "	
	gcc -no-pie mainTest.s -o mainTest
	./mainTest
	

dependencies:
	opam install menhir dune ppx_deriving ocaml 
