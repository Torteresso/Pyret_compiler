Dependencies
==========

To install dependencies using opam simply run :

```
make dependencies
```

What I'm proud of
==================

- The version management with git, see [project on github](https://github.com/Torteresso/Pyret_compiler)
- Discovering functional programming and enjoying it (I did not knew Ocaml in September).
- Passing all the tests provided.

Extras and key points in the code
=======

- Added the fact that only one statement is allowed per line to parser.
- Strings in lexer are handle using the Buffer module.
- Space management in lexer is handle partly by using the Queue module.
- Most of the debugging use the [ppx_deriving librairy](https://github.com/ocaml-ppx/ppx_deriving), explaining all the `[@@deriving show]` in [ast.ml file](https://github.com/Torteresso/Pyret_compiler/blob/master/ast.ml).
- The type checking of "cases" expressions is a bit more general that the one requested by the subject (not necessary on Lists and with an arbritary number of branches).
- The type checking of function calls also work when chained like in this example :
```
fun g(x :: Number, y :: Number) -> (Number -> Number):
  lam(z :: Number) -> Number:
    x + y + z
  end
end

g(1, 2)(3) 
```
- The type checking process fills the abstract syntax tree with type information for every expressions and statements.
- Error messages give some extra details about the issue most of the time.

Difficulties
============

- One shift/reduce conflict persists in the parser, could not figure out how to resolve it with priorities. Note : the parser still gives the good result with the arbitrary resolution of menhir.
- I was a bit short on time and I did not yet have time to refactor the code to be more readable so the typer may be a bit complex to read.
