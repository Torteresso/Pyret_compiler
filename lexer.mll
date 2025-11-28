{
    open Parser

    exception Lexing_error of char
}

let space = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter ('-'* (letter | digit)+)*
let integer = ('-' | '+')? digit+

rule token = parse
    | space+                        {token lexbuf}  
    | eof                           { EOF }
    | integer as i                  { CONST (int_of_string i) }
    | _ as c                        { raise (Lexing_error c)}
