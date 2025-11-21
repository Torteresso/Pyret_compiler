{
    open Parser

    exception Lexing_error of char
}

rule token = parse
    | [' ' '\t' '\n']+          {token lexbuf}
    | eof                       { EOF }
