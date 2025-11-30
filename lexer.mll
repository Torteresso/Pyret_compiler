{
    open Parser

    let charBuffer = Buffer.create 60

    let kwd_tbl = 
        ["and", AND;
         "block", BLOCK;
         "cases", CASES;
         "else", ELSE;
         "end", END;
         "false", FALSE;
         "for", FOR;
         "from", FROM;
         "fun", FUN;
         "if", IF;
         "lam", LAM;
         "or", OR;
         "true", TRUE;
         "var", VAR;]
    

    let id_or_kwd =
        let h = Hashtbl.create 17 in
        List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
        fun s ->
        let s = String.lowercase_ascii s in (* la casse n'est pas significative *)
        try Hashtbl.find h s with _ -> IDENT s

    exception Lexing_error of string
}

let space = [' ' '\t']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter ('-'* (letter | digit)+)*
let integer = ('-' | '+')? digit+

rule token = parse
    | space+                        { token lexbuf}  
    | '\n'                          { Lexing.new_line lexbuf; token lexbuf }
    | "#|"                          { comment lexbuf; token lexbuf}
    | eof                           { EOF }
    | '<'                           { INF }
    | '>'                           { SUP }
    | '+'                           { ADD }
    | '-'                           { SUB }
    | '*'                           { MUL }
    | '/'                           { DIV }
    | '='                           { EQ }
    | integer as i                  { CONST (int_of_string i) }
    | ident as i                    { id_or_kwd i}
    | '\'' as c | '"' as c          { Buffer.reset charBuffer; string c lexbuf }
    | _ as c                        { raise (Lexing_error ("The caracter " 
                                                            ^ String.make 1 c 
                                                            ^ " is not recognized"))}

and comment = parse 
    | "|#"                          { () }
    | "#|"                          { comment lexbuf; comment lexbuf}
    | '\n'                          { Lexing.new_line lexbuf; comment lexbuf }
    | _                             { comment lexbuf }
    | eof                           { raise (Lexing_error "Unterminated comment") }

and string closingChar = parse 
    | '\'' as c | '"' as c          { if closingChar = c 
                                      then STRING (Buffer.contents charBuffer) 
                                      else (Buffer.add_char charBuffer c;
                                            string closingChar lexbuf) } 
    | '\\' 't'                      { Buffer.add_char charBuffer '\t'; 
                                      string closingChar lexbuf}

    | '\\' '\''                      { Buffer.add_char charBuffer '\''; 
                                      string closingChar lexbuf}
    | '\\' '"'                      { Buffer.add_char charBuffer '"'; 
                                      string closingChar lexbuf}
    | '\\' '\\'                      { Buffer.add_char charBuffer '\\'; 
                                      string closingChar lexbuf}
    | '\n'                          { raise (Lexing_error "String must be defined in only one line")}
    | '\\' 'n'                      { Buffer.add_char charBuffer '\n'; 
                                      Lexing.new_line lexbuf;
                                      string closingChar lexbuf}
    | _ as c                        { Buffer.add_char charBuffer c;
                                      string closingChar lexbuf}
    | eof                           { raise (Lexing_error "Unterminated string")}
