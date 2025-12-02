{
    open Parser

    let charBuffer = Buffer.create 60

    let kwdTbl = 
        ["cases", CASES;
         "else", ELSE;
         "end", END;
         "false", FALSE;
         "for", FOR;
         "from", FROM;
         "fun", FUN;
         "if", IF;
         "lam", LAM;
         "true", TRUE;
         "var", VAR;]

    module StringMap = Map.Make(String)
    let binopMap = StringMap.of_seq @@ List.to_seq  
        ["<", INF;
         ">", SUP;
         "+", ADD;
         "-", SUB;
         "*", MUL;
         "/", DIV;
         "==", EQEQ;
         "<>", DIF;
         "<=", INFEQ;
         ">=", SUPEQ;
         "and", AND;
         "or", OR;
        ]

    let idOrKwd =
        let h = Hashtbl.create 17 in
        List.iter (fun (s,t) -> Hashtbl.add h s t) kwdTbl;
        fun s -> try Hashtbl.find h s with _ -> IDENT s

    exception Lexing_error of string

    let binopError () = raise (Lexing_error "A binop operator must be followed by a space")
}

let space = [' ' '\t']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter ('-'* (letter | digit)+)*
let integer = ('-' | '+')? digit+
let op = ['<' '>' '+' '-' '*' '/'] | "==" | "<>" | "<=" | ">=" | "and" | "or"

rule token = parse
    | space+                        { token lexbuf}  
    | '\n'                          { Lexing.new_line lexbuf; token lexbuf }
    | "#|"                          { comment lexbuf; token lexbuf}
    | eof                           { EOF }
    | '('                           { LEFTPAR }
    | ')'                           { RIGHTPAR }
    | space+ (op as b)               { binop b lexbuf}
    | "block:"                      { BLOCK }
    | ':'                           { COLON }
    | '='                           { EQ }
    | '<'                           { LEFTTYSYMBOL }
    | '>'                           { RIGHTTYSYMBOL }
    | ','                           { COMMA }
    | integer as i                  { CONST (int_of_string i) }
    | ident as i                    { idOrKwd i}
    | '\'' as c | '"' as c          { Buffer.reset charBuffer; string c lexbuf }
    | _ as c                        { raise (Lexing_error ("The caracter " 
                                                            ^ String.make 1 c 
                                                            ^ " is not recognized"))}

and binop b = parse
    | space                         { StringMap.find b binopMap  }  
    | '>'                           { if b = "-" then ARROW else binopError ()}
    | _                             { if b = "<" then LEFTTYSYMBOL else binopError ()} 

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
