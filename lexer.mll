{
    open Parser

    let charBuffer = Buffer.create 60

    let kwdTbl = 
        ["cases", [CASES];
         "else", [ELSE];
         "end", [END];
         "false", [FALSE];
         "for", [FOR];
         "from", [FROM];
         "fun", [FUN];
         "if", [IF];
         "lam", [LAM];
         "true", [TRUE];
         "var", [VAR];]

    module StringMap = Map.Make(String)
    let binopMap = StringMap.of_seq @@ List.to_seq  
        ["<", [INF];
         "+", [ADD];
         "-", [SUB];
         "*", [MUL];
         "/", [DIV];
         "==", [EQEQ];
         "<>", [DIF];
         "<=", [INFEQ];
         ">=", [SUPEQ];
         "and", [AND];
         "or", [OR];
        ]

    let idOrKwd =
        let h = Hashtbl.create 17 in
        List.iter (fun (s,t) -> Hashtbl.add h s t) kwdTbl;
        fun s -> try Hashtbl.find h s with _ -> [IDENT s]

    exception Lexing_error of string

    let binopError () = raise (Lexing_error "A binop operator must be followed by a space")

    let correctedIdent i = 
        if (Buffer.length charBuffer) = 0
        then i 
        else (let firstLetter = (Buffer.contents charBuffer) in
        Buffer.reset charBuffer; firstLetter ^ i)
}

let space = [' ' '\t']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter ('-'* (letter | digit)+)*
let integer = ('-' | '+')? digit+
let op = ['<' '+' '-' '*' '/'] | "==" | "<>" | "<=" | ">=" | "and" | "or"

rule token = parse
    | space+                        { token lexbuf}  
    | '\n'                          { Lexing.new_line lexbuf; token lexbuf }
    | "#|"                          { comment lexbuf; token lexbuf}
    | '#'[^'|'][^'\n']*'\n'         { Lexing.new_line lexbuf; token lexbuf }
    | '#'[^'|'][^ '\n']*eof
    | eof                           { [EOF] }
    | space+ '('                    { [SPACELEFTPAR] }
    | '('                           { [LEFTPAR] }
    | ')'                           { [RIGHTPAR] }
    | space+ (op as b)              { binop b lexbuf}
    | space+ '<'                    { sup lexbuf }
    | "block:"                      { [BLOCK] }
    | "else:"                       { [ELSECOLON] }
    | ':'                           { [COLON] }
    | '='                           { [EQ] }
    | '<'                           { [LEFTTYSYMBOL] }
    | '>'                           { [RIGHTTYSYMBOL] }
    | ','                           { [COMMA] }
    | '|'                           { [PIPE] }
    | integer as i                  { [CONST (int_of_string i)] }
    | ident as i                    { idOrKwd i}
    | '\'' as c | '"' as c          { string c lexbuf }
    | _ as c                        { raise (Lexing_error ("The caracter " 
                                                            ^ String.make 1 c 
                                                            ^ " is not recognized"))}

and binop b = parse
    | space                         { StringMap.find b binopMap  }  
    | '>'                           { if b = "-" then [ARROW] 
                                      else binopError ()}
    | _ as c                        { if b = "<" then
                                      ([LEFTTYSYMBOL] @ unterminatedIndent c lexbuf)
                                      else binopError ()} 

and sup = parse
    | space                         { [SUP] }
    | '('                           { [RIGHTTYSYMBOL; LEFTPAR] }
    | '='                           { [RIGHTTYSYMBOL; EQ] }
    | "block:"                      { [RIGHTTYSYMBOL; BLOCK] }
    | ':'                           { [RIGHTTYSYMBOL; COLON] }
    | ','                           { [RIGHTTYSYMBOL; COMMA] }
    | ')'                           { [RIGHTTYSYMBOL; RIGHTPAR] }
    | "from"                        { [RIGHTTYSYMBOL; FROM] }

and unterminatedIndent firstLetter = parse
    | ('-'* (letter | digit)+)* as i { idOrKwd ((String.make 1 firstLetter) ^ i) }
    | '>'                            { [IDENT (String.make 1 firstLetter); RIGHTTYSYMBOL] }
    | ','                            { [IDENT (String.make 1 firstLetter); COMMA] }


and comment = parse 
    | "|#"                          { () }
    | "#|"                          { comment lexbuf; comment lexbuf}
    | '\n'                          { Lexing.new_line lexbuf; comment lexbuf }
    | _                             { comment lexbuf }
    | eof                           { raise (Lexing_error "Unterminated comment") }

and string closingChar = parse 
    | '\'' as c | '"' as c          { if closingChar = c 
                                      then (let s = (Buffer.contents charBuffer)
                                      in (Buffer.reset charBuffer;
                                      [STRING s]))
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
    | '\\' _ as c                   { raise (Lexing_error ("\"" ^ c ^ 
                                             "\" is undefined in a string"))}
    | _ as c                        { Buffer.add_char charBuffer c;
                                      string closingChar lexbuf}
    | eof                           { raise (Lexing_error "Unterminated string")}


{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = token lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}
