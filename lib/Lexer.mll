{
  [@@@coverage exclude_file]
  open Parser
}

(* Define helper regexes *)
let whiteSpace = [' ' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphaNum = alpha|digit
let name = alpha alphaNum*

rule lex_root = parse
    | whiteSpace { lex_root lexbuf }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ';' { SEMI }
    | ':' { COLON }
    | "::" { DCOLON }
    | "::" whiteSpace* '*' { DCOLONS }
    | "::" whiteSpace* '{' { DCOLONB }
    | '*' { STAR }
    | "use" { USE }
    | '=' { EQ }
    | ',' { COMA }
    | "EXPORT" { EXPORT }
    | "DUMMY" { DUMMY } (*PLACEHOLDER MUST REMOVE*)
    | name { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}