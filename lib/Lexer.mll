{
  [@@@coverage exclude_file]
  open Parser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphaNum = alpha|digit
let name = alpha alphaNum*

rule lex_root = parse
    | [' ' '\t' '\n'] { lex_root lexbuf }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ';' { SEMI }
    | ':' { COLON }
    | "::" { DCOLON }
    | ":: " { SDCOLON }
    | " ::" { SDCOLON }
    | " :: " { SDCOLON }
    | '*' { STAR }
    | "use" { USE }
    | '=' { EQ }
    | ',' { COMA }
    | 'a' { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}