{
  [@@@coverage exclude_file]
  open Parser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphaNum = alpha|digit

rule lex_root = parse
    | [' ' '\t' '\n'] { lex_root lexbuf }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ';' { SEMI }
    | ':' { COLON }
    | '*' { STAR }
    | "use" { USE }
    | '=' { EQ }
    | ',' { COMA }
    | alpha alphaNum* { DSTR (Lexing.lexeme lexbuf) }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}