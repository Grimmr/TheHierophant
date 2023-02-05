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

let escape = '\n'
let stringChar = [^ '"' '\\'] | escape
let hstring = '"' stringChar* '"'
let rawstringChar = [^ '`']
let rawstring =  '`' rawstringChar* '`'
let stringLit = hstring | rawstring

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
    | "export" { EXPORT }
    | "let" { LET }
    | "const" { CONST }
    | "@symbol" { ASYMBOL }
    | "@threadlocal" { ATHREADLOCAL }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "def" { DEF }
    | '!' { BANG }
    | "DUMMY" { DUMMY } (*PLACEHOLDER MUST REMOVE*)
    | stringLit { STRINGLIT (let t=Lexing.lexeme lexbuf in String.sub t 1 ((String.length t) - 2)) }
    | name { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}