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
    | "rune" { RUNE }
    | "bool" { BOOL }
    | "valist" { VALIST }
    | "void" { VOID }
    | "i8" { I8 }
    | "i16" { I16 }
    | "i32" { I32 }
    | "i64" { I64 }
    | "u8" { U8 }
    | "u16" { U16 }
    | "u32" { U32 }
    | "u64" { U64 }
    | "int" { INT }
    | "uint" { UINT }
    | "size" { SIZE }
    | "uintptr" { UINTPTR }
    | "char" { CHAR }
    | "f32" { F32 }
    | "f64" { F64 }
    | "nullable" { NULLABLE }
    | "struct" { STRUCT }
    | "union" { UNION }
    | "@offset" { AOFFSET }
    | "@packed" { APACKED }
    | '|' { PIPE }
    | '[' { LBRACKET }
    | ']' { RBRACKET }
    | '_' { LBAR }
    | "str" { STR }
    | "fn" { FN }
    | "@noreturn" { ANORETURN }
    | "..." { DOTS }
    | "enum" { ENUM }
    | "type" { TYPE }
    | "@fini" { AFINI }
    | "@init" { AINIT }
    | "@test" { ATEST }
    | "+=" { PEQ }
    | "-=" { MEQ }
    | "*=" { SEQ }
    | "/=" { DEQ }
    | "%=" { PEREQ }
    | "<<=" { LEQ }
    | ">>=" { REQ }
    | "&=" { LAEQ }
    | "|=" { LOEQ }
    | "^=" { LHEQ }
    | "&&=" { AEQ }
    | "||=" { OEQ }
    | "^^=" { HEQ }
    | "&&" { LAND }
    | "^^" { LHAT }
    | "||" { LOR }
    | "<" { LT }
    | ">" { GT }
    | "<=" { LTE }
    | ">=" { GTE }
    | "==" { EQUIV }
    | "!=" { BANGEQUIV }
    | "&" { AMPERSAND }
    | "^" { HAT }
    | "DUMMY" { DUMMY } (*PLACEHOLDER MUST REMOVE*)
    | stringLit { STRINGLIT (let t=Lexing.lexeme lexbuf in String.sub t 1 ((String.length t) - 2)) }
    | name { NAME (Lexing.lexeme lexbuf) }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}