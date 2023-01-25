{
  open Printf
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphaNum = alpha|digit

rule lex_root = parse
    | '\n' { printf "NL "; lex_root lexbuf }
    | 'a' { printf "A "; lex_root lexbuf }
    | eof { () }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}