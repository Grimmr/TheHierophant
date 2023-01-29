{
  [@@@coverage exclude_file]
  open Parser
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphaNum = alpha|digit

rule lex_root = parse
    | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | 'a' { A }
    | eof { EOF }

{
  let lex () =
    let lexbuf = Lexing.from_channel stdin in
    lex_root lexbuf
}