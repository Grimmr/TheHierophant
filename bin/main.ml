
let l = Lexing.from_channel stdin

;; let root = TheHierophant.Parser.subUnit TheHierophant.Lexer.lex_root l in 
    let importList = match root.imports with
        | Some i -> i
        | None -> {import={alias=None; ident="nil"; members=None; qualify=false}; imports=None} in
            print_string importList.import.ident