
let l = Lexing.from_channel stdin

;; let root = TheHierophant.Parser.subUnit TheHierophant.Lexer.lex_root l in 
    print_string (TheHierophant.Ast.sprint_ast (SubUnit root))