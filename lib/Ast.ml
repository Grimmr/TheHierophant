type dummy = A | B | C | D

type nodeDummy = dummy

type nodeIdentifier = { name: astNode; tail:astNode option}
and nodeStringConstant = { literal:astNode; tail:astNode option}
and nodeDeclarations = { export:astNode; declaration: astNode; declarations: astNode option}
and nodeDeclaration = { declaration:astNode }
and nodeGlobalDeclaration = { bindings: astNode }
and nodeGlobalBindings = { bindings: astNode option; binding: astNode}
and nodeGlobalBinding = { attr: astNode option; ident: astNode; typ: astNode option; expr: astNode option}
and nodeDeclAttr = { threadLocal: astNode; symbol: astNode option }
and nodeSubUnit = {imports: astNode option; declarations: astNode option}
and nodeImports = {import: astNode; imports: astNode option}
and nodeUseStatement = {alias: astNode option; ident:astNode; members:astNode option; qualify:astNode}
and nodeImportAlias = {name: astNode}
and nodeMemberList = {member: astNode; members: astNode option}
and nodeMember = {alias: astNode option; ident: astNode}



and astNode =  Identifier of nodeIdentifier
             | Name of string
             | StringConstant of nodeStringConstant
             | StringLiteral of string
             | Declarations of nodeDeclarations
             | Declaration of nodeDeclaration
             | GlobalDeclaration of nodeGlobalDeclaration
             | GlobalBindings of nodeGlobalBindings
             | GlobalBinding of nodeGlobalBinding
             | DeclAttr of nodeDeclAttr
             | SubUnit of nodeSubUnit 
             | Imports of nodeImports
             | UseStatement of nodeUseStatement
             | ImportAlias of nodeImportAlias
             | MemberList of nodeMemberList
             | Member of nodeMember
             | Bool of bool
             | Dummy of nodeDummy
             | ConstantDeclaration of nodeDummy
             | TypeDeclaration of nodeDummy
             | FunctionDeclaration of nodeDummy
             | Expression of nodeDummy
             | Typ of nodeDummy
             
let rec sprint_ast (root:astNode) : string = match root with  
  | Identifier n -> "(Identifier " ^ sprint_ast n.name ^ sprint_ast_o n.tail ^ ")"
  | Name n -> "(NAME " ^ n ^ ")"
  | StringConstant n -> "(StringConstant " ^ sprint_ast n.literal ^ sprint_ast_o n.tail ^ ")" 
  | StringLiteral n -> "(StringLiteral " ^ n ^ ")"
  | Declarations n -> "(Declarations " ^ sprint_ast n.export ^ " " ^ sprint_ast n.declaration ^ " " ^ sprint_ast_o n.declarations ^ ")"
  | Declaration n -> "(Declaration " ^ sprint_ast n.declaration ^ ")"
  | GlobalDeclaration n -> "(GlobalDeclaration " ^ sprint_ast n.bindings ^ ")"
  | GlobalBindings n -> "(GlobalBindings" ^ sprint_ast_o n.bindings ^ " " ^ sprint_ast n.binding ^ ")"
  | GlobalBinding n -> "(GlobalBinding" ^ sprint_ast_o n.attr ^ " " ^ sprint_ast n.ident ^ sprint_ast_o n.typ ^ sprint_ast_o n.expr ^ ")"
  | DeclAttr n -> "(DeclAttr " ^ sprint_ast n.threadLocal ^ sprint_ast_o n.symbol ^ ")"
  | SubUnit n -> "(SubUnit" ^ sprint_ast_o n.imports ^ sprint_ast_o n.declarations ^ ")"
  | Imports n -> "(Imports " ^ sprint_ast n.import ^ sprint_ast_o n.imports ^ ")"
  | UseStatement n -> "(UseStatement" ^ sprint_ast_o n.alias ^ " " ^ sprint_ast n.ident ^ sprint_ast_o n.members ^ " " ^ sprint_ast n.qualify ^ ")"
  | ImportAlias n -> "(ImportAlias " ^ sprint_ast n.name ^ ")"
  | MemberList n -> "(MemberList " ^ sprint_ast n.member ^ sprint_ast_o n.members ^ ")" 
  | Member n -> "(Member" ^ sprint_ast_o n.alias ^ " " ^ sprint_ast n.ident ^ ")"
  | Bool n -> "(BOOL " ^ string_of_bool(n) ^ ")"
  | _ -> "(NO-PRINT)"

and sprint_ast_o (o: astNode option) : string = match o with
  | Some t -> " " ^ sprint_ast t
  | None -> " (NONE)"