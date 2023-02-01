type dummy = A | B | C | D

type nodeDummy = dummy

type nodeIdentifier = { name: astNode; tail:astNode option}
and nodeDeclarations = { export:astNode; declaration: astNode; declarations: astNode option}
and nodeSubUnit = {imports: astNode option; declarations: astNode option}
and nodeImports = {import: astNode; imports: astNode option}
and nodeUseStatement = {alias: astNode option; ident:astNode; members:astNode option; qualify:astNode}
and nodeImportAlias = {name: astNode}
and nodeMemberList = {member: astNode; members: astNode option}
and nodeMember = {alias: astNode option; ident: astNode}



and astNode =  Identifier of nodeIdentifier
             | Name of string
             | Declarations of nodeDeclarations
             | SubUnit of nodeSubUnit 
             | Imports of nodeImports
             | UseStatement of nodeUseStatement
             | ImportAlias of nodeImportAlias
             | MemberList of nodeMemberList
             | Member of nodeMember
             | Bool of bool
             | Declaration of nodeDummy
             | Dummy of nodeDummy
             
let rec sprint_ast (root:astNode) : string = match root with  
  | Identifier n -> "(Name " ^ sprint_ast n.name ^ sprint_ast_o n.tail ^ ")"
  | Name n -> n
  | Declarations n -> "(Declarations " ^ sprint_ast n.export ^ " " ^ sprint_ast n.declaration ^ " " ^ sprint_ast_o n.declarations ^ ")"
  | SubUnit n -> "(SubUnit" ^ sprint_ast_o n.imports ^ sprint_ast_o n.declarations ^ ")"
  | Imports n -> "(Imports " ^ sprint_ast n.import ^ sprint_ast_o n.imports ^ ")"
  | UseStatement n -> "(UseStatement" ^ sprint_ast_o n.alias ^ " " ^ sprint_ast n.ident ^ sprint_ast_o n.members ^ " " ^ sprint_ast n.qualify ^ ")"
  | ImportAlias n -> "(ImportAlias " ^ sprint_ast n.name ^ ")"
  | MemberList n -> "(MemberList " ^ sprint_ast n.member ^ sprint_ast_o n.members ^ ")" 
  | Member n -> "(Member" ^ sprint_ast_o n.alias ^ " " ^ sprint_ast n.ident ^ ")"
  | Bool n -> "(Bool " ^ string_of_bool(n) ^ ")"
  | _ -> "(NO-PRINT)"

and sprint_ast_o (o: astNode option) : string = match o with
  | Some t -> " " ^ sprint_ast t
  | None -> " (NONE)"