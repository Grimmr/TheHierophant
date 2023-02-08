type dummy = A | B | C | D

type nodeDummy = dummy

type basicScalarType = RUNE | BOOL | VALIST | VOID
let string_of_basicScalarType = function 
  | RUNE -> "RUNE"
  | BOOL -> "BOOL"
  | VALIST -> "VALIST"
  | VOID -> "VOID"

type integerSize = S8 | S16 | S32 | S64 | POINTER | SIZE | DEFAULT
let string_of_integerSize = function 
  | S8 -> "S8"
  | S16 -> "S16"
  | S32 -> "S32"
  | S64 -> "S64"
  | POINTER -> "POINTER"
  | SIZE -> "SIZE"
  | DEFAULT -> "DEFAULT"

type floatSize = F32 | F64
let string_of_floatSize = function 
  | F32 -> "F32"
  | F64 -> "F64"

type nodeIdentifier = { name: astNode; tail:astNode option}
and nodeTyp = { const: astNode; error: astNode; storage: astNode}
and nodeStorageClass = { storage: astNode }
and nodeScalarType = { subType: astNode }
and nodeIntegerType = { signed: astNode; size: astNode; numeric: astNode}
and nodeFloatingType = { size: astNode }
and nodePointerType = { nullable: astNode; baseType: astNode }
and nodeStructUnionType = { union: astNode; packed: astNode; fields:astNode }
and nodeStructUnionFields = { field: astNode; tail: astNode option }
and nodeStructUnionField = { unwrap: astNode; offset: astNode option; ident: astNode option; typ: astNode option }
and nodeOffsetSpecifier = { expr: astNode }
and nodeIndirectTypes = { types: astNode }
and nodeTypes = { typ: astNode; tail: astNode option }
and nodeStringConstant = { literal:astNode; tail:astNode option}
and nodeDeclarations = { export:astNode; declaration: astNode; declarations: astNode option}
and nodeDeclaration = { declaration:astNode }
and nodeGlobalDeclaration = { globalBindings: astNode }
and nodeGlobalBindings = { bindings: astNode option; binding: astNode}
and nodeGlobalBinding = { attr: astNode option; ident: astNode; typ: astNode option; expr: astNode option}
and nodeDeclAttr = { threadLocal: astNode; symbol: astNode option }
and nodeConstantDeclaration = { constantBindings: astNode }
and nodeConstantBindings = { bindings: astNode option; binding: astNode }
and nodeConstantBinding = { ident: astNode; typ: astNode; expr:astNode}
and nodeSubUnit = {imports: astNode option; declarations: astNode option}
and nodeImports = {import: astNode; imports: astNode option}
and nodeUseStatement = {alias: astNode option; ident:astNode; members:astNode option; qualify:astNode}
and nodeImportAlias = {name: astNode}
and nodeMemberList = {member: astNode; members: astNode option}
and nodeMember = {alias: astNode option; ident: astNode}



and astNode =  Identifier of nodeIdentifier
            | Name of string
            | Typ of nodeTyp
            | StorageClass of nodeStorageClass
            | ScalarType of nodeScalarType
            | BasicScalarType of basicScalarType 
            | IntegerType of nodeIntegerType
            | IntegerSize of integerSize
            | FloatingType of nodeFloatingType
            | FloatSize of floatSize
            | PointerType of nodePointerType
            | StructUnionType of nodeStructUnionType
            | StructUnionFields of nodeStructUnionFields
            | StructUnionField of nodeStructUnionField
            | OffsetSpecifier of nodeOffsetSpecifier
            | TupleType of nodeIndirectTypes
            | TupleTypes of nodeTypes
            | TaggedUnionType of nodeIndirectTypes
            | TaggedTypes of nodeTypes
            | StringConstant of nodeStringConstant
            | StringLiteral of string
            | Declarations of nodeDeclarations
            | Declaration of nodeDeclaration
            | GlobalDeclaration of nodeGlobalDeclaration
            | GlobalBindings of nodeGlobalBindings
            | GlobalBinding of nodeGlobalBinding
            | DeclAttr of nodeDeclAttr
            | ConstantDeclaration of nodeConstantDeclaration
            | ConstantBindings of nodeConstantBindings
            | ConstantBinding of nodeConstantBinding
            | SubUnit of nodeSubUnit 
            | Imports of nodeImports
            | UseStatement of nodeUseStatement
            | ImportAlias of nodeImportAlias
            | MemberList of nodeMemberList
            | Member of nodeMember
            | Bool of bool
            | Dummy of nodeDummy
            | TypeDeclaration of nodeDummy
            | FunctionDeclaration of nodeDummy
            | Expression of nodeDummy
             
let rec sprint_ast (root:astNode) : string = match root with  
  | Identifier n -> "(Identifier " ^ sprint_ast n.name ^ sprint_ast_o n.tail ^ ")"
  | Name n -> "(NAME " ^ n ^ ")"
  | Typ n -> "(Typ " ^ sprint_ast n.const ^ " " ^ sprint_ast n.error ^ " " ^ sprint_ast n.storage ^ ")"
  | StorageClass n -> "(StorageClass " ^ sprint_ast n.storage ^ ")"
  | ScalarType n -> "(ScalarType " ^ sprint_ast n.subType ^ ")"
  | BasicScalarType n -> "(BasicScalarType " ^ string_of_basicScalarType n ^ ")"
  | IntegerType n -> "(IntegerType " ^ sprint_ast n.signed ^ " " ^ sprint_ast n.size ^ " " ^ sprint_ast n.numeric ^ ")"
  | IntegerSize n -> "(IntegerSize " ^ string_of_integerSize n ^ ")"
  | FloatingType n -> "(FloatType " ^ sprint_ast n.size ^ ")"
  | FloatSize n -> "(FloatSize " ^ string_of_floatSize n ^ ")"
  | PointerType n -> "(PointerType " ^ sprint_ast n.nullable ^ " " ^ sprint_ast n.baseType ^ ")"
  | StructUnionType n -> "(StructUnionType " ^ sprint_ast n.union ^ " " ^ sprint_ast n.packed ^ " " ^ sprint_ast n.fields ^ ")" 
  | StructUnionFields n -> "(StructUnionFields " ^ sprint_ast n.field ^ sprint_ast_o n.tail ^ ")" 
  | StructUnionField n -> "(StructUnionField " ^ sprint_ast n.unwrap ^ sprint_ast_o n.offset ^ sprint_ast_o n.ident ^ sprint_ast_o n.typ ^ ")"
  | OffsetSpecifier n -> "(OffsetSpecifier " ^ sprint_ast n.expr ^ ")"
  | TupleType n -> "(TupleType " ^ sprint_ast n.types ^ ")"
  | TupleTypes n -> "(TupleTypes " ^ sprint_ast n.typ ^ sprint_ast_o n.tail ^ ")"
  | TaggedUnionType n -> "(TaggedUnionType " ^ sprint_ast n.types ^ ")"
  | TaggedTypes n -> "(TaggedTypes " ^ sprint_ast n.typ ^ sprint_ast_o n.tail ^ ")" 
  | StringConstant n -> "(StringConstant " ^ sprint_ast n.literal ^ sprint_ast_o n.tail ^ ")" 
  | StringLiteral n -> "(StringLiteral " ^ n ^ ")"
  | Declarations n -> "(Declarations " ^ sprint_ast n.export ^ " " ^ sprint_ast n.declaration ^ sprint_ast_o n.declarations ^ ")"
  | Declaration n -> "(Declaration " ^ sprint_ast n.declaration ^ ")"
  | GlobalDeclaration n -> "(GlobalDeclaration " ^ sprint_ast n.globalBindings ^ ")"
  | GlobalBindings n -> "(GlobalBindings" ^ sprint_ast_o n.bindings ^ " " ^ sprint_ast n.binding ^ ")"
  | GlobalBinding n -> "(GlobalBinding" ^ sprint_ast_o n.attr ^ " " ^ sprint_ast n.ident ^ sprint_ast_o n.typ ^ sprint_ast_o n.expr ^ ")"
  | DeclAttr n -> "(DeclAttr " ^ sprint_ast n.threadLocal ^ sprint_ast_o n.symbol ^ ")"
  | ConstantDeclaration n -> "(ConstantDeclaration " ^ sprint_ast n.constantBindings ^ ")"
  | ConstantBindings n -> "(ConstantBindings" ^ sprint_ast_o n.bindings ^ " " ^ sprint_ast n.binding ^ ")" 
  | ConstantBinding n -> "(ConstantBinding " ^ sprint_ast n.ident ^ " " ^ sprint_ast n.typ ^ " " ^ sprint_ast n.expr ^ ")"  
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