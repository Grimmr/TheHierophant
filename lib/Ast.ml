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

type arrayMode = SLICE | BOUNDED | CONTEXT | UNBOUNDED
let string_of_arrayMode = function
  | SLICE -> "(SLICE)"
  | BOUNDED -> "(BOUNDED)"
  | CONTEXT -> "(CONTEXT)"
  | UNBOUNDED -> "(UNBOUNDED)"

type fntypeAttr = ANORETURN
let string_of_fntypeAttr = function _ -> "(ANORETURN)"

type parameterMode = NORMAL | HVARIADIC | CVARIADIC
let string_of_parameterMode = function
  | NORMAL -> "(NORMAL)"
  | HVARIADIC -> "(HVARIADIC)"
  | CVARIADIC -> "(CVARIADIC)"

type fndecAttrType = AFINI | AINIT | ATEST 
let string_of_fndecAttrType = function
  | AFINI -> "(AFINI)"
  | AINIT -> "(AINIT)"
  | ATEST -> "(ATEST)"

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
and nodeSliceArrayType = { mode: astNode; expr: astNode option; baseType: astNode }
and nodeFunctionType = { attr: astNode option; prototype: astNode }
and nodePrototype = { parameters: astNode option; return: astNode }
and nodeParameterList = { parameters: astNode }
and nodeParameters = { parameter: astNode; tail:astNode option; mode:astNode option }
and nodeParameter = { name: astNode option; typ:astNode }
and nodeAlias = { ident: astNode; }
and nodeStringConstant = { literal:astNode; tail:astNode option}
and nodeDeclarations = { export:astNode; declaration: astNode; declarations: astNode option}
and nodeDeclaration = { declaration:astNode }
and nodeGlobalDeclaration = { globalBindings: astNode }
and nodeGlobalBindings = { bindings: astNode option; binding: astNode}
and nodeGlobalBinding = { attr: astNode option; ident: astNode; typ: astNode option; expr: astNode option}
and nodeDeclAttr = { threadLocal: astNode; symbol: astNode option }
and nodeConstantDeclaration = { constantBindings: astNode }
and nodeConstantBindings = { bindings: astNode option; binding: astNode }
and nodeConstantBinding = { ident: astNode; typ: astNode; expr:astNode }
and nodeTypeDeclaration = { typeBindings: astNode }
and nodeTypeBindings = { binding: astNode; tail: astNode option }
and nodeTypeBinding = { ident: astNode; typ: astNode }
and nodeEnumType = { storage: astNode option; values: astNode }
and nodeEnumValues = { value: astNode; tail: astNode option }
and nodeEnumValue = { name: astNode; expr: astNode option }
and nodeEnumStorage = { baseType: astNode }
and nodeFunctionDeclaration = { attrs: astNode option; ident: astNode; prototype: astNode; expr: astNode option }
and nodeFndecAttrs = { attr: astNode; attrs: astNode option }
and nodeFndecAttr = { attr: astNode }
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
            | SliceArrayType of nodeSliceArrayType
            | ArrayMode of arrayMode
            | StringType
            | FunctionType of nodeFunctionType
            | Prototype of nodePrototype
            | ParameterList of nodeParameterList
            | Parameters of nodeParameters
            | ParameterMode of parameterMode
            | Parameter of nodeParameter
            | FntypeAttr of fntypeAttr
            | AliasType of nodeAlias
            | UnwrappedAliasType of nodeAlias
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
            | TypeDeclaration of nodeTypeDeclaration
            | TypeBindings of nodeTypeBindings
            | TypeBinding of nodeTypeBinding
            | EnumType of nodeEnumType
            | EnumValues of nodeEnumValues
            | EnumValue of nodeEnumValue
            | EnumStorage of nodeEnumStorage
            | FunctionDeclaration of nodeFunctionDeclaration
            | FndecAttrs of nodeFndecAttrs
            | FndecAttr of nodeFndecAttr
            | FndecAttrType of fndecAttrType
            | SubUnit of nodeSubUnit 
            | Imports of nodeImports
            | UseStatement of nodeUseStatement
            | ImportAlias of nodeImportAlias
            | MemberList of nodeMemberList
            | Member of nodeMember
            | Bool of bool
            | Dummy of nodeDummy
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
  | SliceArrayType n -> "(SliceArrayType " ^ sprint_ast n.mode ^ sprint_ast_o n.expr ^ " " ^ sprint_ast n.baseType ^ ")"
  | ArrayMode n -> "(ArrayMode " ^ string_of_arrayMode n ^ ")"
  | StringType -> "(StringType)"
  | FunctionType n -> "(FunctionType" ^ sprint_ast_o n.attr ^ " " ^ sprint_ast n.prototype ^ ")" 
  | Prototype n -> "(Prototype" ^ sprint_ast_o n.parameters ^ " " ^ sprint_ast n.return ^ ")" 
  | FntypeAttr n -> "(FntypeAttr " ^ string_of_fntypeAttr n ^ ")"
  | AliasType n -> "(AliasType " ^ sprint_ast n.ident ^ ")"
  | UnwrappedAliasType n -> "(UnwrappedAliasType " ^ sprint_ast n.ident ^ ")"
  | ParameterList n -> "(ParameterList " ^ sprint_ast n.parameters ^ ")"
  | Parameters n -> "(Parameters " ^ sprint_ast n.parameter ^ sprint_ast_o n.tail ^ sprint_ast_o n.mode ^ ")"
  | ParameterMode n -> "(ParameterMode " ^ string_of_parameterMode n ^ ")" 
  | Parameter n -> "(Parameter" ^ sprint_ast_o n.name ^ " " ^ sprint_ast n.typ ^ ")"
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
  | TypeDeclaration n -> "(TypeDeclaration " ^ sprint_ast n.typeBindings ^ ")"
  | TypeBindings n -> "(TypeBindings " ^ sprint_ast n.binding ^ sprint_ast_o n.tail ^ ")"
  | TypeBinding n -> "(TypeBinding " ^ sprint_ast n.ident ^ " " ^ sprint_ast n.typ ^ ")"
  | EnumType n -> "(EnumType" ^ sprint_ast_o n.storage ^ " " ^ sprint_ast n.values ^ ")"
  | EnumValues n -> "(EnumValues " ^ sprint_ast n.value ^ sprint_ast_o n.tail ^ ")" 
  | EnumValue n -> "(EnumValue " ^ sprint_ast n.name ^ sprint_ast_o n.expr ^ ")"
  | EnumStorage n -> "(EnumStorage " ^ sprint_ast n.baseType ^ ")"
  | FunctionDeclaration n -> "(FunctionDeclaration" ^ sprint_ast_o n.attrs ^ " " ^ sprint_ast n.ident ^ " " ^ sprint_ast n.prototype ^ sprint_ast_o n.expr ^ ")"
  | FndecAttrs n -> "(FndecAttrs " ^ sprint_ast n.attr ^ sprint_ast_o n.attrs ^ ")"
  | FndecAttr n -> "(FndecAttr " ^ sprint_ast n.attr ^ ")"
  | FndecAttrType n -> "(FndecAttrType " ^ string_of_fndecAttrType n ^ ")"
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