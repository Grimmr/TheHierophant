%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 

%token EOF USE SEMI STAR COLON DCOLON DCOLONB DCOLONS LBRACE RBRACE EQ COMA EXPORT LET CONST ASYMBOL ATHREADLOCAL LPAREN RPAREN DEF BANG BOOL RUNE VALIST VOID I8 I16 I32 I64 U8 U16 U32 U64 INT UINT SIZE UINTPTR CHAR F32 F64 NULLABLE STRUCT UNION AOFFSET APACKED PIPE LBRACKET RBRACKET LBAR STR FN ANORETURN DOTS ENUM TYPE AINIT AFINI ATEST PEQ MEQ SEQ DEQ PEREQ LEQ REQ LAEQ LOEQ LHEQ AEQ OEQ HEQ LAND LHAT LOR
%token <string> NAME STRINGLIT

%start subUnit

%type <nodeIdentifier> identifier
%type <nodeTyp> typ
%type <nodeStorageClass> storageClass
%type <nodeStringConstant> stringConstant
%type <nodeDeclarations> declarations
%type <nodeDeclaration> declaration
%type <nodeGlobalDeclaration> globalDeclaration
%type <nodeGlobalBindings> globalBindings 
%type <nodeGlobalBinding> globalBinding
%type <nodeDeclAttr> declAttr
%type <nodeConstantDeclaration> constantDeclaration
%type <nodeConstantBindings> constantBindings
%type <nodeConstantBinding> constantBinding
%type <nodeSubUnit> subUnit
%type <nodeImports> imports
%type <nodeUseStatement> useStatement
%type <nodeImportAlias> importAlias
%type <nodeMemberList> memberList
%type <nodeMember> member


%%
//6.4 Identifiers
identifier: n=NAME                   { {name=Name n; tail=None} }
          | n=NAME DCOLON i=identifier { {name=Name n; tail=Some (Identifier i)} }

//6.5 Types
typ: CONST; BANG; c=storageClass; { {const=Bool true; error=Bool true; storage=StorageClass c} }
   | CONST; c=storageClass;       { {const=Bool true; error=Bool false; storage=StorageClass c} }
   | BANG; c=storageClass;        { {const=Bool false; error=Bool true; storage=StorageClass c} }
   | c=storageClass;              { {const=Bool false; error=Bool false; storage=StorageClass c} }
storageClass: t=scalarType;         { {storage=ScalarType t} }
            | t=structUnionType;    { {storage=StructUnionType t} }
            | t=tupleType;          { {storage=TupleType t} }
            | t=taggedUnionType;    { {storage=TaggedUnionType t} }
            | t=sliceArrayType;     { {storage=SliceArrayType t} }
            | t=functionType;       { {storage=FunctionType t} }
            | t=aliasType;          { {storage=AliasType t} }
            | t=unwrappedAliasType; { {storage=UnwrappedAliasType t} }
            | t=stringType;         { {storage=StringType} }
scalarType: t=integerType;  { {subType=IntegerType t} }
          | t=floatingType; { {subType=FloatingType t} }
          | t=pointerType;  { {subType=PointerType t} }
          | RUNE;           { {subType=BasicScalarType RUNE} }
          | BOOL;           { {subType=BasicScalarType BOOL} }
          | VALIST;         { {subType=BasicScalarType VALIST} }
          | VOID;           { {subType=BasicScalarType VOID} }
integerType: I8;      { {signed=Bool true; size=IntegerSize S8; numeric=Bool true} }
           | I16;     { {signed=Bool true; size=IntegerSize S16; numeric=Bool true} }
           | I32;     { {signed=Bool true; size=IntegerSize S32; numeric=Bool true} }
           | I64;     { {signed=Bool true; size=IntegerSize S64; numeric=Bool true} }
           | U8;      { {signed=Bool false; size=IntegerSize S8; numeric=Bool true} }
           | U16;     { {signed=Bool false; size=IntegerSize S16; numeric=Bool true} }
           | U32;     { {signed=Bool false; size=IntegerSize S32; numeric=Bool true} }
           | U64;     { {signed=Bool false; size=IntegerSize S64; numeric=Bool true} }
           | INT;     { {signed=Bool true; size=IntegerSize DEFAULT; numeric=Bool true} }
           | UINT;    { {signed=Bool false; size=IntegerSize DEFAULT; numeric=Bool true} }
           | SIZE;    { {signed=Bool false; size=IntegerSize SIZE; numeric=Bool true} }
           | UINTPTR; { {signed=Bool false; size=IntegerSize POINTER; numeric=Bool true} }
           | CHAR;    { {signed=Bool false; size=IntegerSize S8; numeric=Bool false} }
floatingType: F32; { {size=FloatSize F32} }
            | F64; { {size=FloatSize F64} }
pointerType: STAR; t=typ;          { {nullable=Bool false; baseType=Typ t} }
           | NULLABLE; STAR; t=typ { {nullable=Bool true; baseType=Typ t} }
structUnionType: STRUCT; APACKED; LBRACE; f=structUnionFields RBRACE; { {union=Bool false; packed=Bool true; fields=StructUnionFields f} }
               | STRUCT; LBRACE; f=structUnionFields; RBRACE;         { {union=Bool false; packed=Bool false; fields=StructUnionFields f} }
               | UNION; LBRACE; f=structUnionFields; RBRACE;          { {union=Bool true; packed=Bool false; fields=StructUnionFields f} }
structUnionFields: f=structUnionField; COMA;                     { {field=StructUnionField f; tail=None} }
                 | f=structUnionField;                           { {field=StructUnionField f; tail=None} }
                 | f=structUnionField; COMA; t=structUnionFields { {field=StructUnionField f; tail=Some (StructUnionFields t)} }
structUnionField: o=offsetSpecifier; n=NAME; COLON; t=typ; { {unwrap=Bool false; offset=Some (OffsetSpecifier o); ident=Some (Name n); typ=Some (Typ t)} }
                | n=NAME; COLON; t=typ;                    { {unwrap=Bool false; offset=None; ident=Some (Name n); typ=Some (Typ t)} }
                | o=offsetSpecifier; t=structUnionType;    { {unwrap=Bool true; offset=Some (OffsetSpecifier o); ident=None; typ=Some (StructUnionType t)} }
                | t=structUnionType;                       { {unwrap=Bool true; offset=None; ident=None; typ=Some (StructUnionType t)} }
                | o=offsetSpecifier; i=identifier;         { {unwrap=Bool true; offset=Some (OffsetSpecifier o); ident=Some (Identifier i); typ=None} }
                | i=identifier                             { {unwrap=Bool true; offset=None; ident=Some (Identifier i); typ=None} }
offsetSpecifier: AOFFSET; LPAREN; e=expression; RPAREN; { {expr=Expression e} }
tupleType: LPAREN; t=tupleTypes; RPAREN; { {types=TupleTypes t} }
tupleTypes: ta=typ; COMA; tb=typ; COMA; { {typ=Typ ta; tail=Some (TupleTypes {typ=Typ tb; tail=None})} }
          | ta=typ; COMA; tb=typ;       { {typ=Typ ta; tail=Some (TupleTypes {typ=Typ tb; tail=None})} }
          | ta=typ; COMA; l=tupleTypes; { {typ=Typ ta; tail=Some (TupleTypes l)} }
taggedUnionType: LPAREN; t=taggedTypes; RPAREN { {types=TaggedTypes t} }
taggedTypes: ta=typ; PIPE; tb=typ; PIPE  { {typ=Typ ta; tail=Some (TaggedTypes {typ=Typ tb; tail=None})} }
           | ta=typ; PIPE; tb=typ;       { {typ=Typ ta; tail=Some (TaggedTypes {typ=Typ tb; tail=None})} }
           | t=typ; PIPE;  l=taggedTypes { {typ=Typ t; tail=Some (TaggedTypes l)} }
sliceArrayType: LBRACKET; RBRACKET; t=typ;               { {mode=ArrayMode SLICE; expr=None; baseType=Typ t} }
              | LBRACKET; e=expression; RBRACKET; t=typ; { {mode=ArrayMode BOUNDED; expr=Some (Expression e); baseType=Typ t} }
              | LBRACKET; STAR; RBRACKET; t=typ;         { {mode=ArrayMode UNBOUNDED; expr=None; baseType=Typ t} }
              | LBRACKET; LBAR; RBRACKET; t=typ;         { {mode=ArrayMode CONTEXT; expr=None; baseType=Typ t} }
stringType: STR; {}
functionType: a=fntypeAttr; FN; p=prototype; { {attr=Some (FntypeAttr a); prototype=Prototype p} }
            | FN; p=prototype;               { {attr=None; prototype=Prototype p} }
prototype: LPAREN; p=parameterList; RPAREN t=typ; { {parameters=Some (ParameterList p); return=Typ t} }
         | LPAREN; RPAREN; t=typ;                 { {parameters=None; return=Typ t} }
fntypeAttr: ANORETURN; { ANORETURN }
parameterList: p=parameters; { {parameters=Parameters p} }
parameters: p=parameter; COMA;               { {parameter=Parameter p; tail=None; mode=Some (ParameterMode NORMAL)} }
          | p=parameter;                     { {parameter=Parameter p; tail=None; mode=Some (ParameterMode NORMAL)} }
          | p=parameter; DOTS; COMA;         { {parameter=Parameter p; tail=None; mode=Some (ParameterMode HVARIADIC)} }
          | p=parameter; DOTS;               { {parameter=Parameter p; tail=None; mode=Some (ParameterMode HVARIADIC)} }
          | p=parameter; COMA; DOTS; COMA;   { {parameter=Parameter p; tail=None; mode=Some (ParameterMode CVARIADIC)} }
          | p=parameter; COMA; DOTS;         { {parameter=Parameter p; tail=None; mode=Some (ParameterMode CVARIADIC)} }
          | p=parameter; COMA; t=parameters; { {parameter=Parameter p; tail=Some (Parameters t); mode=None} }
parameter: n=NAME; COLON; t=typ; { {name=Some (Name n); typ=Typ t} }
         | LBAR; COLON; t=typ;   { {name=None; typ=Typ t} }
aliasType: i=identifier; { {ident=Identifier i} }
unwrappedAliasType: DOTS; i=identifier; { {ident=Identifier i} } 

//6.6 Expressions
//6.6.16 String constants
stringConstant: l=STRINGLIT;                   { {literal=StringLiteral l; tail=None} }
              | l=STRINGLIT; t=stringConstant; { {literal=StringLiteral l; tail=Some (StringConstant t)} }
//6.6.31 Postfix Expressions 
objectSelector: i=identifier;            { {child=Identifier i} }
              | i=indexingExpression;    { {child=Dummy i} }
              | a=fieldAccessExpression; { {child=Dummy a} }
//6.6.41 Logical arithmetic
logicalAndExpression: b=equalityExpression;                               { {bypass=Some (Dummy b); lhs=None; rhs=None} }
                    | l=logicalAndExpression; LAND; r=equalityExpression; { {bypass=None; lhs=Some (LogicalAndExpression l); rhs=Some (Dummy r)} }
logicalXorExpression: b=logicalAndExpression;                               { {bypass=Some(LogicalAndExpression b); lhs=None; rhs=None} }
                    | l=logicalXorExpression; LHAT; r=logicalAndExpression; { {bypass=None; lhs=Some (LogicalXorExpression l); rhs=Some (LogicalAndExpression r)} }
logicalOrExpression: b=logicalXorExpression;                             { {bypass=Some (LogicalXorExpression b); lhs=None; rhs=None} }
                   | l=logicalOrExpression; LOR; r=logicalXorExpression; { {bypass=None; lhs=Some (LogicalOrExpression l); rhs=Some (LogicalXorExpression r)} }
//6.6.46 Assignment
assignment: l=objectSelector; o=assignmentOp; e=expression;        { {lhs=ObjectSelector l; op=AssignmentOp o; expr=Expression e} }
          | STAR; l=unaryExpression; o=assignmentOp; e=expression; { {lhs=Dummy l; op=AssignmentOp o; expr=Expression e} }
          | l=slicingExpression; EQ; e=expression;                 { {lhs=Dummy l; op=AssignmentOp EQ; expr=Expression e} }
          | LPAREN; l=bindingNames; RPAREN; EQ; e=expression;      { {lhs=Dummy l; op=AssignmentOp EQ; expr=Expression e} }
assignmentOp: EQ;    { EQ }
            | PEQ;   { PLUS_EQ }
            | MEQ;   { MINUS_EQ }
            | SEQ;   { STAR_EQ }
            | DEQ;   { DIV_EQ }
            | PEREQ; { MOD_EQ }
            | LEQ;   { LEFT_EQ }
            | REQ;   { RIGHT_EQ }
            | LAEQ;  { LAND_EQ }
            | LOEQ;  { LOR_EQ }
            | LHEQ;  { LHAT_EQ }
            | AEQ;   { AND_EQ }
            | OEQ;   { OR_EQ }
            | HEQ;   { HAT_EQ }
//6.6.51 High Level Expression Class
expression: a=assignment;          { {child=Assignment a} }
          | l=logicalOrExpression; { {child=LogicalOrExpression l} }
          | i=ifExpression;        { {child=Dummy i} }
          | f=forLoop;             { {child=Dummy f} }
          | c=controlExpression;   { {child=Dummy c} }
        

//6.11 Declarations
declarations: EXPORT; d=declaration; SEMI;                { {export=Bool true; declaration=Declaration d; declarations=None} }
            | d=declaration; SEMI;                        { {export=Bool false; declaration=Declaration d; declarations=None} }
            | EXPORT; d=declaration; SEMI; t=declarations { {export=Bool true; declaration=Declaration d; declarations=Some (Declarations t)} }
            | d=declaration; SEMI; t=declarations         { {export=Bool false; declaration=Declaration d; declarations=Some (Declarations t)} }
declaration: d=globalDeclaration;   { {declaration=GlobalDeclaration d} }
           | d=constantDeclaration; { {declaration=ConstantDeclaration d} }
           | d=typeDeclaration;     { {declaration=TypeDeclaration d} }
           | d=functionDeclaration; { {declaration=FunctionDeclaration d} }

//6.11.3 Global Declarations
globalDeclaration: LET; b=globalBindings; COMA;   { {globalBindings=GlobalBindings b} }
                 | LET; b=globalBindings;         { {globalBindings=GlobalBindings b} }
                 | CONST; b=globalBindings; COMA; { {globalBindings=GlobalBindings b} } 
                 | CONST; b=globalBindings;       { {globalBindings=GlobalBindings b} } 
globalBindings: b=globalBinding;                          { {bindings=None; binding=GlobalBinding b} }
              | h=globalBindings; COMA; b=globalBinding;  { {bindings=Some (GlobalBindings h); binding=GlobalBinding b} }
globalBinding: a=declAttr; i=identifier; COLON; t=typ;                    { {attr=Some (DeclAttr a); ident=Identifier i; typ=Some (Typ t); expr=None} }
             | i=identifier; COLON; t=typ;                                { {attr=None; ident=Identifier i; typ=Some (Typ t); expr=None} }
             | a=declAttr; i=identifier; COLON; t=typ; EQ; e=expression;  { {attr=Some (DeclAttr a); ident=Identifier i; typ=Some (Typ t); expr=Some (Expression e)} }
             | i=identifier; COLON; t=typ; EQ; e=expression;              { {attr=None; ident=Identifier i; typ=Some (Typ t); expr=Some (Expression e)} }
             | a=declAttr; i=identifier; EQ; e=expression;                { {attr=Some (DeclAttr a); ident=Identifier i; typ=None; expr=Some (Expression e)} }
             | i=identifier; EQ; e=expression;                            { {attr=None; ident=Identifier i; typ=None; expr=Some (Expression e)} }
declAttr: ASYMBOL; LPAREN; s=stringConstant; RPAREN; { {threadLocal=Bool false; symbol=Some (StringConstant s)} }
        | ATHREADLOCAL;                              { {threadLocal=Bool true; symbol=None} }
constantDeclaration: DEF; b=constantBindings; COMA; { {constantBindings=ConstantBindings b} }
                   | DEF; b=constantBindings        { {constantBindings=ConstantBindings b} }
constantBindings: b=constantBinding;                          { {bindings=None; binding=ConstantBinding b} }
                | h=constantBindings; COMA; b=constantBinding { {bindings=Some (ConstantBindings h); binding=ConstantBinding b} }
constantBinding: i=identifier; COLON; t=typ; EQ; e=expression { {ident=Identifier i; typ=Typ t; expr=Expression e} }

//6.11.5 Type Declarations
typeDeclaration: TYPE; b=typeBindings; { {typeBindings=TypeBindings b} }
typeBindings: b=typeBinding; COMA;                { {binding=TypeBinding b; tail=None} }
            | b=typeBinding;                      { {binding=TypeBinding b; tail=None} }
            | b=typeBinding; COMA; t=typeBindings { {binding=TypeBinding b; tail=Some (TypeBindings t)} }
typeBinding: i=identifier; EQ; t=typ;      { {ident=Identifier i; typ=Typ t} }
           | i=identifier; EQ; t=enumType; { {ident=Identifier i; typ=EnumType t} }
enumType: ENUM; LBRACE; v=enumValues; RBRACE ;               { {storage=None; values=EnumValues v} }
        | ENUM; s=enumStorage; LBRACE; v=enumValues; RBRACE; { {storage=Some (EnumStorage s); values=EnumValues v} }
enumValues: v=enumValue; COMA;              { {value=EnumValue v; tail=None} }
          | v=enumValue;                    { {value=EnumValue v; tail=None} }
          | v=enumValue; COMA; t=enumValues { {value=EnumValue v; tail=Some (EnumValues t)} }
enumValue: n=NAME;                   { {name=Name n; expr=None} }
         | n=NAME; EQ; e=expression; { {name=Name n; expr=Some (Expression e)} }
enumStorage: t=integerType { {baseType=IntegerType t} }
           | RUNE          { {baseType=ScalarType {subType=BasicScalarType RUNE}} }

//6.22.6 Function Declarations
functionDeclaration: a=fndecAttrs; FN; i=identifier; p=prototype;             { {attrs=Some (FndecAttrs a); ident=Identifier i; prototype=Prototype p; expr=None} }
                   | FN; i=identifier; p=prototype;                           { {attrs=None; ident=Identifier i; prototype=Prototype p; expr=None} }
                   | a=fndecAttrs; FN; n=identifier; p=prototype; EQ; e=expression; { {attrs=Some (FndecAttrs a); ident=Identifier n; prototype=Prototype p; expr=Some (Expression e)} }
                   | FN; n=identifier; p=prototype; EQ; e=expression;               { {attrs=None; ident=Identifier n; prototype=Prototype p; expr=Some (Expression e)} }
fndecAttrs: a=fndecAttr;               { {attr=FndecAttr a; attrs=None} }
          | a=fndecAttr; t=fndecAttrs; { {attr=FndecAttr a; attrs=Some (FndecAttrs t)} }
fndecAttr: AFINI;        { {attr=FndecAttrType AFINI} }
         | AINIT;        { {attr=FndecAttrType AINIT} }
         | ATEST;        { {attr=FndecAttrType ATEST} }
         | a=fntypeAttr; { {attr=FntypeAttr a} }
         | a=declAttr;   { {attr=DeclAttr a} }


//6.12 Units
subUnit: i=imports; d=declarations; EOF; { {imports=Some (Imports i); declarations=Some (Declarations d)} }
       | i=imports; EOF;                 { {imports=Some (Imports i); declarations=None} }
       | d=declarations; EOF;            { {imports=None; declarations=Some (Declarations d)} }
imports: u=useStatement;             { {import=UseStatement u; imports=None} }
       | u=useStatement; i=imports;  { {import=UseStatement u; imports=Some (Imports i)} }
useStatement: USE; a=importAlias; i=identifier; SEMI;                                        { {alias=Some (ImportAlias a); ident=Identifier i; members=None; qualify=Bool true} } 
            | USE; i=identifier; SEMI;                                                       { {alias=None; ident=Identifier i; members=None; qualify=Bool true} }
            | USE; a=importAlias; i=identifier; DCOLONB; m=memberList; RBRACE; SEMI;  { {alias=Some (ImportAlias a); ident=Identifier i; members=Some (MemberList m); qualify=Bool true} } 
            | USE; i=identifier; DCOLONB; m=memberList; RBRACE; SEMI;                 { {alias=None; ident=Identifier i; members=Some (MemberList m); qualify=Bool true} } 
            | USE; i=identifier; DCOLONS; SEMI;                                              { {alias=None; ident=Identifier i; members=None; qualify=Bool false} }
importAlias: n=NAME; EQ; { {name=Name n} }
memberList: m=member; COMA;               { {member=Member m; members=None} }
          | m=member;                     { {member=Member m; members=None} }
          | m=member; COMA; t=memberList; { {member=Member m; members=Some (MemberList t)} }
member: i=NAME;             { {alias=None; ident=Name i} }
      | a=NAME; EQ; i=NAME; { {alias=Some (Name a); ident=Name i} } 

//dummies
equalityExpression: DUMMY { A } 
ifExpression:        DUMMY { A } 
forLoop:             DUMMY { A } 
controlExpression:   DUMMY { A } 

indexingExpression: DUMMY { A }
fieldAccessExpression: DUMMY { A }
unaryExpression:     DUMMY { A }
slicingExpression:   DUMMY { A }
bindingNames:        DUMMY { A }