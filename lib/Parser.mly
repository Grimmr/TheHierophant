%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> functionDeclaration typeDeclaration expression

%token EOF USE SEMI STAR COLON DCOLON DCOLONB DCOLONS LBRACE RBRACE EQ COMA EXPORT LET CONST ASYMBOL ATHREADLOCAL LPAREN RPAREN DEF BANG BOOL RUNE VALIST VOID I8 I16 I32 I64 U8 U16 U32 U64 INT UINT SIZE UINTPTR CHAR F32 F64 NULLABLE STRUCT UNION AOFFSET APACKED
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
            | t=taggedUnionType;    { {storage=Dummy t} }
            | t=sliceArrayType;     { {storage=Dummy t} }
            | t=functionType;       { {storage=Dummy t} }
            | t=aliasType;          { {storage=Dummy t} }
            | t=unwrappedAliasType; { {storage=Dummy t} }
            | t=stringType;         { {storage=Dummy t} }
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

//6.6 Expressions
//6.6.16 String constants
stringConstant: l=STRINGLIT;                   { {literal=StringLiteral l; tail=None} }
              | l=STRINGLIT; t=stringConstant; { {literal=StringLiteral l; tail=Some (StringConstant t)} }

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
functionDeclaration: DUMMY; { B }
typeDeclaration: DUMMY;     { C }
expression: DUMMY; { A }
taggedUnionType: DUMMY; { A }
sliceArrayType: DUMMY; { A }
functionType: DUMMY; { A }
aliasType: DUMMY; { A }
unwrappedAliasType: DUMMY; { A }
stringType: DUMMY; { A }