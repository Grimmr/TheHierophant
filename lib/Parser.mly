%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> functionDeclaration typeDeclaration expression

%token EOF USE SEMI STAR COLON DCOLON DCOLONB DCOLONS LBRACE RBRACE EQ COMA EXPORT LET CONST ASYMBOL ATHREADLOCAL LPAREN RPAREN DEF BANG BOOL RUNE VALIST VOID
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
            | t=structUnionType;    { {storage=Dummy t} }
            | t=tupleType;          { {storage=Dummy t} }
            | t=taggedUnionType;    { {storage=Dummy t} }
            | t=sliceArrayType;     { {storage=Dummy t} }
            | t=functionType;       { {storage=Dummy t} }
            | t=aliasType;          { {storage=Dummy t} }
            | t=unwrappedAliasType; { {storage=Dummy t} }
            | t=stringType;         { {storage=Dummy t} }
scalarType: t=integerType;  { {subType=Dummy t} }
          | t=floatingType; { {subType=Dummy t} }
          | t=pointerType;  { {subType=Dummy t} }
          | RUNE;           { {subType=BasicScalarType RUNE} }
          | BOOL;           { {subType=BasicScalarType BOOL} }
          | VALIST;         { {subType=BasicScalarType VALIST} }
          | VOID;           { {subType=BasicScalarType VOID} }

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
structUnionType: DUMMY; { A }
tupleType: DUMMY; { A }
taggedUnionType: DUMMY; { A }
sliceArrayType: DUMMY; { A }
functionType: DUMMY; { A }
aliasType: DUMMY; { A }
unwrappedAliasType: DUMMY; { A }
stringType: DUMMY; { A }
integerType: DUMMY; {A}
pointerType: DUMMY; {A}
floatingType: DUMMY; {A}