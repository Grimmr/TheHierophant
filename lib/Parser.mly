%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> functionDeclaration typeDeclaration constantDeclaration typ expression

%token EOF USE SEMI STAR COLON DCOLON DCOLONB DCOLONS LBRACE RBRACE EQ COMA EXPORT LET CONST ASYMBOL ATHREADLOCAL LPAREN RPAREN
%token <string> NAME STRINGLIT

%start subUnit

%type <nodeIdentifier> identifier
%type <nodeStringConstant> stringConstant
%type <nodeDeclarations> declarations
%type <nodeDeclaration> declaration
%type <nodeGlobalDeclaration> globalDeclaration
%type <nodeGlobalBindings> globalBindings 
%type <nodeGlobalBinding> globalBinding
%type <nodeDeclAttr> declAttr
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
globalDeclaration: LET; b=globalBindings;   { {bindings=GlobalBindings b} }
                 | CONST; b=globalBindings; { {bindings=GlobalBindings b} } 
globalBindings: b=globalBinding; COMA;                   { {bindings=None; binding=GlobalBinding b} }
              | b=globalBinding;                         { {bindings=None; binding=GlobalBinding b} }
              | h=globalBindings; COMA; b=globalBinding;  { {bindings=Some (GlobalBindings h); binding=GlobalBinding b} }
globalBinding: a=declAttr; i=identifier; COLON; t=typ;                     { {attr=Some (DeclAttr a); ident=Identifier i; typ=Some (Typ t); expr=None} }
             | i=identifier; COLON; t=typ;                               { {attr=None; ident=Identifier i; typ=Some (Typ t); expr=None} }
             | a=declAttr; i=identifier; COLON; t=typ; EQ; e=expression; { {attr=Some (DeclAttr a); ident=Identifier i; typ=Some (Typ t); expr=Some (Expression e)} }
             | i=identifier; COLON; t=typ; EQ; e=expression;             { {attr=None; ident=Identifier i; typ=Some (Typ t); expr=Some (Expression e)} }
             | a=declAttr; i=identifier; EQ; e=expression;                { {attr=Some (DeclAttr a); ident=Identifier i; typ=None; expr=Some (Expression e)} }
             | i=identifier; EQ; e=expression;                            { {attr=None; ident=Identifier i; typ=None; expr=Some (Expression e)} }
declAttr: ASYMBOL; LPAREN; s=stringConstant; RPAREN; { {threadLocal=Bool false; symbol=Some (StringConstant s)} }
        | ATHREADLOCAL;                              { {threadLocal=Bool true; symbol=None} }

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
constantDeclaration: DUMMY; { A }
functionDeclaration: DUMMY; { B }
typeDeclaration: DUMMY;     { C }
typ: DUMMY; { D }
expression: DUMMY; { A }