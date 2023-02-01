%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> declaration 

%token EOF USE SEMI STAR COLON DCOLON DCOLONB DCOLONS LBRACE RBRACE EQ COMA EXPORT
%token <string> NAME 

%start subUnit

%type <nodeIdentifier> identifier
%type <nodeDeclarations> declarations
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

//6.11 Declarations
declarations: EXPORT; d=declaration; SEMI;                { {export=Bool true; declaration=Declaration d; declarations=None} }
            | d=declaration; SEMI;                        { {export=Bool false; declaration=Declaration d; declarations=None} }
            | EXPORT; d=declaration; SEMI; t=declarations { {export=Bool true; declaration=Declaration d; declarations=Some (Declarations t)} }
            | d=declaration; SEMI; t=declarations         { {export=Bool false; declaration=Declaration d; declarations=Some (Declarations t)} }

//6.12 Units
subUnit: i=imports; d=declarations; EOF; { {imports=Some (Imports i); declarations=Some (Declarations d)} }
       | i=imports; EOF;                 { {imports=Some (Imports i); declarations=None} }
       | d=declarations; EOF;            { {imports=None; declarations=Some (Declarations d)} }
imports: u=useStatement; EOF;            { {import=UseStatement u; imports=None} }
       | u=useStatement; i=imports; EOF; { {import=UseStatement u; imports=Some (Imports i)} }
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
declaration: DUMMY; { A }
