%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> declarations

%token EOF USE SEMI STAR COLON DCOLON SDCOLON LBRACE RBRACE EQ COMA
%token <string> NAME 

%start subUnit

%type <nodeIdentifier> identifier
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

//6.12 Units
subUnit: i=imports; d=declarations; EOF; { {imports=Some (Imports i); declarations=Some (Declarations d)} }
       | i=imports; EOF;                 { {imports=Some (Imports i); declarations=None} }
       | d=declarations; EOF;            { {imports=None; declarations=Some (Declarations d)} }
imports: u=useStatement; EOF;            { {import=UseStatement u; imports=None} }
       | u=useStatement; i=imports; EOF; { {import=UseStatement u; imports=Some (Imports i)} }
useStatement: USE; a=importAlias; i=identifier; SEMI;                                        { {alias=Some (ImportAlias a); ident=Identifier i; members=None; qualify=true} } 
            | USE; i=identifier; SEMI;                                                       { {alias=None; ident=Identifier i; members=None; qualify=true} }
            | USE; a=importAlias; i=identifier; DCOLON; LBRACE; m=memberList; RBRACE; SEMI;  { {alias=Some (ImportAlias a); ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; a=importAlias; i=identifier; SDCOLON; LBRACE; m=memberList; RBRACE; SEMI; { {alias=Some (ImportAlias a); ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; i=identifier; DCOLON; LBRACE; m=memberList; RBRACE; SEMI;                 { {alias=None; ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; i=identifier; SDCOLON; LBRACE; m=memberList; RBRACE; SEMI;                { {alias=None; ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; i=identifier; DCOLON; STAR; SEMI;                                         { {alias=None; ident=Identifier i; members=None; qualify=false} }
            | USE; i=identifier; SDCOLON; STAR; SEMI;                                        { {alias=None; ident=Identifier i; members=None; qualify=false} }
importAlias: n=NAME; EQ; { {name=Name n} }
memberList: m=member; COMA;               { {member=Member m; members=None} }
          | m=member;                     { {member=Member m; members=None} }
          | m=member; COMA; t=memberList; { {member=Member m; members=Some (MemberList t)} }
member: i=NAME;             { {alias=None; ident=Name i} }
      | a=NAME; EQ; i=NAME; { {alias=Some (Name a); ident=Name i} } 

//dummies
declarations: DUMMY; { A }
