%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> declarations name
%token <string> DSTR

%token EOF USE SEMI STAR COLON LBRACE RBRACE EQ COMA

%start subUnit

%type <nodeSubUnit> subUnit
%type <nodeImports> imports
%type <nodeUseStatement> useStatement
%type <nodeImportAlias> importAlias
%type <nodeMemberList> memberList


%%

//6.12 Units
subUnit: i=imports; d=declarations; EOF; { {imports=Some (Imports i); declarations=Some (Declarations d)} }
       | i=imports; EOF;                 { {imports=Some (Imports i); declarations=None} }
       | d=declarations; EOF;            { {imports=None; declarations=Some (Declarations d)} }
imports: u=useStatement; EOF;            { {import=UseStatement u; imports=None} }
       | u=useStatement; i=imports; EOF; { {import=UseStatement u; imports=Some (Imports i)} }
useStatement: USE; a=importAlias; i=identifier; SEMI;                                             { {alias=Some (ImportAlias a); ident=Identifier i; members=None; qualify=true} } 
            | USE; i=identifier; SEMI;                                                            { {alias=None; ident=Identifier i; members=None; qualify=true} }
            | USE; a=importAlias; i=identifier; COLON; COLON; LBRACE; m=memberList; RBRACE; SEMI; { {alias=Some (ImportAlias a); ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; i=identifier; COLON; COLON; LBRACE; m=memberList; RBRACE; SEMI;                { {alias=None; ident=Identifier i; members=Some (MemberList m); qualify=true} } 
            | USE; i=identifier; COLON; COLON; STAR; SEMI;                                        { {alias=None; ident=Identifier i; members=None; qualify=false} }
importAlias: n=name; EQ; { {name=Name n} }
memberList: m=member; COMA;               { {member=Member m; members=None} }
          | m=member;                     { {member=Member m; members=None} }
          | m=member; COMA; t=memberList; { {member=Member m; members=Some (MemberList t)} }

//dummies
declarations: DUMMY; { A }
identifier: i=DSTR; { i }
name: DUMMY; { B }
member: DUMMY; { C }