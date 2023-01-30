%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token DUMMY 
%type <nodeDummy> declarations importAlias memberList 
%token <string> DSTR

%token EOF USE SEMI STAR COLON LBRACE RBRACE

%start subUnit

%type <nodeSubUnit> subUnit
%type <nodeImports> imports
%type <nodeUseStatement> useStatement


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

//dummies
declarations: DUMMY; { A }
importAlias: DUMMY; { B }
identifier: i=DSTR; { i }
memberList: DUMMY; { D }