%{
    open Ast
%}

//%token <int> INT
//%type <int> program

%token <Ast.dummy> DUMMY 
%token <string> DSTR
%type <Ast.dummy> declarations 

%token EOF USE SEMI STAR COLON LBRACE RBRACE

%start subUnit

%type <nodeSubUnit> subUnit
%type <nodeImports> imports
%type <nodeUseStatement> useStatement


%%

//6.12 Units
subUnit: i=imports; d=declarations; EOF; { {imports=Some i; declarations=Some d} }
       | i=imports; EOF;                 { {imports=Some i; declarations=None} }
       | d=declarations; EOF;            { {imports=None; declarations=Some d} }
imports: u=useStatement; EOF;            { {import=u; imports=None} }
       | u=useStatement; i=imports; EOF; { {import=u; imports=Some i} }
useStatement: USE; a=importAlias; i=identifier; SEMI;                                             { {alias=Some a; ident=i; members=None; qualify=true} } 
            | USE; i=identifier; SEMI;                                                            { {alias=None; ident=i; members=None; qualify=true} }
            | USE; a=importAlias; i=identifier; COLON; COLON; LBRACE; m=memberList; RBRACE; SEMI; { {alias=Some a; ident=i; members=Some m; qualify=true} } 
            | USE; i=identifier; COLON; COLON; LBRACE; m=memberList; RBRACE; SEMI;                { {alias=None; ident=i; members=Some m; qualify=true} } 
            | USE; i=identifier; COLON; COLON; STAR; SEMI;                                        { {alias=None; ident=i; members=None; qualify=false} }

//dummies
declarations: DUMMY; { A }
importAlias: DUMMY; { B }
identifier: i=DSTR; { i }
memberList: DUMMY; { D }