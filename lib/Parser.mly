%token <int> INT
%token A
%token EOF

%start program
%type <int> program

%%

program: A i=INT { i };