%token <string> IDENT
%token LPAREN RPAREN LAMBDA ARROW EOF
%start main
%type <Def.term> main
%%

main: term EOF { $1 }

term:
    term factor { Def.EApp ($1, $2) }
  | factor      { $1 }

factor:
    IDENT                   { Def.EVar $1 }
  | LPAREN term RPAREN      { $2 }
  | LAMBDA IDENT ARROW term { Def.EAbs ($2, $4) }

