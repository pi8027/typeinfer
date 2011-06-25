%token <string> IDENT
%token REC IN
%token LPAREN RPAREN LAMBDA ARROW EQUAL EOF
%start main
%type <Def.lexpr> main
%%

main: expr EOF { $1 }

expr:
    expr factor { Def.EApply ($1, $2) }
  | factor      { $1 }

factor:
    REC IDENT EQUAL expr IN expr { Def.ERec ($2, $4, $6) }
  | IDENT                        { Def.EVar $1 }
  | LPAREN expr RPAREN           { $2 }
  | LAMBDA IDENT ARROW expr      { Def.ELambda ($2, $4) }

