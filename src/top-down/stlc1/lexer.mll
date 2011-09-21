{

open Parser

}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | ['a'-'z']+      { IDENT (Lexing.lexeme lexbuf) }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "\\"            { LAMBDA }
  | "->"            { ARROW }
  | eof             { EOF }

