{

open Parser

}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | ['a'-'z']+      { match Lexing.lexeme lexbuf with
                        | "fix" -> FIX
                        | str -> IDENT str }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "\\"            { LAMBDA }
  | "->"            { ARROW }
  | "="             { EQUAL }
  | eof             { EOF }

