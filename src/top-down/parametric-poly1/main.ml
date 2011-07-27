
let parse (input : in_channel) : Def.lexpr =
  Parser.main Lexer.token (Lexing.from_channel input);;

let expr = parse stdin;;

let _, subst, ty = Infer.infer 0 Infer.StrMap.empty Infer.IntMap.empty expr;;

print_string "expression :\n";;
Def.print_lexpr 2 expr;;
print_string "\n\n";;

print_string "infer :\n";;

Def.print_ltype 1 (Infer.expand_type subst ty);;
print_string "\n";;

