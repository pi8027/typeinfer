
let parse (input : in_channel) : Def.lexpr =
  Parser.main Lexer.token (Lexing.from_channel input);;

let expr = parse stdin;;

let _, cr, ty = Infer.constraints 0 Infer.StrMap.empty expr;;

print_string "expression :\n";;
Def.print_lexpr 2 expr;;
print_string "\n\n";;

print_string "constraints :\n";;

List.iter
  (fun (t1, t2) ->
    Def.print_ltype 1 t1 ;
    print_string ", " ;
    Def.print_ltype 1 t2 ;
    print_string "\n")
  cr;;

print_string "\n";;
Def.print_ltype 1 ty;;
print_string "\n\n";;

print_string "solve :\n";;

let tenv = List.fold_left Infer.solve Infer.IntMap.empty cr;;

Def.print_ltype 1 (Infer.expand_type tenv ty);;
print_string "\n";;

