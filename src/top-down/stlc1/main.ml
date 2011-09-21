
let parse (input : in_channel) : Def.lexpr =
  Parser.main Lexer.token (Lexing.from_channel input);;
let expr = parse stdin;;
print_string "expression :\n";;
Def.print_lexpr 2 expr;;
print_string "\n\n";;

match Infer.infer 0 Infer.StrMap.empty expr with
  | Some (_, cr, ty) ->
    print_string "constraints :\n";
    List.iter (fun (t1, t2) ->
        Def.print_ltype 1 t1 ;
        print_string ", " ;
        Def.print_ltype 1 t2 ;
        print_string "\n")
      cr;
    print_string "\n";
    Def.print_ltype 1 ty;
    print_string "\n\n";

    begin match List.fold_left
        (function | Some s -> Infer.solve s | None -> fun _ -> None)
        (Some Infer.IntMap.empty) cr with
      | Some tenv ->
        print_string "solve :\n";
        Def.print_ltype 1 (Infer.expand_type tenv ty);
        print_string "\n";
      | None -> print_string "Error: occurs check\n"
    end
  | None -> print_string "Error: free variable\n"
  ;;

