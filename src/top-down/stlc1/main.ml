
let parse (input : in_channel) : Def.lexpr =
  Parser.main Lexer.token (Lexing.from_channel input);;
let expr = parse stdin;;
print_string ("expression :\n" ^ Def.str_lexpr 2 expr ^ "\n\n");;

match Infer.infer 0 Infer.StrMap.empty expr with
  | Some (_, cr, ty) ->
    print_string "constraints :\n";
    List.iter (fun (t1, t2) ->
      print_string
        (Def.str_ltype 1 t1 ^ ", " ^ Def.str_ltype 1 t2 ^ "\n")) cr;
    print_string "\n";
    print_string (Def.str_ltype 1 ty ^ "\n\n");

    begin match List.fold_left
        (function | Some s -> Infer.solve s | None -> fun _ -> None)
        (Some Infer.IntMap.empty) cr with
      | Some tenv ->
        print_string
          ("solve :\n" ^ Def.str_ltype 1 (Infer.expand_type tenv ty) ^ "\n");
      | None -> print_string "Error: occurs check\n"
    end
  | None -> print_string "Error: free variable\n"
  ;;

