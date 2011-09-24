
open Def;;

let bracket (flag : bool) (str : string) : string =
  if flag then "(" ^ str ^ ")" else str
;;

let rec strterm (level : int) : term -> string = function
  | EVar str -> str
  | EApp (e1, e2) ->
    bracket (level <= 0) (strterm 1 e1 ^ " " ^ strterm 0 e2)
  | EAbs (ident, term) ->
    bracket (level <= 1) ("\\" ^ ident ^ " -> " ^ strterm 2 term)
;;

let rec strtype (level : int) : ty -> string = function
  | TFun (t1, t2) ->
    bracket (level = 0) (strtype 0 t1 ^ " -> " ^ strtype 1 t2)
  | TVar n -> string_of_int n
;;

let parse (input : in_channel) : term =
  Parser.main Lexer.token (Lexing.from_channel input);;
let term = parse stdin;;
print_string ("term:\n" ^ strterm 2 term ^ "\n\n");;

match Infer.infer 0 Infer.StrMap.empty term with
  | Some (_, cr, ty) ->
    print_string "constraints:\n";
    List.iter (fun (t1, t2) ->
      print_string (strtype 1 t1 ^ ", " ^ strtype 1 t2 ^ "\n")) cr;
    print_string "\n";
    print_string ("type:\n" ^ strtype 1 ty ^ "\n\n");
    begin match List.fold_left
        (function | Some s -> Infer.solve s | None -> fun _ -> None)
        (Some Infer.IntMap.empty) cr with
      | Some tenv ->
        print_string
          ("solve:\n" ^ strtype 1 (Infer.expand_type tenv ty) ^ "\n");
      | None -> print_string "Error: occurs check\n"
    end
  | None -> print_string "Error: free variable\n"
  ;;

