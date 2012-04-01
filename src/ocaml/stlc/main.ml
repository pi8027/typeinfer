
open Core.Std
open Def

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

let term = Parser.main Lexer.token (Lexing.from_channel stdin);;
print_string ("term:\n" ^ strterm 2 term ^ "\n\n");;

match Infer.constraints 0 String.Map.empty term with
  | Some (_, cr, ty) ->
    print_string "constraints:\n";
    List.iter cr (fun (t1, t2) ->
      print_string (strtype 1 t1 ^ ", " ^ strtype 1 t2 ^ "\n"));
    print_string "\n";
    print_string ("type:\n" ^ strtype 1 ty ^ "\n\n");
    begin match Infer.unify Int.Map.empty cr with
      | Some tenv ->
        print_string ("solve:\n" ^ strtype 1 (Infer.substitute tenv ty) ^ "\n");
      | None -> print_string "Error: occurs check\n"
    end
  | None -> print_string "Error: free variable\n"
;;

