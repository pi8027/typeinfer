
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
  | ELet (ident, term1, term2) ->
    bracket (level <=1)
        ("let " ^ ident ^ " = " ^ strterm 2 term1 ^ " in " ^ strterm 2 term2)
;;

let rec strtype (level : int) : ty -> string = function
  | TFun (t1, t2) ->
    bracket (level = 0) (strtype 0 t1 ^ " -> " ^ strtype 1 t2)
  | TVar n -> string_of_int n
;;

let term = Parser.main Lexer.token (Lexing.from_channel stdin);;
print_string ("term:\n" ^ strterm 2 term ^ "\n\n");;

match Infer.infer 0 String.Map.empty term with
  | Some (_, _, ty) ->
    print_string ("infer:\n" ^ strtype 1 ty ^ "\n\n")
  | None -> print_string "Error: free variable\n"
;;

