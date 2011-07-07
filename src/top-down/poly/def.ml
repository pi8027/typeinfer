
type lexpr
  = EVar of string
  | EApp of (lexpr * lexpr)
  | EAbs of (string * lexpr)
  | ELet of (string * lexpr * lexpr)
;;

type ltype
  = TFun of (ltype * ltype)
  | TVar of int
;;

let print_bracket (flag : bool) (printer : unit lazy_t) : unit =
  if flag
    then (print_string "(" ; Lazy.force printer ; print_string ")")
    else Lazy.force printer
;;

let rec print_lexpr (level : int) : lexpr -> unit = function
  | EVar str -> print_string str
  | EApp (e1, e2) ->
    print_bracket (level <= 0)
      (lazy (print_lexpr 1 e1 ; print_string " " ; print_lexpr 0 e2))
  | EAbs (ident, expr) ->
    print_bracket (level <= 1)
      (lazy (print_string ("\\" ^ ident ^ " -> ") ; print_lexpr 2 expr))
  | ELet (ident, expr1, expr2) ->
    print_bracket (level <= 1)
      (lazy (print_string ("let " ^ ident ^ " = ") ;
             print_lexpr 2 expr1 ;
             print_string " in ";
             print_lexpr 2 expr2))
;;

let rec print_ltype (level : int) : ltype -> unit = function
  | TFun (t1, t2) ->
    print_bracket (level = 0)
      (lazy (print_ltype 0 t1 ; print_string " -> " ; print_ltype 1 t2))
  | TVar n -> print_int n
;;

