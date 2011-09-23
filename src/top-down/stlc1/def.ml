
type lexpr
  = EVar of string
  | EApp of (lexpr * lexpr)
  | EAbs of (string * lexpr)
;;

type ltype
  = TFun of (ltype * ltype)
  | TVar of int
;;

let bracket (flag : bool) (str : string) : string =
  if flag then "(" ^ str ^ ")" else str
;;

let rec str_lexpr (level : int) : lexpr -> string = function
  | EVar str -> str
  | EApp (e1, e2) ->
    bracket (level <= 0) (str_lexpr 1 e1 ^ " " ^ str_lexpr 0 e2)
  | EAbs (ident, expr) ->
    bracket (level <= 1) ("\\" ^ ident ^ " -> " ^ str_lexpr 2 expr)
;;

let rec str_ltype (level : int) : ltype -> string = function
  | TFun (t1, t2) ->
    bracket (level = 0) (str_ltype 0 t1 ^ " -> " ^ str_ltype 1 t2)
  | TVar n -> string_of_int n
;;

