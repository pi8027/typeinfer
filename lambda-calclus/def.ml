
type lexpr
  = ELambda of (string * lexpr)
  | EApply of (lexpr * lexpr)
  | EVar of string

type ltype
  = FunctionType of (ltype * ltype)
  | VarType of int

let print_bracket (flag : bool) (printer : unit lazy_t) : unit =
  if flag
    then (print_string "(" ; Lazy.force printer ; print_string ")")
    else Lazy.force printer

let rec print_lexpr (level : int) (expr : lexpr) : unit =
  match expr with
    | ELambda (ident, expr) ->
        print_bracket
          (level == 0)
          (lazy (print_string "\\" ;
                 print_string ident ;
                 print_string " -> " ;
                 print_lexpr 1 expr))
    | EApply (e1, e2) -> print_bracket (level == 0)
        (lazy (print_lexpr 1 e1 ;
               print_string " " ;
               print_lexpr 0 e2))
    | EVar str -> print_string str

