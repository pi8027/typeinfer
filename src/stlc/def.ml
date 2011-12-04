
type term
  = EVar of string
  | EApp of (term * term)
  | EAbs of (string * term)

type ty
  = TFun of (ty * ty)
  | TVar of int

