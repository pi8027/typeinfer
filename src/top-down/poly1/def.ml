
type term
  = EVar of string
  | EApp of (term * term)
  | EAbs of (string * term)
  | ELet of (string * term * term)

type ty
  = TFun of (ty * ty)
  | TVar of int

