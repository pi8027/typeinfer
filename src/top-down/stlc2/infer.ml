
open Def

module IntMap = Map.Make(
    struct
      type t = int
      let compare i j = i - j
    end
  );;

module StrMap = Map.Make(String);;

type tconst = ltype * ltype;;
type assump = ltype StrMap.t;;
type subst = ltype IntMap.t;;

exception Occurs_check;;

let rec infer (n : int) (env : assump) : lexpr -> int * tconst list * ltype =
  let nt = TVar n in
  function
    | EVar str -> n, [], StrMap.find str env
    | EApp (expr1, expr2) ->
      let n1, c1, t1 = infer (succ n) env expr1 in
      let n2, c2, t2 = infer n1 env expr2 in
      n2, (t1, TFun (t2, nt)) :: c1 @ c2, nt
    | EAbs (ident, expr) ->
      let n', c, t = infer (succ n) (StrMap.add ident nt env) expr in
      n', c, TFun (nt, t)
;;

let rec type_pc (env : subst) : ltype -> ltype = function
  | TVar n when IntMap.mem n env -> type_pc env (IntMap.find n env)
  | t -> t
;;

let rec solve (env : subst) ((lt, rt) : tconst) : subst =
  let rec occurs_check n t =
    match type_pc env t with
      | TVar n' -> if n = n' then raise Occurs_check else ()
      | TFun (tl, tr) -> occurs_check n tl ; occurs_check n tr
  in
  match type_pc env lt, type_pc env rt with
    | TVar n, TVar n' when n = n' -> env
    | TVar n, t | t, TVar n -> occurs_check n t ; IntMap.add n t env
    | TFun (t1l, t1r), TFun (t2l, t2r) ->
      solve (solve env (t1l, t2l)) (t1r, t2r)
;;

let rec expand_type (env : subst) (t : ltype) : ltype =
  match type_pc env t with
    | TVar n -> TVar n
    | TFun (tl, tr) -> TFun (expand_type env tl, expand_type env tr)
;;

