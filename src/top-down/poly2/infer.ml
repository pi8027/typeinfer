
open Def

module IntMap = Map.Make(
    struct
      type t = int
      let compare i j = i - j
    end
  );;

module StrMap = Map.Make(String);;

type tconst = ltype * ltype;;
type assump_elem
    = Monovar of ltype
    | Polyvar of (int * int * tconst list * ltype);;
type assump = assump_elem StrMap.t;;
type subst = ltype IntMap.t;;

exception Occurs_check;;

let rec rename_tyvars (n1 : int) (n2 : int) : ltype -> ltype =
  function
    | TFun (t1, t2) -> TFun (rename_tyvars n1 n2 t1, rename_tyvars n1 n2 t2)
    | TVar n when n <= n1 -> TVar (n - n1 + n2)
    | TVar n -> TVar n
;;

let rec infer (n : int) (env : assump) : lexpr -> int * tconst list * ltype =
  let nt = TVar n in
  function
    | EVar str -> (match StrMap.find str env with
      | Monovar t -> n, [], t
      | Polyvar (nb, ne, cr, ty) ->
        let ren = rename_tyvars nb n in
        n + ne - nb, List.map (fun (l, r) -> ren l, ren r) cr, ren ty)
    | EApp (expr1, expr2) ->
      let n1, c1, t1 = infer (succ n) env expr1 in
      let n2, c2, t2 = infer n1 env expr2 in
      n2, (t1, TFun (t2, nt)) :: c1 @ c2, nt
    | EAbs (ident, expr) ->
      let n', c, t = infer (succ n) (StrMap.add ident (Monovar nt) env) expr in
      n', c, TFun (nt, t)
    | ELet (ident, expr1, expr2) ->
      let ne, cr, ty = infer n env expr1 in
      infer n (StrMap.add ident (Polyvar (n, ne, cr, ty)) env) expr2
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

