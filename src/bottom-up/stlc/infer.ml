
open Def

module IntMap = Map.Make(
    struct
      type t = int
      let compare i j = i - j
    end
  );;

module StrMap = Map.Make(String);;

type tconst = ltype * ltype;;
type assump = (ltype list) StrMap.t;;
type subst = ltype IntMap.t;;

exception Occurs_check;;

let rec infer (n : int) : lexpr -> int * assump * tconst list * ltype =
  let nt = TVar n in
  let unopt = function None -> [] | Some l -> l in
  function
    | EVar str -> succ n, StrMap.singleton str [nt], [], nt
    | EApp (expr1, expr2) ->
      let n1, a1, c1, t1 = infer (succ n) expr1 in
      let n2, a2, c2, t2 = infer n1 expr2 in
      n2, StrMap.merge (fun _ a1 a2 -> Some (unopt a1 @ unopt a2)) a1 a2,
        (t1, TFun (t2, nt)) :: c1 @ c2, nt
    | EAbs (ident, expr) ->
      let n', a, c1, t = infer (succ n) expr in
      let c2 = if StrMap.mem ident a
        then List.map (fun t -> (t, nt)) (StrMap.find ident a) else [] in
      n', StrMap.remove ident a, c1 @ c2, TFun (nt, t)
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

