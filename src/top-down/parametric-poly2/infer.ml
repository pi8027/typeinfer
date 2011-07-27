
open Def

module Int = 
  struct
    type t = int
    let compare i j = i - j
  end;;

module IntMap = Map.Make(Int);;
module StrMap = Map.Make(String);;
module IntSet = Set.Make(Int);;

type typescheme = IntSet.t * ltype;;
type tconst = ltype * ltype;;
type assump = typescheme StrMap.t;;
type subst = ltype IntMap.t;;

exception Occurs_check;;

let rec type_pc (env : subst) : ltype -> ltype = function
  | TVar n when IntMap.mem n env -> type_pc env (IntMap.find n env)
  | t -> t
;;

let rec expand_type (env : subst) (t : ltype) : ltype =
  match type_pc env t with
    | TVar n -> TVar n
    | TFun (tl, tr) -> TFun (expand_type env tl, expand_type env tr)
;;

let rec freevars : ltype -> IntSet.t = function
  | TVar n -> IntSet.singleton n
  | TFun (tl, tr) -> IntSet.union (freevars tl) (freevars tr)
;;

let rec solve (env : subst) ((lt, rt) : tconst) : subst =
  match type_pc env lt, type_pc env rt with
    | TVar n, TVar n' when n = n' -> env
    | TVar n, TVar n' when n < n' -> IntMap.add n' (TVar n) env
    | TVar n, TVar n' when n > n' -> IntMap.add n (TVar n') env
    | TVar n, t | t, TVar n ->
      if IntSet.mem n (freevars (expand_type env t))
        then raise Occurs_check else ();
      IntMap.add n t env
    | TFun (t1l, t1r), TFun (t2l, t2r) ->
      solve (solve env (t1l, t2l)) (t1r, t2r)
;;

let generalize (n : int) (ty : ltype) : typescheme =
  IntSet.filter (fun n' -> n <= n') (freevars ty), ty
;;

let instantiate (n : int) (pvs, ty : typescheme) : int * ltype =
  let rec replace (env : subst) : ltype -> ltype = function
    | TVar n when IntMap.mem n env -> IntMap.find n env
    | TFun (tl, tr) -> TFun (replace env tl, replace env tr)
    | t -> t
  in
  let phi v (n, s) = succ n, IntMap.add v (TVar n) s in
  let n', s = IntSet.fold phi pvs (n, IntMap.empty) in n', replace s ty
;;

let rec infer (n : int) (enva : assump) (envs : subst) :
  lexpr -> int * subst * ltype =
    let nt = TVar n in
    function
      | EVar str ->
        let n', t = instantiate n (StrMap.find str enva) in n', envs, t
      | EApp (expr1, expr2) ->
        let n1, s1, t1 = infer (succ n) enva envs expr1 in
        let n2, s2, t2 = infer n1 enva s1 expr2 in
        n2, solve s2 (t1, TFun (t2, nt)), nt
      | EAbs (ident, expr) ->
        let enva' = StrMap.add ident (IntSet.empty, nt) enva in
        let n', s, t = infer (succ n) enva' envs expr in
        n', s, TFun (nt, t)
      | ELet (ident, expr1, expr2) ->
        let n', s, t = infer n enva envs expr1 in
        infer n (StrMap.add ident (generalize n (expand_type s t)) enva) s expr2
;;

