
open Def

module IntMap = Core.Std.Int.Map;;
module StrMap = Core.Std.String.Map;;

type tconst = ltype * ltype;;
type assump = ltype StrMap.t;;
type subst = ltype IntMap.t;;

let rec infer (n : int) (env : assump) :
    lexpr -> (int * tconst list * ltype) option =
  function
    | EVar str ->
      begin match StrMap.find env str with
        | Some v -> Some (n, [], v)
        | None -> None
      end
    | EApp (expr1, expr2) ->
      begin match infer (succ n) env expr1 with
        | Some (n1, c1, t1) ->
          begin match infer n1 env expr2 with
            | Some (n2, c2, t2) ->
              Some (n2, (t1, TFun (t2, TVar n)) :: c1 @ c2, TVar n)
            | None -> None
          end
        | None -> None
      end
    | EAbs (ident, expr) ->
      begin match infer (succ n) (StrMap.add ident (TVar n) env) expr with
        | Some (n', c, t) -> Some (n', c, TFun (TVar n, t))
        | None -> None
      end
;;

let rec solve (env : subst) : tconst -> subst option =
  let rec occurs_check n t =
    match t with
      | TVar n' ->
        begin match IntMap.find env n' with
          | Some t' -> occurs_check n t'
          | None -> n = n'
        end
      | TFun (tl, tr) -> occurs_check n tl || occurs_check n tr
  in
  function
    | TVar n, TVar n' when n = n' -> Some env
    | TVar n, t | t, TVar n ->
      begin match IntMap.find env n with
        | Some t' -> solve env (t, t')
        | None when occurs_check n t -> None
        | None -> Some (IntMap.add n t env)
      end
    | TFun (t1l, t1r), TFun (t2l, t2r) ->
      begin match solve env (t1l, t2l) with
        | Some env' -> solve env' (t1r, t2r)
        | None -> None
      end
;;

let rec expand_type (env : subst) : ltype -> ltype =
  function
    | TVar n ->
      begin match IntMap.find env n with
        | Some t -> expand_type env t
        | None -> TVar n
      end
    | TFun (tl, tr) -> TFun (expand_type env tl, expand_type env tr)
;;
