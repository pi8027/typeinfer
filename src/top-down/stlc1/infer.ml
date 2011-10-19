
open Def

module IntMap = Core.Std.Int.Map;;
module StrMap = Core.Std.String.Map;;

type tconst = ty * ty;;
type assump = ty StrMap.t;;
type subst = ty IntMap.t;;

let rec constraints (n : int) (env : assump) :
    term -> (int * tconst list * ty) option =
  function
    | EVar str ->
      begin match StrMap.find env str with
        | Some t -> Some (n, [], t)
        | None -> None
      end
    | EApp (term1, term2) ->
      begin match constraints (succ n) env term1 with
        | Some (n1, c1, t1) ->
          begin match constraints n1 env term2 with
            | Some (n2, c2, t2) ->
              let tn = TVar n in
              Some (n2, (t1, TFun (t2, tn)) :: c1 @ c2, tn)
            | None -> None
          end
        | None -> None
      end
    | EAbs (ident, term) ->
      begin
        let tn = TVar n in
        match constraints (succ n) (StrMap.add ident tn env) term with
          | Some (n', c, t) -> Some (n', c, TFun (tn, t))
          | None -> None
      end
;;

let rec substitute (s : subst) : ty -> ty =
  function
    | TVar n ->
      begin match IntMap.find s n with
        | Some t -> t
        | None -> TVar n
      end
    | TFun (tl, tr) -> TFun (substitute s tl, substitute s tr)
;;

let rec occurs_check (n : int) : ty -> bool =
  function
    | TVar n' -> n = n'
    | TFun (tl, tr) -> occurs_check n tl || occurs_check n tr
;;

let rec unify (env : subst) : tconst list -> subst option =
  function
    | [] -> Some env
    | (TVar n, TVar n') :: cs when n = n' -> unify env cs
    | (TVar n, t) :: cs | (t, TVar n) :: cs ->
      let sub = substitute (IntMap.singleton n t) in
      if occurs_check n t
        then None
        else unify
          (IntMap.add n t (IntMap.map sub env))
          (List.map (fun (l, r) -> sub l, sub r) cs)
    | (TFun (t1l, t1r), TFun (t2l, t2r)) :: cs ->
      unify env ((t1l, t2l) :: (t1r, t2r) :: cs)
;;

let type_inference (e : term) : ty option =
  match constraints 0 StrMap.empty e with
    | Some (_, c, t) ->
      begin match unify IntMap.empty c with
        | Some s -> Some (substitute s t)
        | None -> None
      end
    | None -> None
;;
