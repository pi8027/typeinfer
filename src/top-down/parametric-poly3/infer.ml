
open Core.Std
open Def

type tconst = ty * ty
type typescheme = Int.Set.t * ty
type assump = typescheme String.Map.t
type subst = ty Int.Map.t

let rec freevars : ty -> Int.Set.t =
  function
    | TVar n -> Int.Set.singleton n
    | TFun (tl, tr) -> Int.Set.union (freevars tl) (freevars tr)

let rec substitute (s : subst) : ty -> ty =
  function
    | TVar n ->
      begin match Int.Map.find s n with
        | Some t -> t
        | None -> TVar n
      end
    | TFun (tl, tr) -> TFun (substitute s tl, substitute s tr)

let rec occurs_check (n : int) : ty -> bool =
  function
    | TVar n' -> n = n'
    | TFun (tl, tr) -> occurs_check n tl || occurs_check n tr

let rec
  unify (env : subst) : tconst list -> subst option =
    function
      | [] -> Some env
      | (TVar n, TVar n') :: cs when n = n' -> unify env cs
      | (TVar n, TVar n') :: cs when n < n' ->
        unify' n' (TVar n) env cs
      | (TVar n, TVar n') :: cs when n' < n ->
        unify' n (TVar n') env cs
      | (TVar n, t) :: cs | (t, TVar n) :: cs ->
        if occurs_check n t then None else unify' n t env cs
      | (TFun (t1l, t1r), TFun (t2l, t2r)) :: cs ->
        unify env ((t1l, t2l) :: (t1r, t2r) :: cs)
  and
  unify' (n : int) (t : ty) (env : subst) (cs : tconst list) :
      subst option =
    let f = substitute (Int.Map.singleton n t) in
    unify (Int.Map.add n t (Int.Map.map f env))
      (List.map cs (fun (l, r) -> f l, f r))

let generalize (n : int) (t : ty) : typescheme =
  Int.Set.filter (freevars t) ((<=) n) , t

let instantiate (n : int) (vs, ty : typescheme) : int * ty =
  let (n', s) = Int.Set.fold vs ~init:(n, Int.Map.empty)
    ~f:(fun v (n, s) -> (succ n, Int.Map.add v (TVar n) s)) in
  (n', substitute s ty)

let rec
  infer' (n : int) (enva : assump) (envs : subst) :
      term -> (int * subst * ty) option =
    function
      | EVar str ->
        begin match String.Map.find enva str with
          | Some ts ->
            let (n', ty) = instantiate n ts in
            Some (n', Int.Map.empty, ty)
          | None -> None
        end
      | EApp (term1, term2) ->
        begin match infer (succ n) enva envs term1 with
          | Some (n1, s1, t1) ->
            begin match infer n1 enva s1 term2 with
              | Some (n2, s2, t2) ->
                let tn = TVar n in
                begin match unify s2 [t1, TFun (t2, tn)] with
                  | Some s3 -> Some (n2, s3, tn)
                  | None -> None
                end
              | None -> None
            end
          | None -> None
        end
      | EAbs (ident, term) ->
        let tn = TVar n in
        let newenva = String.Map.add ident (Int.Set.empty, tn) enva in
        begin match infer (succ n) newenva envs term with
          | Some (n', s, t) -> Some (n', s, TFun (tn, t))
          | None -> None
        end
      | ELet (ident, term1, term2) ->
        begin match infer n enva envs term1 with
          | Some (n1, s1, t1) ->
            infer n1 (String.Map.add ident (generalize n t1) enva) s1 term2
          | None -> None
        end
  and
  infer (n : int) (enva : assump) (envs : subst) (term : term) :
      (int * subst * ty) option =
    match infer' n enva envs term with
      | Some (n', s, t) -> Some (n', s, substitute s t)
      | None -> None

