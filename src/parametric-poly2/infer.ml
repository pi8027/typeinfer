
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

let substitute_assump (subst : subst) (assump : assump) : assump =
  String.Map.map (fun (vs, t) -> vs, substitute subst t) assump

let rec occurs_check (n : int) : ty -> bool =
  function
    | TVar n' -> n = n'
    | TFun (tl, tr) -> occurs_check n tl || occurs_check n tr

let rec unify (env : subst) : tconst list -> subst option =
  function
    | [] -> Some env
    | (TVar n, TVar n') :: cs when n = n' -> unify env cs
    | (TVar n, t) :: cs | (t, TVar n) :: cs ->
      let sub = substitute (Int.Map.singleton n t) in
      if occurs_check n t
        then None
        else unify
          (Int.Map.add n t (Int.Map.map sub env))
          (List.map cs (fun (l, r) -> sub l, sub r))
    | (TFun (t1l, t1r), TFun (t2l, t2r)) :: cs ->
      unify env ((t1l, t2l) :: (t1r, t2r) :: cs)

let merge_substs (s : subst list) : subst option =
  let f s = List.map (Int.Map.to_alist s) (fun (k, v) -> TVar k, v) in
  unify Int.Map.empty (List.concat (List.map s f))

let generalize (env : assump) (t : ty) : typescheme =
  let vs = String.Map.fold ~init:Int.Set.empty env
    ~f:(fun ~key:_ ~data:(_, t) m -> Int.Set.union (freevars t) m) in
  Int.Set.diff (freevars t) vs, t

let instantiate (n : int) (vs, ty : typescheme) : int * ty =
  let (n', s) = Int.Set.fold vs ~init:(n, Int.Map.empty)
    ~f:(fun v (n, s) -> (succ n, Int.Map.add v (TVar n) s)) in
  n', substitute s ty

let rec infer (n : int) (env : assump) : term -> (int * subst * ty) option =
  let open Option.Monad_infix in
  function
    | EVar str ->
      String.Map.find env str >>| fun ts ->
      let (n', ty) = instantiate n ts in (n', Int.Map.empty, ty)
    | EApp (term1, term2) ->
      infer (succ n) env term1 >>= fun (n1, s1, t1) ->
      infer n1 (substitute_assump s1 env) term2 >>= fun (n2, s2, t2) ->
      let tn = TVar n in
      unify Int.Map.empty [substitute s2 t1, TFun (t2, tn)] >>= fun s3 ->
      merge_substs [s1; s2; s3] >>| fun s4 ->
      n2, s4, substitute s3 tn
    | EAbs (ident, term) ->
      let tn = TVar n in
      let newenv = String.Map.add ident (Int.Set.empty, tn) env in
      infer (succ n) newenv term >>| fun (n', s, t) ->
      n', s, substitute s (TFun (tn, t))
    | ELet (ident, term1, term2) ->
      infer n env term1 >>= fun (n1, s1, t1) ->
      let env' = substitute_assump s1 env in
      let sigma = generalize env' t1 in
      let newenv = String.Map.add ident sigma env' in
      infer n1 newenv term2 >>= fun (n2, s2, t2) ->
      merge_substs [s1; s2] >>| fun s3 ->
      n2, s3, t2

