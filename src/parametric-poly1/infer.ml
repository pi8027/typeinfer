
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

let freevars_ts (vs, t : typescheme) : Int.Set.t =
  Int.Set.diff (freevars t) vs

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

let generalize (env : assump) (c : tconst list) (t : ty) :
    typescheme option =
  let vs = String.Map.fold ~init:Int.Set.empty env
    ~f:(fun ~key:_ ~data:t m -> Int.Set.union (freevars_ts t) m) in
  match unify Int.Map.empty c with
    | Some s ->
      let t' = substitute s t in
      Some (Int.Set.diff (freevars t') vs , t')
    | None -> None

let instantiate (n : int) (vs, ty : typescheme) : int * ty =
  let (n', s) = Int.Set.fold vs ~init:(n, Int.Map.empty)
    ~f:(fun v (n, s) -> (succ n, Int.Map.add v (TVar n) s)) in
  (n', substitute s ty)

let rec constraints (n : int) (env : assump) :
    term -> (int * tconst list * ty) option =
  function
    | EVar str ->
      begin match String.Map.find env str with
        | Some ts ->
          let (n', ty) = instantiate n ts in Some (n', [], ty)
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
      let tn = TVar n in
      let newenv = String.Map.add ident (Int.Set.empty, tn) env in
      begin match constraints (succ n) newenv term with
        | Some (n', c, t) -> Some (n', c, TFun (tn, t))
        | None -> None
      end
    | ELet (ident, term1, term2) ->
      begin match constraints n env term1 with
        | Some (n1, c1, t1) ->
          begin match generalize env c1 t1 with
            | Some ts ->
              let newenv = String.Map.add ident ts env in
              begin match constraints n1 newenv term2 with
                | Some (n2, c2, t2) -> Some (n2, c1 @ c2, t2)
                | None -> None
              end
            | None -> None
          end
        | None -> None
      end

let type_inference (e : term) : ty option =
  match constraints 0 String.Map.empty e with
    | Some (_, c, t) ->
      begin match unify Int.Map.empty c with
        | Some s -> Some (substitute s t)
        | None -> None
      end
    | None -> None

