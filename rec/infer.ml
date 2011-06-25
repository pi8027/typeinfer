
module IntMap = Map.Make(
    struct
      type t = int
      let compare i j = i - j
    end
  );;

type subst = Def.ltype IntMap.t;;

exception Occurs_check;;

let rec find (f : 'a -> bool) : 'a list -> 'a option =
  function
    | [] -> None
    | x :: _ when f x -> Some x
    | _ :: xs -> find f xs
  ;;

let rec constraints (n : int) (env : (string * Def.ltype) list) :
  Def.lexpr -> int * (Def.ltype * Def.ltype) list * Def.ltype =
    let nt = Def.VarType n in
    function
      | Def.ERec (ident, expr1, expr2) ->
        let env' = (ident, nt) :: env in
        let n', c1, t1 = constraints (succ n) env' expr1 in
        let n'', c2, t2 = constraints n' env' expr2 in
          n'', (nt, t1) :: c1 @ c2, t2
      | Def.ELambda (ident, expr) ->
        let n', c, t = constraints (succ n) ((ident, nt) :: env) expr in
          n', c, Def.FunctionType (nt, t)
      | Def.EApply (expr1, expr2) ->
        let n1, c1, t1 = constraints (succ n) env expr1 in
        let n2, c2, t2 = constraints n1 env expr2 in
          n2, (t1, Def.FunctionType (t2, nt)) :: c1 @ c2, nt
      | Def.EVar str ->
        match find (fun t -> fst t = str) env with
          | Some (_, t) -> n, [], t
          | None -> raise Not_found
  ;;

let esubst : subst = IntMap.empty;;

let rec
  solve (env : subst) : Def.ltype * Def.ltype -> subst = function
    | Def.VarType n, t -> solve' env n t
    | t, Def.VarType n -> solve' env n t
    | Def.FunctionType (t1l, t1r), Def.FunctionType (t2l, t2r) ->
      solve (solve env (t1l, t2l)) (t1r, t2r)
  and
  solve' (env : subst) (n : int) (t : Def.ltype) : subst =
    let rec eq_check (n : int) : Def.ltype -> bool = function
      | Def.VarType n' when IntMap.mem n' env ->
        eq_check n (IntMap.find n' env)
      | Def.VarType n' -> n = n'
      | Def.FunctionType _ -> false
    in
    let rec occurs_check (n : int) : Def.ltype -> unit = function
      | Def.VarType n' when IntMap.mem n' env ->
        occurs_check n (IntMap.find n' env)
      | Def.VarType n' when n = n' -> raise Occurs_check
      | Def.VarType n' -> ()
      | Def.FunctionType (tl, tr) ->
        occurs_check n tl ; occurs_check n tr
    in
    let write (env : subst) (n : int) (t : Def.ltype) : subst =
      if eq_check n t
        then env
        else (occurs_check n t ; IntMap.add n t env)
    in
    if IntMap.mem n env
      then solve env (IntMap.find n env, t)
      else write env n t
  ;;

let rec expand_type (env : subst) : Def.ltype -> Def.ltype = function
  | Def.VarType n when IntMap.mem n env ->
    expand_type env (IntMap.find n env)
  | Def.VarType n -> Def.VarType n
  | Def.FunctionType (tl, tr) ->
    Def.FunctionType (expand_type env tl, expand_type env tr)
  ;;

