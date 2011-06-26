
module IntMap = Map.Make(
    struct
      type t = int
      let compare i j = i - j
    end
  );;

module StrMap = Map.Make(String);;

type subst = Def.ltype IntMap.t;;

exception Occurs_check;;

let rec constraints (n : int) (env : Def.ltype StrMap.t) :
  Def.lexpr -> int * (Def.ltype * Def.ltype) list * Def.ltype =
    let nt = Def.VarType n in
    function
      | Def.ERec (ident, expr1, expr2) ->
        let env' = StrMap.add ident nt env in
        let n', c1, t1 = constraints (succ n) env' expr1 in
        let n'', c2, t2 = constraints n' env' expr2 in
          n'', (nt, t1) :: c1 @ c2, t2
      | Def.ELambda (ident, expr) ->
        let n', c, t = constraints (succ n) (StrMap.add ident nt env) expr in
          n', c, Def.FunctionType (nt, t)
      | Def.EApply (expr1, expr2) ->
        let n1, c1, t1 = constraints (succ n) env expr1 in
        let n2, c2, t2 = constraints n1 env expr2 in
          n2, (t1, Def.FunctionType (t2, nt)) :: c1 @ c2, nt
      | Def.EVar str -> n, [], StrMap.find str env
  ;;

let rec solve (env : subst) : Def.ltype * Def.ltype -> subst =
  let rec occurs_check (n : int) : Def.ltype -> unit = function
    | Def.VarType n' when IntMap.mem n' env ->
      occurs_check n (IntMap.find n' env)
    | Def.VarType n' -> if n = n' then raise Occurs_check else ()
    | Def.FunctionType (tl, tr) -> occurs_check n tl ; occurs_check n tr
  in
  function
    | Def.VarType n, Def.VarType n' when n = n' -> env
    | Def.VarType n, t | t, Def.VarType n when IntMap.mem n env ->
      solve env (IntMap.find n env, t)
    | Def.VarType n, t | t, Def.VarType n ->
      occurs_check n t ; IntMap.add n t env
    | Def.FunctionType (t1l, t1r), Def.FunctionType (t2l, t2r) ->
      solve (solve env (t1l, t2l)) (t1r, t2r)
  ;;

let rec expand_type (env : subst) : Def.ltype -> Def.ltype = function
  | Def.VarType n when IntMap.mem n env ->
    expand_type env (IntMap.find n env)
  | Def.VarType n -> Def.VarType n
  | Def.FunctionType (tl, tr) ->
    Def.FunctionType (expand_type env tl, expand_type env tr)
  ;;

