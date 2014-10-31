open Ast

(* representation for variable sets *)
module VarSet = Set.Make(struct
  type t = var
  let compare = Pervasives.compare
end)
type varset = VarSet.t

(* calculate free variables of an expression e *)
let rec fvs_exp (e:exp) : VarSet.t = 
  match e with 
    | Var x -> 
      VarSet.singleton x
    | Lam(x,e) -> 
      VarSet.remove x (fvs_exp e)
    | Let(x,e1,e2) -> 
      VarSet.union (fvs_exp e1) (VarSet.remove x (fvs_exp e2))
    | Int _ | True | False | Unit -> 
      VarSet.empty
    | App(e1,e2) | Plus(e1,e2) | Pair(e1,e2) | Eq(e1,e2) -> 
      VarSet.union (fvs_exp e1) (fvs_exp e2)
    | Fst e | Snd e -> 
      fvs_exp e
    | If(e1,e2,e3) -> 
      VarSet.union (fvs_exp e1) (VarSet.union (fvs_exp e2) (fvs_exp e3))

(* calculate free variables of a CPS expression c *)
let rec fvs_cps_exp (c:cps_exp) : VarSet.t = 
  match c with 
    | CAtom a -> 
      fvs_cps_atom a
    | CApp(c1,a2) -> 
      VarSet.union (fvs_cps_exp c1) (fvs_cps_atom a2)

(* calculate free variables of a CPS atom a *)
and fvs_cps_atom (a:cps_atom) : VarSet.t = 
match a with 
  | CVar x -> 
    VarSet.singleton x
  | CLam(x,e) -> 
    VarSet.remove x (fvs_cps_exp e)
  | CUnit | CInt _ | CTrue | CFalse -> 
    VarSet.empty
  | CPlus(x,y) | CPair(x,y) | CEq(x,y) -> 
    VarSet.add x (VarSet.singleton y)
  | CFst(x) | CSnd(x) -> 
    VarSet.singleton x
  | CIf(x,y,z) -> 
    VarSet.add x
      (VarSet.add y 
         (VarSet.singleton z))

(* generate a variable that is similar to x but fresh for vs *)
let rec fresh (x:var) (xs:VarSet.t) : var = 
  let rec aux (x:var) (n:int) : var = 
    let x_n = x ^ "_" ^ string_of_int n in 
    if VarSet.mem x_n xs then 
      aux x (succ n) 
    else x_n in 
  if VarSet.mem x xs then 
    aux x 0
  else 
    x

