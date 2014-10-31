type var = string
type loc = int

type exp =
    Var of var
  | App of exp * exp
  | Lam of var * exp
  | Let of var * exp * exp
  | Unit 
  | Int of int
  | Plus of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | True
  | False
  | Eq of exp * exp
  | If of exp * exp * exp

type cps_exp = 
    CApp of cps_exp * cps_atom
  | CAtom of cps_atom 

and cps_atom = 
    CVar of var
  | CLam of var * cps_exp
  | CUnit 
  | CInt of int
  | CPlus of var * var 
  | CPair of var * var
  | CFst of var
  | CSnd of var
  | CTrue
  | CFalse
  | CEq of var * var
  | CIf of var * var * var

and cps_env = var -> cps_val

and cps_val = 
  | VUnit 
  | VInt of int
  | VClosure of cps_env * var * cps_exp 
  | VPair of cps_val * cps_val
  | VTrue 
  | VFalse

exception UnboundVariable of var

let empty = (fun x -> raise (UnboundVariable x))
let lookup g x = g x
let extend g x v = (fun y -> if x = y then v else g y)     
