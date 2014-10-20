open Ast

(* lexp_of_aexp: convert an aexp into an lexp *)
let rec lexp_of_aexp (a:aexp) : lexp = 
  match a with 
    | Int n -> 
      LInt n
    | Var x -> 
      LPVar x 
    | Plus(a1,a2) -> 
      LPlus(lexp_of_aexp a1, lexp_of_aexp a2)
    | Minus(a1,a2) -> 
      LMinus(lexp_of_aexp a1, lexp_of_aexp a2)
    | Times(a1,a2) -> 
      LTimes(lexp_of_aexp a1, lexp_of_aexp a2)

(* assn_of_bexp: convert a bexp into a assn *)
let rec assn_of_bexp (b:bexp) : assn = 
  match b with 
    | True -> 
      ATrue
    | False -> 
      AFalse
    | Equals(a1,a2) -> 
      AEquals(lexp_of_aexp a1, lexp_of_aexp a2)
    | NotEquals(a1,a2) -> 
      ANotEquals(lexp_of_aexp a1, lexp_of_aexp a2)
    | Less(a1,a2) -> 
      ALess(lexp_of_aexp a1, lexp_of_aexp a2)
    | LessEq(a1,a2) -> 
      ALessEq(lexp_of_aexp a1, lexp_of_aexp a2)
    | Greater(a1,a2) -> 
      AGreater(lexp_of_aexp a1, lexp_of_aexp a2)
    | GreaterEq(a1,a2) -> 
      AGreaterEq(lexp_of_aexp a1, lexp_of_aexp a2)
    | Not(b1) -> 
      ANot(assn_of_bexp b1)
    | And(b1,b2) -> 
      AAnd(assn_of_bexp b1, assn_of_bexp b2)
    | Or(b1,b2) -> 
      AOr(assn_of_bexp b1, assn_of_bexp b2)

(* substLexp l x l1: substitute l for x in l1 *)
let rec substLexp (l:lexp) (x:var) (l1:lexp) : lexp = 
  match l1 with 
    | LInt n -> 
      LInt n
    | LLVar i -> 
      LLVar i 
    | LPVar y -> 
      if y = x then l
      else LPVar y
    | LPlus(l1,l2) -> 
      LPlus(substLexp l x l1, substLexp l x l2)
    | LMinus(l1,l2) -> 
      LMinus(substLexp l x l1, substLexp l x l2)
    | LTimes(l1,l2) -> 
      LTimes(substLexp l x l1, substLexp l x l2)

(* substAssn l x p: substitute l for x in p *)
and substAssn (l:lexp) (x:var) (p:assn) : assn = 
  match p with 
    | ATrue -> ATrue
    | AFalse -> AFalse
    | AEquals(l1,l2) -> 
      AEquals(substLexp l x l1, substLexp l x l2)
    | ANotEquals(l1,l2) -> 
      ANotEquals(substLexp l x l1, substLexp l x l2)
    | ALess(l1,l2) -> 
      ALess(substLexp l x l1, substLexp l x l2)
    | ALessEq(l1,l2) -> 
      ALessEq(substLexp l x l1, substLexp l x l2)
    | AGreater(l1,l2) -> 
      AGreater(substLexp l x l1, substLexp l x l2)
    | AGreaterEq(l1,l2) -> 
      AGreaterEq(substLexp l x l1, substLexp l x l2)
    | AAnd(p1,p2) -> 
      AAnd(substAssn l x p1, substAssn l x p2)
    | AOr(p1,p2) -> 
      AOr(substAssn l x p1, substAssn l x p2)
    | ANot(p1) ->
      ANot(substAssn l x p1)
    | AImplies(p1,p2) -> 
      AImplies(substAssn l x p1, substAssn l x p2)
    | AForall(i,p1) -> 
      AForall(i,substAssn l x p1)
    | AExists(i,p1) -> 
      AExists(i,substAssn l x p1)

let rec gens ((pre,sc,post): assn * scom * assn) : assn list = 
  failwith "Kurt"

and genc ((pre,c,post): assn * com * assn) : assn list = 
  failwith "Godel"
