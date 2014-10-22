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
    match sc with
    | Skip              ->
      [AImplies( pre, post )]
    | Print (a)         ->
      [AImplies( pre, post )]
    | Test (info, b)    ->
      [AImplies ( pre, AAnd( assn_of_bexp(b), post ) )]
    | Assign ( var, a)  ->
      let la = lexp_of_aexp(a) in
      [AImplies ( pre, substAssn la var post )]
	(*| _ -> failwith "Kurt"*)
            
and genc ((pre,c,post): assn * com * assn) : assn list = 
    match c with 
    | Simple (sc)           ->
      gens( pre, sc, post )
    | SeqSimple ( c1, s2 )  ->
      (match s2 with
      | Skip            ->
        genc( pre, c1, post )
      | Print (a)       ->
        genc( pre, c1, post )
      | Test (info, b)  ->
        let middle = AAnd ( assn_of_bexp(b), post ) in
        genc( pre, c1, middle)
      | Assign (var, a) ->
        let la = lexp_of_aexp(a) in
        let middle = substAssn la var post in
        genc( pre, c1, middle))
	| Seq (c1, p, c2)      ->
	  genc(pre, c1, p) @ genc(p, c2, post)
	| If ( b, c1, c2 )      ->
      let assnb = assn_of_bexp(b) in   
      genc(AAnd( assnb, pre ), c1, post) @ genc(AAnd ( ANot(assnb), pre ), c2, post)
	| While (b, p, c)       ->
	  let assnb = assn_of_bexp(b) in
	  [AImplies(pre, p)] @ [AImplies( AAnd( p, ANot(assnb) ), post )] @ genc(AAnd( p, assnb ), c, p)	
    (*| _ -> failwith "Godel"*)
