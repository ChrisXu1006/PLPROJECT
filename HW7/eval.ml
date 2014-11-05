open Ast
open Pprint

exception IllformedExpression

(* evaluate CPS expression e in environment g *)
let rec eval (g:cps_env) (c:cps_exp) : cps_val = 
  match c with
  | CApp(e1, e2) ->
      let v1 = eval g e1 in
      (
        match v1 with 
        | VClosure(g1, x, ce) ->
            let v2 = eval_atom g e2 in 
                eval (extend g1 x v2 ) ce
        | _ -> raise IllformedExpression
      )
  | CAtom(c)     -> eval_atom g c 

(* evaluate CPS atom a in environment g *) 
and eval_atom (g:cps_env) (a:cps_atom) : cps_val = 
  match a with 
    | CVar(x)       -> lookup g x
    | CLam(x, ce)   -> VClosure(g, x, ce) 
    | CUnit         -> VUnit
    | CInt(n)       -> VInt(n)
    | CPlus(v1,v2)  ->
        let n = lookup g v1 in 
            ( match n with
              | VInt(n1) ->
                let m = lookup g v2 in
                ( match m with
                  | VInt(m1) -> VInt(n1+m1)
                  | _ -> raise IllformedExpression
                )
              | _ -> raise IllformedExpression
            )
    | CPair(v1,v2)  -> VPair((lookup g v1),(lookup g v2))
    | CFst(pair)    -> 
        let v = lookup g pair in
        ( match v with
          | VPair(v1, v2) -> v1
          | _ -> raise IllformedExpression
        )
    | CSnd(pair)    ->
        let v = lookup g pair in
        ( match v with
          | VPair(v1, v2) -> v2
          | _ -> raise IllformedExpression
        )
    | CTrue         -> VTrue
    | CFalse        -> VFalse
    | CEq(v1, v2)   -> if (lookup g v1) = (lookup g v2) then VTrue else VFalse
    | CIf(b, v1, v2)-> if (lookup g b) = VTrue then lookup g v1 else lookup g v2

