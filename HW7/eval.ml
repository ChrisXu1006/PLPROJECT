open Ast

exception IllformedExpression 

(* evaluate CPS expression e in environment g *)
let rec eval (g:cps_env) (c:cps_exp) : cps_val = 
  match c with
  | CApp(CAtom(e1), e2) ->
    (match e1 with
     | CLam(x, ce) -> eval (extend g x (eval_atom g e2)) ce
     | _ -> raise IllformedExpression
    ) 
  | CAtom(c)        -> eval_atom g c 
  | _ ->failwith "Adriaan van Wijngaarden"

(* evaluate CPS atom a in environment g *) 
and eval_atom (g:cps_env) (a:cps_atom) : cps_val = 
  match a with 
    | CVar(x)       -> lookup g x
    | CLam(x, ce)   -> VClosure(g, x, ce) 
    | CUnit         -> VUnit
    | CInt(n)       -> VInt(n)
    | CPlus(v1,v2)  ->
        let VInt(n) = lookup g v1 in 
            let VInt(m) = lookup g v2 in
                VInt(n+m)
    | CPair(v1,v2)  -> VPair((lookup g v1),(lookup g v2))
    | CFst(pair)    -> 
        let VPair(v1, v2) = lookup g pair in v1
    | CSnd(pair)    ->
        let VPair(v1, v2) = lookup g pair in v2
    | CTrue         -> VTrue
    | CFalse        -> VFalse
    | CEq(v1, v2)   -> if (lookup g v1) = (lookup g v2) then VTrue else VFalse
    | CIf(b, v1, v2)-> if (lookup g b) = VTrue then lookup g v1 else lookup g v2
    | _ -> failwith "F. Lockwood Morris"
