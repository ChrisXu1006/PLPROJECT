open Ast
open Fvs

let rec cps (e:exp) (k:cps_atom) : cps_exp =
  match e with
    | Var(x)        -> CApp(CAtom(k), CVar(x))
    | App(e1, e2)   -> 
        let f = fresh "f" (VarSet.union (fvs_exp e2) (fvs_cps_atom k)) in
            let v = fresh "v" (fvs_cps_atom k) in
                let k0 = CLam(f, (cps e2 (CLam(v, CApp(CApp(CAtom(CVar(f)), CVar(v)) ,k))))) in
                cps e1 k0
    | Lam(x, e)     ->
        let k_prime = fresh "k0" (VarSet.union (fvs_exp e) (fvs_cps_atom k)) in
            let k0 = CLam(x, CAtom(CLam(k_prime, cps e (CVar(k_prime))))) in
                CApp(CAtom(k), k0)
    | Let(x, e1, e2)->
        cps (App((Lam(x,e2)), e1)) k
    | Unit          -> CApp(CAtom(k), CUnit)
    | Int(n)        -> CApp(CAtom(k), CInt(n)) 
    | Plus(e1, e2)  -> 
        let n = fresh "n" (VarSet.union (fvs_exp e2) (fvs_cps_atom k)) in
            let m = fresh "m" (fvs_cps_atom k) in
                let k0 = CLam(n, (cps e2 (CLam(m, CApp(CAtom(k), CPlus(n,m)))))) in
                    cps e1 k0
    | Pair(e1, e2)  -> 
        let v = fresh "v" (VarSet.union (fvs_exp e2) (fvs_cps_atom k)) in
            let w = fresh "w" (fvs_cps_atom k) in
                let k0 = CLam(v, (cps e2 (CLam(w, CApp(CAtom(k), CPair(v,w)))))) in
                    cps e1 k0
    | Fst(e)        -> 
        let v = fresh "v" (fvs_cps_atom k) in
            let k0 = CLam(v, CApp(CAtom(k), CFst(v))) in 
                cps e k0
    | Snd(e)        ->
        let v = fresh "v" (fvs_cps_atom k) in
            let k0 = CLam(v, CApp(CAtom(k), CSnd(v))) in
                cps e k0
    | True          -> CApp(CAtom(k), CTrue)
    | False         -> CApp(CAtom(k), CFalse)
    | Eq(e1, e2)    ->
        let n = fresh "n" (VarSet.union (fvs_exp e2) (fvs_cps_atom k)) in
            let m = fresh "m" (fvs_cps_atom k) in
                let k0 = CLam(n, (cps e2 (CLam(m, CApp(CAtom(k), CEq(n,m)))))) in
                    cps e1 k0
    | If(e1, e2, e3)->
        let b = fresh "b" (VarSet.union (VarSet.union (fvs_exp e2) (fvs_exp e3)) (fvs_cps_atom k)) in
            let c = fresh "c" (VarSet.union (fvs_exp e3) (fvs_cps_atom k)) in
                let d = fresh "d" (fvs_cps_atom k) in
                    let k0 =  CLam(c, cps e3 (CLam(d, CApp(CAtom(k), CIf(b, c, d))))) in
                        let k1 = CLam(b, cps e2 k0) in
                            cps e1 k1
    (*| _ -> failwith "John C. Reynolds"*)
