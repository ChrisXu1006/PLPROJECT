open Ast

(* Pretty printing helper functions *)
let print_ident x = 
  Format.printf "%s" x

let print_int n = 
  Format.printf "%d" n

let print_binop p s x y = 
  Format.printf "@[<2>(";
  p x;
  Format.printf "@ %s@ " s;
  p y;
  Format.printf ")@]" 

let print_lambda p x e = 
  Format.printf "@[<2>(lambda %s.@ " x;
  p e;
  Format.printf ")@]"

let print_if p e t f = 
  Format.printf "@[(if@ ";
  p e;
  Format.printf "@ then@ ";
  p t;
  Format.printf "@ else@ ";
  p f;
  Format.printf ")@]"

let print_let p x e1 e2 = 
  Format.printf "@[<2>let %s =@ " x; 
  p e1;
  Format.printf "@ in@ ";
  p e2;
  Format.printf "@]"

let print_fst p e = 
  Format.printf "@[#1 ";
  p e;
  Format.printf "@]"

let print_snd p e = 
  Format.printf "@[#2 ";
  p e;
  Format.printf "@]"

(* Pretty print expression e *)
let print_exp e = 
  let rec loop e = 
    match e with 
      | Var x -> print_ident x
      | App (l,r) -> print_binop loop "" l r 
      | Lam(x,e) -> print_lambda loop x e
      | Let(x,e1,e2) -> print_let loop x e1 e2 
      | Plus (l,r) -> print_binop loop "+" l r 
      | Unit -> print_ident "()"
      | Int n -> print_int n 
      | Pair(l,r) -> print_binop loop "," l r
      | Fst e -> print_fst loop e
      | Snd e -> print_snd loop e
      | True -> print_ident "true"
      | False -> print_ident "false"
      | If (e,t,f) -> print_if loop e t f
      | Eq (l,r) -> print_binop loop "=" l r in
  Format.printf "@[";
  loop e;
  Format.printf "@]"

(* Pretty print CPS expression e *)
let rec  print_cps_exp c = 
  match c with 
    | CAtom a -> print_cps_atom a
    | CApp(c,a) -> 
      Format.printf "@[<2>(";
      print_cps_exp c;
      Format.printf "@ ";
      print_cps_atom a;
      Format.printf ")@]"

(* pretty print CPS atom a *)
and print_cps_atom a = 
  let rec loop a = 
    match a with 
      | CVar(x) -> print_ident x
      | CLam(x,e) -> print_lambda print_cps_exp x e         
      | CUnit -> print_ident "()" 
      | CInt(n) -> print_int n
      | CPlus(x,y) -> print_binop print_ident "+" x y
      | CPair(x,y) -> print_binop print_ident "," x y
      | CFst(x) -> print_fst print_ident x
      | CSnd(x) -> print_snd print_ident x
      | CTrue -> print_ident "true"
      | CFalse -> print_ident "true"
      | CEq(x,y) -> print_binop print_ident "=" x y
      | CIf(x,y,z) -> print_if print_ident x y z in 
  Format.printf "@[";
  loop a;
  Format.printf "@]"

(* Pretty print value v *)
let print_val v = 
  let rec loop v = 
    match v with 
      | VUnit -> 
        Format.printf "()"
      | VInt n -> 
        Format.printf "%d" n
      | VClosure(_) -> 
        Format.printf "<fun>"
      | VPair(v1,v2) -> 
        Format.printf "@[(<2>"; 
        loop v1;
        Format.printf ",@ ";
        loop v2;
        Format.printf "@]"
      | VTrue -> 
        Format.printf "true"
      | VFalse -> 
        Format.printf "false" in 
  Format.printf "@[";
  loop v;
  Format.printf "@]"
