open Ast
open Hashtbl
open Printf
open Pprint

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var
    
(* A type for stores *)
type store =
    ( var, int ) Hashtbl.t;;
    
(* A type for configurations *)
type configuration = store * com;;

(* Initialize the store for the program*)
let ( sigma : store ) = Hashtbl.create 200;;

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
    ( sigma , c );;
    (* failwith "Not yet implemented" *)

(* evaluate an aexp *)
let rec evala (ari:aexp) : int = 
    match ari with 
    | Int               ari -> ari
    | Var               ari -> Hashtbl.find sigma ari
    | Plus    (aexp1, aexp2)-> (evala aexp1) + (evala aexp2)
    | Minus   (aexp1, aexp2)-> (evala aexp1) - (evala aexp2)
    | Times   (aexp1, aexp2)-> (evala aexp1) * (evala aexp2)
    | Input                 -> print_endline ">";
                               let i = read_int() in i

(* evaluate a bexp *)
let rec evalb (boo:bexp) : bool = 
    match boo with
    | True                  -> true
    | False                 -> false
    | Equals  (aexp1, aexp2)-> if ((evala aexp1) == (evala aexp2)) then true else false
    | NotEquals(aexp1,aexp2)-> if ((evala aexp1) <> (evala aexp2)) then true else false
    | Less    (aexp1, aexp2)-> if ((evala aexp1) <  (evala aexp2)) then true else false
    | LessEq  (aexp1, aexp2)-> if ((evala aexp1) <= (evala aexp2)) then true else false
    | Greater (aexp1, aexp2)-> if ((evala aexp1) >  (evala aexp2)) then true else false
    | GreaterEq(aexp1,aexp2)-> if ((evala aexp1) >= (evala aexp2)) then true else false
    | Not              boo  -> if (evalb boo) then false else true
    | And     (bexp1, bexp2)-> (evalb bexp1) && (evalb bexp2)
    | Or      (bexp1, bexp2)-> (evalb bexp1) || (evalb bexp2) 

(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
    match conf with 
    | (sigma, Skip)             ->  sigma
    | (sigma, Assign (x, a))    ->  if (Hashtbl.mem sigma x) 
									then Hashtbl.replace sigma x (evala a)
									else Hashtbl.add sigma x (evala a); 
									sigma
    | (sigma, Seq  (c1, c2))    ->  (evalc (sigma, c1));
                                    (evalc (sigma, c2))
    | (sigma, If(b, c1, c2))    ->  if (evalb b)
                                    then (evalc (sigma, c1))
                                    else (evalc (sigma, c2))
    | (sigma, While  (b, c))    ->  if (evalb b)
                                    then (evalc (sigma, c);
                                    evalc (sigma, While(b, c)))
                                    else (evalc (sigma, Skip))
    | (sigma, Print a      )    ->  sp "%d\n" (evala a); sigma
    | (sigma, Test  (i,  b))    ->  if (evalb b)
                                    then (evalc (sigma, Skip))
                                    else (Printf.printf "TestFailed\n";
                                    pprintInfo i;
                                    Printf.printf "\n";
                                    sigma)
     (*| _ failwith "Not yet implemented"*)


