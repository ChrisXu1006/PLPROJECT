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

(* evaluate a aexp *)
let rec evala (a:aexp) : int = 
    match a with 
    | Int                 m -> m
	| Var                 x -> Hashtbl.find sigma x
    | Plus         (a1, a2) -> (evala a1) + (evala a2)
  	| Minus        (a1, a2) -> (evala a1) - (evala a2)
  	| Times        (a1, a2) -> (evala a1) * (evala a2)
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
    match (snd conf) with 
    | Assign (x, a) ->  Hashtbl.add sigma x (evala a); sigma  
     (*| _ failwith "Not yet implemented"*)


