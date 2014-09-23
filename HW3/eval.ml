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
    | Int m -> m
	| Var x -> Hashtbl.find sigma x
    | Plus(a1, a2) -> (evala a1) + (evala a2)
  	| Minus(a1, a2) -> (evala a1) - (evala a2)
  	| Times(a1, a2) -> (evala a1) * (evala a2)
  	| Input -> sp ">"; 1   

(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
    match (snd conf) with 
    | Assign (x, a) ->  Hashtbl.add sigma x (evala a); sigma  
     (*| _ failwith "Not yet implemented"*)


