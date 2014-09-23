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
let rec evala (arithmetic:aexp) : int = 
    match arithmetic with 
    | Int arithmetic -> arithmetic

(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
    match (snd conf) with 
    | Assign (left, right) ->  Hashtbl.add sigma left (evala right);
      print_int (Hashtbl.find sigma left);
      sigma  
     (*| _ failwith "Not yet implemented"*)


