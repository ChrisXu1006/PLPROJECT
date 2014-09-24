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
type aconfiguration = store * aexp;;
type bconfiguration = store * bexp;;

(* Initialize the store for the program*)
let ( sigma : store ) = Hashtbl.create 200;;

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
	( sigma , c );;
    (* failwith "Not yet implemented" *)

(* evaluate an aexp *)
let rec evala (aconf:aconfiguration) : int = 
    match aconf with 
    | (sigma, Int               ari )-> ari
    | (sigma, Var               ari )-> Hashtbl.find sigma ari
    | (sigma, Plus    (aexp1, aexp2))-> (evala (sigma, aexp1)) + (evala (sigma, aexp2))
    | (sigma, Minus   (aexp1, aexp2))-> (evala (sigma, aexp1)) - (evala (sigma, aexp2))
    | (sigma, Times   (aexp1, aexp2))-> (evala (sigma, aexp1)) * (evala (sigma, aexp2))
    | (sigma, Input                 )-> print_endline ">";
                               			let i = read_int() in i

(* evaluate a bexp *)
let rec evalb (bconf:bconfiguration) : bool = 
    match bconf with
    | (sigma, True)                  -> true
    | (sigma, False)                 -> false
    | (sigma, Equals  (aexp1, aexp2))-> if ((evala (sigma, aexp1)) == (evala (sigma, aexp2))) then true else false
    | (sigma, NotEquals(aexp1,aexp2))-> if ((evala (sigma, aexp1)) <> (evala (sigma, aexp2))) then true else false
    | (sigma, Less    (aexp1, aexp2))-> if ((evala (sigma, aexp1)) <  (evala (sigma, aexp2))) then true else false
    | (sigma, LessEq  (aexp1, aexp2))-> if ((evala (sigma, aexp1)) <= (evala (sigma, aexp2))) then true else false
    | (sigma, Greater (aexp1, aexp2))-> if ((evala (sigma, aexp1)) >  (evala (sigma, aexp2))) then true else false
    | (sigma, GreaterEq(aexp1,aexp2))-> if ((evala (sigma, aexp1)) >= (evala (sigma, aexp2))) then true else false
    | (sigma, Not              bexp )-> if (evalb (sigma, bexp)) then false else true
    | (sigma, And     (bexp1, bexp2))-> (evalb (sigma, bexp1)) && (evalb (sigma, bexp2))
    | (sigma, Or      (bexp1, bexp2))-> (evalb (sigma, bexp1)) || (evalb (sigma, bexp2)) 

(* evaluate a command *)
let rec evalc (cconf:configuration) : store = 
    match cconf with 
    | (sigma, Skip)             ->  sigma
    | (sigma, Assign (x, a))    ->  if (Hashtbl.mem sigma x) 
									then Hashtbl.replace sigma x (evala (sigma, a))
									else Hashtbl.add sigma x (evala (sigma, a)); 
									sigma
    | (sigma, Seq  (c1, c2))    ->  (evalc (sigma, c1));
                                    (evalc (sigma, c2))
    | (sigma, If(b, c1, c2))    ->  if (evalb (sigma, b))
                                    then (evalc (sigma, c1))
                                    else (evalc (sigma, c2))
    | (sigma, While  (b, c))    ->  if (evalb (sigma, b))
                                    then (evalc (sigma, c);
                                    evalc (sigma, While(b, c)))
                                    else (evalc (sigma, Skip))
    | (sigma, Print a      )    ->  Printf.printf "%d\n" (evala (sigma, a)); sigma
    | (sigma, Test  (i,  b))    ->  if (evalb (sigma, b))
                                    then (evalc (sigma, Skip))
                                    else (Printf.printf "TestFailed\n";
                                    pprintInfo i;
                                    Printf.printf "\n";
                                    sigma)
     (*| _ failwith "Not yet implemented"*)


