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

type klist = (com * com) list

(* A type for configurations *)
type configuration = store * com * com * klist;;
type aconfiguration = store * aexp;;
type bconfiguration = store * bexp;;

(* Initialize the store for the program*)
let ( sigma : store ) = Hashtbl.create 200;;

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
	( sigma , c, Skip, [] );;
    (* failwith "Not yet implemented" *)

(* evaluate an aexp *)
let rec evala (aconf:aconfiguration) : int = 
    match aconf with 
    | (sigma, Int               ari )-> ari
    | (sigma, Var               ari )-> (try Hashtbl.find sigma ari with _ -> raise (UnboundVariable ari)); 
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
    | (sigma, Skip, Skip, l)                ->  sigma

	| (sigma, Skip, c, l) when c != Skip    ->  evalc (sigma, c, Skip, l)

	| (sigma, Assign (x, a), c, l)          ->  if (Hashtbl.mem sigma x) 
											    then Hashtbl.replace sigma x (evala (sigma, a))
											    else Hashtbl.add sigma x (evala (sigma, a)); 
											    evalc (sigma, c, Skip, l)
	| (sigma, Seq(c1, c2), Skip, l)         ->  evalc (sigma, c1, c2, l)

	| (sigma, Seq(c1, c2), cprime, l) 
    when cprime != Skip                     -> evalc (sigma, c1, Seq(c2, cprime), l)

	| (sigma, If(b, c1, c2), cprime, l)     ->  if (evalb (sigma, b)) 
										        then evalc (sigma, c1, cprime, l)
										        else evalc (sigma, c2, cprime, l)

	(* | (sigma, While(b, c), Skip, l)         ->  if (evalb (sigma, b)) then (evalc (sigma, c, While(b,c), l))
										        else evalc (sigma, Skip, Skip, l) *)
	| (sigma, While(b, c), cprime, l)       ->  let lprime = 
												(if (List.length l == 0) then [(cprime, While(b,c))]
												else let ccontinue = snd (List.hd l) in
													(if (ccontinue = While(b, c)) 
														then l 
														else [(cprime, While(b,c))]@l)) in
												if (evalb (sigma, b)) 
										        then (evalc (sigma, c, While(b,c), lprime))
										        else (let lpop = List.tl lprime in evalc (sigma, cprime, Skip, lpop))

 	| (sigma, Break, cprime, l)             -> 	if (List.length l == 0) then (raise IllegalBreak; sigma)
									            else let cbreak = fst (List.hd l) in 
										        let lprime = List.tl l in
										        evalc (sigma, cbreak, Skip, lprime);

  	| (sigma, Continue, cprime, l)          -> 	if (List.length l == 0) then (raise IllegalContinue; sigma)
 									            else let ccontinue = snd (List.hd l) in 
 										        evalc (sigma, ccontinue, (fst (List.hd l)), l);

	| (sigma, Print a, cprime, l)           ->   Printf.printf "%d\n" (evala (sigma, a)); evalc (sigma, cprime, Skip, l)

	| (sigma, Test (i,  b), cprime, l)      ->   if (evalb (sigma, b))
                                                then (
                                                      evalc (sigma, cprime, Skip, l))
	                                            else (
                                                Printf.printf "TestFailed\n";
	                                            pprintInfo i;
                                                Printf.printf "\n";
                                                raise (TestFailure "TestFailed");
	                                            sigma) 											 
