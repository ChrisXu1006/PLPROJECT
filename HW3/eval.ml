open Ast

(* Interpreter exceptions *)
exception IllegalBreak
exception IllegalContinue
exception TestFailure of string
exception UnboundVariable of var
    
(* A type for stores *)
type store = (* FILL ME IN *) unit

(* A type for configurations *)
type configuration = (* FILL ME IN *) unit

(* create an initial configuration from a command *)
let make_configuration (c:com) : configuration = 
  failwith "Not yet implemented"

(* evaluate a command *)
let rec evalc (conf:configuration) : store = 
  failwith "Not yet implemented"
