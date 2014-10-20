open Ast 

let valid (a:assn) : bool option = 
  let buf = Buffer.create 1000 in
  let binop f o x1 x2 = 
    Printf.bprintf buf "(%s " o;
    f x1;
    Printf.bprintf buf " ";
    f x2;
    Printf.bprintf buf ")" in
  let rec serialize_lexp = function
    | LInt(n) -> 
      Printf.bprintf buf "%d" n
    | LLVar(i) -> 
      Printf.bprintf buf "%s" i
    | LPVar(x) -> 
      Printf.bprintf buf "%s" x
    | LPlus(e1,e2) ->
      binop serialize_lexp "+" e1 e2
    | LMinus(e1,e2) -> 
      binop serialize_lexp "-" e1 e2
    | LTimes(e1,e2) -> 
      binop serialize_lexp "*" e1 e2
  and serialize_assn = function
    | ATrue ->
      Printf.bprintf buf "true"
    | AFalse ->
      Printf.bprintf buf "false"
    | AEquals (e1,e2) ->
      binop serialize_lexp "=" e1 e2
    | ANotEquals (e1,e2) ->
      serialize_assn (ANot (AEquals(e1,e2)))
    | ALess (e1,e2) ->
      binop serialize_lexp "<" e1 e2
    | ALessEq (e1,e2) ->
      binop serialize_lexp "<=" e1 e2
    | AGreater (e1,e2) ->
      binop serialize_lexp ">" e1 e2
    | AGreaterEq (e1,e2) ->
      binop serialize_lexp ">=" e1 e2
    | AAnd (a1,a2) ->
      binop serialize_assn "and" a1 a2
    | AOr (a1,a2) ->
      binop serialize_assn "or" a1 a2
    | ANot(a) ->
      Printf.bprintf buf "(not ";
      serialize_assn a;
      Printf.bprintf buf ")"
    | AImplies(a1,a2) ->
      binop serialize_assn "=>" a1 a2
    | AForall(x,a) ->
      Printf.bprintf buf "(forall ((%s Int)) " x;
      serialize_assn a;
      Printf.bprintf buf ")"
    | AExists(x,a) ->
      Printf.bprintf buf "(exists ((%s Int)) " x;
      serialize_assn a;
      Printf.bprintf buf ")" in 
  begin 
    VarSet.iter (fun x -> Printf.bprintf buf "(declare-var %s Int)\n" x) (fvsAssn a);
    Printf.bprintf buf "(assert ";
    serialize_assn (ANot a);
    Printf.bprintf buf ")\n";
    Printf.bprintf buf "(check-sat)\n";
    let str = Buffer.contents buf in 
    let err o e f a = Printf.printf "%s: %s|%s|%s\n" o (Unix.error_message e) f a; exit 1 in
    let z3_out,z3_in = 
      try Unix.open_process "z3 -in -smt2 -nw"
      with Unix.Unix_error (e,f,a) -> err "open" e f a in
    let _ = try output_string z3_in str
      with Unix.Unix_error (e,f,a) -> err "out" e f a in
    let _ = try flush z3_in
      with Unix.Unix_error (e,f,a) -> err "flush" e f a in 
    let _ = try close_out z3_in
      with Unix.Unix_error (e,f,a) -> err "close" e f a in 
    let res_buf = Buffer.create 17 in
    begin 
      try 
        while true do
          Buffer.add_string res_buf (input_line z3_out);
          Buffer.add_char res_buf '\n';
        done
      with
        | End_of_file -> ()
        | Unix.Unix_error (e,f,a) -> err "open" e f a
    end;
    let _ = 
      try close_in z3_out  
      with Unix.Unix_error (e,f,a) -> err "close" e f a in
    let res = Buffer.contents res_buf in 
    Printf.printf "%s\n\n%s\n\n" str res;
    match res with 
      | "sat\n" -> Some false
      | "unsat\n" -> Some true
      | _ -> None
  end
