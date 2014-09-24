(* main function *)
let () = 
  let _ = 
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: imp <file>\n";
       exit 0) in 
  let filename = Sys.argv.(1) in 
  let lexbuf = Lexing.from_channel (open_in filename) in 
  let c = 
    try Parser.p Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n" 
        !Lexer.lineno 
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in 
  let sigma = Eval.evalc (Eval.make_configuration c) in
  let hashstrint_print = Hashtbl.iter (fun key data -> Printf.printf "%s = %d;\n" key data) in
  hashstrint_print sigma;
  Printf.printf "%s \027[32m passed\n\027[37m" filename
