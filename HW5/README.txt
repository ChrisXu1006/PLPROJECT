See Homework 5 for instructions.

Description of Files:
---------------------
  - ast.ml
      Defines the datatypes for the abstract syntax trees (ASTs).

  - eval.ml
      The interpreter for the ASTs. 

  - main.ml
      The top level code that parses in an input file, and generates 
      verification conditions.

  - lexer.mll
  - parser.mly
      The lexer and parser for annotated IMP programs.

  - pprint.ml
      The pretty printer for the ASTs.

  - vcgen.ml
      The verification condition generator.

  - solve.ml
      The checker that automatically verifies the validity of assertions 
      using the Z3 theorem prover.

  - test*.imp
      Several test IMP programs.

How to compile:
---------------

  Simply type "make" at the command line. Successful compilation will
  produce an executable file called "imp".

How to execute:
---------------

  Run the executable on a test program: "./imp -verify test.imp". This will
  parse the file test.imp, calculate the VCs, print them, and check
  them for validity.

  For testing purposes, you can run the interpreter: "./imp -run test.imp". 
  However, keep in mind that some of the programs contain expressions with 
  free variables. For the purposes of this assignment, we only care about 
  verification.

  Passing the "-debug" flag prints extra information.

  Run all tests in the "tests" directory by typing "make test".

  
Note for Windows users:
-----------------------

Depending on your environment, your code may use a different
end-of-line symbol. If so, you may need to change the "\n" to "\r\n"
or similar in solve.ml
