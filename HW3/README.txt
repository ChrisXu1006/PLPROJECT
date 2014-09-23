See Homework 3 for instructions.

Description of Files:
---------------------
  - ast.ml
      Defines the datatypes for the abstract syntax trees (ASTs).

  - eval.ml
      The interpreter for the ASTs. Your task is to edit this (and
      only this) file to A lexer and parser for IMP programs.

  - main.ml
      The top level code that parses in an input file, and executes it.

  - lexer.mll
  - parser.mly
      A lexer and parser for IMP programs.

  - pprint.ml
      A pretty printer for the ASTs.

  - test.imp
      A test IMP program.

How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. Successful compilation will produce an executable file 
  called "imp".

How to execute:
---------------

  Run the executable on a test program: "imp test.imp".

  This will parse the file test.imp and evaluate the program.

Notes:
------
  OCaml modes exist for the Emacs and Vim editors:
  * http://caml.inria.fr/pub/docs/u3-ocaml/emacs/index.html
  * http://www.ocaml.info/vim/ftplugin/ocaml.vim
