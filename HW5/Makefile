.SILENT: test

SHELL=bash

MAIN=imp

LIBS = unix.cma

OBJS = ast.cmo lexer.cmo parser.cmo pprint.cmo eval.cmo \
       vcgen.cmo solve.cmo main.cmo

TEST_FILES = $(shell (ls tests/*.imp))

%.cmo : %.ml
	ocamlc $(FLAGS) -c $<

%.cmi : %.mli
	ocamlc $(FLAGS) -c $<

$(MAIN): $(OBJS)
	ocamlc -o $(MAIN) $(LIBS) $(OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -v $<

parser.mli : parser.mly
	ocamlyacc -v $<

test: $(MAIN) $(TEST_FILES)
	for F in $(TEST_FILES); do \
		echo -n "[[5m Testing [0m] $$F[G"; \
		./imp -verify $$F > /dev/null 2>&1; \
		if [ $$? -ne 0 ]; then \
			echo "[[41;30m Failed  [m][91m $$F [m"; \
		else \
			echo "[[32m    OK   [m] $$F"; \
		fi; \
	done

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)
