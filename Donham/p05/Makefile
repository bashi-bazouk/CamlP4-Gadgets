.PHONY: test clean

to_of_string: to_of_string.ml
	ocamlfind ocamlc -package camlp4.quotations.o -syntax camlp4o \
	  -c to_of_string.ml

test: to_of_string
	camlp4o to_of_string.cmo test.ml

clean:
	-rm *.cmo *.cmi a.out *~
