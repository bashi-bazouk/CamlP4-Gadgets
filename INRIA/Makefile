PHONY: clean test_pa_float

pa_float.cmo : pa_float.ml
	ocamlc -o pa_float.cmo -I +camlp4 camlp4lib.cma -pp camlp4of.opt  -c pa_float.ml


test_pa_float: pa_float
	

clean:
	-rm *.cmo *.cmi *~
