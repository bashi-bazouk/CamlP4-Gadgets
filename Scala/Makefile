.PHONY: test clean diff

util.cmo: util.ml
	ocamlc -c str.cma util.ml

pr_scala.cmo: pr_scala.ml util.cmo
	ocamlc.opt -c -pp camlp4rf -I +camlp4 dynlink.cma camlp4lib.cma str.cma pr_scala.ml

OCaml.cmo: OCaml.ml
	ocamlc.opt -c -pp camlp4rf -I +camlp4 dynlink.cma camlp4lib.cma str.cma OCaml.ml

test: pr_scala_of_ocaml.cmo test.ml OCaml.cmo
	camlp4 pa_o.cmo pa_op.cmo str.cma util.cmo OCaml.cmo pr_scala_of_ocaml.cmo test.ml > result.ml
	opendiff test.ml result.ml

pr_scala_of_ocaml.cmo: pr_scala_of_ocaml.ml OCaml.cmo util.cmo
	ocamlc.opt -c -pp camlp4rf -I +camlp4 dynlink.cma camlp4lib.cma str.cma OCaml.cmo pr_scala_of_ocaml.ml

clean:
	-rm *.cm[oi] *~ result.ml

diff: 
	opendiff OCaml.ml pr_scala.ml
