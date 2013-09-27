.PHONY: test clean

util.cmo: util.ml
	ocamlc -c str.cma util.ml

pr_scala.cmo: pr_scala.ml util.cmo
	ocamlc.opt -c -pp camlp4rf -I +camlp4 dynlink.cma camlp4lib.cma str.cma pr_scala.ml

test: pr_scala.cmo test.ml
	camlp4 pa_o.cmo pa_op.cmo str.cma util.cmo pr_scala.cmo test.ml > result.ml
	-killall FileMerge
	opendiff test.ml result.ml

clean:
	-rm *.cm[oi] *~ result.ml
