.PHONY: clean

output: input.ml
	camlp4of input.ml -printer o > output
	cat input.ml
	cat output

clean:
	-rm output
