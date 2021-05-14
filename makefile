build:
	ocamlopt -o ocamlfucked src/ocamlfucked.ml

run:
	ocaml src/ocamlfucked.ml

clean:
	rm src/*.cm*
	rm ocamlfucked
