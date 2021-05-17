# https://github.com/ocaml/ocaml/issues/9140
build:
	ocamlopt -I src src/util.mli src/util.ml src/ocamlfucked.ml -o ocamlfucked

run:
	ocaml src/ocamlfucked.ml

clean:
	rm src/*.cm*
	rm src/*.o
	rm ocamlfucked
