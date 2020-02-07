default:
	utop -init enigma.ml
	
build:
	ocamlbuild -use-ocamlfind enigma.cmo enigma_test.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' enigma_test.byte && ./enigma_test.byte

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash finalcheck.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html enigma.ml

clean:
	ocamlbuild -clean
	rm -rf doc
