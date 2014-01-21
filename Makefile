show:
	ocaml test.ml
test:
	ocaml test.ml | tee test_r.ml 
	ocaml test1.ml
