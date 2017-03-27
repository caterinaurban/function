ocamlbuild -use-ocamlfind -use-menhir -pkgs 'apron,gmp,oUnit' -I utils -I domains -I frontend -I main -libs boxMPQ,octD,polkaMPQ,str Main.byte
