# FuncTion

FuncTion is a research prototype static analyzer designed for proving conditional termination and conditional guarantee and recurrence properties of C programs. The tool automatically infers piecewise-defined ranking functions and sufficient preconditions for termination, guarantee and recurrence properties by means of abstract interpretation.

FuncTion was developed to implement and test the analysis method and abstract domains described in the articles:

	N. Courant, C. Urban. Precise Widening Operators for Proving Termination by Abstract Interpretation.
	
	C. Urban, A. Miné. Proving Guarantee and Recurrence Temporal Properties by Abstract Interpretation.
	In Proc. 16th International Conference on Verification, Model Checking, and Abstract Interpretation (VMCAI 2015). 
	
	C. Urban, A. Miné. A Decision Tree Abstract Domain for Proving Conditional Termination.
	In Proc. 21st International Static Analysis Symposium (SAS 2014).

	C. Urban, A. Miné. An Abstract Domain to Infer Ordinal-Valued Ranking Functions.
	In Proc. 23rd European Symposium on Programming (ESOP 2014).

	C. Urban. The Abstract Domain of Segmented Ranking Functions.
	In Proc. 20th International Static Analysis Symposium (SAS 2013).

FuncTion's main website is: http://www.di.ens.fr/~urban/FuncTion.html

## Author

	Caterina Urban, ETH Zurich, caterina.urban@inf.ethz.ch 
	
## Contributors

	Samuel Ueltschi, ETH Zurich
	Nathanaël Courant, École Normale Supérieure
	
# Installation

FuncTion requires the following applications and libraries:

* OCaml 

	```
	(sudo) apt-get install ocaml-interp
	```

* Findlib

	```
	(sudo) apt-get install ocaml-findlib
	```

* Menhir: LR(1) parser generator

	```
	(sudo) apt-get install menhir
	```
  
* Opam: https://opam.ocaml.org/doc/Install.html

	```
	(sudo) apt-get install opam
	```
  
* OUnit

	```
	opam install ounit
	```

* APRON: numerical abstract domain library

	```
	opam install apron
	```

* Zarith: arbitrary-precision integer operations

	```
	opam install zarith
	```


# Compiling FuncTion

Once all required libraries are installed, FuncTion can be compiled with 'make':

```
make
```

This will generate the command line program 'function' in the project directory. 

Alternatively, 'ocamlbuild' can be used to build FuncTion with the following command:

```
ocamlbuild ml_float.o Main.native -use-ocamlfind -use-menhir -pkgs 'apron,gmp,oUnit,zarith' -I utils -I banal -I domains -I frontend -I newfrontend -I main -libs boxMPQ,octD,polkaMPQ,str,zarith -lflags banal/ml_float.o
```

# Usage

The command-line analyzer can be invoked using the following call pattern:

	./function <file> 

where <file> is the path to the C file to be analyzed for termination of the function main(). Alternatively, the following call pattern can be used for guarantee properties or recurrence properties:

	./function <file> [-guarantee <property> | -recurrence <property]

where <property> is the path to the file containing the guarantee or recurrence property.

The analyzer first performs a forward reachability analysis, and then a backward analysis to find a piecewise-defined ranking function and sufficient preconditions at the entry point for the program to always terminate or satisfy the given guarantee or recurrence property.

The following command-line options are recognized
(showing their default value):

	 -main main                         set the analyzer entry point (defaults to main)
	 -domain boxes|octagons|polyhedra   set the abstract domain (defaults to boxes)
	 -joinfwd 2                         set the widening delay in forward analysis
	 -joinbwd 2                         set the widening delay in backward analysis
	 -meetbwd 2			    set the dual widening delay in backward analysis
	 -ordinals 2                        set the maximum ordinal value for the ranking functions
	 -refine			    reduces the backward analysis to the reachabile states

The analyzer answers TRUE in case it can successfully prove termination or the guarantee or recurrence property. 
Otherwise, it answers UNKNOWN.
