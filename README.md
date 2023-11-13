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

	Naïm Moussaoui Remil, École Normale Supérieure
	Samuel Ueltschi, ETH Zurich
	Nathanaëlle Courant, École Normale Supérieure
	
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
* Dune: Ocaml build system

	```
	opam install dune
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

Once all required libraries are installed, FuncTion can be compiled with 'dune':

```
dune build
```

This will generate the command line program 'function' in the project directory. 

Alternatively, 'ocamlbuild' can be used to build FuncTion with the following command:

```
ocamlbuild ml_float.o Main.native -use-ocamlfind -use-menhir -pkgs 'apron,gmp,oUnit,zarith' -I utils -I banal -I domains -I frontend -I cfgfrontend -I main -libs boxMPQ,octD,polkaMPQ,str,zarith -lflags banal/ml_float.o
```

# Usage

The command-line analyzer can be invoked using the following call pattern:

	./function <file> <analysis> [options] 

where "file" is the path to the C file to be analyzed and "analysis" the type of the analysis to perform. 

The analyzer first performs a forward reachability analysis, and then a backward analysis to find a 
piecewise-defined ranking function and sufficient preconditions at the entry point for the program 
to satisfy the given analyzed property.

The following general command-line options are recognized
(showing their default value):

	 -main main                         set the analyzer entry point (defaults to main)
	 -domain boxes|octagons|polyhedra   set the abstract domain (defaults to boxes)
	 -joinfwd 2                         set the widening delay in forward analysis
	 -joinbwd 2                         set the widening delay in backward analysis
	 -meetbwd 2			                set the dual widening delay in backward analysis
	 -ordinals 2                        set the maximum ordinal value for the ranking functions
	 -refine            			    reduces the backward analysis to the reachabile states

The analyzer answers TRUE in case it can successfully prove the property. Otherwise, it answers UNKNOWN.

FuncTion can analyze the following properties:

* Termination
* Guarantee / Recurrence 
* Computation-Tree-Logic (CTL) 


# Tests
Inline tests with ppx_inline_test sare available in tests/TestCTL, tests/TerminationBoxesTest.ml, tests/TerminationBoxesTest.ml. To launch them you must call the following command: 
	dune runtest


## Termination

To check for termination, call the analyzer with the following call pattern:

	./function <file> -termination [options]

## Guarantee / Recurrence

The following call pattern can be used for guarantee properties or recurrence properties:

	./function <file> -guarantee <property_file> [options]
	./function <file> -recurrence <property_file> [options] 

where "property\_file" is the path to the file containing the guarantee or recurrence property.

## CTL

CTL properties can be analyzed using the following call pattern:

	./function <file> -ctl <property> [options]

where "property" is the CTL property to be analyzed. 

The following additional command-line options exist for the CTL analysis:
(showing their default value):

	 -precondition true                 a precondition that is assumed to hold at the start of the program
	 -noinline			                disable inlining of function calls
     -ast                               run the analysis on the abstract-syntax-tree instead of the control-flow-graph,
                                        note that in that case function calls and goto/break/continue are not supported
     -dot                               print out control-flow-graph and decision trees in graphviz dot format


