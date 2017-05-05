
# tools
#######

CC =        gcc
OCAMLC =    ocamlc
OCAMLOPT =  ocamlopt
OCAMLDEP =  ocamldep
OCAMLDOC =  ocamldoc
OCAMLFIND = ocamlfind
OCAMLLEX =  ocamllex
MENHIR =    menhir

# external libs

APRONDIR = `$(OCAMLFIND) query apron`
# APRONDIR = /users/absint/urban/lib/ocaml/site-lib/apron
GMPDIR = `$(OCAMLFIND) query gmp`
# GMPDIR = /users/absint/urban/lib/ocaml/site-lib/gmp
OUNITDIR = `$(OCAMLFIND) query oUnit`

# flags & paths
###############

OCAMLDIR = `$(OCAMLC) -where`
OCAMLFLAGS = -thread -g
OCAMLOPTFLAGS = -thread
OCAMLINC = -I $(APRONDIR) -I frontend -I utils -I domains -I main -I web -I $(OUNITDIR) -I tests -I $(GMPDIR)
#OCAMLLIBS = bigarray.cma gmp.cma apron.cma boxMPQ.cma octD.cma polkaMPQ.cma str.cma unix.cma threads.cma
OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa boxMPQ.cmxa octD.cmxa polkaMPQ.cmxa str.cmxa threads.cmxa
MENHIRFLAGS = --explain
CFLAGS = -I $(OCAMLDIR) -O3 -Wall
CLIBS = -lgmp
DATE = `date +'%Y%m%d'`
VERSION = 2.0

DIST = function-$(VERSION)

ifeq ($(DEBUG),1)
OCAMLFLAGS := $(OCAMLFLAGS) -g
OCAMLOPTFLAGS := $(OCAMLOPTFLAGS) -g
CFLAGS := $(CFLAGS) -g
endif

# files
#######

TARGETS = function test

AUTOGEN = \
  frontend/Lexer.ml \
  frontend/Parser.ml \
  frontend/Parser.mli \
  frontend/PropertyLexer.ml \
  frontend/PropertyParser.ml \
  frontend/PropertyParser.mli \
  frontend/CTLPropertyLexer.ml \
  frontend/CTLPropertyParser.ml \
  frontend/CTLPropertyParser.mli \
  
MLFILES = \
  frontend/IntermediateSyntax.ml \
  frontend/CTLProperty.ml \
  frontend/Lexer.ml \
  frontend/PropertyLexer.ml \
  frontend/CTLPropertyLexer.ml \
  frontend/Parser.ml \
  frontend/PropertyParser.ml \
  frontend/CTLPropertyParser.ml \
  frontend/AbstractSyntax.ml \
  frontend/ItoA.ml \
  utils/Constraints.ml \
  domains/Partition.ml \
  domains/Numerical.ml \
  domains/Functions.ml \
  domains/Affines.ml \
  domains/Ordinals.ml \
  domains/Domain.ml \
  domains/DecisionTree.ml \
  main/Iterator.ml \
  main/TerminationIterator.ml \
  main/GuaranteeIterator.ml \
  main/RecurrenceIterator.ml \
  main/CTLIterator.ml \

MLIFILES = \
  frontend/Parser.mli \
  frontend/PropertyParser.mli \

CMDMLFILES = main/Main.ml
TSTMLFILES = \
  tests/TestCommon.ml \
  tests/TerminationBoxesTest.ml \
  tests/TerminationPolyhedraTest.ml \
  tests/Test.ml \

CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)
CMIFILES = $(MLIFILES:%.ml=%.cmi)
OFILES = $(CFILES:%.c=%.o)
CMDCMOFILES = $(CMDMLFILES:%.ml=%.cmo)
CMDCMXFILES = $(CMDMLFILES:%.ml=%.cmx)
TSTCMOFILES = $(TSTMLFILES:%.ml=%.cmo)
TSTCMXFILES = $(TSTMLFILES:%.ml=%.cmx)

# rules
#######

all: $(AUTOGEN) function test

function: $(OFILES) $(CMXFILES) $(CMDCMXFILES)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(OCAMLINC) -cclib "$(CLIBS)" unix.cmxa $(OCAMLOPTLIBS) $+

test: $(OFILES) $(CMXFILES) $(TSTCMXFILES) 
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -package oUnit -linkpkg $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.ml

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC)  -c $*.ml

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(MENHIR) $(MENHIRFLAGS) $*.mly

clean:
	rm -f depend $(AUTOGEN) $(TARGETS)
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f `find . -name "*.cache"`
	rm -f `find . -name "*.conflicts"`
	rm -f *.svg out/*.svg out/*.tex analysis.log
	rm -f *.tgz

MLSOURCES = $(MLFILES) $(MLIFILES) $(CMDMLFILES) $(WEBMLFILES) 
depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > depend

.phony:	all clean

include depend
