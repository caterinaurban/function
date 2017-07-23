
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
ZARITHDIR = `$(OCAMLFIND) query zarith`
GMPDIR = `$(OCAMLFIND) query gmp`
OUNITDIR = `$(OCAMLFIND) query oUnit`

# flags & paths
###############

OCAMLDIR = `$(OCAMLC) -where`
OCAMLFLAGS = -thread -g
OCAMLOPTFLAGS = -thread
OCAMLINC = -I $(APRONDIR) -I $(ZARITHDIR) -I frontend -I cfgfrontend -I utils -I banal -I domains -I main -I web -I $(OUNITDIR) -I tests -I $(GMPDIR)
#OCAMLLIBS = bigarray.cma gmp.cma apron.cma boxMPQ.cma octD.cma zarith.cma polkaMPQ.cma str.cma unix.cma threads.cma
OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa boxMPQ.cmxa octD.cmxa zarith.cmxa polkaMPQ.cmxa str.cmxa #threads.cmxa
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
  cfgfrontend/program_lexer.ml \
  cfgfrontend/program_parser.ml \
  cfgfrontend/program_parser.mli \
  
MLFILES = \
  utils/setext.ml \
  utils/mapext.ml \
  utils/Constraints.ml \
  utils/InvMap.ml \
  cfgfrontend/abstract_syntax_tree.ml \
  cfgfrontend/program_parser.ml \
  cfgfrontend/program_lexer.ml \
  cfgfrontend/cfg.ml \
  cfgfrontend/cfg_printer.ml \
  cfgfrontend/tree_to_cfg.ml \
  cfgfrontend/file_parser.ml \
  cfgfrontend/loop_detection.ml \
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
  cfgfrontend/conversion.ml \
  banal/banal_int.ml \
  banal/banal_float.ml \
  banal/banal_intinf.ml \
  banal/banal_datatypes.ml \
  banal/banal_rat.ml \
  banal/banal_itv_int.ml \
  banal/banal_itv_rat.ml \
  banal/banal_itv_float.ml \
  banal/banal_mathtypes.ml \
  banal/banal_affine.ml \
  banal/banal_abstract_syntax.ml \
  banal/banal_typed_syntax.ml \
  banal/banal_semantics.ml \
  banal/banal_domain.ml \
  banal/banal_linearization.ml \
  banal/banal_apron_domain.ml \
  banal/function_banal_converter.ml \
  domains/Partition.ml \
  domains/Numerical.ml \
  domains/Functions.ml \
  domains/Affines.ml \
  domains/Ordinals.ml \
  domains/Domain.ml \
  domains/DecisionTree.ml \
  main/Iterator.ml \
  main/CFGInterpreter.ml \
  main/CFGForwardIterator.ml \
  main/ForwardIterator.ml \
  main/TerminationIterator.ml \
  main/GuaranteeIterator.ml \
  main/RecurrenceIterator.ml \
  main/CTLIterator.ml \
  main/CFGCTLIterator.ml \

MLIFILES = \
  frontend/Parser.mli \
  frontend/PropertyParser.mli \
  cfgfrontend/program_parser.mli \

CMDMLFILES = main/Main.ml
TSTMLFILES = \
  tests/TestCommon.ml \
  tests/TerminationBoxesTest.ml \
  tests/TerminationPolyhedraTest.ml \
  tests/CTLTest.ml \
  tests/Test.ml \


CFILES = \
  banal/ml_float.c \


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
