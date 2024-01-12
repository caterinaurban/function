#!/bin/sh

### termination (AST) with default setting:
### -domain boxes
### -joinbwd 2

./function tests/termination/boolean.c > logs/termination/boolean_termAST.log				# TRUE
./function tests/termination/cacm2009a.c > logs/termination/cacm2009a_termAST.log			# TRUE
# ./function tests/termination/cacm2009b.c > logs/termination/cacm2009b_termAST.log
./function tests/termination/cav2006.c > logs/termination/cav2006_termAST.log				# TRUE
./function tests/termination/example1.c > logs/termination/example1_termAST.log				# TRUE
# ./function tests/termination/example1a.c > logs/termination/example1a_termAST.log
# ./function tests/termination/example1b.c > logs/termination/example1b_termAST.log
# ./function tests/termination/example1c.c > logs/termination/example1c_termAST.log
# ./function tests/termination/example1d.c > logs/termination/example1d_termAST.log
# ./function tests/termination/example1e.c > logs/termination/example1e_termAST.log
# ./function tests/termination/example2.c > logs/termination/example2_termAST.log
./function tests/termination/example2a.c > logs/termination/example2a_termAST.log			# TRUE
# ./function tests/termination/example2b.c > logs/termination/example2b_termAST.log
# ./function tests/termination/example2c.c > logs/termination/example2c_termAST.log
# ./function tests/termination/example2d.c > logs/termination/example2d_termAST.log
# ./function tests/termination/example2e.c > logs/termination/example2e_termAST.log
# ./function tests/termination/example8.c > logs/termination/example8_termAST.log
# ./function tests/termination/example10.c > logs/termination/example10_termAST.log
# ./function tests/termination/mccarthy91.c > logs/termination/mccarthy91_termAST.log
./function tests/termination/postdecrement.c > logs/termination/postdecrement_termAST.log	# TRUE
./function tests/termination/postincrement.c > logs/termination/postincrement_termAST.log	# TRUE
./function tests/termination/predecrement.c > logs/termination/predecrement_termAST.log		# TRUE
./function tests/termination/preincrement.c > logs/termination/preincrement_termAST.log		# TRUE
# ./function tests/termination/recursion.c > logs/termination/recursion_termAST.log
./function tests/termination/sas2010.c > logs/termination/sas2010_termAST.log				# TRUE
# ./function tests/termination/sas2014b.c > logs/termination/sas2014b_termAST.log
# ./function tests/termination/sorting4.c > logs/termination/sorting4_termAST.log
./function tests/termination/tacas2013a.c > logs/termination/tacas2013a_termAST.log			# TRUE
# ./function tests/termination/tacas2013b.c > logs/termination/tacas2013b_termAST.log
# ./function tests/termination/tacas2013c.c > logs/termination/tacas2013c_termAST.log
# ./function tests/termination/tacas2013d.c > logs/termination/tacas2013d_termAST.log
# ./function tests/termination/vijay.c > logs/termination/vijay_termAST.log
# ./function tests/termination/vmcai2004a.c > logs/termination/vmcai2004a_termAST.log
# ./function tests/termination/widening1.c > logs/termination/widening1_termAST.log
# ./function tests/termination/widening2.c > logs/termination/widening2_termAST.log

### termination (CFG) with default setting:
### -domain boxes
### -joinbwd 2

./function tests/termination/boolean.c -ctl-cfg "AF{exit: true}" > logs/termination/boolean_termCFG.log				# TRUE
./function tests/termination/cacm2009a.c -ctl-cfg "AF{exit: true}" > logs/termination/cacm2009a_termCFG.log			# TRUE
./function tests/termination/cav2006.c -ctl-cfg "AF{exit: true}" > logs/termination/cav2006_termCFG.log				# TRUE
./function tests/termination/example1.c -ctl-cfg "AF{exit: true}" > logs/termination/example1_termCFG.log			# TRUE
./function tests/termination/example2a.c -ctl-cfg "AF{exit: true}" > logs/termination/example2a_termCFG.log			# TRUE
# ./function tests/termination/postdecrement.c -ctl-cfg "AF{exit: true}" > logs/termination/postdecrement_termCFG.log	# TODO: fix parser?
# ./function tests/termination/postincrement.c -ctl-cfg "AF{exit: true}" > logs/termination/postincrement_termCFG.log	# TODO: fix parser?
# ./function tests/termination/predecrement.c -ctl-cfg "AF{exit: true}" > logs/termination/predecrement_termCFG.log		# TODO: fix parser?
# ./function tests/termination/preincrement.c -ctl-cfg "AF{exit: true}" > logs/termination/preincrement_termCFG.log		# TODO: fix parser?
 ./function tests/termination/sas2010.c -ctl-cfg "AF{exit: true}" > logs/termination/sas2010_termCFG#TODO.log				# TODO: ?
./function tests/termination/tacas2013a.c -ctl-cfg "AF{exit: true}" > logs/termination/tacas2013a_termCFG.log		# TRUE

## conditional termination

# ./function tests/termination/euclid.c > logs/termination/euclid_termAST.log					# x = y || (x > 0 && y > 0)
# ./function tests/termination/example0.c > logs/termination/example0_termAST.log				# x > 10 OR (x <= 10 AND x odd)
# ./function tests/termination/example5.c > logs/termination/example5_termAST.log				# x > 0
# ./function tests/termination/example7.c > logs/termination/example7_termAST.log				# x > 6
# ./function tests/termination/issue8.c > logs/termination/issue8_termAST.log					# x + z >= 0 || y >= 1 || -2y + z >= 0 || -x >= 2
# ./function tests/termination/sas2014a.c > logs/termination/sas2014a_termAST.log				# r <= 0 || x < y
# ./function tests/termination/sas2014c.c > logs/termination/sas2014c_termAST.log				# x <= 0 OR y > 0
# ./function tests/termination/tap2008a.c > logs/termination/tap2008a_termAST.log				# x < 25 OR x > 30
# ./function tests/termination/tap2008b.c > logs/termination/tap2008b_termAST.log				# x < -5 OR 0 <= x <= 30 OR x > 35
# ./function tests/termination/tap2008c.c > logs/termination/tap2008c_termAST.log				# x < 30
# ./function tests/termination/tap2008d.c > logs/termination/tap2008d_termAST.log				# x <= 0
# ./function tests/termination/tap2008e.c > logs/termination/tap2008e_termAST.log				# x <= 11 OR x >= 40
# ./function tests/termination/tap2008f.c > logs/termination/tap2008f_termAST.log				# x even
# ./function tests/termination/vmcai2004b.c > logs/termination/vmcai2004b_termAST.log			# x != 3
# ./function tests/termination/widening3.c > logs/termination/widening3_termAST.log				# x <= 0 || y > 0
# ./function tests/termination/zune.c > logs/termination/zune_termAST.log

#################################################################################

### termination (AST) with increased widening delay:
### -domain boxes
### -joinbwd 5

# ./function tests/termination/cacm2009b.c -joinbwd 5 > logs/termination/cacm2009b_termAST_join5.log
# ./function tests/termination/example10.c -joinbwd 5 > logs/termination/example10_termAST_join5.log
# ./function tests/termination/example1a.c -joinbwd 5 > logs/termination/example1a_termAST_join5.log
# ./function tests/termination/example1b.c -joinbwd 5 > logs/termination/example1b_termAST_join5.log
# ./function tests/termination/example1c.c -joinbwd 5 > logs/termination/example1c_termAST_join5.log
# ./function tests/termination/example1d.c -joinbwd 5 > logs/termination/example1d_termAST_join5.log
# ./function tests/termination/example1e.c -joinbwd 5 > logs/termination/example1e_termAST_join5.log
# ./function tests/termination/example2.c -joinbwd 5 > logs/termination/example2_termAST_join5.log
# ./function tests/termination/example2b.c -joinbwd 5 > logs/termination/example2b_termAST_join5.log
# ./function tests/termination/example2c.c -joinbwd 5 > logs/termination/example2c_termAST_join5.log
# ./function tests/termination/example2d.c -joinbwd 5 > logs/termination/example2d_termAST_join5.log
# ./function tests/termination/example2e.c -joinbwd 5 > logs/termination/example2e_termAST_join5.log
# ./function tests/termination/example8.c -joinbwd 5 > logs/termination/example8_termAST_join5.log
# ./function tests/termination/mccarthy91.c -joinbwd 5 > logs/termination/mccarthy91_termAST_join5.log
# ./function tests/termination/recursion.c -joinbwd 5 > logs/termination/recursion_termAST_join5.log
# ./function tests/termination/sas2014b.c -joinbwd 5 > logs/termination/sas2014b_termAST_join5.log
# ./function tests/termination/sorting4.c -joinbwd 5 > logs/termination/sorting4_termAST_join5.log
# ./function tests/termination/tacas2013b.c -joinbwd 5 > logs/termination/tacas2013b_termAST_join5.log
# ./function tests/termination/tacas2013c.c -joinbwd 5 > logs/termination/tacas2013c_termAST_join5.log
# ./function tests/termination/tacas2013d.c -joinbwd 5 > logs/termination/tacas2013d_termAST_join5.log
# ./function tests/termination/vijay.c -joinbwd 5 > logs/termination/vijay_termAST_join5.log
./function tests/termination/vmcai2004a.c -joinbwd 5 > logs/termination/vmcai2004a_termAST_join5.log		# TRUE
# ./function tests/termination/widening1.c -joinbwd 5 > logs/termination/widening1_termAST_join5.log
# ./function tests/termination/widening2.c -joinbwd 5 > logs/termination/widening2_termAST_join5.log

### termination (CFG) with increased widening delay (5):
### -domain boxes
### -joinbwd 5

./function tests/termination/vmcai2004a.c -joinbwd 5 -ctl-cfg "AF{exit: true}" > logs/termination/vmcai2004a_termCFG_join5.log		# TRUE

#################################################################################

### termination (AST) with increased widening delay (7):
### -domain boxes
### -joinbwd 7

./function tests/termination/example8.c -joinbwd 7 > logs/termination/example8_termAST_join7.log		# TRUE

### termination (CFG) with increased widening delay (7):
### -domain boxes
### -joinbwd 7

./function tests/termination/example8.c -joinbwd 7 -ctl-cfg "AF{exit: true}" > logs/termination/example8_termCFG_join7.log		# TRUE

#################################################################################

### termination (AST) with forward refinement:
### -domain boxes
### -joinbwd 2
### -refine

# ./function tests/termination/cacm2009b.c -refine > logs/termination/cacm2009b_termAST_refine.log
# ./function tests/termination/example10.c -refine > logs/termination/example10_termAST_refine.log
# ./function tests/termination/example1a.c -refine > logs/termination/example1a_termAST_refine.log
# ./function tests/termination/example1b.c -refine > logs/termination/example1b_termAST_refine.log
# ./function tests/termination/example1c.c -refine > logs/termination/example1c_termAST_refine.log
# ./function tests/termination/example1d.c -refine > logs/termination/example1d_termAST_refine.log
# ./function tests/termination/example1e.c -refine > logs/termination/example1e_termAST_refine.log
# ./function tests/termination/example2.c -refine > logs/termination/example2_termAST_refine.log
./function tests/termination/example2b.c -refine > logs/termination/example2b_termAST_refine.log		# TRUE
./function tests/termination/example2c.c -refine > logs/termination/example2c_termAST_refine.log		# TRUE
./function tests/termination/example2d.c -refine > logs/termination/example2d_termAST_refine.log		# TRUE
./function tests/termination/example2e.c -refine > logs/termination/example2e_termAST_refine.log		# TRUE
# ./function tests/termination/mccarthy91.c -refine > logs/termination/mccarthy91_termAST_refine.log
# ./function tests/termination/recursion.c -refine > logs/termination/recursion_termAST_refine.log
./function tests/termination/sas2014b.c -refine > logs/termination/sas2014b_termAST_refine.log			# TRUE
# ./function tests/termination/sorting4.c -refine > logs/termination/sorting4_termAST_refine.log
# ./function tests/termination/tacas2013b.c -refine > logs/termination/tacas2013b_termAST_refine.log
# ./function tests/termination/tacas2013c.c -refine > logs/termination/tacas2013c_termAST_refine.log
# ./function tests/termination/tacas2013d.c -refine > logs/termination/tacas2013d_termAST_refine.log
# ./function tests/termination/vijay.c -refine > logs/termination/vijay_termAST_refine.log
# ./function tests/termination/widening1.c -refine > logs/termination/widening1_termAST_refine.log
# ./function tests/termination/widening2.c -refine > logs/termination/widening2_termAST_refine.log

### termination (CFG) with forward refinement:
### -domain boxes
### -joinbwd 2
### -refine

./function tests/termination/example2b.c -refine -ctl-cfg "AF{exit: true}" > logs/termination/example2b_termCFG_refine#TODO.log		# TODO: ?
./function tests/termination/example2c.c -refine -ctl-cfg "AF{exit: true}" > logs/termination/example2c_termCFG_refine#TODO.log		# TODO: ?
./function tests/termination/example2d.c -refine -ctl-cfg "AF{exit: true}" > logs/termination/example2d_termCFG_refine#TODO.log		# TODO: ?
./function tests/termination/example2e.c -refine -ctl-cfg "AF{exit: true}" > logs/termination/example2e_termCFG_refine#TODO.log		# TODO: ?
./function tests/termination/sas2014b.c -refine -ctl-cfg "AF{exit: true}" > logs/termination/sas2014b_termCFG_refine#TODO.log			# TODO: ?

#################################################################################

### termination (AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

# ./function tests/termination/cacm2009b.c -domain polyhedra > logs/termination/cacm2009b_termAST_poly.log
# ./function tests/termination/example10.c -domain polyhedra > logs/termination/example10_termAST_poly.log
# ./function tests/termination/example1a.c -domain polyhedra > logs/termination/example1a_termAST_poly.log
# ./function tests/termination/example1b.c -domain polyhedra > logs/termination/example1b_termAST_poly.log
# ./function tests/termination/example1c.c -domain polyhedra > logs/termination/example1c_termAST_poly.log
# ./function tests/termination/example1d.c -domain polyhedra > logs/termination/example1d_termAST_poly.log
# ./function tests/termination/example1e.c -domain polyhedra > logs/termination/example1e_termAST_poly.log
# ./function tests/termination/example2.c -domain polyhedra > logs/termination/example2_termAST_poly.log
# ./function tests/termination/mccarthy91.c -domain polyhedra > logs/termination/mccarthy91_termAST_poly.log
# ./function tests/termination/recursion.c -domain polyhedra > logs/termination/recursion_termAST_poly.log
./function tests/termination/sorting4.c -domain polyhedra > logs/termination/sorting4_termAST_poly.log			# TRUE
./function tests/termination/tacas2013b.c -domain polyhedra > logs/termination/tacas2013b_termAST_poly.log		# TRUE
# ./function tests/termination/tacas2013c.c -domain polyhedra > logs/termination/tacas2013c_termAST_poly.log
# ./function tests/termination/tacas2013d.c -domain polyhedra > logs/termination/tacas2013d_termAST_poly.log
# ./function tests/termination/vijay.c -domain polyhedra > logs/termination/vijay_termAST_poly.log
# ./function tests/termination/widening1.c -domain polyhedra > logs/termination/widening1_termAST_poly.log
# ./function tests/termination/widening2.c -domain polyhedra > logs/termination/widening2_termAST_poly.log

### termination (CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/termination/sorting4.c -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/sorting4_termCFG_poly.log			# TRUE
./function tests/termination/tacas2013b.c -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/tacas2013b_termCFG_poly.log		# TRUE

#################################################################################

### termination (AST) with ordinals (2):
### -domain boxes
### -joinbwd 2
### -ordinals 2

./function tests/termination/cacm2009b.c -ordinals 2 > logs/termination/cacm2009b_termAST_ord2.log		# TRUE
# ./function tests/termination/example1a.c -ordinals 2 > logs/termination/example1a_termAST_ord2.log
# ./function tests/termination/example1b.c -ordinals 2 > logs/termination/example1b_termAST_ord2.log
# ./function tests/termination/example1c.c -ordinals 2 > logs/termination/example1c_termAST_ord2.log
# ./function tests/termination/example1d.c -ordinals 2 > logs/termination/example1d_termAST_ord2.log
# ./function tests/termination/example1e.c -ordinals 2 > logs/termination/example1e_termAST_ord2.log
./function tests/termination/example2.c -ordinals 2 > logs/termination/example2_termAST_ord2.log		# TRUE
./function tests/termination/example10.c -ordinals 2 > logs/termination/example10_termAST_ord2.log		# TRUE
# ./function tests/termination/mccarthy91.c -ordinals 2 > logs/termination/mccarthy91_termAST_ord2.log
# ./function tests/termination/recursion.c -ordinals 2 > logs/termination/recursion_termAST_ord2.log
# ./function tests/termination/tacas2013c.c -ordinals 2 > logs/termination/tacas2013c_termAST_ord2.log
./function tests/termination/tacas2013d.c -ordinals 2 > logs/termination/tacas2013d_termAST_ord2.log	# TRUE
# ./function tests/termination/vijay.c -ordinals 2 > logs/termination/vijay_termAST_ord2.log
./function tests/termination/widening1.c -ordinals 2 > logs/termination/widening1_termAST_ord2.log		# TRUE
./function tests/termination/widening2.c -ordinals 2 > logs/termination/widening2_termAST_ord2.log		# TRUE

### termination (CFG) with ordinals (2):
### -domain boxes
### -joinbwd 2
### -ordinals 2

./function tests/termination/cacm2009b.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/cacm2009b_termCFG_ord2.log		# TRUE
./function tests/termination/example2.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/example2_termCFG_ord2.log			# TRUE
./function tests/termination/example10.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/example10_termCFG_ord2.log		# TRUE
./function tests/termination/tacas2013d.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/tacas2013d_termCFG_ord2.log		# TRUE
./function tests/termination/widening1.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/widening1_termCFG_ord2.log		# TRUE
./function tests/termination/widening2.c -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/widening2_termCFG_ord2.log		# TRUE

#################################################################################

### termination (AST) with increased widening delay (3) and ordinals (2):
### -domain boxes
### -joinbwd 3
### -ordinals 2

# ./function tests/termination/example1a.c -joinbwd 3 -ordinals 2 > logs/termination/example1a_termAST_join3ord2.log
# ./function tests/termination/example1b.c -joinbwd 3 -ordinals 2 > logs/termination/example1b_termAST_join3ord2.log
# ./function tests/termination/example1c.c -joinbwd 3 -ordinals 2 > logs/termination/example1c_termAST_join3ord2.log
# ./function tests/termination/example1d.c -joinbwd 3 -ordinals 2 > logs/termination/example1d_termAST_join3ord2.log
# ./function tests/termination/example1e.c -joinbwd 3 -ordinals 2 > logs/termination/example1e_termAST_join3ord2.log
# ./function tests/termination/mccarthy91.c -joinbwd 3 -ordinals 2 > logs/termination/mccarthy91_termAST_join3ord2.log
# ./function tests/termination/recursion.c -joinbwd 3 -ordinals 2 > logs/termination/recursion_termAST_join3ord2.log
./function tests/termination/tacas2013c.c -joinbwd 3 -ordinals 2 > logs/termination/tacas2013c_termAST_join3ord2.log		# TRUE
# ./function tests/termination/vijay.c -joinbwd 3 -ordinals 2 > logs/termination/vijay_termAST_join3ord2.log

### termination (CFG) with increased widening delay (3) ordinals (2):
### -domain boxes
### -joinbwd 3
### -ordinals 2

./function tests/termination/tacas2013c.c -joinbwd 3 -ordinals 2 -ctl-cfg "AF{exit: true}" > logs/termination/tacas2013c_termCFG_join3ord2.log	# TRUE

#################################################################################

### TODO

# ./function tests/termination/example1a.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1a_AST.log
# ./function tests/termination/example1b.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1b_AST.log
# ./function tests/termination/example1c.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1c_AST.log
# ./function tests/termination/example1d.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1d_AST.log
# ./function tests/termination/example1e.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1e_AST.log
# ./function tests/termination/mccarthy91.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/mccarthy91_AST.log
# ./function tests/termination/recursion.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/recursion_AST.log
# ./function tests/termination/vijay.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/vijay_AST.log

# ./function tests/termination/example1a.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/example1a_CFG.log
# ./function tests/termination/example1b.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/example1b_CFG.log
# ./function tests/termination/example1c.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/example1c_CFG.log
# ./function tests/termination/example1d.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/example1d_CFG.log
# ./function tests/termination/example1e.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/example1e_CFG.log
# ./function tests/termination/mccarthy91.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/mccarthy91_CFG.log
# ./function tests/termination/recursion.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/recursion_CFG.log
# ./function tests/termination/vijay.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl-cfg "AF{exit: true}" > logs/termination/vijay_CFG.log
