#!/bin/sh

### termination (AST) with default setting:
### -domain boxes
### -joinbwd 2

./function tests/boolean.c > logs/termination/boolean_termAST.log				# TRUE
./function tests/cacm2009a.c > logs/termination/cacm2009a_termAST.log			# TRUE
# ./function tests/cacm2009b.c > logs/termination/cacm2009b_termAST.log
./function tests/cav2006.c > logs/termination/cav2006_termAST.log				# TRUE
./function tests/example1.c > logs/termination/example1_termAST.log				# TRUE
# ./function tests/example1a.c > logs/termination/example1a_termAST.log
# ./function tests/example1b.c > logs/termination/example1b_termAST.log
# ./function tests/example1c.c > logs/termination/example1c_termAST.log
# ./function tests/example1d.c > logs/termination/example1d_termAST.log
# ./function tests/example1e.c > logs/termination/example1e_termAST.log
# ./function tests/example2.c > logs/termination/example2_termAST.log
./function tests/example2a.c > logs/termination/example2a_termAST.log			# TRUE
# ./function tests/example2b.c > logs/termination/example2b_termAST.log
# ./function tests/example2c.c > logs/termination/example2c_termAST.log
# ./function tests/example2d.c > logs/termination/example2d_termAST.log
# ./function tests/example2e.c > logs/termination/example2e_termAST.log
# ./function tests/example8.c > logs/termination/example8_termAST.log
# ./function tests/example10.c > logs/termination/example10_termAST.log
# ./function tests/mccarthy91.c > logs/termination/mccarthy91_termAST.log
./function tests/postdecrement.c > logs/termination/postdecrement_termAST.log	# TRUE
./function tests/postincrement.c > logs/termination/postincrement_termAST.log	# TRUE
./function tests/predecrement.c > logs/termination/predecrement_termAST.log		# TRUE
./function tests/preincrement.c > logs/termination/preincrement_termAST.log		# TRUE
# ./function tests/recursion.c > logs/termination/recursion_termAST.log
./function tests/sas2010.c > logs/termination/sas2010_termAST.log				# TRUE
# ./function tests/sas2014b.c > logs/termination/sas2014b_termAST.log
# ./function tests/sorting4.c > logs/termination/sorting4_termAST.log
./function tests/tacas2013a.c > logs/termination/tacas2013a_termAST.log			# TRUE
# ./function tests/tacas2013b.c > logs/termination/tacas2013b_termAST.log
# ./function tests/tacas2013c.c > logs/termination/tacas2013c_termAST.log
# ./function tests/tacas2013d.c > logs/termination/tacas2013d_termAST.log
# ./function tests/vijay.c > logs/termination/vijay_termAST.log
# ./function tests/vmcai2004a.c > logs/termination/vmcai2004a_termAST.log
# ./function tests/widening1.c > logs/termination/widening1_termAST.log
# ./function tests/widening2.c > logs/termination/widening2_termAST.log

### termination (CFG) with default setting:
### -domain boxes
### -joinbwd 2

./function tests/boolean.c -ctl "AF{exit: true}" > logs/termination/boolean_termCFG.log				# TRUE
./function tests/cacm2009a.c -ctl "AF{exit: true}" > logs/termination/cacm2009a_termCFG.log			# TRUE
./function tests/cav2006.c -ctl "AF{exit: true}" > logs/termination/cav2006_termCFG.log				# TRUE
./function tests/example1.c -ctl "AF{exit: true}" > logs/termination/example1_termCFG.log			# TRUE
./function tests/example2a.c -ctl "AF{exit: true}" > logs/termination/example2a_termCFG.log			# TRUE
# ./function tests/postdecrement.c -ctl "AF{exit: true}" > logs/termination/postdecrement_termCFG.log	# TODO: fix parser?
# ./function tests/postincrement.c -ctl "AF{exit: true}" > logs/termination/postincrement_termCFG.log	# TODO: fix parser?
# ./function tests/predecrement.c -ctl "AF{exit: true}" > logs/termination/predecrement_termCFG.log		# TODO: fix parser?
# ./function tests/preincrement.c -ctl "AF{exit: true}" > logs/termination/preincrement_termCFG.log		# TODO: fix parser?
 ./function tests/sas2010.c -ctl "AF{exit: true}" > logs/termination/sas2010_termCFG#TODO.log				# TODO: ?
./function tests/tacas2013a.c -ctl "AF{exit: true}" > logs/termination/tacas2013a_termCFG.log		# TRUE

## conditional termination

# ./function tests/euclid.c > logs/termination/euclid_termAST.log					# x = y || (x > 0 && y > 0)
# ./function tests/example0.c > logs/termination/example0_termAST.log				# x > 10 OR (x <= 10 AND x odd)
# ./function tests/example5.c > logs/termination/example5_termAST.log				# x > 0
# ./function tests/example7.c > logs/termination/example7_termAST.log				# x > 6
# ./function tests/issue8.c > logs/termination/issue8_termAST.log					# x + z >= 0 || y >= 1 || -2y + z >= 0 || -x >= 2
# ./function tests/sas2014a.c > logs/termination/sas2014a_termAST.log				# r <= 0 || x < y
# ./function tests/sas2014c.c > logs/termination/sas2014c_termAST.log				# x <= 0 OR y > 0
# ./function tests/tap2008a.c > logs/termination/tap2008a_termAST.log				# x < 25 OR x > 30
# ./function tests/tap2008b.c > logs/termination/tap2008b_termAST.log				# x < -5 OR 0 <= x <= 30 OR x > 35
# ./function tests/tap2008c.c > logs/termination/tap2008c_termAST.log				# x < 30
# ./function tests/tap2008d.c > logs/termination/tap2008d_termAST.log				# x <= 0
# ./function tests/tap2008e.c > logs/termination/tap2008e_termAST.log				# x <= 11 OR x >= 40
# ./function tests/tap2008f.c > logs/termination/tap2008f_termAST.log				# x even
# ./function tests/vmcai2004b.c > logs/termination/vmcai2004b_termAST.log			# x != 3
# ./function tests/widening3.c > logs/termination/widening3_termAST.log				# x <= 0 || y > 0
# ./function tests/zune.c > logs/termination/zune_termAST.log

#################################################################################

### termination (AST) with increased widening delay:
### -domain boxes
### -joinbwd 5

# ./function tests/cacm2009b.c -joinbwd 5 > logs/termination/cacm2009b_termAST_join5.log
# ./function tests/example10.c -joinbwd 5 > logs/termination/example10_termAST_join5.log
# ./function tests/example1a.c -joinbwd 5 > logs/termination/example1a_termAST_join5.log
# ./function tests/example1b.c -joinbwd 5 > logs/termination/example1b_termAST_join5.log
# ./function tests/example1c.c -joinbwd 5 > logs/termination/example1c_termAST_join5.log
# ./function tests/example1d.c -joinbwd 5 > logs/termination/example1d_termAST_join5.log
# ./function tests/example1e.c -joinbwd 5 > logs/termination/example1e_termAST_join5.log
# ./function tests/example2.c -joinbwd 5 > logs/termination/example2_termAST_join5.log
# ./function tests/example2b.c -joinbwd 5 > logs/termination/example2b_termAST_join5.log
# ./function tests/example2c.c -joinbwd 5 > logs/termination/example2c_termAST_join5.log
# ./function tests/example2d.c -joinbwd 5 > logs/termination/example2d_termAST_join5.log
# ./function tests/example2e.c -joinbwd 5 > logs/termination/example2e_termAST_join5.log
# ./function tests/example8.c -joinbwd 5 > logs/termination/example8_termAST_join5.log
# ./function tests/mccarthy91.c -joinbwd 5 > logs/termination/mccarthy91_termAST_join5.log
# ./function tests/recursion.c -joinbwd 5 > logs/termination/recursion_termAST_join5.log
# ./function tests/sas2014b.c -joinbwd 5 > logs/termination/sas2014b_termAST_join5.log
# ./function tests/sorting4.c -joinbwd 5 > logs/termination/sorting4_termAST_join5.log
# ./function tests/tacas2013b.c -joinbwd 5 > logs/termination/tacas2013b_termAST_join5.log
# ./function tests/tacas2013c.c -joinbwd 5 > logs/termination/tacas2013c_termAST_join5.log
# ./function tests/tacas2013d.c -joinbwd 5 > logs/termination/tacas2013d_termAST_join5.log
# ./function tests/vijay.c -joinbwd 5 > logs/termination/vijay_termAST_join5.log
./function tests/vmcai2004a.c -joinbwd 5 > logs/termination/vmcai2004a_termAST_join5.log		# TRUE
# ./function tests/widening1.c -joinbwd 5 > logs/termination/widening1_termAST_join5.log
# ./function tests/widening2.c -joinbwd 5 > logs/termination/widening2_termAST_join5.log

### termination (CFG) with increased widening delay (5):
### -domain boxes
### -joinbwd 5

./function tests/vmcai2004a.c -joinbwd 5 -ctl "AF{exit: true}" > logs/termination/vmcai2004a_termCFG_join5.log		# TRUE

#################################################################################

### termination (AST) with increased widening delay (7):
### -domain boxes
### -joinbwd 7

./function tests/example8.c -joinbwd 7 > logs/termination/example8_termAST_join7.log		# TRUE

### termination (CFG) with increased widening delay (7):
### -domain boxes
### -joinbwd 7

./function tests/example8.c -joinbwd 7 -ctl "AF{exit: true}" > logs/termination/example8_termCFG_join7.log		# TRUE

#################################################################################

### termination (AST) with forward refinement:
### -domain boxes
### -joinbwd 2
### -refine

# ./function tests/cacm2009b.c -refine > logs/termination/cacm2009b_termAST_refine.log
# ./function tests/example10.c -refine > logs/termination/example10_termAST_refine.log
# ./function tests/example1a.c -refine > logs/termination/example1a_termAST_refine.log
# ./function tests/example1b.c -refine > logs/termination/example1b_termAST_refine.log
# ./function tests/example1c.c -refine > logs/termination/example1c_termAST_refine.log
# ./function tests/example1d.c -refine > logs/termination/example1d_termAST_refine.log
# ./function tests/example1e.c -refine > logs/termination/example1e_termAST_refine.log
# ./function tests/example2.c -refine > logs/termination/example2_termAST_refine.log
./function tests/example2b.c -refine > logs/termination/example2b_termAST_refine.log		# TRUE
./function tests/example2c.c -refine > logs/termination/example2c_termAST_refine.log		# TRUE
./function tests/example2d.c -refine > logs/termination/example2d_termAST_refine.log		# TRUE
./function tests/example2e.c -refine > logs/termination/example2e_termAST_refine.log		# TRUE
# ./function tests/mccarthy91.c -refine > logs/termination/mccarthy91_termAST_refine.log
# ./function tests/recursion.c -refine > logs/termination/recursion_termAST_refine.log
./function tests/sas2014b.c -refine > logs/termination/sas2014b_termAST_refine.log			# TRUE
# ./function tests/sorting4.c -refine > logs/termination/sorting4_termAST_refine.log
# ./function tests/tacas2013b.c -refine > logs/termination/tacas2013b_termAST_refine.log
# ./function tests/tacas2013c.c -refine > logs/termination/tacas2013c_termAST_refine.log
# ./function tests/tacas2013d.c -refine > logs/termination/tacas2013d_termAST_refine.log
# ./function tests/vijay.c -refine > logs/termination/vijay_termAST_refine.log
# ./function tests/widening1.c -refine > logs/termination/widening1_termAST_refine.log
# ./function tests/widening2.c -refine > logs/termination/widening2_termAST_refine.log

### termination (CFG) with forward refinement:
### -domain boxes
### -joinbwd 2
### -refine

./function tests/example2b.c -refine -ctl "AF{exit: true}" > logs/termination/example2b_termCFG_refine#TODO.log		# TODO: ?
./function tests/example2c.c -refine -ctl "AF{exit: true}" > logs/termination/example2c_termCFG_refine#TODO.log		# TODO: ?
./function tests/example2d.c -refine -ctl "AF{exit: true}" > logs/termination/example2d_termCFG_refine#TODO.log		# TODO: ?
./function tests/example2e.c -refine -ctl "AF{exit: true}" > logs/termination/example2e_termCFG_refine#TODO.log		# TODO: ?
./function tests/sas2014b.c -refine -ctl "AF{exit: true}" > logs/termination/sas2014b_termCFG_refine#TODO.log			# TODO: ?

#################################################################################

### termination (AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

# ./function tests/cacm2009b.c -domain polyhedra > logs/termination/cacm2009b_termAST_poly.log
# ./function tests/example10.c -domain polyhedra > logs/termination/example10_termAST_poly.log
# ./function tests/example1a.c -domain polyhedra > logs/termination/example1a_termAST_poly.log
# ./function tests/example1b.c -domain polyhedra > logs/termination/example1b_termAST_poly.log
# ./function tests/example1c.c -domain polyhedra > logs/termination/example1c_termAST_poly.log
# ./function tests/example1d.c -domain polyhedra > logs/termination/example1d_termAST_poly.log
# ./function tests/example1e.c -domain polyhedra > logs/termination/example1e_termAST_poly.log
# ./function tests/example2.c -domain polyhedra > logs/termination/example2_termAST_poly.log
# ./function tests/mccarthy91.c -domain polyhedra > logs/termination/mccarthy91_termAST_poly.log
# ./function tests/recursion.c -domain polyhedra > logs/termination/recursion_termAST_poly.log
./function tests/sorting4.c -domain polyhedra > logs/termination/sorting4_termAST_poly.log			# TRUE
./function tests/tacas2013b.c -domain polyhedra > logs/termination/tacas2013b_termAST_poly.log		# TRUE
# ./function tests/tacas2013c.c -domain polyhedra > logs/termination/tacas2013c_termAST_poly.log
# ./function tests/tacas2013d.c -domain polyhedra > logs/termination/tacas2013d_termAST_poly.log
# ./function tests/vijay.c -domain polyhedra > logs/termination/vijay_termAST_poly.log
# ./function tests/widening1.c -domain polyhedra > logs/termination/widening1_termAST_poly.log
# ./function tests/widening2.c -domain polyhedra > logs/termination/widening2_termAST_poly.log

### termination (CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/sorting4.c -domain polyhedra -ctl "AF{exit: true}" > logs/termination/sorting4_termCFG_poly.log			# TRUE
./function tests/tacas2013b.c -domain polyhedra -ctl "AF{exit: true}" > logs/termination/tacas2013b_termCFG_poly.log		# TRUE

#################################################################################

### termination (AST) with ordinals (2):
### -domain boxes
### -joinbwd 2
### -ordinals 2

./function tests/cacm2009b.c -ordinals 2 > logs/termination/cacm2009b_termAST_ord2.log		# TRUE
# ./function tests/example1a.c -ordinals 2 > logs/termination/example1a_termAST_ord2.log
# ./function tests/example1b.c -ordinals 2 > logs/termination/example1b_termAST_ord2.log
# ./function tests/example1c.c -ordinals 2 > logs/termination/example1c_termAST_ord2.log
# ./function tests/example1d.c -ordinals 2 > logs/termination/example1d_termAST_ord2.log
# ./function tests/example1e.c -ordinals 2 > logs/termination/example1e_termAST_ord2.log
./function tests/example2.c -ordinals 2 > logs/termination/example2_termAST_ord2.log		# TRUE
./function tests/example10.c -ordinals 2 > logs/termination/example10_termAST_ord2.log		# TRUE
# ./function tests/mccarthy91.c -ordinals 2 > logs/termination/mccarthy91_termAST_ord2.log
# ./function tests/recursion.c -ordinals 2 > logs/termination/recursion_termAST_ord2.log
# ./function tests/tacas2013c.c -ordinals 2 > logs/termination/tacas2013c_termAST_ord2.log
./function tests/tacas2013d.c -ordinals 2 > logs/termination/tacas2013d_termAST_ord2.log	# TRUE
# ./function tests/vijay.c -ordinals 2 > logs/termination/vijay_termAST_ord2.log
./function tests/widening1.c -ordinals 2 > logs/termination/widening1_termAST_ord2.log		# TRUE
./function tests/widening2.c -ordinals 2 > logs/termination/widening2_termAST_ord2.log		# TRUE

### termination (CFG) with ordinals (2):
### -domain boxes
### -joinbwd 2
### -ordinals 2

./function tests/cacm2009b.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/cacm2009b_termCFG_ord2.log		# TRUE
./function tests/example2.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/example2_termCFG_ord2.log			# TRUE
./function tests/example10.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/example10_termCFG_ord2.log		# TRUE
./function tests/tacas2013d.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/tacas2013d_termCFG_ord2.log		# TRUE
./function tests/widening1.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/widening1_termCFG_ord2.log		# TRUE
./function tests/widening2.c -ordinals 2 -ctl "AF{exit: true}" > logs/termination/widening2_termCFG_ord2.log		# TRUE

#################################################################################

### termination (AST) with increased widening delay (3) and ordinals (2):
### -domain boxes
### -joinbwd 3
### -ordinals 2

# ./function tests/example1a.c -joinbwd 3 -ordinals 2 > logs/termination/example1a_termAST_join3ord2.log
# ./function tests/example1b.c -joinbwd 3 -ordinals 2 > logs/termination/example1b_termAST_join3ord2.log
# ./function tests/example1c.c -joinbwd 3 -ordinals 2 > logs/termination/example1c_termAST_join3ord2.log
# ./function tests/example1d.c -joinbwd 3 -ordinals 2 > logs/termination/example1d_termAST_join3ord2.log
# ./function tests/example1e.c -joinbwd 3 -ordinals 2 > logs/termination/example1e_termAST_join3ord2.log
# ./function tests/mccarthy91.c -joinbwd 3 -ordinals 2 > logs/termination/mccarthy91_termAST_join3ord2.log
# ./function tests/recursion.c -joinbwd 3 -ordinals 2 > logs/termination/recursion_termAST_join3ord2.log
./function tests/tacas2013c.c -joinbwd 3 -ordinals 2 > logs/termination/tacas2013c_termAST_join3ord2.log		# TRUE
# ./function tests/vijay.c -joinbwd 3 -ordinals 2 > logs/termination/vijay_termAST_join3ord2.log

### termination (CFG) with increased widening delay (3) ordinals (2):
### -domain boxes
### -joinbwd 3
### -ordinals 2

./function tests/tacas2013c.c -joinbwd 3 -ordinals 2 -ctl "AF{exit: true}" > logs/termination/tacas2013c_termCFG_join3ord2.log	# TRUE

#################################################################################

### TODO

# ./function tests/example1a.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1a_AST.log
# ./function tests/example1b.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1b_AST.log
# ./function tests/example1c.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1c_AST.log
# ./function tests/example1d.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1d_AST.log
# ./function tests/example1e.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/example1e_AST.log
# ./function tests/mccarthy91.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/mccarthy91_AST.log
# ./function tests/recursion.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/recursion_AST.log
# ./function tests/vijay.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra > logs/termination/vijay_AST.log

# ./function tests/example1a.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/example1a_CFG.log
# ./function tests/example1b.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/example1b_CFG.log
# ./function tests/example1c.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/example1c_CFG.log
# ./function tests/example1d.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/example1d_CFG.log
# ./function tests/example1e.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/example1e_CFG.log
# ./function tests/mccarthy91.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/mccarthy91_CFG.log
# ./function tests/recursion.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/recursion_CFG.log
# ./function tests/vijay.c -joinbwd 3 -ordinals 2 -refine -domain polyhedra -ctl "AF{exit: true}" > logs/termination/vijay_CFG.log
