#!/bin/sh

## conditional termination (CFG)

./function tests/euclid.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x == y" > logs/ctl/euclid_1CFG.log					            # TRUE
#./function tests/euclid.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 0 && y > 0" > logs/ctl/euclid_2CFG.log            # TODO: ?
./function tests/example0.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 10" > logs/ctl/example0CFG.log                   # TRUE
#./function tests/example0.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x <= 10 && x % 2 == 1" > logs/ctl/example0CFG.log   # TODO: needs parity domain
./function tests/example5.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 0" -joinbwd 4 > logs/ctl/example5CFG.log         # TRUE
./function tests/example7.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 6" > logs/ctl/example7_1CFG.log	  			        # TRUE
#./function tests/example7.c -domain polyhedra -ctl "EF{exit: true}" > logs/ctl/example7_2CFG.log				                                # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x + z >= 0" > logs/ctl/issue8_1CFG.log				        # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -precondition "y >= 1" > logs/ctl/issue8_2CFG.log				            # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -precondition "-2 * y + z >= 0" > logs/ctl/issue8_3CFG.log				    # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -precondition "-x >= 2" > logs/ctl/issue8_4CFG.log				            # TODO: ?
./function tests/sas2014a.c -domain polyhedra -ctl "AF{exit: true}" -precondition "r <= 0" > logs/ctl/sas2014a_1CFG.log				          # TRUE
#./function tests/sas2014a.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x < y" > logs/ctl/sas2014a_2CFG.log				          # TODO: ?
./function tests/sas2014c.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x <= 0" > logs/ctl/sas2014c_1CFG.log				          # TRUE
#./function tests/sas2014c.c -domain polyhedra -ctl "AF{exit: true}" -precondition "y > 0" > logs/ctl/sas2014c_2CFG.log				          # TODO: ?
./function tests/tap2008a.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x < 25" > logs/ctl/tap2008a_1CFG.log				          # TRUE
./function tests/tap2008a.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 30" > logs/ctl/tap2008a_2CFG.log				          # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x < -5" > logs/ctl/tap2008b_1CFG.log				          # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -precondition "0 <= x && x <= 30" > logs/ctl/tap2008b_2CFG.log      # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 35" > logs/ctl/tap2008b_3CFG.log				          # TRUE
./function tests/tap2008c.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x < 30" > logs/ctl/tap2008cCFG.log				            # TRUE
./function tests/tap2008d.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x <= 0" > logs/ctl/tap2008dCFG.log				            # TRUE
./function tests/tap2008e.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x <= 11" > logs/ctl/tap2008d_1CFG.log				        # TRUE
./function tests/tap2008e.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x >= 40" > logs/ctl/tap2008d_2CFG.log				        # TRUE
#./function tests/tap2008f.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x % 2 == 0" > logs/ctl/tap2008d_2CFG.log				    # TODO: needs parity domain
./function tests/vmcai2004b.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x < 3" > logs/ctl/vmcai2004b_1CFG.log				      # TRUE
./function tests/vmcai2004b.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x > 3" -joinbwd 3 > logs/ctl/vmcai2004b_2CFG.log   # TRUE
./function tests/widening3.c -domain polyhedra -ctl "AF{exit: true}" -precondition "x <= 0" > logs/ctl/widening3_1CFG.log				        # TRUE
#./function tests/widening3.c -domain polyhedra -ctl "AF{exit: true}" -precondition "y > 0" > logs/ctl/widening3_2CFG.log               # TODO: ?
#./function tests/zune.c -domain polyhedra -ctl "AF{exit: true}" -precondition "days <= 365" > logs/ctl/zuneCFG.log				              # TODO: call to unknown functions should be approximated with non-determinism

## conditional termination (AST)

./function tests/euclid.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x == y" > logs/ctl/euclid_1AST.log					            # TRUE
#./function tests/euclid.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 0 && y > 0" > logs/ctl/euclid_2AST.log            # TODO: ?
./function tests/example0.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 10" > logs/ctl/example0AST.log                   # TRUE
#./function tests/example0.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x <= 10 && x % 2 == 1" > logs/ctl/example0AST.log   # TODO: needs parity domain
./function tests/example5.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 0" -joinbwd 5 > logs/ctl/example5AST.log         # TRUE
./function tests/example7.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 6" > logs/ctl/example7_1AST.log	  			        # TRUE
#./function tests/example7.c -domain polyhedra -ctl "EF{exit: true}" -ast > logs/ctl/example7_2AST.log				                                # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x + z >= 0" > logs/ctl/issue8_1AST.log				        # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "y >= 1" > logs/ctl/issue8_2AST.log				            # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "-2 * y + z >= 0" > logs/ctl/issue8_3AST.log				    # TODO: ?
#./function tests/issue8.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "-x >= 2" > logs/ctl/issue8_4AST.log				            # TODO: ?
./function tests/sas2014a.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "r <= 0" > logs/ctl/sas2014a_1AST.log				          # TRUE
#./function tests/sas2014a.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x < y" > logs/ctl/sas2014a_2AST.log				          # TODO: ?
./function tests/sas2014c.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x <= 0" > logs/ctl/sas2014c_1AST.log				          # TRUE
#./function tests/sas2014c.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "y > 0" > logs/ctl/sas2014c_2AST.log				          # TODO: ?
./function tests/tap2008a.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x < 25" > logs/ctl/tap2008a_1AST.log				          # TRUE
./function tests/tap2008a.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 30" > logs/ctl/tap2008a_2AST.log				          # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x < -5" > logs/ctl/tap2008b_1AST.log				          # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "0 <= x && x <= 30" > logs/ctl/tap2008b_2AST.log      # TRUE
./function tests/tap2008b.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 35" > logs/ctl/tap2008b_3AST.log				          # TRUE
./function tests/tap2008c.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x < 30" > logs/ctl/tap2008cAST.log				            # TRUE
./function tests/tap2008d.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x <= 0" > logs/ctl/tap2008dAST.log				            # TRUE
./function tests/tap2008e.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x <= 11" > logs/ctl/tap2008d_1AST.log				        # TRUE
./function tests/tap2008e.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x >= 40" > logs/ctl/tap2008d_2AST.log				        # TRUE
#./function tests/tap2008f.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x % 2 == 0" > logs/ctl/tap2008d_2AST.log				    # TODO: needs parity domain
./function tests/vmcai2004b.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x < 3" -joinbwd 3 > logs/ctl/vmcai2004b_1AST.log				      # TRUE
./function tests/vmcai2004b.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x > 3" -joinbwd 4 > logs/ctl/vmcai2004b_2AST.log   # TRUE
./function tests/widening3.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "x <= 0" > logs/ctl/widening3_1AST.log				        # TRUE
#./function tests/widening3.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "y > 0" > logs/ctl/widening3_2AST.log               # TODO: ?
#./function tests/zune.c -domain polyhedra -ctl "AF{exit: true}" -ast -precondition "days <= 365" > logs/ctl/zuneAST.log				              # TODO: call to unknown functions should be approximated with non-determinism

# conditional guarantee (CFG)

#./function tests/simple.c -ctl "AF{x == 3}" -domain polyhedra -precondition "x <= 3" > logs/ctl/simple_1CFG.log                      # TODO: ?

# conditional guarantee (AST)

./function tests/simple.c -ctl "AF{x == 3}" -ast -domain polyhedra -precondition "x <= 3" > logs/ctl/simple_1AST.log

# conditional recurrence (CFG)

# ./function tests/simple.c -ctl "AG{AF{x == 3}}" -domain polyhedra -joinbwd 3 -precondition "x < 0" > logs/recurrence/simple_2CFG.log      # TODO: ?

# conditional recurrence (AST)

 ./function tests/simple.c -ctl "AG{AF{x == 3}}" -ast -domain polyhedra -joinbwd 3 -precondition "x < 0" > logs/ctl/simple_2CFG.log

##########
#
#### CTL (CFG) with default setting:
#### -domain polyhedra
#### -joinbwd 2
#
#./function tests/ctl/and_ef_test.c -domain polyhedra -ctl "AND{EF{x == 2}}{EF{x==3}}" > logs/ctl/and_ef_test.log														# TRUE
#./function tests/ctl/and_test.c -domain polyhedra -ctl "AND{AG{AF{n==1}}}{AF{n==0}}" -precondition "n > 0" > logs/ctl/and_test.log										# TRUE
#./function tests/ctl/existential_test1.c -domain polyhedra -ctl "EF{r==1}" -precondition "2*x <= y+3" > logs/ctl/existential_test1.log									#
#./function tests/ctl/existential_test2.c -domain polyhedra -ctl "EF{r==1}" > logs/ctl/existential_test2.log
#./function tests/ctl/existential_test3.c -domain polyhedra -ctl "EF{r==1}" -precondition "x > 0" > logs/ctl/existential_test3.log
#./function tests/ctl/existential_test3.c -domain polyhedra -ctl "EF{r==1}" -precondition "x > 0" -ctl_existential_equivalence > logs/ctl/existential_test3_exeq.log		# TRUE
#./function tests/ctl/existential_test4.c -domain polyhedra -ctl "EF{r==1}" > logs/ctl/existential_test4.log																# TRUE
## ./function tests/ctl/fin_ex.c -domain polyhedra -ctl "EF{n==1}" -precondition "n > 0" > logs/ctl/fin_ex.log
## ./function tests/ctl/fin_ex.c -domain polyhedra -ctl "EG{EF{n==1}}" -precondition "n > 0" > logs/ctl/fin_ex.log
#./function tests/ctl/global_test_simple.c -domain polyhedra -joinbwd 4 -ctl "AG{AF{x <= -10}}" > logs/ctl/global_test_simple.log											#
#./function tests/ctl/global_test_simple.c -domain polyhedra -joinbwd 6 -ctl "AG{AF{x <= -10}}" > logs/ctl/global_test_simple_join6.log									# TRUE
#./function tests/ctl/global_test_simple.c -domain polyhedra -joinbwd 10 -ctl "AG{AF{x <= -10}}" > logs/ctl/global_test_simple_join10.log								# TRUE
#./function tests/ctl/multi_branch_choice.c -domain polyhedra -ctl "AF{OR{x==4}{x==-4}}" > logs/ctl/multi_branch_choice1.log												# TRUE
#./function tests/ctl/multi_branch_choice.c -domain polyhedra -ctl "EF{x==-4}" > logs/ctl/multi_branch_choice2.log														#
#./function tests/ctl/next.c -domain polyhedra -ctl "AX{x==0}" -precondition "x == 1" > logs/ctl/next.log																#
#./function tests/ctl/or_test.c -domain polyhedra -ctl "OR{AF{AG{x < -100}}}{AF{x==20}}" > logs/ctl/or_test.log													      # TRUE
#./function tests/ctl/potential_termination_1.c -domain polyhedra -ctl "EF{exit: true}" > logs/ctl/potential_termination_1.log
#./function tests/ctl/until_existential.c -domain polyhedra -ctl "EU{x >= y}{x == y}" -precondition "x > y" > logs/ctl/until_existential.log								#
#./function tests/ctl/until_test.c -domain polyhedra -ctl "AU{x >= y}{x==y}" -precondition "x == y + 20" > logs/ctl/until_test.log										# TRUE
#
## drwxr-xr-x@ 17 Caterina  staff     544 Apr  3  2023 koskinen
## drwxr-xr-x@ 32 Caterina  staff    1024 Apr  3  2023 ltl_automizer
## drwxr-xr-x@  6 Caterina  staff     192 Apr  3  2023 report
## drwxr-xr-x@ 19 Caterina  staff     608 Apr  3  2023 sv_comp
## drwxr-xr-x@ 11 Caterina  staff     352 Apr  3  2023 t2_cav13
#
### conditional termination



