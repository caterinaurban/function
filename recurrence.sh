#!/bin/sh

### recurrence (AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/countdown.c -recurrence tests/countdown.txt -domain polyhedra > logs/recurrence/countdown_recuAST.log		# TRUE
./function tests/peterson.c -recurrence tests/peterson.txt -domain polyhedra > logs/recurrence/peterson_recuAST.log			# TRUE

# conditional recurrence (AST)

./function tests/simple.c -recurrence tests/simple.txt -domain polyhedra -joinbwd 3 > logs/recurrence/simple_recuAST.log
./function tests/sink.c -recurrence tests/sink.txt -domain polyhedra > logs/recurrence/sink_recuAST.log

### recurrence (CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

# ./function tests/countdown.c -ctl "AG{AF{x == 0}}" -domain polyhedra > logs/recurrence/countdown_recuCFG.log				# TODO: ?
./function tests/peterson.c -ctl "AG{AF{C1: true}}" -domain polyhedra > logs/recurrence/peterson_recuCFG.log				# TRUE

# conditional recurrence (CFG)

# ./function tests/simple.c -ctl "AG{AF{x == 3}}" -domain polyhedra -joinbwd 3 > logs/recurrence/simple_guarCFG.log			# TODO: ?
./function tests/sink.c -ctl "AG{AF{x == 0}}" -domain polyhedra > logs/recurrence/sink_recuCFG.log