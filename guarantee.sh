#!/bin/sh

### guarantee with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -guarantee tests/guarantee/countdown.txt -domain polyhedra > logs/guarantee/countdown.log
./function tests/guarantee/mnav.c -guarantee tests/guarantee/mnav.txt -domain polyhedra > logs/guarantee/mnav.log
./function tests/guarantee/peterson.c -guarantee tests/guarantee/peterson.txt -domain polyhedra > logs/guarantee/peterson.log
./function tests/guarantee/pingpong.c -guarantee tests/guarantee/pingpong.txt -domain polyhedra > logs/guarantee/pingpong.log

### guarantee with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -guarantee tests/guarantee/sink.txt -domain polyhedra -ordinals 1 > logs/guarantee/sink.log

# conditional guarantee

./function tests/guarantee/simple.c -guarantee tests/guarantee/simple.txt -domain polyhedra > logs/guarantee/simple.log          # x <= 3

###

### guarantee (CTL-CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -ctl-cfg "AF{x == 0}" -domain polyhedra > logs/guarantee/countdownCFG#TODO.log           # TODO: ?
./function tests/guarantee/peterson.c -ctl-cfg "AF{C1: true}" -domain polyhedra > logs/guarantee/petersonCFG.log
./function tests/guarantee/pingpong.c -ctl-cfg "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpongCFG.log

### guarantee (CTL-CFG) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -ctl-cfg "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sinkCFG#TODO.log         # TODO: ?

# conditional guarantee (CTL-CFG)

./function tests/guarantee/simple.c -ctl-cfg "AF{x == 3}" -domain polyhedra > logs/guarantee/simpleCFG#TODO.log                 # TODO: ?

###

### guarantee (CTL-AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -ctl-ast "AF{x == 0}" -domain polyhedra > logs/guarantee/countdownAST.log
./function tests/guarantee/peterson.c -ctl-ast "AF{C1: true}" -domain polyhedra > logs/guarantee/petersonAST.log
./function tests/guarantee/pingpong.c -ctl-ast "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpongAST.log

### guarantee (CTL-AST) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -ctl-ast "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sinkAST.log

# conditional guarantee (CTL-AST)

./function tests/guarantee/simple.c -ctl-ast "AF{x == 3}" -domain polyhedra > logs/guarantee/simpleAST.log            # x <= 3