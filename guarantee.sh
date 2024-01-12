#!/bin/sh

### guarantee with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/countdown.c -guarantee tests/countdown.txt -domain polyhedra > logs/guarantee/countdown_guar.log
./function tests/peterson.c -guarantee tests/peterson.txt -domain polyhedra > logs/guarantee/peterson_guar.log
./function tests/pingpong.c -guarantee tests/pingpong.txt -domain polyhedra > logs/guarantee/pingpong_guar.log

### guarantee with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/sink.c -guarantee tests/sink.txt -domain polyhedra -ordinals 1 > logs/guarantee/sink_guar.log

# conditional guarantee

./function tests/simple.c -guarantee tests/simple.txt -domain polyhedra > logs/guarantee/simple_guar.log          # x <= 3

###

### guarantee (CTL-CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/countdown.c -ctl-cfg "AF{x == 0}" -domain polyhedra > logs/guarantee/countdown_guarCFG#TODO.log           # TODO: ?
./function tests/peterson.c -ctl-cfg "AF{C1: true}" -domain polyhedra > logs/guarantee/peterson_guarCFG.log
./function tests/pingpong.c -ctl-cfg "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpong_guarCFG.log

### guarantee (CTL-CFG) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/sink.c -ctl-cfg "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sink_guarCFG#TODO.log         # TODO: ?

# conditional guarantee (CTL-CFG)

./function tests/simple.c -ctl-cfg "AF{x == 3}" -domain polyhedra > logs/guarantee/simple_guarCFG#TODO.log                 # TODO: ?

###

### guarantee (CTL-AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/countdown.c -ctl-ast "AF{x == 0}" -domain polyhedra > logs/guarantee/countdown_guarAST.log
./function tests/peterson.c -ctl-ast "AF{C1: true}" -domain polyhedra > logs/guarantee/peterson_guarAST.log
./function tests/pingpong.c -ctl-ast "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpong_guarAST.log

### guarantee (CTL-AST) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/sink.c -ctl-ast "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sink_guarAST.log

# conditional guarantee (CTL-AST)

./function tests/simple.c -ctl-ast "AF{x == 3}" -domain polyhedra > logs/guarantee/simple_guarAST.log            # x <= 3