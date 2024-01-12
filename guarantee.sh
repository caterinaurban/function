#!/bin/sh

### guarantee with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -guarantee tests/guarantee/countdown.txt -domain polyhedra > logs/guarantee/countdown_guar.log
./function tests/guarantee/mnav.c -guarantee tests/guarantee/mnav.txt -domain polyhedra > logs/guarantee/mnav_guar.log
./function tests/guarantee/peterson.c -guarantee tests/guarantee/peterson.txt -domain polyhedra > logs/guarantee/peterson_guar.log
./function tests/guarantee/pingpong.c -guarantee tests/guarantee/pingpong.txt -domain polyhedra > logs/guarantee/pingpong_guar.log

### guarantee with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -guarantee tests/guarantee/sink.txt -domain polyhedra -ordinals 1 > logs/guarantee/sink_guar.log

# conditional guarantee

./function tests/guarantee/simple.c -guarantee tests/guarantee/simple.txt -domain polyhedra > logs/guarantee/simple_guar.log          # x <= 3

###

### guarantee (CTL-CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -ctl-cfg "AF{x == 0}" -domain polyhedra > logs/guarantee/countdown_guarCFG#TODO.log           # TODO: ?
./function tests/guarantee/peterson.c -ctl-cfg "AF{C1: true}" -domain polyhedra > logs/guarantee/peterson_guarCFG.log
./function tests/guarantee/pingpong.c -ctl-cfg "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpong_guarCFG.log

### guarantee (CTL-CFG) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -ctl-cfg "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sink_guarCFG#TODO.log         # TODO: ?

# conditional guarantee (CTL-CFG)

./function tests/guarantee/simple.c -ctl-cfg "AF{x == 3}" -domain polyhedra > logs/guarantee/simple_guarCFG#TODO.log                 # TODO: ?

###

### guarantee (CTL-AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/guarantee/countdown.c -ctl-ast "AF{x == 0}" -domain polyhedra > logs/guarantee/countdown_guarAST.log
./function tests/guarantee/peterson.c -ctl-ast "AF{C1: true}" -domain polyhedra > logs/guarantee/peterson_guarAST.log
./function tests/guarantee/pingpong.c -ctl-ast "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpong_guarAST.log

### guarantee (CTL-AST) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/guarantee/sink.c -ctl-ast "AF{x == 0}" -domain polyhedra -ordinals 1 > logs/guarantee/sink_guarAST.log

# conditional guarantee (CTL-AST)

./function tests/guarantee/simple.c -ctl-ast "AF{x == 3}" -domain polyhedra > logs/guarantee/simple_guarAST.log            # x <= 3