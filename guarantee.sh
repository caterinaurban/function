#!/bin/sh

### guarantee (AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/countdown.c -guarantee tests/countdown.txt -domain polyhedra > logs/guarantee/countdown_guarAST.log		# TRUE
./function tests/peterson.c -guarantee tests/peterson.txt -domain polyhedra > logs/guarantee/peterson_guarAST.log			# TRUE
./function tests/pingpong.c -guarantee tests/pingpong.txt -domain polyhedra > logs/guarantee/pingpong_guarAST.log			# TRUE

# conditional guarantee (AST)

./function tests/simple.c -guarantee tests/simple.txt -domain polyhedra > logs/guarantee/simple_guarAST.log
./function tests/sink.c -guarantee tests/sink.txt -domain polyhedra > logs/guarantee/sink_guarAST.log

### guarantee (CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

# ./function tests/countdown.c -ctl "AF{x == 0}" -domain polyhedra > logs/guarantee/countdown_guarCFG.log					# TODO: ?
./function tests/peterson.c -ctl "AF{C1: true}" -domain polyhedra > logs/guarantee/peterson_guarCFG.log						# TRUE
./function tests/pingpong.c -ctl "AF{z == 1}" -domain polyhedra > logs/guarantee/pingpong_guarCFG.log						# TRUE

# conditional guarantee (CFG)

# ./function tests/simple.c -ctl "AF{x == 3}" -domain polyhedra > logs/guarantee/simple_guarCFG.log							# TODO: ?	
./function tests/sink.c -ctl "AF{x == 0}" -domain polyhedra > logs/guarantee/sink_guarCFG.log