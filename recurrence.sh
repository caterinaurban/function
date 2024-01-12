#!/bin/sh

### recurrence with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/recurrence/countdown.c -recurrence tests/recurrence/countdown.txt -domain polyhedra > logs/recurrence/countdown_recu.log
./function tests/recurrence/peterson.c -recurrence tests/recurrence/peterson.txt -domain polyhedra > logs/recurrence/peterson_recu.log

### recurrence with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/recurrence/sink.c -recurrence tests/recurrence/sink.txt -domain polyhedra -ordinals 1 > logs/recurrence/sink_recu.log

# conditional recurrence

./function tests/recurrence/simple.c -recurrence tests/recurrence/simple.txt -domain polyhedra -joinbwd 3 > logs/recurrence/simple_recu.log     # x < 0

###

### recurrence (CTL-CFG) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/recurrence/countdown.c -ctl-cfg "AG{AF{x == 0}}" -domain polyhedra > logs/recurrence/countdown_recuCFG#TODO.log				      # TODO: ?
./function tests/recurrence/peterson.c -ctl-cfg "AG{AF{C1: true}}" -domain polyhedra > logs/recurrence/peterson_recuCFG.log

### recurrence (CTL-CFG) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/recurrence/sink.c -ctl-cfg "AG{AF{x == 0}}" -domain polyhedra -ordinals 1 > logs/recurrence/sink_recuCFG#TODO.log            # TODO: ?

# conditional recurrence (CTL-CFG)

./function tests/recurrence/simple.c -ctl-cfg "AG{AF{x == 3}}" -domain polyhedra -joinbwd 3 > logs/recurrence/simple_recuCFG#TODO.log			    # TODO: ?

###

### recurrence (CTL-AST) with polyhedra:
### -domain polyhedra
### -joinbwd 2

./function tests/recurrence/countdown.c -ctl-ast "AG{AF{x == 0}}" -domain polyhedra > logs/recurrence/countdown_recuAST.log
./function tests/recurrence/peterson.c -ctl-ast "AG{AF{C1: true}}" -domain polyhedra > logs/recurrence/peterson_recuAST.log

### recurrence (CTL-AST) with polyhedra and ordinals:
### -domain polyhedra
### -joinbwd 2
### -ordinals 1

./function tests/recurrence/sink.c -ctl-ast "AG{AF{x == 0}}" -domain polyhedra -ordinals 1 > logs/recurrence/sink_recuAST.log

# conditional recurrence (CTL-AST)

./function tests/recurrence/simple.c -ctl-ast "AG{AF{x == 3}}" -domain polyhedra -joinbwd 3 > logs/recurrence/simple_recuAST.log        # x < 0