./Main.native -dot -domain polyhedra -joinbwd 10 "./tests/ctl/global_test_simple.c" -ctl "AG{AF{x <= -10}}" | python ./pretty_cfg.py > ./tests/ctl/global_test_simple_cfg.html
./Main.native -dot -domain polyhedra -precondition "x == y + 20" "./tests/ctl/until_test.c" -ctl "AU{x >= y}{x==y}"| python ./pretty_cfg.py > ./tests/ctl/until_test_cfg.html
./Main.native -dot -domain polyhedra -precondition "n > 0" "./tests/ctl/and_test.c" -ctl "AND{AG{AF{n==1}}}{AF{n==0}}"| python ./pretty_cfg.py > ./tests/ctl/and_test_cfg.html
./Main.native -dot -domain polyhedra "./tests/ctl/or_test.c" -ctl "OR{AF{AG{x < -100}}}{AF{x==20}}"| python ./pretty_cfg.py > ./tests/ctl/or_test_cfg.html
./Main.native -dot -domain polyhedra -precondition "x==1" "./tests/ctl/next.c" -ctl "AX{AX{AX{x==0}}}"| python ./pretty_cfg.py > ./tests/ctl/next_cfg.html
./Main.native -dot -domain polyhedra -precondition "2*x <= y+3" "./tests/ctl/existential_test1.c" -ctl "EF{r==1}"| python ./pretty_cfg.py > ./tests/ctl/existential_test1_cfg.html
./Main.native -dot -domain polyhedra -precondition "a!=1" "./tests/ctl/acqrel.c" -ctl "AG{OR{a!=1}{AF{r==1}}}"| python ./pretty_cfg.py > ./tests/ctl/acqrel_cfg.html
./Main.native -dot -domain polyhedra -joinbwd 5 "./tests/ctl/win4.c" -ctl "AF{AG{WItemsNum >= 1}}"| python ./pretty_cfg.py > ./tests/ctl/win4_cfg.html


./Main.native -dot -domain polyhedra ./tests/ctl/ltl_automizer/coolant_basis_1_safe_sfty.c -ctl "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" -precondition "chainBroken == 0" | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_1_safe_sfty.html 
./Main.native -dot -domain polyhedra -joinbwd 7 ./tests/ctl/ltl_automizer/coolant_basis_2_safe_lifeness.c -ctl "AG{AF{otime < time}}" | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_2_safe_lifeness.html
./Main.native -dot -domain polyhedra tests/ctl/ltl_automizer/coolant_basis_3_safe_sfty.c -ctl "AG{OR{init != 3}{AG{AF{time > otime}}}}" -precondition "init == 0" | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_3_safe_sfty.html 
./Main.native -dot -domain polyhedra tests/ctl/ltl_automizer/coolant_basis_4_safe_sfty.c -ctl "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}" -precondition "init == 0 && temp < limit" | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_4_safe_sfty.html 
./Main.native -dot -domain polyhedra tests/ctl/ltl_automizer/coolant_basis_5_safe_cheat.c -ctl "AU{init == 0}{OR{AU{init == 1}{AG{init == 3}}}{AG{init == 1}}}" -precondition "init==0" | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_5_safe_cheat.html 
./Main.native -dot -domain polyhedra tests/ctl/ltl_automizer/coolant_basis_6_safe_sfty.c -ctl "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED == 1}}}}" -precondition "init == 0 && temp < limit"  | python ./pretty_cfg.py > ./tests/ctl/ltl_automizer/coolant_basis_6_safe_sfty.html 

./main.exe -ctl "AF{exit: true}" -dot -domain polyhedra -joinbwd 3 "./tests/example.c" | python3 ./pretty_cfg.py > ./tests/example.html