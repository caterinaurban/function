./function -domain polyhedra -joinbwd 4 "./tests/ctl/global_test_simple.c" -ctl_str "AG{AF{x <= -10}}" | python ./pretty.py > ./tests/ctl/global_test_simple.html
./function -domain polyhedra -precondition "x == y + 20" "./tests/ctl/until_test.c" -ctl_str "AU{x >= y}{x==y}"| python ./pretty.py > ./tests/ctl/until_test.html
./function -domain polyhedra -precondition "n > 0" "./tests/ctl/and_test.c" -ctl_str "AND{AG{AF{n==1}}}{AF{n==0}}"| python ./pretty.py > ./tests/ctl/and_test.html
./function -domain polyhedra "./tests/ctl/or_test.c" -ctl_str "OR{AF{AG{x < -100}}}{AF{x==20}}"| python ./pretty.py > ./tests/ctl/or_test.html
./function -domain polyhedra -precondition "x==1" "./tests/ctl/next.c" -ctl_str "AX{x==0}"| python ./pretty.py > ./tests/ctl/next.html
./function -domain polyhedra -precondition "2*x <= y+3" "./tests/ctl/existential_test1.c" -ctl_str "EF{r==1}"| python ./pretty.py > ./tests/ctl/existential_test1.html
./function -domain polyhedra -precondition "a!=1" "./tests/ctl/acqrel.c" -ctl_str "AG{OR{a!=1}{AF{r==1}}}"| python ./pretty.py > ./tests/ctl/acqrel.html
./function -domain polyhedra "./tests/ctl/win4.c" -ctl_str "AF{AG{WItemsNum >= 1}}"| python ./pretty.py > ./tests/ctl/win4.html
