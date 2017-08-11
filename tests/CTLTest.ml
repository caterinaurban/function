(*******************************************)
(*                                         *)
(*                CTL Test                 *)
(*                                         *)
(*             Samuel Ueltschi             *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2017                   *)
(*                                         *)
(*******************************************)

open OUnit

type domain = BOXES | POLYHEDRA

let string_of_domain d = match d with 
  | BOXES -> "boxes" 
  | POLYHEDRA -> "polyhedra"

let (--) (filename, property) expected =
  filename >:: (fun test_ctxt ->
    assert_bool "Test marked as not working but non-terminating" expected;
    todo (filename ^ "Doesn't work yet")
  )

let test_ast file property 
    ?(precondition = "true")
    ?(domain = POLYHEDRA) 
    ?(joinbwd = 2)
    ?setup
  = TestCommon.make_analyser [
    "-minimal";
    "-ast";
    "-domain"; string_of_domain domain; 
    "-joinbwd"; string_of_int joinbwd; 
    "-precondition"; precondition;
    "-ctl"; property;
  ] ?setup:setup file

let test_cfg file property 
    ?(precondition = "true")
    ?(domain = POLYHEDRA) 
    ?(joinbwd = 2)
    ?setup
  = TestCommon.make_analyser [
    "-minimal";
    "-domain"; string_of_domain domain; 
    "-joinbwd"; string_of_int joinbwd; 
    "-precondition"; precondition;
    "-ctl"; property;
  ] ?setup:setup file

let ctl_ast_testcases = "ctl_ast" >:::
[

  test_ast ~joinbwd:4 "./tests/ctl/global_test_simple.c" "AG{AF{x <= -10}}" true;
  test_ast ~precondition:"x == y + 20" "./tests/ctl/until_test.c" "AU{x >= y}{x==y}" true;
  test_ast "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false;
  (* NOTE: Currently can't use BOXES domain because of missing underapproximation support *)
  (* test ~domain:BOXES "./tests/ctl/until_test.c" "AF{x < y + 20}" false; *) 
  test_ast "./tests/countdown.c" "AF{x == 0}" true;
  test_ast "./tests/countdown.c" "AG{AF{x == 0}}" true;
  test_ast "./tests/mnav.c" "AF{enable == 0}" true;
  test_ast "./tests/peterson.c" "AF{C1: true}" true;
  test_ast "./tests/peterson.c" "AG{AF{C1: true}}" true;
  test_ast "./tests/pingpong.c" "AF{z==1}" true;
  test_ast ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true;
  test_ast ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}" true;
  test_ast ~precondition: "n > 0" "./tests/ctl/and_test.c" "AND{AG{AF{n==1}}}{AF{n==0}}" true;
  test_ast "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}" true;
  test_ast ~precondition: "x==1" "./tests/ctl/next.c" "AX{x==0}" true;
  test_ast "./tests/ctl/next.c" "AX{x==0}" false;
  test_ast "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_ast ~precondition:"2*x <= y+3" "./tests/ctl/existential_test1.c" "EF{r==1}" true;
  test_ast "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_ast "./tests/ctl/existential_test2.c" "EF{r==1}" false;
  test_ast "./tests/ctl/existential_test3.c" "EF{r==1}" false;
  test_ast 
    ~setup:["-ctl_existential_equivalence"] 
    ~precondition: "x > 0"
    "./tests/ctl/existential_test3.c" "EF{r==1}" true;
  test_ast 
    ~joinbwd:5
    ~precondition: "x==2"
    "./tests/ctl/existential_test3.c" "EF{r==1}" true;
  test_ast ~precondition:"y<0" "./tests/ctl/existential_test4.c" "EF{r==1}" true;
  test_ast ~precondition:"a!=1" "./tests/ctl/koskinen/acqrel_mod.c" "AG{OR{a!=1}{AF{r==1}}}" true;
  test_ast 
    ~setup:["-ordinals"; "3"] 
    ~precondition:"A==0 && R==0" "./tests/ctl/koskinen/acqrel.c" "AG{OR{A!=1}{AF{R==1}}}" true;
  test_ast "./tests/ctl/koskinen/win4.c"  "AF{AG{WItemsNum >= 1}}" true;

]


let ctl_cfg_testcases = "ctl_cfg" >:::
[

  (* Test Cases for the report*)
  test_cfg ~precondition:"x >= y" "./tests/ctl/report/test_until.c" "AU{x >= y}{x==y}" true;
  test_cfg ~precondition:"x < 10" "./tests/ctl/report/test_global.c" "AF{AG{y > 0}}" true;
  test_cfg ~precondition:"x < 200" "./tests/ctl/report/test_existential2.c" "EF{r==1}" true;
  test_cfg 
    ~joinbwd:5
    ~precondition: "x==2"
    "./tests/ctl/report/test_existential3.c" "EF{r==1}" true;
  test_cfg ~setup:["-ctl_existential_equivalence"] 
    ~precondition: "x > 0"
    "./tests/ctl/report/test_existential3.c" "EF{r==1}" true;

  (* Toy Test Cases*)
  test_cfg ~joinbwd:6 "./tests/ctl/global_test_simple.c" "AG{AF{x <= -10}}" true;
  test_cfg ~precondition:"x == y + 20" "./tests/ctl/until_test.c" "AU{x >= y}{x==y}" true;
  test_cfg "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false;
  test_cfg "./tests/countdown.c" "AF{x == 0}" true;
  test_cfg "./tests/countdown.c" "AG{AF{x == 0}}" true;
  test_cfg "./tests/peterson.c" "AF{C1: true}" true;
  test_cfg "./tests/peterson.c" "AG{AF{C1: true}}" true;
  test_cfg "./tests/mnav.c" "AF{enable == 0}" true;
  test_cfg "./tests/pingpong.c" "AF{z==1}" true;
  test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true;
  test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}" true;
  test_cfg ~precondition: "n > 0" "./tests/ctl/and_test.c" "AND{AG{AF{n==1}}}{AF{n==0}}" true;
  test_cfg "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}" true;
  test_cfg ~precondition: "x==1" "./tests/ctl/next.c" "AX{AX{AX{x==0}}}" true;
  test_cfg "./tests/ctl/next.c" "AX{AX{x==0}}" false;
  test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_cfg ~precondition:"2*x <= y+3" "./tests/ctl/existential_test1.c" "EF{r==1}" true;
  test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_cfg "./tests/ctl/existential_test2.c" "EF{r==1}" false;
  test_cfg "./tests/ctl/existential_test3.c" "EF{r==1}" false;
  test_cfg 
    ~setup:["-ctl_existential_equivalence"] 
    ~precondition: "x > 0"
    "./tests/ctl/existential_test3.c" "EF{r==1}" true;
  test_cfg 
    ~joinbwd:5
    ~precondition: "x==2"
    "./tests/ctl/existential_test3.c" "EF{r==1}" true;
  test_cfg ~precondition:"y<0" "./tests/ctl/existential_test4.c" "EF{r==1}" true;
  test_cfg ~precondition:"a!=1" "./tests/ctl/koskinen/acqrel_mod.c" "AG{OR{a!=1}{AF{r==1}}}" true;
  test_cfg 
    ~setup:["-ordinals"; "3"] 
    ~precondition:"A==0 && R==0" "./tests/ctl/koskinen/acqrel.c" "AG{OR{A!=1}{AF{R==1}}}" true;
  test_cfg "./tests/ctl/koskinen/win4.c"  "AF{AG{WItemsNum >= 1}}" true;
  test_cfg ~joinbwd:4 "./tests/ctl/koskinen/fig8-2007_mod.c" "OR{set==0}{AF{unset == 1}}" true;

  test_cfg 
    "./tests/ctl/multi_branch_choice.c"
    "AF{OR{x==4}{x==-4}}" true;

  test_cfg 
    "./tests/ctl/multi_branch_choice.c"
    "AND{EF{x==4}}{EF{x==-4}}" true;

  test_cfg 
    "./tests/ctl/potential_termination_1.c"
    "EF{exit: true}" true;

  (* Some SV Comp testcases *)

  test_cfg 
    "./tests/ctl/sv_comp/Bangalore_false-no-overflow.c" 
    "EF{x < 0}" true;

  test_cfg 
    "./tests/ctl/sv_comp/Ex02_false-termination_true-no-overflow.c"
    "OR{i >= 5}{AF{exit: true}}" true;

  test_cfg 
    "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
    "AF{AG{i==0}}" true;

  test_cfg 
    "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
    "AF{AND{AF{j >= 21}}{i==100}}" true;

  test_cfg 
    "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
    "AF{AND{x==7}{AF{AG{x==2}}}}" true;

  test_cfg 
    "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
    "AF{AG{j==0}}" true;


  (* (1* Testcases from Ultimate LTL Automizer *1) *)

  test_cfg ~precondition:"chainBroken == 0" 
    "./tests/ctl/ltl_automizer/coolant_basis_1_safe_sfty.c" 
    "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" true;

  test_cfg ~precondition:"chainBroken == 0" 
    "./tests/ctl/ltl_automizer/coolant_basis_1_unsafe_sfty.c" 
    "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" false;

  test_cfg ~joinbwd:7
    "./tests/ctl/ltl_automizer/coolant_basis_2_safe_lifeness.c"
    "AG{AF{otime < time}}" true;

  test_cfg ~joinbwd:7
    "./tests/ctl/ltl_automizer/coolant_basis_2_unsafe_lifeness.c"
    "AG{AF{otime < time}}" false;

  test_cfg ~precondition:"init == 0" 
    "./tests/ctl/ltl_automizer/coolant_basis_3_safe_sfty.c" 
    "AG{OR{init != 3}{AG{AF{time > otime}}}}" true;

  test_cfg ~precondition:"init == 0" 
    "./tests/ctl/ltl_automizer/coolant_basis_3_unsafe_sfty.c" 
    "AG{OR{init != 3}{AG{AF{time > otime}}}}" false;

  test_cfg ~precondition:"init == 0 && temp < limit"
    "./tests/ctl/ltl_automizer/coolant_basis_4_safe_sfty.c" 
    "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}" true;

  (*TODO performs realy bad, I suspect because of a problem in 'dual_widen' *)
  (* test_cfg ~precondition:"init == 0 && temp < limit" *)
  (*   "./tests/ctl/ltl_automizer/coolant_basis_4_unsafe_sfty.c" *) 
  (*   "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}" false; *)

  (* Analysis can't prove coolant_basis_5_safe_sfty.c, but we can show a slightly modified example: *)
  test_cfg ~precondition:"init == 0"
    "./tests/ctl/ltl_automizer/coolant_basis_5_safe_cheat.c" 
    "AU{init == 0}{OR{AU{init == 1}{AG{init == 3}}}{AG{init == 1}}}" true;

  test_cfg ~precondition:"init == 0 && temp < limit"
    "./tests/ctl/ltl_automizer/coolant_basis_6_safe_sfty.c" 
    "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED == 1}}}}" true;

  test_cfg ~precondition:"init == 0 && temp < limit"
    "./tests/ctl/ltl_automizer/coolant_basis_6_unsafe_sfty.c" 
    "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED == 1}}}}" false;


  test_cfg ~precondition:"i == 1 && n >= 0 && i > n"
    "./tests/ctl/ltl_automizer/nestedRandomLoop_true-valid-ltl.c" 
    "AG{i >= n}" true;

  (* imprecision *)
  ("./tests/ctl/ltl_automizer/timer-simple.c", "NOT{AG{OR{timer_1 != 0}{AF{output_1 == 1}}}}") -- true;

  (* imprecision due to modulo *)
  ("./tests/ctl/ltl_automizer/togglecounter_true-valid-ltl.c", "AG{AND{AF{t == 1}}{AF{t == 0}}}") -- true;

  test_cfg ~precondition:"i == 1 && n >= 0 && i > n"
    "./tests/ctl/ltl_automizer/nestedRandomLoop_true-valid-ltl.c" 
    "AG{i >= n}" true;

  test_cfg ~precondition:"t >= 0"
    "./tests/ctl/ltl_automizer/toggletoggle_true-valid-ltl.c" 
    "AG{AND{AF{t==1}}{AF{t==0}}}" true;

  test_cfg ~precondition:"x < 0"
    ~setup:["-ordinals"; "3"] 
    "./tests/ctl/ltl_automizer/PotentialMinimizeSEVPABug.c" 
    "AG{OR{x <= 0}{AF{y == 0}}}" true;

  test_cfg ~precondition:"x < 0"
    ~setup:["-ordinals"; "3"] 
    "./tests/ctl/ltl_automizer/cav2015.c" 
    "AG{OR{x <= 0}{AF{y == 0}}}" true;

  test_cfg 
    "./tests/ctl/ltl_automizer/simple-1.c" 
    "AF{x > 10000}" true;

  test_cfg 
    "./tests/ctl/ltl_automizer/simple-2.c" 
    "AF{x > 100}" true;

  test_cfg ~precondition:"ap==0"
    "./tests/ctl/ltl_automizer/Bug_NoLoopAtEndForTerminatingPrograms_safe.c" 
    "NOT{AF{ap > 2}}" true;

]
