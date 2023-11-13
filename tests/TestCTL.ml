(* 
(*******************************************)
(*                CTL Test                 *)
(*                                         *)
(*             Samuel Ueltschi             *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2017                   *)
(*                                         *)
(*******************************************)
open TestCommon

let%test "test_until " = testit ~precond: "x >= y"  ~prop: "AU{x >= y}{x==y}" ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/report/test_until.c"
let%test "test_global " = testit ~precond: "x < 10"  ~prop: "AF{AG{y > 0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/report/test_global.c"
let%test "test_existential2" = testit ~precond: "x < 200"  ~prop: "EF{r==1}" ~ctl_eq:true  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/report/test_existential2.c"
let%test "test_existential3a" = testit ~precond: "x == 2"  ~prop:  "EF{r==1}" ~ctl_eq:true  ~dom:POLYHEDRA  ~joinbwd:5 ~analysis_type:"ctl"  "ctl/report/test_existential3.c"
let%test "test_existential3b" = testit ~precond: "x>0"  ~prop:  "EF{r==1}" ~ctl_eq:true   ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/report/test_existential3.c" 
let%test "test_existential3c" = testit ~precond: "x>0"  ~prop:  "EF{r==1}" ~ctl_eq:true   ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/report/test_existential3.c" 
let%test "test_gsimple" = testit ~prop:"AG{AF{x <= -10}}"~dom:POLYHEDRA  ~joinbwd:6  ~analysis_type:"ctl"  "ctl/global_test_simple.c" 
let%test "test_until" = testit ~precond:"x == y + 20"  ~prop:"AU{x >= y}{x==y}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/until_test.c" 
let%test "test_countdowna" = testit   ~prop:"AF{x==0}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/countdown.c" 
let%test "test_countdownb" = testit   ~prop:"AG{AF{x==0}}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/countdown.c" 
let%test "test_petersona" = testit   ~prop:"AF{C1: true}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/peterson.c" 
let%test "test_petersonb" = testit   ~prop:"AG{AF{C1: true}}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/peterson.c" 
let%test "test_mnav" = testit   ~prop:"AF{enable == 0 }"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/mnav.c" 
let%test "test_pingpong" = testit   ~prop:"AF{z==1}"  ~dom:POLYHEDRA ~analysis_type:"ctl"  "guarantee/pingpong.c" 
let%test "test_sink1a" = testit   ~prop:"AF{x==0}"  ~dom:POLYHEDRA ~analysis_type:"ctl" ~ord:(true,1) "guarantee/sink.c" 
let%test "test_sink1b" = testit   ~prop:"AG{AF{x==0}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" ~ord:(true,1) "guarantee/sink.c" 
let%test "test_andctl" = testit  ~precond:"n > 0" ~prop:"AND{AG{AF{n==1}}}{AF{n==0}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/and_test.c" 
let%test "test_orctl" = testit   ~prop:"OR{AF{AG{x < -100}}}{AF{x==20}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/or_test.c" 
let%test "test_nnnextctl" = testit   ~prop:"AX{AX{AX{x==0}}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/next.c" 
let%test "test_nnextctl" = testit   ~prop:"AX{AX{x==0}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/next.c"  = false
let%test "test_existential1a" = testit ~prop: "EF{r==1}" ~ctl_eq:true  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test1.c" = false
let%test "test_existential1b" = testit  ~precond:"2*x <= y + 3" ~prop: "EF{r==1}" ~ctl_eq:true  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test1.c" 
let%test "test_existential2" = testit ~prop:"EF{r==1}" ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test2.c" = false
let%test "test_existential3a" = testit ~prop:"EF{r==1}" ~ctl_eq:true  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test3.c" = false
let%test "test_existential3b" = testit ~prop:"EF{r==1}" ~precond:"x > 0" ~ctl_eq:true  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test3.c" 
let%test "test_existential3c" = testit ~prop:"EF{r==1}" ~precond:"x == 2" ~joinbwd:5  ~dom:POLYHEDRA ~analysis_type:"ctl"  "ctl/existential_test3.c" 
let%test "test_existential4" = testit ~prop:"EF{r==1}" ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/existential_test4.c"
let%test "test_koskinen_acqrel" = testit ~precond:"a!=1" ~prop:"AG{OR{a!=1}{AF{r==1}}}" ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/koskinen/acqrel_mod.c"
let%test "test_fin_ex" = testit ~precond:"n > 0" ~prop:"EG{EF{n==1}}" ~ctl_eq:true ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/fin_ex.c"
let%test "test_until_exist" = testit ~precond:"x > y" ~prop:"EU{x >= y}{x == y}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/until_existential.c"
let%test "test_koskinen_acqrel2" = testit ~precond:"a==0 && r==0" ~prop:"AG{OR{r!=1}{AF{r==1}}}" ~ord:(true,3) ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/koskinen/acqrel_mod.c"
let%test "test_koskinen_win4" = testit  ~prop:"AF{AG{WItemsNum >= 1}}" ~ord:(true,3) ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/koskinen/win4.c"
let%test "test_fig8-2007_mod" = testit  ~prop:"OR{set==0}{AF{unset == 1}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/koskinen/fig8-2007_mod.c"
let%test "test_multi_branch_choice" = testit  ~prop:"AF{OR{x==4}{x==-4}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/multi_branch_choice.c"
let%test "test_multi_branch_choice2" = testit  ~prop:"AND{EF{x==4}}{EF{x==-4}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/multi_branch_choice.c"
(* Samuel's com:  can't handle existential non-det assignments *)
 let%test "test_potential_termination_1" = testit  ~prop:"EF{exit: true}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/potential_termination_1.c"
let%test "sv_comp_Bangalore_false" = testit  ~prop:"EF{x < 0}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Bangalore_false-no-overflow.c"
let%test "sv_comp_Ex02_false-termination_true-no-overflow1" = testit  ~prop:"OR{i >= 5}{AF{exit: true}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Ex02_false-termination_true-no-overflow.c"
let%test "sv_comp_Ex07_false-termination_true-no-overflow2" = testit  ~prop:"AF{AG{i==0}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
let%test "sv_comp_Ex07_false-termination_true-no-overflow3" = testit  ~prop:"EF{EG{i==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
let%test "sv_comp_Ex07_false-termination_true-no-overflow4" = testit  ~prop:"EF{AG{i==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
let%test "sv_comp_Ex07_false-termination_true-no-overflow5" = testit  ~prop:"AF{EG{i==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
let%test "sv_comp_java_Sequence_true-termination_true-no-overflow" = testit  ~prop:"AF{AND{AF{j >= 21}}{i==100}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
let%test "sv_comp_java_Sequence_true-termination_true-no-overflow2" = testit  ~prop:"AF{AND{EF{j >= 21}}{i==100}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
let%test "sv_comp_java_Sequence_true-termination_true-no-overflow3" = testit  ~prop:"EF{AND{AF{j >= 21}}{i==100}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
let%test "sv_comp_java_Sequence_true-termination_true-no-overflow4" = testit  ~prop:"EF{AND{EF{j >= 21}}{i==100}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
let%test "sv_comp_Madrid_true-no-overflow_false-termination_true-valid-memsafety" = testit  ~prop:"AF{AND{x==7}{AF{AG{x==2}}}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
let%test "sv_comp_Madrid_true-no-overflow_false-termination_true-valid-memsafety2" = testit  ~prop:"AF{AND{x==7}{AF{EG{x==2}}}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
let%test "sv_comp_Madrid_true-no-overflow_false-termination_true-valid-memsafety3" = testit  ~prop:"AF{AND{x==7}{EF{AG{x==2}}}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
let%test "sv_comp_Madrid_true-no-overflow_false-termination_true-valid-memsafety4" = testit  ~prop:"AF{AND{x==7}{EF{EG{x==2}}}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
let%test "sv_comp_NO_02_false-termination_true-no-overflow" = testit  ~prop:"AF{AG{j==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
let%test "sv_comp_NO_02_false-termination_true-no-overflow2" = testit  ~prop:"EF{AG{j==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
let%test "sv_comp_NO_02_false-termination_true-no-overflow3" = testit  ~prop:"EF{EG{j==0}}"   ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/sv_comp/NO_02_false-termination_true-no-overflow.c" 

   
(*
    (*Samuel's comment :TODO performs realy bad, I suspect because of a problem in 'dual_widen' *)
    let%test "LTLaut_coolant_basis_4_unsafe_sfty" = testit  ~prop:"AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}" ~precond:"init == 0 && temp < limit"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_4_unsafe_sfty.c" = false 
    (* Analysis can't prove coolant_basis_5_safe_sfty.c, but we can show
            a slightly modified example: *)
    let%test "LTLaut_coolant_basis_1_safe_sfty" = testit  ~prop:"AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" ~precond:"chainBroken == 0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_1_safe_sfty.c"
    let%test "LTLaut_coolant_basis_1_unsafe_sfty" = testit  ~prop:"AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" ~precond:"chainBroken == 0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_1_unsafe_sfty.c" = false
    let%test "LTLaut_coolant_basis_1_safe_liveness" = testit  ~prop:"AG{AF{otime < time}}" ~joinbwd:7  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_1_safe_liveness.c"
    let%test "LTLaut_coolant_basis_1_unsafe_liveness" = testit  ~prop:"AG{AF{otime < time}}" ~joinbwd:7  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_1_unsafe_liveness.c" = false
    let%test "LTLaut_coolant_basis_3_safe_sfty" = testit  ~prop:"AG{OR{init != 3}{AG{AF{time > otime}}}" ~precond:"init == 0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_3_safe_sfty.c"
    let%test "LTLaut_coolant_basis_3_unsafe_sfty" = testit  ~prop:"AG{OR{init != 3}{AG{AF{time > otime}}}" ~precond:"init == 0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_3_unsafe_sfty.c" = false
    let%test "LTLaut_coolant_basis_4_safe_sfty" = testit  ~prop:"AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}" ~precond:"init == 0 && temp < limit"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_4_safe_sfty.c" 
    let%test "LTLaut_coolant_basis_5_safe_sfty" = testit  ~prop:"AU{init == 0}{OR{AU{init == 1}{AG{init == 3}}}{AG{init == 1}}}" ~precond:"init == 0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_5_safe_cheat.c" 
    let%test "LTLaut_coolant_basis_6_safe_sfty" = testit  ~prop:"AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED == 1}}}} " ~precond:"init == 0 && temp < limit"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_6_safe_sfty.c"
    let%test "LTLaut_coolant_basis_6_unsafe_sfty" = testit  ~prop:"AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED == 1}}}} " ~precond:"init == 0 && temp < limit"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/coolant_basis_6_unsafe_sfty.c" = false
    let%test "LTLaut_nestedRandomLoop_true-valid-ltl" = testit  ~prop:"AG{i >= n}" ~precond:"i == 1 && n >= 0 && i > n"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/nestedRandomLoop_true-valid-ltl.c"  
    let%test "LTLaut_timer" = testit  ~prop:"NOT{AG{OR{timer_1 != 0}{AF{output_1 == 1}}}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/timer-simple.c" 
        (* imprecision due to modulo *)
    let%test "LTLaut_togglecounter_true-valid-ltl" = testit  ~prop:"AG{AND{AF{t == 1}}{AF{t == 0}}}"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/togglecounter_true-valid-ltl.c" 
    let%test "LTLaut_togglecounter_true-valid-ltl" = testit  ~prop:"AG{AND{AF{t==1}}{AF{t==0}}}" ~precond:"t>=0"  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/togglecounter_true-valid-ltl.c" 
    let%test "LTLaut_PotentialMinimizeSEVPABug" = testit  ~prop:"AG{OR{x <= 0}{AF{y == 0}}}" ~precond:"x < 0" ~ord:(true,3)  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/PotentialMinimizeSEVPABug.c" 
    let%test "LTLaut_cav2015" = testit  ~prop:"AG{OR{x <= 0}{AF{y == 0}}}" ~precond:"x < 0" ~ord:(true,3)  ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/cav2015.c" 
    let%test "LTLaut_simple1" = testit  ~prop:"AF{x > 10000}" ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/simple-1.c" 
    let%test "LTLaut_simple2" = testit  ~prop:"AF{x > 100}" ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/simple-2.c" 
    let%test "LTLaut_Bug_NoLoopAtEndForTerminatingPrograms_safe" = testit  ~prop:"NOT{AF{ap > 2}}" ~precond:"ap==0" ~dom:POLYHEDRA ~analysis_type:"ctl" "ctl/ltl_automizer/Bug_NoLoopAtEndForTerminatingPrograms_safe.c"   

    *)





(***  Former test cases with oUnit by Samuel  ***)
(*let ctl_testcases =
    "ctl"
    >::: [ (* Test Cases for the report*)
            test_cfg ~precondition:"x >= y" "./tests/ctl/report/test_until.c"
            "AU{x >= y}{x==y}" true
        ; test_cfg ~precondition:"x < 10" "./tests/ctl/report/test_global.c"
            "AF{AG{y > 0}}" true
        ; test_cfg ~precondition:"x < 200"
            "./tests/ctl/report/test_existential2.c" "EF{r==1}" true
        ; test_cfg ~joinbwd:5 ~precondition:"x==2"
            "./tests/ctl/report/test_existential3.c" "EF{r==1}" true
        ; test_cfg
            ~setup:["-ctl_existential_equivalence"]
            ~precondition:"x > 0" "./tests/ctl/report/test_existential3.c"
            "EF{r==1}" true
        ; (* Toy Test Cases*)
            test_cfg ~joinbwd:6 "./tests/ctl/global_test_simple.c"
            "AG{AF{x <= -10}}" true
        ; test_cfg ~precondition:"x == y + 20" "./tests/ctl/until_test.c"
            "AU{x >= y}{x==y}" true
        ; test_cfg "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false
        ; test_cfg "./tests/countdown.c" "AF{x == 0}" true
        ; test_cfg "./tests/countdown.c" "AG{AF{x == 0}}" true
        ; test_cfg "./tests/peterson.c" "AF{C1: true}" true
        ; test_cfg "./tests/peterson.c" "AG{AF{C1: true}}" true
        ; test_cfg "./tests/mnav.c" "AF{enable == 0}" true
        ; test_cfg "./tests/pingpong.c" "AF{z==1}" true
        ; test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true
        ; test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}"
            true
        ; test_cfg ~precondition:"n > 0" "./tests/ctl/and_test.c"
            "AND{AG{AF{n==1}}}{AF{n==0}}" true
        ; test_cfg "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}"
            true
        ; test_cfg ~precondition:"x==1" "./tests/ctl/next.c"
            "AX{AX{AX{x==0}}}" true
        ; test_cfg "./tests/ctl/next.c" "AX{AX{x==0}}" false
        ; test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false
        ; test_cfg ~precondition:"2*x <= y+3"
            "./tests/ctl/existential_test1.c" "EF{r==1}" true
        ; test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false
        ; test_cfg "./tests/ctl/existential_test2.c" "EF{r==1}" false
        ; test_cfg "./tests/ctl/existential_test3.c" "EF{r==1}" false
        ; test_cfg
            ~setup:["-ctl_existential_equivalence"]
            ~precondition:"x > 0" "./tests/ctl/existential_test3.c" "EF{r==1}"
            true
        ; test_cfg ~joinbwd:5 ~precondition:"x==2"
            "./tests/ctl/existential_test3.c" "EF{r==1}" true
        ; test_cfg "./tests/ctl/existential_test4.c" "EF{r==1}" true
        ; test_cfg ~precondition:"a!=1" "./tests/ctl/koskinen/acqrel_mod.c"
            "AG{OR{a!=1}{AF{r==1}}}" true
        ; test_cfg ~precondition:"n > 0" "./tests/ctl/fin_ex.c" "EG{EF{n==1}}"
            true
        ; test_cfg ~precondition:"x > y" "./tests/ctl/until_existential.c"
            "EU{x >= y}{x == y}" true
        ; test_cfg ~setup:["-ordinals"; "3"] ~precondition:"A==0 && R==0"
            "./tests/ctl/koskinen/acqrel.c" "AG{OR{A!=1}{AF{R==1}}}" true
        ; test_cfg "./tests/ctl/koskinen/win4.c" "AF{AG{WItemsNum >= 1}}" true
        ; test_cfg ~joinbwd:4 "./tests/ctl/koskinen/fig8-2007_mod.c"
            "OR{set==0}{AF{unset == 1}}" true
        ; test_cfg "./tests/ctl/multi_branch_choice.c" "AF{OR{x==4}{x==-4}}"
            true(* -- SV COMP --*)
    (* 
    let ctl_testcases =
    "ctl"
    >::: [ (* Test Cases for the report*)
            test_cfg ~precondition:"x >= y" "./tests/ctl/report/test_until.c"
            "AU{x >= y}{x==y}" true
        ; test_cfg ~precondition:"x < 10" "./tests/ctl/report/test_global.c"
            "AF{AG{y > 0}}" true
        ; test_cfg ~precondition:"x < 200"
            "./tests/ctl/report/test_existential2.c" "EF{r==1}" true
        ; test_cfg ~joinbwd:5 ~precondition:"x==2"
            "./tests/ctl/report/test_existential3.c" "EF{r==1}" true
        ; test_cfg
            ~setup:["-ctl_existential_equivalence"]
            ~precondition:"x > 0" "./tests/ctl/report/test_existential3.c"
            "EF{r==1}" true
        ; (* Toy Test Cases*)
            test_cfg ~joinbwd:6 "./tests/ctl/global_test_simple.c"
            "AG{AF{x <= -10}}" true
        ; test_cfg ~precondition:"x == y + 20" "./tests/ctl/until_test.c"
            "AU{x >= y}{x==y}" true
        ; test_cfg "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false
        ; test_cfg "./tests/countdown.c" "AF{x == 0}" true
        ; test_cfg "./tests/countdown.c" "AG{AF{x == 0}}" true
        ; test_cfg "./tests/peterson.c" "AF{C1: true}" true
        ; test_cfg "./tests/peterson.c" "AG{AF{C1: true}}" true
        ; test_cfg "./tests/mnav.c" "AF{enable == 0}" true
        ; test_cfg "./tests/pingpong.c" "AF{z==1}" true
        ; test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true
        ; test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}"
            true
        ; test_cfg ~precondition:"n > 0" "./tests/ctl/and_test.c"
            "AND{AG{AF{n==1}}}{AF{n==0}}" true
        ; test_cfg "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}"
            true
        ; test_cfg ~precondition:"x==1" "./tests/ctl/next.c"
            "AX{AX{AX{x==0}}}" true
        ; test_cfg "./tests/ctl/next.c" "AX{AX{x==0}}" false
        ; test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false
        ; test_cfg ~precondition:"2*x <= y+3"
            "./tests/ctl/existential_test1.c" "EF{r==1}" true
        ; test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false
        ; test_cfg "./tests/ctl/existential_test2.c" "EF{r==1}" false
        ; test_cfg "./tests/ctl/existential_test3.c" "EF{r==1}" false
        ; test_cfg
            ~setup:["-ctl_existential_equivalence"]
            ~precondition:"x > 0" "./tests/ctl/existential_test3.c" "EF{r==1}"
            true
        ; test_cfg ~joinbwd:5 ~precondition:"x==2"
            "./tests/ctl/existential_test3.c" "EF{r==1}" true
        ; test_cfg "./tests/ctl/existential_test4.c" "EF{r==1}" true
        ; test_cfg ~precondition:"a!=1" "./tests/ctl/koskinen/acqrel_mod.c"
            "AG{OR{a!=1}{AF{r==1}}}" true
        ; test_cfg ~precondition:"n > 0" "./tests/ctl/fin_ex.c" "EG{EF{n==1}}"
            true
        ; test_cfg ~precondition:"x > y" "./tests/ctl/until_existential.c"
            "EU{x >= y}{x == y}" true
        ; test_cfg ~setup:["-ordinals"; "3"] ~precondition:"A==0 && R==0"
            "./tests/ctl/koskinen/acqrel.c" "AG{OR{A!=1}{AF{R==1}}}" true
        ; test_cfg "./tests/ctl/koskinen/win4.c" "AF{AG{WItemsNum >= 1}}" true
        ; test_cfg ~joinbwd:4 "./tests/ctl/koskinen/fig8-2007_mod.c"
            "OR{set==0}{AF{unset == 1}}" true
        ; test_cfg "./tests/ctl/multi_branch_choice.c" "AF{OR{x==4}{x==-4}}"
            true
        ; test_cfg "./tests/ctl/multi_branch_choice.c"
            "AND{EF{x==4}}{EF{x==-4}}" true
        ; (* can't handle existential non-det assignments *)
            ("./tests/ctl/potential_termination_1.c", "EF{exit: true}") -- true
        ; (* Some SV Comp testcases *)

            (* can't handle existential non-det assignments *)
            ("./tests/ctl/sv_comp/Bangalore_false-no-overflow.c", "EF{x < 0}")
            -- true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex02_false-termination_true-no-overflow.c"
            "OR{i >= 5}{AF{exit: true}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "AF{AG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "EF{EG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "EF{AG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "AF{EG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "AF{AND{AF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "AF{AND{EF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "EF{AND{AF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "EF{AND{EF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{AF{AG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{AF{EG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{EF{AG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{EF{EG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "AF{AG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "EF{AG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "AF{EG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "EF{EG{j==0}}" true
        ; (* (1* Testcases from Ultimate LTL Automizer *1) *)
            test_cfg ~precondition:"chainBroken == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_1_safe_sfty.c"
            "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" true
        ; test_cfg ~precondition:"chainBroken == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_1_unsafe_sfty.c"
            "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" false
        ; test_cfg ~joinbwd:7
            "./tests/ctl/ltl_automizer/coolant_basis_2_safe_lifeness.c"
            "AG{AF{otime < time}}" true
        ; test_cfg ~joinbwd:7
            "./tests/ctl/ltl_automizer/coolant_basis_2_unsafe_lifeness.c"
            "AG{AF{otime < time}}" false
        ; test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_3_safe_sfty.c"
            "AG{OR{init != 3}{AG{AF{time > otime}}}}" true
        ; test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_3_unsafe_sfty.c"
            "AG{OR{init != 3}{AG{AF{time > otime}}}}" false
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_4_safe_sfty.c"
            "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}"
            true
        ; (*TODO performs realy bad, I suspect because of a problem in 'dual_widen' *)
            (* test_cfg ~precondition:"init == 0 && temp < limit" *)
            (*   "./tests/ctl/ltl_automizer/coolant_basis_4_unsafe_sfty.c" *)
            (* "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}"
                false; *)

            (* Analysis can't prove coolant_basis_5_safe_sfty.c, but we can show
                a slightly modified example: *)
            test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_5_safe_cheat.c"
            "AU{init == 0}{OR{AU{init == 1}{AG{init == 3}}}{AG{init == 1}}}"
            true
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_6_safe_sfty.c"
            "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED \
                == 1}}}}"
            true
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_6_unsafe_sfty.c"
            "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED \
                == 1}}}}"
            false
        ; test_cfg ~precondition:"i == 1 && n >= 0 && i > n"
            "./tests/ctl/ltl_automizer/nestedRandomLoop_true-valid-ltl.c"
            "AG{i >= n}" true
        ; (* imprecision *)
            ( "./tests/ctl/ltl_automizer/timer-simple.c"
            , "NOT{AG{OR{timer_1 != 0}{AF{output_1 == 1}}}}" )
            -- true
        ; (* imprecision due to modulo *)
            ( "./tests/ctl/ltl_automizer/togglecounter_true-valid-ltl.c"
            , "AG{AND{AF{t == 1}}{AF{t == 0}}}" )
            -- true
        ; test_cfg ~precondition:"t >= 0"
            "./tests/ctl/ltl_automizer/toggletoggle_true-valid-ltl.c"
            "AG{AND{AF{t==1}}{AF{t==0}}}" true
        ; test_cfg ~precondition:"x < 0" ~setup:["-ordinals"; "3"]
            "./tests/ctl/ltl_automizer/PotentialMinimizeSEVPABug.c"
            "AG{OR{x <= 0}{AF{y == 0}}}" true
        ; test_cfg ~precondition:"x < 0" ~setup:["-ordinals"; "3"]
            "./tests/ctl/ltl_automizer/cav2015.c" "AG{OR{x <= 0}{AF{y == 0}}}"
            true
        ; test_cfg "./tests/ctl/ltl_automizer/simple-1.c" "AF{x > 10000}" true
        ; test_cfg "./tests/ctl/ltl_automizer/simple-2.c" "AF{x > 100}" true
        ; test_cfg ~precondition:"ap==0"
            "./tests/ctl/ltl_automizer/Bug_NoLoopAtEndForTerminatingPrograms_safe.c"
            "NOT{AF{ap > 2}}" true ] *) 

        ; test_cfg "./tests/ctl/multi_branch_choice.c"
            "AND{EF{x==4}}{EF{x==-4}}" true
        ; (* can't handle existential non-det assignments *)
            ("./tests/ctl/potential_termination_1.c", "EF{exit: true}") -- true
        ; (* Some SV Comp testcases *)

            (* can't handle existential non-det assignments *)
            ("./tests/ctl/sv_comp/Bangalore_false-no-overflow.c", "EF{x < 0}")
            -- true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex02_false-termination_true-no-overflow.c"
            "OR{i >= 5}{AF{exit: true}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "AF{AG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "EF{EG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "EF{AG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Ex07_false-termination_true-no-overflow.c"
            "AF{EG{i==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "AF{AND{AF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "AF{AND{EF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "EF{AND{AF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/java_Sequence_true-termination_true-no-overflow.c"
            "EF{AND{EF{j >= 21}}{i==100}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{AF{AG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{AF{EG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{EF{AG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/Madrid_true-no-overflow_false-termination_true-valid-memsafety.c"
            "AF{AND{x==7}{EF{EG{x==2}}}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "AF{AG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "EF{AG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "AF{EG{j==0}}" true
        ; test_cfg
            "./tests/ctl/sv_comp/NO_02_false-termination_true-no-overflow.c"
            "EF{EG{j==0}}" true
        ; (* (1* Testcases from Ultimate LTL Automizer *1) *)
            test_cfg ~precondition:"chainBroken == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_1_safe_sfty.c"
            "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" true
        ; test_cfg ~precondition:"chainBroken == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_1_unsafe_sfty.c"
            "AG{OR{chainBroken != 1}{AG{chainBroken == 1}}}" false
        ; test_cfg ~joinbwd:7
            "./tests/ctl/ltl_automizer/coolant_basis_2_safe_lifeness.c"
            "AG{AF{otime < time}}" true
        ; test_cfg ~joinbwd:7
            "./tests/ctl/ltl_automizer/coolant_basis_2_unsafe_lifeness.c"
            "AG{AF{otime < time}}" false
        ; test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_3_safe_sfty.c"
            "AG{OR{init != 3}{AG{AF{time > otime}}}}" true
        ; test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_3_unsafe_sfty.c"
            "AG{OR{init != 3}{AG{AF{time > otime}}}}" false
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_4_safe_sfty.c"
            "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}"
            true
        ; (*TODO performs realy bad, I suspect because of a problem in 'dual_widen' *)
            (* test_cfg ~precondition:"init == 0 && temp < limit" *)
            (*   "./tests/ctl/ltl_automizer/coolant_basis_4_unsafe_sfty.c" *)
            (* "AG{OR{init != 3}{OR{temp <= limit}{AF{AG{chainBroken == 1}}}}}"
                false; *)

            (* Analysis can't prove coolant_basis_5_safe_sfty.c, but we can show
                a slightly modified example: *)
            test_cfg ~precondition:"init == 0"
            "./tests/ctl/ltl_automizer/coolant_basis_5_safe_cheat.c"
            "AU{init == 0}{OR{AU{init == 1}{AG{init == 3}}}{AG{init == 1}}}"
            true
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_6_safe_sfty.c"
            "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED \
                == 1}}}}"
            true
        ; test_cfg ~precondition:"init == 0 && temp < limit"
            "./tests/ctl/ltl_automizer/coolant_basis_6_unsafe_sfty.c"
            "AG{OR{limit <= -273 && limit >= 10}{OR{tempIn >= 0}{AF{ warnLED \
                == 1}}}}"
            false
        ; test_cfg ~precondition:"i == 1 && n >= 0 && i > n"
            "./tests/ctl/ltl_automizer/nestedRandomLoop_true-valid-ltl.c"
            "AG{i >= n}" true
        ; (* imprecision *)
            ( "./tests/ctl/ltl_automizer/timer-simple.c"
            , "NOT{AG{OR{timer_1 != 0}{AF{output_1 == 1}}}}" )
            -- true
        ; (* imprecision due to modulo *)
            ( "./tests/ctl/ltl_automizer/togglecounter_true-valid-ltl.c"
            , "AG{AND{AF{t == 1}}{AF{t == 0}}}" )
            -- true
        ; test_cfg ~precondition:"t >= 0"
            "./tests/ctl/ltl_automizer/toggletoggle_true-valid-ltl.c"
            "AG{AND{AF{t==1}}{AF{t==0}}}" true
        ; test_cfg ~precondition:"x < 0" ~setup:["-ordinals"; "3"]
            "./tests/ctl/ltl_automizer/PotentialMinimizeSEVPABug.c"
            "AG{OR{x <= 0}{AF{y == 0}}}" true
        ; test_cfg ~precondition:"x < 0" ~setup:["-ordinals"; "3"]
            "./tests/ctl/ltl_automizer/cav2015.c" "AG{OR{x <= 0}{AF{y == 0}}}"
            true
        ; test_cfg "./tests/ctl/ltl_automizer/simple-1.c" "AF{x > 10000}" true
        ; test_cfg "./tests/ctl/ltl_automizer/simple-2.c" "AF{x > 100}" true
        ; test_cfg ~precondition:"ap==0"
            "./tests/ctl/ltl_automizer/Bug_NoLoopAtEndForTerminatingPrograms_safe.c"
            "NOT{AF{ap > 2}}" true ] *)  *)
