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

let test file ctl_str 
    ?(precondition = "true")
    ?(domain = POLYHEDRA) 
    ?(joinbwd = 2)
    ?setup
  = TestCommon.make_analyser [
    "-domain"; string_of_domain domain; 
    "-joinbwd"; string_of_int joinbwd; 
    "-precondition"; precondition;
    "-ctl_str"; ctl_str;
  ] ?setup:setup file

let test_cfg file ctl_property 
    ?(precondition = "true")
    ?(domain = POLYHEDRA) 
    ?(joinbwd = 2)
    ?setup
  = TestCommon.make_analyser [
    "-domain"; string_of_domain domain; 
    "-joinbwd"; string_of_int joinbwd; 
    "-precondition"; precondition;
    "-ctl_cfg"; ctl_property;
  ] ?setup:setup file

let ctl_testcases = "ctl" >:::
[

  (* test ~joinbwd:4 "./tests/ctl/global_test_simple.c" "AG{AF{x <= -10}}" true; *)
  (* test ~precondition:"x == y + 20" "./tests/ctl/until_test.c" "AU{x >= y}{x==y}" true; *)
  (* test "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false; *)
  (* test ~domain:BOXES "./tests/ctl/until_test.c" "AF{x < y + 20}" false; (*TODO: Problem with BOXES domain*) *)
  (* test "./tests/countdown.c" "AF{x == 0}" true; *)
  (* test "./tests/countdown.c" "AG{AF{x == 0}}" true; *)
  (* test "./tests/mnav.c" "AF{enable == 0}" true; *)
  (* test "./tests/peterson.c" "AF{C1: true}" true; *)
  (* test "./tests/peterson.c" "AG{AF{C1: true}}" true; *)
  (* test "./tests/pingpong.c" "AF{z==1}" true; *)
  (* test ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true; *)
  (* test ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}" true; *)
  (* test ~precondition: "n > 0" "./tests/ctl/and_test.c" "AND{AG{AF{n==1}}}{AF{n==0}}" true; *)
  (* test "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}" true; *)
  (* test ~precondition: "x==1" "./tests/ctl/next.c" "AX{x==0}" true; *)
  (* test "./tests/ctl/next.c" "AX{x==0}" false; *)
  (* test "./tests/ctl/existential_test1.c" "EF{r==1}" false; *)
  (* test ~precondition:"2*x <= y+3" "./tests/ctl/existential_test1.c" "EF{r==1}" true; *)
  (* test "./tests/ctl/existential_test1.c" "EF{r==1}" false; *)
  (* test "./tests/ctl/existential_test2.c" "EF{r==1}" false; *)
  (* test "./tests/ctl/existential_test3.c" "EF{r==1}" false; *)
  (* test *) 
  (*   ~setup:["-ctl_existential_equivalence"] *) 
  (*   ~precondition: "x > 0" *)
  (*   "./tests/ctl/existential_test3.c" "EF{r==1}" true; *)
  (* test *) 
  (*   ~joinbwd:5 *)
  (*   ~precondition: "x==2" *)
  (*   "./tests/ctl/existential_test3.c" "EF{r==1}" true; *)
  (* test ~precondition:"y<0" "./tests/ctl/existential_test4.c" "EF{r==1}" true; *)
  (* test ~precondition:"a!=1" "./tests/ctl/acqrel.c" "AG{OR{a!=1}{AF{r==1}}}" true; *)
  (* test "./tests/ctl/win4.c"  "AF{AG{WItemsNum >= 1}}" true; *)


  test_cfg ~joinbwd:6 "./tests/ctl/global_test_simple.c" "AG{AF{x <= -10}}" true;
  test_cfg ~precondition:"x == y + 20" "./tests/ctl/until_test.c" "AU{x >= y}{x==y}" true;
  test_cfg "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false;
  test_cfg ~domain:BOXES "./tests/ctl/until_test.c" "AF{x < y + 20}" false; (*TODO: Problem with BOXES domain*)
  test_cfg "./tests/countdown.c" "AF{x == 0}" true;
  test_cfg "./tests/countdown.c" "AG{AF{x == 0}}" true;
  test_cfg "./tests/mnav.c" "AF{enable == 0}" true;
  test_cfg "./tests/pingpong.c" "AF{z==1}" true;
  test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true;
  test_cfg ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}" true;
  test_cfg ~precondition: "n > 0" "./tests/ctl/and_test.c" "AND{AG{AF{n==1}}}{AF{n==0}}" true;
  test_cfg "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}" true;
  test_cfg ~precondition: "x==1" "./tests/ctl/next.c" "AX{x==0}" true;
  test_cfg "./tests/ctl/next.c" "AX{x==0}" false;
  test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_cfg ~precondition:"2*x <= y+3" "./tests/ctl/existential_test1.c" "EF{r==1}" true;
  test_cfg "./tests/ctl/existential_test1.c" "EF{r==1}" false;
  test_cfg "./tests/ctl/existential_test2.c" "EF{r==1}" false;
  test_cfg "./tests/ctl/existential_test3.c" "EF{r==1}" false;
  test_cfg 
    ~setup:["-ctl_existential_equivalence"] 
    ~precondition: "x > 0"
    "./test_cfgs/ctl/existential_test3.c" "EF{r==1}" true;
  test_cfg 
    ~joinbwd:5
    ~precondition: "x==2"
    "./test_cfgs/ctl/existential_test3.c" "EF{r==1}" true;
  test_cfg ~precondition:"y<0" "./tests/ctl/existential_test4.c" "EF{r==1}" true;
  test_cfg ~precondition:"a!=1" "./tests/ctl/acqrel.c" "AG{OR{a!=1}{AF{r==1}}}" true;
  test_cfg "./tests/ctl/win4.c"  "AF{AG{WItemsNum >= 1}}" true;
]
