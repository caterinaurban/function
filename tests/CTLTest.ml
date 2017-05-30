(*******************************************)
(*                                         *)
(*          CTL Test                       *)
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
    ?(domain = BOXES) 
    ?(joinbwd = 2)
    ?setup
  = TestCommon.make_analyser [
    "-domain"; string_of_domain domain; 
    "-joinbwd"; string_of_int joinbwd; 
    "-precondition"; precondition;
    "-ctl_str"; ctl_str;
  ] ?setup:setup file


let ctl_testcases = "boxes" >:::
[
  test ~joinbwd:4 "./tests/ctl/global_test_simple.c" "AG{AF{x <= -10}}" true;
  test ~precondition:"x == y + 20" "./tests/ctl/until_test.c" "AU{x >= y}{x==y}" true;
  (* test "./tests/ctl/until_test.c" "AU{x <= y}{x==y}" false; *) (*TODO find problem*)
  test "./tests/countdown.c" "AF{x == 0}" true;
  test "./tests/countdown.c" "AG{AF{x == 0}}" true;
  test "./tests/mnav.c" "AF{enable == 0}" true;
  test "./tests/peterson.c" "AF{C1: true}" true;
  test "./tests/peterson.c" "AG{AF{C1: true}}" true;
  test "./tests/pingpong.c" "AF{z==1}" true;
  test ~setup:["-ordinals"; "1"] "./tests/sink.c" "AF{x==0}" true;
  test ~setup:["-ordinals"; "1"] "./tests/sink.c" "AG{AF{x==0}}" true;
  test ~precondition: "n > 0" "./tests/ctl/and_test.c" "AND{AG{AF{n==1}}}{AF{n==0}}" true;
  test "./tests/ctl/or_test.c" "OR{AF{AG{x < -100}}}{AF{x==20}}" true;
  test ~precondition: "x==1" "./tests/ctl/next.c" "AX{x==0}" true;
  test "./tests/ctl/next.c" "AX{x==0}" false;
]
