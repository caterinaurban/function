(*******************************************)
(*                                         *)
(*               Unit Testing              *)
(*                                         *)
(*              Caterina Urban             *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2016                   *)
(*                                         *)
(*******************************************)

open OUnit
open TerminationBoxesTest
open TerminationPolyhedraTest
open CTLTest
				
let _ = begin
  run_test_tt_main ctl_cfg_testcases;
  run_test_tt_main ctl_ast_testcases;
  run_test_tt_main boxes;
  run_test_tt_main polyhedra;
  end
