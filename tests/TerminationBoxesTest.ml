(* *************************************** *)
(*                                         *)
(*          Termination Boxes Test         *)
(*                                         *)
(*             Caterina Urban              *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2016                   *)
(*                                         *)
(*******************************************)

(* *************************************** *)
(*                                         *)
(*        Termination Polyhedra Test       *)
(*                                         *)
(*             Caterina Urban              *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2016                   *)
(*                                         *)
(*******************************************)

open TestCommon

(* default analyzer configuration *)
let ( => ) file result =
  TestCommon.testit ~joinbwd:2 ~analysis_type:"termination" ~retrybwd:5 file
  = result

(* refined analyzer configuration using forward analysis *)

(* best analyzer configuration *)
let ( *=> ) file result =
  TestCommon.testit ~joinbwd:3 ~analysis_type:"termination" ~ord:(true, 2)
    file
  = result

(* costly analyzer configurations needed for few programs *)
let ( +=> ) file result =
  TestCommon.testit ~joinbwd:5 ~analysis_type:"termination" ~retrybwd:5 file
  = result

let ( ^=> ) file result =
  TestCommon.testit ~joinbwd:7 ~analysis_type:"termination" ~retrybwd:5 file
  = result

let ( $=> ) file result =
  TestCommon.testit ~joinbwd:2 ~analysis_type:"termination" ~retrybwd:5 file
  = result (* conflict-driven learning *)

let ( @=> ) file result =
  TestCommon.testit ~joinbwd:2 ~analysis_type:"ctl" ~prop:"AF{exit:true}"
    ~retrybwd:5 file
  = result

let ( @!=> ) file result =
  TestCommon.testit ~joinbwd:2 ~ord:(true, 2) ~analysis_type:"ctl"
    ~prop:"AF{exit:true}" ~retrybwd:5 file
  = result

let ( @@=> ) (filename, conditionalTerminationProperty) result =
  TestCommon.testit ~joinbwd:2 ~analysis_type:"ctl" ~prop:"AF{exit:true}"
    ~retrybwd:5 ~precond:conditionalTerminationProperty filename
  = result

let ( @@+=> ) (filename, conditionalTerminationProperty) result =
  TestCommon.testit ~joinbwd:5 ~analysis_type:"ctl" ~prop:"AF{exit:true}"
    ~retrybwd:5 ~precond:conditionalTerminationProperty filename
  = result

let%test "boolean" = "termination/boolean.c" => true

let%test "cacm2009a" = "termination/cacm2009a.c" => true

let%test "cacm200ba" = "termination/cacm2009b.c" *=> true

let%test "cav2006" = "termination/cav2006.c" => true

let%test "euclid" = "termination/euclid.c" => false

(* conditionally terminating *)
let%test "example0" = "termination/example0.c" => false

(* conditionally terminating *)
let%test "example1" = "termination/example1.c" => true

(* let%test "termination/example1a.c" -- true ; (* conflict-driven learning
   *) "termination/example1b.c" -- true ; (* conflict-driven learning *)
   "termination/example1c.c" -- true ; (* conflict-driven learning *)
   "termination/example1d.c" -- true ; (* conflict-driven learning *)
   "termination/example1e.c" -- true ; conflict-driven learning *)
let%test "example2" = "termination/example2.c" *=> true

(* -ordinals 1 *)
let%test "example2a" = "termination/example2a.c" => true

let%test "example2b" = "termination/example2b.c" => true

let%test "example2c" = "termination/example2c.c" => true

let%test "example2d" = "termination/example2d.c" => true

let%test "example2e" = "termination/example2e.c" => true

let%test "example5" = "termination/example5.c" => false

(* conditionally terminating *)
let%test "example7" = "termination/example7.c" => false

(* conditionally terminating *)
let%test "example8" = "termination/example8.c" ^=> true

(* -joinbwd 7 *)
let%test "example10" = "termination/example10.c" *=> true

(* -ordinals 2 *)
let%test "issue8" = "termination/issue8.c" => false

(* conditionally terminating *)
(* let%test "mccarthy"  =  "termination/mccarthy91.c" -- true *)
let%test "postdec" = "termination/postdecrement.c" => true

let%test "postinc" = "termination/postincrement.c" => true

let%test "predec" = "termination/predecrement.c" => true

let%test "preinc" = "termination/preincrement.c" => true
(* "termination/recursion.c" -- true *)

let%test "sas2010" = "termination/sas2010.c" => true

let%test "sas2014a" = "termination/sas2014a.c" => false
(* conditionally terminating *)

let%test "sas2014b" = "termination/sas2014b.c" => true

(*conditionally terminating *)
let%test "sas2014c" = "termination/sas2014c.c" => false

let%test "sorting4" =
  TestCommon.testit ~dom:POLYHEDRA ~analysis_type:"termination"
    "termination/sorting4.c"

let%test "tacas2013" = "termination/tacas2013a.c" => true

let%test "tacas2013b" = "termination/tacas2013b.c" => true

let%test "tacas2013c" = "termination/tacas2013c.c" *=> true
(* -joinbwd 3, -ordinals 1 *)

let%test "tacas2013d" = "termination/tacas2013d.c" *=> false
(* -ordinals 2 *)

let%test "tap2008" = "termination/tap2008a.c" => false

let%test "tap2008b" = "termination/tap2008b.c" => false

let%test "tap2008c" = "termination/tap2008c.c" => false

let%test "tap2008d" = "termination/tap2008d.c" => false

let%test "tap2008e" = "termination/tap2008e.c" => false

let%test "tap2008f" = "termination/tap2008f.c" => false

(* "termination/vijay.c" -- true ; *)
(* conflict-driven learning *)
let%test "vmcai2004a" = "termination/vmcai2004a.c" +=> true
(* -joinbwd 5 *)

let%test "vmcai2004b" = "termination/vmcai2004b.c" => false
(* conditionally terminating *)

let%test "widening1" = "termination/widening1.c" *=> true
(* ordinals = 1 *)

let%test "widening2" = "termination/widening2.c" *=> true
(* ordinals = 1 *)

let%test "widening3" = "termination/widening3.c" => false

let%test "zune" = "termination/zune.c" => false

(* CTL termination tests *)

let%test "boolean_ctl" = "termination/boolean.c" @=> true

let%test "cacm2009a_ctl" = "termination/cacm2009a.c" @=> true

let%test "cacm200b_ctl" = "termination/cacm2009b.c" @!=> true

let%test "cav2006_ctl" = "termination/cav2006.c" @=> true

let%test "euclid_ctl" = "termination/euclid.c" @=> false

(* conditionally terminating *)
let%test "example0_ctl" = "termination/example0.c" @=> false

(* conditionally terminating *)
let%test "example1_ctl" = "termination/example1.c" @=> true

(* let%test "termination/example1a.c" -- true "termination/example1b.c" --
   true ; "termination/example1c.c" -- true ; "termination/example1d.c" --
   true ; "termination/example1e.c" -- true ; conflict-driven learning
   let%test "example2_ctl" = "termination/example2.c" @=> true

   (* -ordinals 1 *) let%test "example2a_ctl" = "termination/example2a.c" @=>
   true let%test "example2b_ctl" = "termination/example2b.c" @=> true
   let%test "example2c_ctl" = "termination/example2c.c" @=> true let%test
   "example2d_ctl" = "termination/example2d.c" @=> true let%test
   "example2e_ctl" = "termination/example2e.c" @=> true let%test
   "example5_ctl" = "termination/example5.c" @=> false (* conditionally
   terminating *) let%test "example7_ctl" = "termination/example7.c" @=>
   false (* conditionally terminating *) let%test "example8_ctl" =
   "termination/example8.c" @=> true *)
(* -ordinals 2 *)
let%test "issue8_ctl" = "termination/issue8.c" @=> false

(* conditionally terminating *)
(* let%test "mccarthy"  =  "termination/mccarthy91.c" -- true *)
let%test "postdec_ctl" = "termination/postdecrement.c" @=> true

let%test "postinc_ctl" = "termination/postincrement.c" @=> true

let%test "predec_ctl" = "termination/predecrement.c" @=> true

let%test "preinc_ctl" = "termination/preincrement.c" @=> true
(* "termination/recursion.c" -- true *)

let%test "sas2010_ctl" = "termination/sas2010.c" @=> true

let%test "sas2014a_ctl" = "termination/sas2014a.c" @=> false

let%test "sas2014c_ctl" = "termination/sas2014c.c" @=> false
(* conditionally terminating *)

let%test "tacas2013_ctl" = "termination/tacas2013a.c" @=> true

(* -ordinals 2 *)
let%test "tap2008_ctl" = "termination/tap2008a.c" @=> false

let%test "tap2008b_ctl" = "termination/tap2008b.c" @=> false

let%test "tap2008c_ctl" = "termination/tap2008c.c" @=> false

let%test "tap2008d_ctl" = "termination/tap2008d.c" @=> false

let%test "tap2008e_ctl" = "termination/tap2008e.c" @=> false

let%test "tap2008f_ctl" = "termination/tap2008f.c" @=> false

(* "termination/vijay.c" -- true ; *)
(* conflict-driven learning *)

(* -joinbwd 5 *)
let%test "vmcai2004b_ctl" = "termination/vmcai2004b.c" @=> false
(* conditionally terminating *)

let%test "widening3_ctl" = "termination/widening1.c" @=> false
(* ordinals = 1 *)

let%test "zune_ctl" = "termination/zune.c" => false

let%test "exampl0 precond" = ("termination/example0.c", "x > 10") @@=> true

let%test "exampl5 precond" = ("termination/example5.c", "x > 0") @@+=> true

let%test "exampl7 precond" = ("termination/example7.c", "x > 6") @@=> true

let%test "sas2014a precond" = ("termination/sas2014a.c", "r <= 0") @@=> true

let%test "vmcai2004.c precond" =
  ("termination/vmcai2004b.c", "x != 3") @@+=> true
