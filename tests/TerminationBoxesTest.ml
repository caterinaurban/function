(*******************************************)
(*                                         *)
(*          Termination Boxes Test         *)
(*                                         *)
(*             Caterina Urban              *)
(*     ETH Zürich, Zurich, Switzerland     *)
(*                  2016                   *)
(*                                         *)
(*******************************************)

open OUnit2

(* default analyzer configuration *)
let ( => ) =
  TestCommon.make_analyser
    ["-domain"; "boxes"; "-joinbwd"; "2"; "-retrybwd"; "5"]

(* refined analyzer configuration using forward analysis *)
let ( -=> ) = ( => ) ~setup:["-refine"]

(* best analyzer configuration *)
let ( *=> ) = ( => ) ~setup:["-joinbwd"; "3"; "-ordinals"; "2"]

(* costly analyzer configurations needed for few programs *)
let ( +=> ) = ( => ) ~setup:["-joinbwd"; "5"]

let ( ^=> ) = ( => ) ~setup:["-joinbwd"; "7"]

let ( $=> ) = ( => ) ~setup:["-cda"; "2"] (* conflict-driven learning *)

(* CTL based termination analysis *)
let ( @=> ) = ( => ) ~setup:["-ctl"; "AF{exit: true}"; "-ast"]

let ( @@=> ) (filename, conditionalTerminationProperty) =
  ( => )
    ~setup:
      [ "-ctl"
      ; "AF{exit: true}"
      ; "-ast"
      ; "-precondition"
      ; conditionalTerminationProperty ]
    filename

let ( @@+=> ) (filename, conditionalTerminationProperty) =
  ( => )
    ~setup:
      [ "-ctl"
      ; "AF{exit: true}"
      ; "-ast"
      ; "-joinbwd"
      ; "5"
      ; "-precondition"
      ; conditionalTerminationProperty ]
    filename

let ( -- ) filename expected =
  filename
  >:: fun _ ->
  assert_bool "Test marked as not working but non-terminating" expected ;
  todo (filename ^ "Doesn't work yet")

let boxes =
  "boxes"
  >::: [ "./tests/boolean.c" => true
       ; "./tests/cacm2009a.c" => true
       ; "./tests/cacm2009b.c" *=> true
       ; (* -ordinals 1 *)
         "./tests/cav2006.c" => true
       ; "./tests/euclid.c" => false
       ; (* conditionally terminating *)
         "./tests/example0.c" => false
       ; (* conditionally terminating *)
         "./tests/example1.c" => true
       ; "./tests/example1a.c" -- true
       ; (* conflict-driven learning *)
         "./tests/example1b.c" -- true
       ; (* conflict-driven learning *)
         "./tests/example1c.c" -- true
       ; (* conflict-driven learning *)
         "./tests/example1d.c" -- true
       ; (* conflict-driven learning *)
         "./tests/example1e.c" -- true
       ; (* conflict-driven learning *)
         "./tests/example2.c" *=> true
       ; (* -ordinals 1 *)
         "./tests/example2a.c" -=> true
       ; (* -refine *)
         "./tests/example2b.c" -=> true
       ; (* -refine *)
         "./tests/example2c.c" -=> true
       ; (* -refine *)
         "./tests/example2d.c" -=> true
       ; (* -refine *)
         "./tests/example2e.c" -=> true
       ; (* -refine *)
         "./tests/example5.c" => false
       ; (* conditionally terminating *)
         "./tests/example7.c" => false
       ; (* conditionally terminating *)
         "./tests/example8.c" ^=> true
       ; (* -joinbwd 7 *)
         "./tests/example10.c" *=> true
       ; (* -ordinals 2 *)
         "./tests/issue8.c" => false
       ; (* conditionally terminating *)
         "./tests/mccarthy91.c" -- true
       ; "./tests/postdecrement.c" => true
       ; "./tests/postincrement.c" => true
       ; "./tests/predecrement.c" => true
       ; "./tests/preincrement.c" => true
       ; "./tests/recursion.c" -- true
       ; "./tests/sas2010.c" => true
       ; "./tests/sas2014a.c" => false
       ; (* conditionally terminating *)
         "./tests/sas2014b.c" -- true
       ; (* polyhedra *)
         "./tests/sas2014c.c" => false
       ; (* conditionally terminating *)
         "./tests/sorting4.c" -- true
       ; (* polyhedra *)
         "./tests/tacas2013a.c" => true
       ; "./tests/tacas2013b.c" -- true
       ; (* polyhedra *)
         "./tests/tacas2013c.c" *=> true
       ; (* -joinbwd 3, -ordinals 1 *)
         "./tests/tacas2013d.c" *=> true
       ; (* -ordinals 2 *)
         "./tests/tap2008a.c" => false
       ; "./tests/tap2008b.c" => false
       ; "./tests/tap2008c.c" => false
       ; "./tests/tap2008d.c" => false
       ; "./tests/tap2008e.c" => false
       ; "./tests/tap2008f.c" => false
       ; "./tests/vijay.c" -- true
       ; (* conflict-driven learning *)
         "./tests/vmcai2004a.c" +=> true
       ; (* -joinbwd 5 *)
         "./tests/vmcai2004b.c" => false
       ; (* conditionally terminating *)
         "./tests/widening1.c" *=> true
       ; (* ordinals = 1 *)
         "./tests/widening2.c" *=> true
       ; (* ordinals = 1 *)
         "./tests/widening3.c" => false
       ; "./tests/zune.c" => false
       ; (* CTL termination test cases *)
         "./tests/boolean.c" @=> true
       ; "./tests/cacm2009a.c" @=> true
       ; "./tests/cav2006.c" @=> true
       ; "./tests/euclid.c" @=> false
       ; (* conditionally terminating *)
         "./tests/example0.c" @=> false
       ; (* conditionally terminating *)
         "./tests/example1.c" @=> true
       ; "./tests/example5.c" @=> false
       ; (* conditionally terminating *)
         "./tests/example7.c" @=> false
       ; (* conditionally terminating *)
         "./tests/issue8.c" @=> false
       ; (* conditionally terminating *)
         "./tests/postdecrement.c" @=> true
       ; "./tests/postincrement.c" @=> true
       ; "./tests/predecrement.c" @=> true
       ; "./tests/preincrement.c" @=> true
       ; "./tests/sas2010.c" @=> true
       ; "./tests/sas2014a.c" @=> false
       ; (* conditionally terminating *)
         "./tests/sas2014c.c" @=> false
       ; (* conditionally terminating *)
         "./tests/tacas2013a.c" @=> true
       ; "./tests/tap2008a.c" @=> false
       ; "./tests/tap2008b.c" @=> false
       ; "./tests/tap2008c.c" @=> false
       ; "./tests/tap2008d.c" @=> false
       ; "./tests/tap2008e.c" @=> false
       ; "./tests/tap2008f.c" @=> false
       ; "./tests/vmcai2004b.c" @=> false
       ; (* conditionally terminating *)
         "./tests/widening3.c" @=> false
       ; "./tests/zune.c" @=> false
       ; (* CTL conditional termination tests *)
         ("./tests/example0.c", "x > 10") @@=> true
       ; ("./tests/example5.c", "x > 0") @@+=> true
       ; ("./tests/example7.c", "x > 6") @@=> true ]
