(* ********* Forward/Backward Iterator ************ Copyright (C) 2012-2014
   by Caterina Urban. All rights reserved. *)

let abort = ref false

let cda = ref false (* conflict-driven conditional termination *)

let compress = ref true (* false *)

let ctl_existential_equivalence = ref false
(* use logical equivalences to express existential CTL properties based on
   universal properties *)

let fmt = ref Format.std_formatter

let joinbwd = ref 2

let joinfwd = ref 2

let learn = ref false (* conflict-driven conditional termination *)

let meetbwd = ref 2

let minimal = ref false

let refine = ref false

let retrybwd = ref 5

let size = ref 2 (* conflict-driven conditional termination *)

let start = ref 0.0

let stop = ref 0.0

let timebwd = ref false

let timefwd = ref false

let timeout = ref 300.0

let tracefwd = ref false

let tracebwd = ref false

let dot = ref false (* output trees in graphviz dot format *)

exception Abort

exception Timeout
