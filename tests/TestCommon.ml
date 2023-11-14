open Runner
open Driver

type domain = BOXES | OCTAGON | POLYHEDRA

let string_of_domain d =
  match d with
  | BOXES -> "boxes"
  | POLYHEDRA -> "polyhedra"
  | OCTAGON -> "octagons"

let testit ?(timeout = 2.) ?(joinbwd = 2) ?(precond = "true")
    ?(ctl_eq = false) ?(dom = BOXES) ?(ord = (false, 0)) ?(rob = false)
    ?(prop = "") ?(retrybwd = 0) ~analysis_type file =
  ItoA.zeroId () ;
  ASTtoCFG.node_counter := 0 ;
  domain := string_of_domain dom ;
  precondition := precond ;
  analysis := analysis_type ;
  filename := file ;
  minimal := true ;
  ordinals := fst ord ;
  Ordinals.max := snd ord ;
  robust := rob ;
  Iterator.minimal := true ;
  Iterator.retrybwd := retrybwd ;
  Iterator.refine := true ;
  Iterator.joinbwd := joinbwd ;
  Iterator.timeout := timeout ;
  Iterator.ctl_existential_equivalence := ctl_eq ;
  match !analysis with
  | "ctl" ->
      property := prop ;
      ctl ()
  | "termination" -> termination ()
  | "guarantee" ->
      property := "AF{" ^ prop ^ "}" ;
      ctl ()
  | "recurrence" ->
      property := "AG{AF{" ^ prop ^ "}}" ;
      ctl ()
  | _ -> raise (Invalid_argument "Unknown Analysis")
