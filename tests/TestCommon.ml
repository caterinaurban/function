open Runner

type domain = BOXES | OCTAGON | POLYHEDRA

let string_of_domain d =
  match d with
  | BOXES -> "boxes"
  | POLYHEDRA -> "polyhedra"
  | OCTAGON -> "octagon"

let testit ?(timeout=2.) ?(joinbwd=2)?(precond = "true") ?(ctl_eq = false) ?(dom = BOXES) ?(ord = (false, 0))
    ?(rob = false) ?(prop = "") ~analysis_type file =
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
  Iterator.refine := true ;
  Iterator.joinbwd := joinbwd ;
  Iterator.timeout := timeout;
  Iterator.ctl_existential_equivalence := ctl_eq ;
  let _ =
    match !analysis with
    | "ctl" -> property := prop
    | "termination" -> property := "AF{exit:true}"
    | "guarantee" -> property := "AF{" ^ prop ^ "}"
    | "recurrence" -> property := "AG{AF{" ^ prop ^ "}}"
    | _ -> raise (Invalid_argument "Unknown Analysis")
  in
  ctl ()
