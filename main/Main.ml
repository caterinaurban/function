(*Main entry point for application*)
let doit () =
  let open Runner in
  parse_args () ;
  match !analysis with
  | "termination" -> termination ()
  | "guarantee" -> guarantee ()
  | "recurrence" -> recurrence ()
  | "ctl" -> ctl ()
  | _ -> raise (Invalid_argument "Unknown Analysis")

let _ = doit ()
