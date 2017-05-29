open AbstractSyntax 

(* invariant map that assigns values to program labels *)

module InvMap = Map.Make(struct type t=label let compare=compare end)
