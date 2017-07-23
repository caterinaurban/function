(*

   'A' stands for 'all paths'
   'E' stands for 'exists a path'

   Operators
   X: next
   F: future/eventually
   G: global/always
   U: until
   
*)

type 'a generic_property =
  | Atomic of ('a * string option) (* atomic property with optional label *)
  | AX of 'a generic_property (* next *)
  | AF of 'a generic_property (* future/eventually *)
  | AG of 'a generic_property (* global *)
  | AU of ('a generic_property * 'a generic_property) (* until *)
  | EX of 'a generic_property (* next *)
  | EF of 'a generic_property (* future/eventually *)
  | EG of 'a generic_property (* global *)
  | EU of ('a generic_property * 'a generic_property) (* until *)
  | AND of ('a generic_property * 'a generic_property) (* and *)
  | OR of ('a generic_property * 'a generic_property) (* or *)
  | NOT of 'a generic_property (* not *)

let rec map f property = match property with
  | Atomic (x, l) -> Atomic (f x, l)
  | AX e -> AX (map f e)
  | AF e -> AF (map f e)
  | AG e -> AG (map f e)
  | AU (e1, e2) -> AU (map f e1, map f e2)
  | EX e -> EX (map f e)
  | EF e -> EF (map f e)
  | EG e -> EG (map f e)
  | EU (e1, e2) -> EU (map f e1, map f e2)
  | AND (e1, e2) -> AND (map f e1, map f e2)
  | OR (e1, e2) -> OR (map f e1, map f e2)
  | NOT e -> NOT (map f e)

