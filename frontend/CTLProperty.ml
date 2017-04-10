

type 'a generic_property =
  | Atomic of 'a (* atomic *)
  | AX of 'a generic_property (* next *)
  | AF of 'a generic_property (* future/eventually *)
  | AG of 'a generic_property (* global *)
  | AU of ('a generic_property * 'a generic_property) (* until *)
  | AND of ('a generic_property * 'a generic_property) (* and *)
  | OR of ('a generic_property * 'a generic_property) (* or *)

let rec map f property = match property with
  | Atomic x -> Atomic (f x)
  | AX e -> AX (map f e)
  | AF e -> AF (map f e)
  | AG e -> AG (map f e)
  | AU (e1, e2) -> AU (map f e1, map f e2)
  | AND (e1, e2) -> AND (map f e1, map f e2)
  | OR (e1, e2) -> OR (map f e1, map f e2)

