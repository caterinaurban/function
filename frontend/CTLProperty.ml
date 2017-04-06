

type 'a generic_formula =
  | Atomic of 'a (* atomic *)
  | AX of 'a generic_formula (* next *)
  | AF of 'a generic_formula (* future/eventually *)
  | AG of 'a generic_formula (* global *)
  | AU of ('a generic_formula * 'a generic_formula) (* until *)
  | AND of ('a generic_formula * 'a generic_formula) (* and *)
  | OR of ('a generic_formula * 'a generic_formula) (* or *)

let rec map f formula = match formula with
  | Atomic x -> Atomic (f x)
  | AX e -> AX (map f e)
  | AF e -> AF (map f e)
  | AG e -> AG (map f e)
  | AU (e1, e2) -> AU (map f e1, map f e2)
  | AND (e1, e2) -> AND (map f e1, map f e2)
  | OR (e1, e2) -> OR (map f e1, map f e2)


