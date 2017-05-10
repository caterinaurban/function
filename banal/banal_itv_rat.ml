(* 
   Rational intervals.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Apron

module Rat = Banal_rat
module Int = Banal_int

(************************************************************************)
(* TYPES *)
(************************************************************************)

module Bound = Rat

module R = Rat

type elem = R.t
type bound = R.t
type t = bound * bound


(* not all pairs of rationals are valid intervals *)
let sanitize ((l,h):t) : t =
  match R.classify l, R.classify h with
  | R.UNDEF,_ | _,R.UNDEF  | R.MINF,_ | _,R.INF ->
      invalid_arg "Itv_rat.sanitize"
  | _ -> 
      if R.gt l h then invalid_arg "Itv_rat.sanitize" else l,h

let check_bot ((l,h):t) : t bot =
  if R.leq l h then Nb (l,h) else Bot


(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let singleton (x:R.t) : t = x, x

let zero : t = singleton R.zero

let one : t = singleton R.one

let minus_one : t = singleton R.minus_one

let top : t = R.minus_inf, R.inf

let zero_one : t = R.zero, R.one

let minus_one_zero : t = R.minus_one, R.zero

let minus_one_one : t = R.minus_one, R.one

let positive : t = R.zero, R.inf

let negative : t = R.minus_inf, R.zero

let of_ints (l:Int.t) (h:Int.t) : t = R.of_bigint l, R.of_bigint h

let of_int x = of_ints x x

let of_intinf l h = R.of_intinf l, R.of_intinf h

let of_floats (l:float) (h:float) : t = R.of_float l, R.of_float h

let of_float x = of_floats x x

let of_bounds (l:R.t) (h:R.t) :t = l,h


(************************************************************************)
(* CONVERSION AND PRINTING *)
(************************************************************************)


let to_string ((l,h):t) : string = 
  Printf.sprintf "[%a;%a]" R.sprint l R.sprint h

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

let to_apron ((l,h):t) : Interval.t =
  { Interval.inf = Scalar.Mpqf (Rat.to_mpqf l);
    Interval.sup = Scalar.Mpqf (Rat.to_mpqf h);
  }


let of_apron (i:Interval.t) : t =
  R.of_apron_down i.Interval.inf, R.of_apron_up i.Interval.sup

let of_coeff (c:Coeff.t) : t =
  match c with
  | Coeff.Interval i -> of_apron i
  | Coeff.Scalar c -> R.of_apron_down c, R.of_apron_up c


(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

let equal ((l1,h1):t) ((l2,h2):t) : bool =
  R.equal l1 l2 && R.equal h1 h2

let subseteq ((l1,h1):t) ((l2,h2):t) : bool =
  R.geq l1 l2 && R.leq h1 h2

let join ((l1,h1):t) ((l2,h2):t) : t =
  R.min l1 l2, R.max h1 h2

(* returns None if the set-union cannot be exactly represented *)
let union ((l1,h1):t) ((l2,h2):t) : t option =
  if R.leq l1 h2 && R.leq l2 h1 then Some (R.min l1 l2, R.max h1 h2)
  else None

let meet ((l1,h1):t) ((l2,h2):t) : t bot =
  check_bot (R.max l1 l2, R.min h1 h2)

let hull (x:R.t) (y:R.t) : t =
  R.min x y, R.max x y

let contains ((l,h):t) (x:R.t) : bool =
  R.leq l x && R.leq x h

let is_bounded ((l,h):t) =
  R.is_real l && R.is_real h

let is_singleton ((l,h):t) : bool =
  R.is_real l && R.equal l h


(************************************************************************)
(* FORWARD OVER-APPROXIMATED ARITHMETICS *)
(************************************************************************)

let neg ((l,h):t) : t =
  R.neg h, R.neg l

let abs ((l,h):t) : t =
  if contains (l,h) R.zero then R.zero, R.max (R.abs l) (R.abs h)
  else hull (R.abs l) (R.abs h)


let add ((l1,h1):t) ((l2,h2):t) : t =
  R.add l1 l2, R.add h1 h2

let sub ((l1,h1):t) ((l2,h2):t) : t =
  R.sub l1 h2, R.sub h1 l2


(* helper: oo * 0 = 0 when multiplying bounds *)
let bound_mul x y =
  if R.sign x = 0 || R.sign y = 0 then R.zero else R.mul x y

let mul ((l1,h1):t) ((l2,h2):t) : t =
  join 
    (hull (bound_mul l1 l2) (bound_mul h1 h2))
    (hull (bound_mul l1 h2) (bound_mul h1 l2))


(* helper: 0/0 = 0, x/0 = sign(x) oo *)
let bound_div x y =
  match R.sign x, R.sign y with
  |  0,_ -> R.zero
  |  1,0 -> R.inf
  | -1,0 -> R.minus_inf
  | _ -> R.div x y
       
(* helper: assumes i2 has constant sign *) 
let div_sign ((l1,h1):t) ((l2,h2):t) : t =
  join 
    (hull (bound_div l1 l2) (bound_div h1 h2))
    (hull (bound_div l1 h2) (bound_div h1 l2))

(* return valid values + possible division by zero *)
let div (i1:t) (i2:t) : t bot * bool =
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_sign i1)) (meet i2 positive)
  and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
  (* joins the result *)
  join_bot2 join pos neg,
  contains i2 R.zero


(* defaults to the non-infinite bound, or zero *)
let mean ((l,h):t) : R.t =
  match R.is_real l, R.is_real h with
  | true,true -> R.div (R.add l h) (R.of_int 2)
  | true,false -> l
  | false,true -> h
  | false,false -> R.zero

let range ((l,h):t) : R.t = 
  R.sub h l

let magnitude ((l,h):t) : R.t =
  R.max (R.abs l) (R.abs h)


(************************************************************************)
(* BACKWARD OVER-APPROXIMATED ARITHMETICS *)
(************************************************************************)

let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2 (check_bot (l1, R.min h1 h2)) (check_bot (R.max l1 l2, h2))

let filter_geq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2 (check_bot (R.max l1 l2, h1)) (check_bot (l2, R.min h1 h2))

let filter_lt ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
  if is_singleton i1 && is_singleton i2 && R.equal l1 l2 then Bot
  else filter_leq i1 i2

let filter_gt ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
  if is_singleton i1 && is_singleton i2 && R.equal l1 l2 then Bot
  else filter_geq i1 i2

let filter_eq (i1:t) (i2:t) : (t*t) bot =
  lift_bot (fun x -> x,x) (meet i1 i2)

let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
  if is_singleton i1 && is_singleton i2 && R.equal l1 l2 then Bot
  else Nb (i1,i2)


