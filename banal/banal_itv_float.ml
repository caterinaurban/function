(* 
   Floating-point intervals.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Apron

module Float = Banal_float
module Intinf = Banal_intinf
module Int = Banal_int


(************************************************************************)
(* TYPES *)
(************************************************************************)

module Bound = Float.Double

module F = Float
module D = Float.Double

type elem = D.t
type bound = elem
type t = bound * bound


(* not all pairs of rationals are valid intervals *)
let sanitize ((l,h):t) : t =
  match D.classify l, D.classify h with
  | D.NAN,_ | _,D.NAN | D.INF,_ | _,D.MINF ->
      invalid_arg "Itv_float_sanitize"
  | _ -> 
      if D.gt l h then invalid_arg "Itv_rat.sanitize" else l,h

let check_bot ((l,h):t) : t bot =
  if D.leq l h then Nb (l,h) else Bot



(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let singleton (x:D.t) : t = x,x

let zero : t = singleton D.zero

let one : t = singleton D.one

let minus_one : t = singleton D.minus_one

let top : t = D.minus_inf, D.inf

let zero_one : t = D.zero, D.one

let minus_one_zero : t = D.minus_one, D.zero

let minus_one_one : t = D.minus_one, D.one

let positive : t = D.zero, D.inf

let negative : t = D.minus_inf, D.zero

let of_ints (l:Int.t) (h:Int.t) : t = D.of_int_down l, D.of_int_up h

let of_int x = of_ints x x

let of_intinf l h = Intinf.to_float_down l, Intinf.to_float_up h

let of_floats (l:float) (h:float) : t = D.of_float_down l, D.of_float_up h

let of_float x = of_floats x x

let of_bounds (l:D.t) (h:D.t) : t = l, h


(************************************************************************)
(* CONVERSION AND PRINTING *)
(************************************************************************)

let to_string ((l,h):t) : string = 
  Printf.sprintf "[%a;%a]" D.sprint l D.sprint h

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)


let to_apron ((l,h):t) : Interval.t =
  { Interval.inf = Scalar.Float l;
    Interval.sup = Scalar.Float h;
  }

let of_apron (i:Interval.t) : t =
  D.of_apron_down i.Interval.inf, D.of_apron_up i.Interval.sup

let of_coeff (c:Coeff.t) : t =
  match c with
  | Coeff.Interval i -> of_apron i
  | Coeff.Scalar c -> D.of_apron_down c, D.of_apron_up c


(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

let equal ((l1,h1):t) ((l2,h2):t) : bool =
  D.equal l1 l2 && D.equal h1 h2

let subseteq ((l1,h1):t) ((l2,h2):t) : bool =
  D.geq l1 l2 && D.leq h1 h2

let join ((l1,h1):t) ((l2,h2):t) : t =
  D.min l1 l2, D.max h1 h2

let union ((l1,h1):t) ((l2,h2):t) : t option =
  if D.leq l1 h2 && D.leq l2 h1 then Some (D.min l1 l2, D.max h1 h2)
  else None

let meet ((l1,h1):t) ((l2,h2):t) : t bot =
  check_bot (D.max l1 l2, D.min h1 h2)

let hull (x:D.t) (y:D.t) : t =
  D.min x y, D.max x y

let contains ((l,h):t) (x:D.t) : bool =
  D.leq l x && D.leq x h

let is_bounded ((l,h):t) =
  D.classify l <> D.MINF && D.classify h <> D.INF

let is_singleton ((l,h):t) : bool =
  D.classify l <> D.MINF && D.equal l h


(************************************************************************)
(* FORWARD OVER-APPROXIMATED ARITHMETICS *)
(************************************************************************)

let neg ((l,h):t) : t =
  D.neg h, D.neg l

let abs ((l,h):t) : t =
  if contains (l,h) D.zero then D.zero, D.max (D.abs l) (D.abs h)
  else hull (D.abs l) (D.abs h)

let add ((l1,h1):t) ((l2,h2):t) : t =
  D.add_down l1 l2, D.add_up h1 h2

let sub ((l1,h1):t) ((l2,h2):t) : t =
  D.sub_down l1 h2, D.sub_up h1 l2


(* helper: oo * 0 = 0 when multiplying bounds *)
let bound_mul mul x y =
  if D.sign x = 0 || D.sign y = 0 then D.zero else mul x y

(* helper *)
let fourway op1 op2 (l1,h1) (l2,h2) =
  D.min (D.min (op1 l1 l2) (op1 l1 h2)) (D.min (op1 h1 l2) (op1 h1 h2)),
  D.max (D.max (op2 l1 l2) (op2 l1 h2)) (D.max (op2 h1 l2) (op2 h1 h2))

let mul (i1:t) (i2:t) : t =
  fourway (bound_mul D.mul_down) (bound_mul D.mul_up) i1 i2
    
    
(* helper: 0/0 = 0, x/0 = sign(x) oo, other follow IEEE *)
let bound_div div x y =
  match D.sign x, D.sign y with
   |  0,_ -> D.zero
   |  1,0 -> D.inf
   | -1,0 -> D.minus_inf
   | _ -> div x y
         
(* helper: assumes i2 has constant sign *) 
let div_sign (i1:t) (i2:t) : t =
  fourway (bound_div D.div_down) (bound_div D.div_up) i1 i2

(* return valid values + possible division by zero *)
let div (i1:t) (i2:t) : t bot * bool =
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_sign i1)) (meet i2 positive)
  and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
  (* joins the result *)
  join_bot2 join pos neg,
  contains i2 D.zero


(* defaults to the non-infinite bound, or zero *)
let mean ((l,h):t) : D.t =
  match D.classify l, D.classify h with
  | D.NORMAL, D.NORMAL -> D.div_up (D.add_up l h) (D.of_float_up 2.)
  | D.NORMAL, _ -> l
  | _, D.NORMAL -> h
  | _ -> D.zero

let range ((l,h):t) : D.t = 
  D.sub_up h l

let magnitude ((l,h):t) : D.t =
  D.max (D.abs l) (D.abs h)

