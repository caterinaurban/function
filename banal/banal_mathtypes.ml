(* 
   Common signatures of numeric datatypes.

   Useful to parameterize functors.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Apron

module Int = Banal_int
module Float = Banal_float
module Intinf = Banal_intinf


(* floats *)
(* ****** *)

module type FLOAT = sig

  type t = float

  val of_int_up: Int.t -> t
  val of_int_down: Int.t -> t
  val of_float_up: Float.t -> t
  val of_float_down: Float.t -> t

  val to_string: t -> string

  val neg: t -> t
  val abs: t -> t
  val round: t -> t
  val add_up: t -> t -> t
  val sub_up: t -> t -> t
  val mul_up: t -> t -> t
  val div_up: t -> t -> t
  val add_down: t -> t -> t
  val sub_down: t -> t -> t
  val mul_down: t -> t -> t
  val div_down: t -> t -> t

end



(* bounded types *)
(* ************* *)

module type BOUND = sig

  type t
  type base

  val zero: t
  val one: t
  val minus_one: t
  val inf: t
  val minus_inf: t

  val of_base: base -> t
  val of_int_up: Int.t -> t
  val of_int_down: Int.t -> t
  val of_float_up: Float.t -> t
  val of_float_down: Float.t -> t
  val of_apron_up: Scalar.t -> t
  val of_apron_down: Scalar.t -> t

  val to_base: t -> base (* can raise Int.Overflow *)
  val to_apron: t -> Scalar.t
  val to_string: t -> string

  val neg: t -> t
  val abs: t -> t
  val add_up: t -> t -> t
  val sub_up: t -> t -> t
  val mul_up: t -> t -> t
  val div_up: t -> t -> t
  val add_down: t -> t -> t
  val sub_down: t -> t -> t
  val mul_down: t -> t -> t
  val div_down: t -> t -> t

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool
  val leq: t -> t -> bool
  val geq: t -> t -> bool
  val sign: t -> int
  val min: t -> t -> t
  val max: t -> t -> t
  val is_finite: t -> bool
end



(* intervals *)
(* ********* *)

module type INTERVAL = sig

  module Bound: BOUND

  type bound = Bound.t
  type elem = Bound.base
  type t = bound * bound

  val zero: t
  val one: t
  val minus_one: t
  val zero_one: t
  val minus_one_zero: t
  val minus_one_one: t
  val positive: t
  val negative: t
  val top: t

  val singleton: elem -> t
  val of_bounds: bound -> bound -> t
  val of_ints: Int.t -> Int.t -> t
  val of_int: Int.t -> t
  val of_intinf: Intinf.t -> Intinf.t -> t
  val of_floats: Float.t -> Float.t -> t
  val of_float: Float.t -> t
  val of_apron: Interval.t -> t
  val of_coeff: Coeff.t -> t

  val to_apron: t -> Interval.t
  val to_string: t -> string

  val neg: t -> t
  val abs: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> (t bot) * bool

  val join: t -> t -> t
  val meet: t -> t -> t bot

  val equal: t -> t -> bool
  val subseteq: t -> t -> bool
  val contains: t -> elem -> bool
  val is_bounded: t -> bool
  val is_singleton: t -> bool

  val mean: t -> elem
  val range: t -> bound
  val magnitude: t -> bound

end
