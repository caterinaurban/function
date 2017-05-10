(* 
   Mathematical rationals.

   Copyright (C) 2011 Antoine Miné
*)

open Apron

module Int = Banal_int
module Intinf = Banal_intinf
module Datatypes = Banal_datatypes

include Q

type base = t

(* conversions are exact *)
let of_base x = x
let of_int_up = of_bigint 
let of_int_down = of_bigint
let of_float_up = of_float
let of_float_down = of_float
let to_base x = x

(* arithmetics is exact *)
let add_up = add
let sub_up = sub
let mul_up = mul
let div_up = div
let add_down = add
let sub_down = sub
let mul_down = mul
let div_down = div 

let is_finite x = Int.sign x.den <> 0


(* apron and mlgmpidl *)

let to_mpqf x = 
  match classify x with
  | INF ->  Mpqf.of_mpq Intinf.mpq_inf
  | MINF ->  Mpqf.of_mpq Intinf.mpq_minf
  | UNDEF -> invalid_arg "Rat: undefined rational"
  | _ -> Mpqf.of_mpz2 (Int.to_mpz (num x)) (Int.to_mpz (den x))

let of_mpqf x =
  make (Int.of_mpzf (Mpqf.get_num x)) (Int.of_mpzf (Mpqf.get_den x))

let to_apron x =
  Scalar.Mpqf (to_mpqf x)

let of_apron_up = function
  | Scalar.Float x -> of_float_up x
  | Scalar.Mpqf x -> of_mpqf x
  | _ -> invalid_arg "Rat: unsupported Scalar type"

let of_apron_down = of_apron_up

let of_intinf = function
  | Datatypes.MINF -> minus_inf
  | Datatypes.INF -> inf
  | Datatypes.Finite x -> of_bigint x
