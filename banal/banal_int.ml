(* 
   Mathematical integers.

   Copyright (C) 2011 Antoine Miné
*)

open Apron

include Z

let to_float_up (x:t) =
  to_float x

let to_float_down (x:t) =
  -. (to_float (neg x))

let of_float_up (x:float) : t =
  of_float (ceil x)

let of_float_down (x:float) : t =
  of_float (floor x)


(* apron and mlgmpidl *)
(* ****************** *)

external mpz_set: Mpz.t -> t -> unit = "ml_z_mlgmpidl_set_mpz"

external of_mpz: Mpz.t -> t = "ml_z_mlgmpidl_of_mpz"
let to_mpz (x:t) : Mpz.t = let z = Mpz.init () in mpz_set z x; z

let of_mpzf (x:Mpzf.t) : t = of_mpz (Mpzf._mpz x)
let to_mpzf (x:t) : Mpzf.t = Mpzf._mpzf (to_mpz x)

let of_mpqf_up x =
  let n,d = of_mpzf (Mpqf.get_num x), of_mpzf (Mpqf.get_den x) in
  if sign d = 0 then raise Overflow;
  cdiv n d

let of_mpqf_down x =
  let n,d = of_mpzf (Mpqf.get_num x), of_mpzf (Mpqf.get_den x) in
  if sign d = 0 then raise Overflow;
  fdiv n d

let to_mpqf x =
  Mpqf.of_mpz (to_mpz x)

let to_apron x = 
  Scalar.Mpqf (to_mpqf x)

let of_apron_up = function
  | Scalar.Float x -> of_float_up x
  | Scalar.Mpqf x -> of_mpqf_up x
  | _ -> invalid_arg "Int: unsupported Scalar type"

let of_apron_down = function
  | Scalar.Float x -> of_float_down x
  | Scalar.Mpqf x -> of_mpqf_down x
  | _ -> invalid_arg "Int: unsupported Scalar type"
