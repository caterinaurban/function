(* 
   Floating-point arithmetics, with various rounding direction and precision.

   Copyright (C) 2011 Antoine Miné
*)

open Apron

module Int = Banal_int

(* sets FPU rounding mode towards +oo, once and for all *)
external init: unit -> unit = "ml_float_init"
let _ = init ()


(************************************************************************)
(* PRECISION-INDEPENDENT *)
(************************************************************************)

module Generic = struct
  

  (* types *)
  
  type t = float
  type base = t

  let of_base x = x
  let to_base x = x

  (* ordering *)

  let compare (a:t) (b:t) = compare a b
  let equal (a:t) (b:t) = a = b
  let leq (x:t) (y:t) : bool = x <= y
  let geq (x:t) (y:t) : bool = x >= y
  let lt (x:t) (y:t) : bool = x < y
  let gt (x:t) (y:t) : bool = x > y

  let min (x:t) (y:t) : t = min x y
  let max (x:t) (y:t) : t = max x y

  let sign (x:t) : int =
    if x > 0. then 1 else
    if x < 0. then -1 else
    0

  (* conversion, printing *)

  (* TODO: replace with of_string_up / of_string_down
     TODO: precision-dependent versions
   *)
  let of_string x =
    float_of_string x
      
  let to_string x =
    string_of_float x
      
  (* printing *)
  let output chan x = output_string chan (to_string x)
  let sprint () x = to_string x
  let bprint b x = Buffer.add_string b (to_string x)
  let pp_print f x = Format.pp_print_string f (to_string x)
      
      
  type kind = NAN | INF | MINF | NORMAL
    
  let classify (x:t) : kind =
    if classify_float x = FP_nan then NAN
    else if x = infinity then INF
    else if x = neg_infinity then MINF
    else NORMAL

  let is_finite x = classify x = NORMAL


  (* useful constants *)        

  let zero : t = 0.
  let one : t = 1.
  let minus_one : t = -1.
  let inf : t = infinity
  let minus_inf : t = neg_infinity
  let nan : t = nan


  (* precision-independent operators *)

  let neg (x:t) : t = -. x
  let abs (x:t) : t = abs_float x


  let to_apron x = Scalar.Float x

end

include Generic


(************************************************************************)
(* DOUBLE PRECISION *)
(************************************************************************)


module Double = struct

  include Generic

  let round a = a

  let of_int_up a = Int.to_float a
  let of_int_down a = -. (Int.to_float (Int.neg a))
  let of_float_up a = a
  let of_float_down a = a

  let add_up a b = a +. b
  let sub_up a b = a -. b
  let mul_up a b = a *. b
  let div_up a b = a /. b
  let add_down a b = -. (-. a -. b)
  let sub_down a b = -. (b -. a)
  let mul_down a b = -. ((-. a) *. b)
  let div_down a b = -. ((-. a) /. b)

  let (+>) = add_up
  let (+<) = add_down
  let (~>) = sub_up
  let (~<) = sub_down
  let ( *> ) = mul_up
  let ( *< ) = mul_down
  let (/>) = div_up
  let (/<) = div_down

  (* characteristics *)
  let mant_size = 52
  let min_exp = -1022
  let max_exp = 1023
  let min_denormal = ldexp 1. (min_exp-mant_size)
  let min_normal = ldexp 1. min_exp
  let max_normal = ldexp (2. -. ldexp 1. (-mant_size)) max_exp
  let max_exact = ldexp 1. mant_size
  let ulp = ldexp 1. (-mant_size)

  (* mlgmpidl & apron *)

  let of_mpqf_up x =
    let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
    match Int.sign n, Int.sign d with
    | 1,0 -> inf
    | -1,0 -> minus_inf
    | 0,0 -> invalid_arg "Float.of_mpqf_up"
    | _ -> div_up (Int.to_float_up n) (Int.to_float_down d)

  let of_mpqf_down x =
    let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
    match Int.sign n, Int.sign d with
    | 1,0 -> inf
    | -1,0 -> minus_inf
    | 0,0 -> invalid_arg "Float.of_mpqf_up"
    | _ -> div_down (Int.to_float_down n) (Int.to_float_up d)

  let of_apron_up = function
    | Scalar.Float x -> x
    | Scalar.Mpqf x -> of_mpqf_up x
    | _ -> invalid_arg "Float: unsupported Scalar type"

  let of_apron_down = function
    | Scalar.Float x -> x
    | Scalar.Mpqf x -> of_mpqf_down x
    | _ -> invalid_arg "Float: unsupported Scalar type"

end



(************************************************************************)
(* SINGLE PRECISION *)
(************************************************************************)


module Single = struct

  include Generic

  external round_flt: float -> float = "ml_round_flt" "ml_round_flt_f" "float"
  external of_int_flt: int -> float = "ml_of_int_flt" "ml_of_int_flt_f" "float"

  external add_flt: float -> float -> float = "ml_add_flt" "ml_add_flt_f" "float"
  external sub_flt: float -> float -> float = "ml_sub_flt" "ml_sub_flt_f" "float"
  external mul_flt: float -> float -> float = "ml_mul_flt" "ml_mul_flt_f" "float"
  external div_flt: float -> float -> float = "ml_div_flt" "ml_div_flt_f" "float"

  let round a = round_flt a

  let of_int_up a = round (Int.to_float a)
  let of_int_down a = -. (of_int_up (Int.neg a))
  let of_float_up a = round a
  let of_float_down a = -.(round (-.a))

  let add_up a b = add_flt a b
  let sub_up a b = sub_flt a b
  let mul_up a b = mul_flt a b
  let div_up a b = div_flt a b
  let add_down a b = -. (sub_flt (-. a) b)
  let sub_down a b = -. (sub_flt b a)
  let mul_down a b = -. (mul_flt (-. a) b)
  let div_down a b = -. (div_flt (-. a) b)

  let (+>) = add_up
  let (+<) = add_down
  let (~>) = sub_up
  let (~<) = sub_down
  let ( *> ) = mul_up
  let ( *< ) = mul_down
  let (/>) = div_up
  let (/<) = div_down

  (* characteristics *)
  let mant_size = 23
  let min_exp = -126
  let max_exp = 127
  let min_denormal = ldexp 1. (min_exp-mant_size)
  let min_normal = ldexp 1. min_exp
  let max_normal = ldexp (2. -. ldexp 1. (-mant_size)) max_exp
  let max_exact = ldexp 1. mant_size
  let ulp = ldexp 1. (-mant_size)

  (* mlgmpidl & apron *)

  let of_mpqf_up x =
    let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
    match Int.sign n, Int.sign d with
    | 1,0 -> inf
    | -1,0 -> minus_inf
    | 0,0 -> invalid_arg "Float.of_mpqf_up"
    | _ -> div_up (Int.to_float_up n) (Int.to_float_down d)
          
  let of_mpqf_down x =
    let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
    match Int.sign n, Int.sign d with
    | 1,0 -> inf
    | -1,0 -> minus_inf
    | 0,0 -> invalid_arg "Float.of_mpqf_up"
    | _ -> div_down (Int.to_float_down n) (Int.to_float_up d)

  let of_apron_up = function
    | Scalar.Float x -> of_float_up x
    | Scalar.Mpqf x -> of_mpqf_up x
    | _ -> invalid_arg "Float: unsupported Scalar type"

  let of_apron_down = function
    | Scalar.Float x -> of_float_down x
    | Scalar.Mpqf x -> of_mpqf_down x
    | _ -> invalid_arg "Float: unsupported Scalar type"

end
