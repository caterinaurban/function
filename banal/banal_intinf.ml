(* 
   Mathematical integers with infinites.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Apron
module Int = Banal_int
module Float = Banal_float


type base = Int.t
type t = base inf


(* useful constants *)

let zero = Finite Int.zero
let one = Finite Int.one
let minus_one = Finite Int.minus_one
let inf = INF
let minus_inf = MINF


(************************************************************************)
(* CONVERSIONS AND PRINTING *)
(************************************************************************)

let of_base (x:base) : t = Finite x

let to_base x = match x with
| INF | MINF -> raise Int.Overflow
| Finite x -> x

let of_int_up (x:Int.t) : t = Finite x
let of_int_down = of_int_up

let of_float_up (x:float) : t =
  try Finite (Int.of_float (ceil x)) with Int.Overflow -> INF

let of_float_down (x:float) : t =
  try Finite (Int.of_float (floor x)) with Int.Overflow -> MINF

let to_float_up (x:t) =
  match x with
  | INF -> Float.inf
  | MINF -> Float.minus_inf
  | Finite x -> Int.to_float x

let to_float_down (x:t) =
  match x with
  | INF -> Float.inf
  | MINF -> Float.minus_inf
  | Finite x -> -. (Int.to_float (Int.neg x))


let to_string = function
  | Finite x -> Int.to_string x
  | INF -> "+oo"
  | MINF -> "-oo"

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)


(************************************************************************)
(* ORDERING *)
(************************************************************************)

let sign = function
  | Finite x -> Int.sign x
  | INF -> 1
  | MINF -> -1

let equal = (=)

let compare a b = 
  match a,b with
  | Finite a, Finite b -> Int.compare a b
  | INF, INF | MINF, MINF -> 0
  | INF, _ | _, MINF -> 1
  | MINF, _ | _, INF -> -1

let leq  a b = 
  match a,b with
  | Finite a, Finite b -> Int.leq a b
  | MINF, _ | _, INF -> true
  | _ -> false

let lt a b = 
  match a,b with
  | Finite a, Finite b -> Int.lt a b
  | INF, _ | _, MINF -> false
  | _ -> true

let geq a b = leq b a

let gt a b = lt b a

let min a b = 
  match a,b with
  | Finite a, Finite b -> Finite (Int.min a b)
  | MINF, _ | _, MINF -> MINF
  | x, INF | INF, x -> x

let max a b = 
  match a,b with
  | Finite a, Finite b -> Finite (Int.max a b)
  | INF, _ | _, INF -> INF
  | x, MINF | MINF, x -> x

let is_finite = function Finite _ -> true | INF | MINF -> false


(************************************************************************)
(* ARITHMETIC *)
(************************************************************************)

(* unary *)
(* ***** *)

let neg = function
  | Finite x -> Finite (Int.neg x)
  | INF -> MINF
  | MINF -> INF

let abs = function
  | Finite x -> Finite (Int.abs x)
  | INF | MINF -> INF

let succ = function
  | Finite x -> Finite (Int.succ x)
  | INF -> INF
  | MINF -> MINF

let pred = function
  | Finite x -> Finite (Int.pred x)
  | INF -> INF
  | MINF -> MINF


(* binary *)
(* ****** *)

let add a b = 
  match a,b with
  | Finite a, Finite b -> Finite (Int.add a b)
  | Finite _, INF | INF, Finite _ -> INF
  | Finite _, MINF | MINF, Finite _ -> MINF
  | INF, INF -> INF
  | MINF, MINF -> MINF
  | INF, MINF | MINF, INF -> invalid_arg "Intinf.add"

let sub a b = 
  match a,b with
  | Finite a, Finite b -> Finite (Int.add a b)
  | INF, Finite _ | Finite _, MINF -> INF
  | Finite _, INF | MINF, Finite _ -> MINF
  | INF, MINF -> INF
  | MINF, INF -> MINF
  | INF, INF | MINF, MINF -> invalid_arg "Intinf.sub"

let mul a b = 
  match a,b with
  | Finite a, Finite b -> Finite (Int.mul a b)
  | x, y -> match sign x, sign y with
    | 1,1 | -1,-1 -> INF
    | 1,-1 | -1,1 -> MINF
    | _ -> invalid_arg "Intinf.mul"

let wrap_div div a b =  match a,b with
  | Finite a, Finite b -> Finite (div a b)
  | Finite _, INF | Finite _, MINF -> Finite Int.zero
  | (INF | MINF), (INF | MINF) -> invalid_arg "Intinf.div"
  | x, y -> match sign x, sign y with
    | 1,1 | -1,-1 | 1,0 -> INF
    | 1,-1 | -1,1 | -1,0 -> MINF
    | _ -> invalid_arg "Intinf.div"

let div a b = wrap_div Int.div a b

let cdiv a b = wrap_div Int.cdiv a b

let fdiv a b = wrap_div Int.fdiv a b


let add_up = add
let sub_up = sub
let mul_up = mul
let div_up = cdiv
let add_down = add
let sub_down = sub
let mul_down = mul
let div_down = fdiv 


(* operators *)
(* ********* *)

let (~-) = neg
let (+) = add
let (-) = sub
let ( * ) = mul
let (/) = div
let (/>) = cdiv
let (/<) = fdiv



(* mlgmpidl and apron *)
(* ****************** *)

let mpq_inf = Mpq.init ()
let mpq_minf = Mpq.init ()
let _ = Mpq.set_num mpq_inf (Mpz.of_int 1)
let _ = Mpq.set_den mpq_inf (Mpz.of_int 0)
let _ = Mpq.set_num mpq_minf (Mpz.of_int (-1))
let _ = Mpq.set_den mpq_minf (Mpz.of_int 0)


let to_mpqf x = 
  match x with
  | INF -> Mpqf.of_mpq mpq_inf
  | MINF -> Mpqf.of_mpq mpq_minf
  | Finite x -> Mpqf.of_mpz (Int.to_mpz x)

let of_mpqf_up x =
  let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
  match Int.sign n, Int.sign d with
  | 1,0 -> INF
  | -1,0 -> MINF
  | 0,0 -> invalid_arg "Intinf.of_mpqf_up"
  | _ -> Finite (Int.cdiv n d)

let of_mpqf_down x =
  let n,d = Int.of_mpzf (Mpqf.get_num x), Int.of_mpzf (Mpqf.get_den x) in
  match Int.sign n, Int.sign d with
  | 1,0 -> INF
  | -1,0 -> MINF
  | 0,0 -> invalid_arg "Intinf.of_mpqf_up"
  | _ -> Finite (Int.fdiv n d)

let to_apron x = 
  Scalar.Mpqf (to_mpqf x)

let of_apron_up = function
  | Scalar.Float x -> of_float_up x
  | Scalar.Mpqf x -> of_mpqf_up x
  | _ -> invalid_arg "Intinf: unsupported Scalar type"

let of_apron_down = function
  | Scalar.Float x -> of_float_down x
  | Scalar.Mpqf x -> of_mpqf_down x
  | _ -> invalid_arg "Intinf: unsupported Scalar type"


