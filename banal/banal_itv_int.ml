(* 
   Integer intervals.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Apron

module Int = Banal_int
module Intinf = Banal_intinf


(************************************************************************)
(* TYPES *)
(************************************************************************)

module Bound = Intinf

module I = Int
module B = Bound

type elem = I.t
type bound = Bound.t
type t = bound * bound


(* not all pairs of bounds are valid intervals *)
let sanitize (i:t) : t =
  match i with
  | Finite a, Finite b when I.gt a b -> invalid_arg "Itv_int.sanitize"
  | _,INF | MINF,_ -> invalid_arg "Itv_int.sanitize"
  | _ -> i

let check_bot (i:t) : t bot =
  match i with
  | Finite a, Finite b -> if I.leq a b then Nb i else Bot
  | MINF, INF -> Nb i
  | _ -> invalid_arg "Itv_int.check_bot"


(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let singleton (x:I.t) : t = Finite x, Finite x

let zero : t = singleton I.zero

let one : t = singleton I.one

let minus_one : t = singleton I.minus_one

let top : t = MINF, INF

let zero_one : t = Finite I.zero, Finite I.one

let minus_one_one : t = Finite I.minus_one, Finite I.one

let minus_one_zero : t = Finite I.minus_one, Finite I.zero

let positive : t = B.zero, INF

let negative : t = MINF, B.zero

let positive_strict : t = B.one, INF

let negative_strict : t = MINF, B.minus_one

let of_ints (l:I.t) (h:I.t) : t = Finite l, Finite h

let of_int x = of_ints x x

let of_intinf l h = l,h

let of_floats (l:float) (h:float) : t = B.of_float_down l, B.of_float_up h

let of_float x = of_floats x x

let of_bounds (l:B.t) (h:B.t) : t = l, h



(************************************************************************)
(* CONVERSION AND PRINTING *)
(************************************************************************)


let to_string ((l,h):t) : string = 
  Printf.sprintf "[%a;%a]" B.sprint l B.sprint h

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

let to_apron ((l,h):t) : Interval.t =
  { Interval.inf = Scalar.Mpqf (B.to_mpqf l);
    Interval.sup = Scalar.Mpqf (B.to_mpqf h);
  }

let of_apron (i:Interval.t) : t =
  B.of_apron_down i.Interval.inf, B.of_apron_up i.Interval.sup

let of_coeff (c:Coeff.t) : t =
  match c with
  | Coeff.Interval i -> of_apron i
  | Coeff.Scalar c -> B.of_apron_down c, B.of_apron_up c


(************************************************************************)
(* SET-THEORETIC *)
(*************************************in***********************************)

let equal (a:t) (b:t) : bool =
  a = b

let subseteq ((l1,h1):t) ((l2,h2):t) : bool =
  B.geq l1 l2 && B.leq h1 h2

let join ((l1,h1):t) ((l2,h2):t) : t =
  B.min l1 l2, B.max h1 h2

(* returns None if the set-union cannot be exactly represented *)
let union ((l1,h1):t) ((l2,h2):t) : t option =
  if B.leq l1 h2 && B.leq l2 h1 then Some (B.min l1 l2, B.max h1 h2)
  else None

let meet ((l1,h1):t) ((l2,h2):t) : t bot =
  check_bot (B.max l1 l2, B.min h1 h2)

let hull (x:B.t) (y:B.t) : t =
  B.min x y, B.max x y

let contains ((l,h):t) (x:I.t) : bool =
  B.leq l (Finite x) && B.leq (Finite x) h

let is_bounded ((l,h):t) =
  match l,h with Finite _, Finite _ -> true | _ -> false

let is_singleton ((l,h):t) : bool =
  match l,h with Finite a, Finite b -> I.equal a b | _ -> false


(************************************************************************)
(* FORWARD OVER-APPROXIMATED ARITHMETICS *)
(************************************************************************)

let neg ((l,h):t) : t =
  B.neg h, B.neg l

let abs ((l,h):t) : t =
  if contains (l,h) I.zero then B.zero, B.max (B.abs l) (B.abs h)
  else hull (B.abs l) (B.abs h)


let add ((l1,h1):t) ((l2,h2):t) : t =
  B.add l1 l2, B.add h1 h2

let sub ((l1,h1):t) ((l2,h2):t) : t =
  B.sub l1 h2, B.sub h1 l2


(* helper: oo * 0 = 0 when multiplying bounds *)
let bound_mul x y =
  match x,y with
  | Finite a, Finite b -> Finite (I.mul a b)
  | x, y -> match B.sign x, B.sign y with
    | 1,1 | -1,-1 -> INF
    | 1,-1 | -1,1 -> MINF
    | _ -> B.zero

let mul ((l1,h1):t) ((l2,h2):t) : t =
  join 
    (hull (bound_mul l1 l2) (bound_mul h1 h2))
    (hull (bound_mul l1 h2) (bound_mul h1 l2))


(* helper: oo / oo = oo, 0 / oo = 0 when dividing bounds; assume y != 0 *)
let bound_div div x y =
  match x,y with
  | Finite a, Finite b -> Finite (div a b)
  | Finite _, INF | Finite _, MINF -> Finite I.zero
  | x, y -> match B.sign x, B.sign y with
    | 1,1 | -1,-1 -> INF
    | 1,-1 | -1,1 -> MINF
    | _ -> raise Division_by_zero

(* helper: assumes 0 not in (l2,h2) *)
let div_trunc_nonzero ((l1,h1):t) ((l2,h2):t) : t =
  join 
    (hull (bound_div I.div l1 l2) (bound_div I.div h1 h2))
    (hull (bound_div I.div l1 h2) (bound_div I.div h1 l2))

(* C integer division: division with truncation *)
(* return valid values + possible division by zero *)
let div_trunc (i1:t) (i2:t) : t bot * bool =
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_trunc_nonzero i1)) (meet i2 positive_strict)
  and neg = (lift_bot (div_trunc_nonzero i1)) (meet i2 negative_strict) in
  (* joins the result *)
  join_bot2 join pos neg,
  contains i2 I.zero


(* helper *)
let fourway op1 op2 (l1,h1) (l2,h2) =
  B.min (B.min (op1 l1 l2) (op1 l1 h2)) (B.min (op1 h1 l2) (op1 h1 h2)),
  B.max (B.max (op2 l1 l2) (op2 l1 h2)) (B.max (op2 h1 l2) (op2 h1 h2))

(* helper: assumes 0 not in (l2,h2) *)
let div_nonzero (i1:t) (i2:t) : t =
  fourway (bound_div I.fdiv) (bound_div I.cdiv) i1 i2

(* over-approximation of the real division (outwards rounding) *)
let div  (i1:t) (i2:t) : t bot * bool =
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_nonzero i1)) (meet i2 positive_strict)
  and neg = (lift_bot (div_nonzero i1)) (meet i2 negative_strict) in
  (* joins the result *)
  join_bot2 join pos neg,
  contains i2 I.zero

  
(* C % operator *)
let rem ((l1,h1):t) (i2:t) : t bot * bool =
  (* a % b = a % |b| *)
  let l2,h2 = abs i2 in
  (* i2 = [0;0] => _|_, error *)
  if h2 = B.zero then Bot, true else
  (* max |i1| < min |i2| => i1, no-error *)
  if B.lt (snd (abs (l1,h1))) l2 then Nb (l1,h1), false else
  (* singleton => singleton, no-error *)
  if is_singleton (l1,h1) && is_singleton i2 then 
    match l1,l2 with
    | Finite a, Finite b -> let r = Finite (I.rem a b) in Nb (r,r), false
    | _ -> invalid_arg "Itv_int.rem"
  else 
    let b, z = B.pred h2, (B.sign l2 = 0) in
    (* i1 >= 0 => [0; max |i2|-1] *)
    if B.sign l1 >= 0 then Nb (B.zero, b), z else
    (* i1 <= 0 => [-max |i2|+1; 0] *)
    if B.sign h1 <= 0 then Nb (B.neg b, B.zero), z else
    (* other cases [-max |i2|+1; max |i2|-1] *)
    Nb (B.neg b, b), z


(* put back i into [l;h] by modular arithmetics *)
let modular (i:t) (l:I.t) (h:I.t) =
   match i with
   | Finite a, Finite b ->
       let a,b = I.sub a l, I.sub b l in
       let w = I.succ (I.sub h l) in
       if not (I.equal (I.fdiv a w) (I.fdiv b w)) 
       then Finite l, Finite h (* overlap *) 
       else Finite (I.add l (I.erem a w)), Finite (I.add l (I.erem b w))
   | _ -> Finite l, Finite h


(* defaults to the non-infinite bound, or zero *)
let mean ((l,h):t) : I.t =
  match l,h with
  | Finite a, Finite b -> I.div (I.add a b) (I.of_int 2)
  | Finite a,_ | _,Finite a -> a
  | MINF,INF -> I.zero
  | _ -> invalid_arg "Itv_int.middle"

let range ((l,h):t) : B.t = 
  B.sub h l

let magnitude ((l,h):t) : B.t =
  B.max (B.abs l) (B.abs h)


(************************************************************************)
(* BACKWARD OVER-APPROXIMATED ARITHMETICS *)
(************************************************************************)

(* filters return an overapproximation of the subset of the arguments 
   satisfying the test 
 *)

let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2 (check_bot (l1, B.min h1 h2)) (check_bot (B.max l1 l2, h2))

let filter_lt ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2
    (check_bot (l1, B.min h1 (B.pred h2)))
    (check_bot (B.max (B.succ l1) l2, h2))

let filter_geq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2 (check_bot (B.max l1 l2, h1)) (check_bot (l2, B.min h1 h2))

let filter_gt ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
  merge_bot2 
    (check_bot (B.max l1 (B.succ l2), h1))
    (check_bot (l2, B.min (B.pred h1) h2))

let filter_eq (i1:t) (i2:t) : (t*t) bot =
  lift_bot (fun x -> x,x) (meet i1 i2)

let filter_neq ((l1,h1) as i1 :t) ((l2,h2) as i2:t) : (t*t) bot =
  (* remove singleton at end of interval *)
  match is_singleton i1, is_singleton i2 with
  | true,true -> if B.equal l1 l2 then Bot else Nb (i1,i2)
  | true,false ->
      if B.equal l1 l2 then Nb (i1, (B.succ l2, h2)) else
      if B.equal l2 h2 then Nb (i1, (l2, B.pred h2))
      else Nb (i1,i2)
  | false,true ->
      if B.equal l1 l2 then Nb ((B.succ l1, h1), i2) else
      if B.equal l2 h2 then Nb ((l1, B.pred h1), i2)
      else Nb (i1,i2)
  | false,false -> Nb (i1,i2)


(* given the argument(s) i and the refined result r, returns refined arguments 
*)

let bwd_neg (i:t) (r:t) : t bot =
  meet i (neg r)

let bwd_add (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))
    
let bwd_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))



