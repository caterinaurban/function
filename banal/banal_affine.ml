(* 
   Affine forms with interval coefficients.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_mathtypes

module Itv_int = Banal_itv_int
module Itv_float = Banal_itv_float
module Itv_rat = Banal_itv_rat


module type VAR = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end


(* generic functor *)
(* *************** *)

module Make( V : VAR )( I : INTERVAL ) = struct

  module Itv = I
  module Var = V

  module VMap = Mapext.Make(V)
  module B = I.Bound

  type var = V.t
  type bound = B.t
  type elem = I.elem
  type itv = I.t
  type env = var -> itv

  (* interval constant + interval coefficients *)
  type t = itv * (itv VMap.t)


  (* constructors *)
  (* ************ *)

  let cst (a:itv) : t = a, VMap.empty

  let zero : t = cst I.zero

  let one : t = cst I.one

  let minus_one : t = cst I.minus_one

  let top : t = cst I.top

  let term (c:itv) (v:var) : t = I.zero, VMap.singleton v c

  let var (v:var): t = term I.one v


  (* printing *)
  (* ******** *)

  let to_string ((i,m):t) =
    let first = ref true in
    let b = Buffer.create 16 in
    (* constant part *)
    if not (I.equal i I.zero) then (
      first := false;
      if I.is_singleton i then Buffer.add_string b (B.to_string (fst i))
      else Buffer.add_string b (I.to_string i)
     );
    (* terms *)
    VMap.iter
      (fun v ((l,_) as i) ->
        if not (I.equal i I.zero) then (
          if I.is_singleton i then (
            if !first then
              if B.equal l B.minus_one then Buffer.add_string b "-" else
              if B.equal l B.one then () else
              Buffer.add_string b (B.to_string l)
            else
              if B.equal l B.minus_one then Buffer.add_string b "-" else
              if B.equal l B.one then Buffer.add_string b "+" else (
              if B.sign l > 0 then Buffer.add_string b "+";
              Buffer.add_string b (B.to_string l)
             )
           )
          else (
            if not !first then Buffer.add_string b "+";
            Buffer.add_string b (I.to_string i)
           );
          first := false;
          Buffer.add_string b (V.to_string v)
         )
      )
      m;
    if !first then "0" else Buffer.contents b
    


  (* comparison *)
  (* ********** *)

  let equal ((i1,m1):t) ((i2,m2):t) =
    (I.equal i1 i2) &&
    (VMap.for_all2zo
       (fun _ -> I.equal I.zero)
       (fun _ -> I.equal I.zero)
       (fun _ -> I.equal)
       m1 m2
    )

  (* point-wise ordering of intervals *)
  let leq ((i1,m1):t) ((i2,m2):t) =
    (I.subseteq i1 i2) &&
    (VMap.for_all2zo
       (fun _ a -> I.subseteq a I.zero)
       (fun _ b -> I.subseteq I.zero b)
       (fun _ -> I.subseteq)
       m1 m2
    )


  (* operators *)
  (* ********* *)


  (* evaluates form in the given environment *)
  let eval (env:env) ((i,m):t) : itv =
    VMap.fold
      (fun v i acc -> 
        if I.equal i I.zero then acc
        else I.add acc (I.mul (env v) i)
      )
      m i


  let neg ((i,m):t) : t =
    I.neg i, VMap.map I.neg m


  let add ((i1,m1):t) ((i2,m2):t) : t =
    I.add i1 i2, 
    VMap.map2o 
      (fun _ i -> i) (fun _ i -> i) (fun _ i1 i2 -> I.add i1 i2) 
      m1 m2

  let add_cst ((i1,m1):t) (i2:itv) : t =
    I.add i1 i2, m1


  let sub ((i1,m1):t) ((i2,m2):t) : t =
    I.sub i1 i2, 
    VMap.map2o 
      (fun _ i -> i) (fun _ i -> I.neg i) 
      (fun _ i1 i2 -> I.sub i1 i2) m1 m2

  let sub_cst ((i1,m1):t) (i2:itv) : t =
    I.sub i1 i2, m1


  let mul_cst ((i1,m1):t) (i2:itv) : t =
    I.mul i1 i2, VMap.map (fun i -> I.mul i i2) m1

  (* is i positive or negative ? *)
  let itv_cst_sign ((l,h):itv) =
    B.sign l >= 0 || B.sign h <= 0

  let mul (env:env) ((i1,m1) as l1:t) ((i2,m2) as l2:t) : t =
    (* case where an argument is constant *)
    if VMap.is_empty m1 then mul_cst l2 i1 else
    if VMap.is_empty m2 then mul_cst l1 i2 else
    let i1,i2 = eval env l1, eval env l2 in
    (* prefer the interval with constant sign *)
    match itv_cst_sign i1, itv_cst_sign i2 with
    | true, false -> mul_cst l2 i1
    | false, true -> mul_cst l1 i2
    | _ ->
        (* prefer the interval with smallest range *)
        if I.range i1 <= I.range i2 then mul_cst l2 i1
        else mul_cst l1 i2


  (* maps division by an interval containing zero to Division_by_zero *)
  let itv_safe_div (i1:itv) (i2:itv) : itv =
    if I.equal i1 I.zero then I.zero else
    match I.div i1 i2 with
    | Nb i, false -> i
    | _ -> raise Division_by_zero

  let div_cst ((i1,m1):t) (i2:itv) : t =
    itv_safe_div i1 i2, VMap.map (fun i -> itv_safe_div i i2) m1

  let div (env:env) (l1:t) (l2:t) : t =
    div_cst l1 (eval env l2)


  (* substitues variables by affine forms;
     (env v) can raise Not_found, in which case the variable is unmodified
   *)
  let subst (env:var -> t) ((i,m):t) : t =
    VMap.fold
      (fun v c acc ->
        add acc (try mul_cst (env v) c  with Not_found -> term c v)
      )
      m (i,VMap.empty)


  (* substitutes v with l in (i,m) *)
  let subst_var (v:var) (l:t) ((i,m):t) : t =
    try
      let c = VMap.find v m in
      add (mul_cst l c) (i,VMap.remove v m)
    with Not_found -> 
      i,m


  (* applies f to each interval *)
  let map (f:itv -> itv) ((i,m):t) : t =
    f i, VMap.map f m


  let itv_symmetric ((l,h):itv) : itv =
    let m = B.max (B.abs l) (B.abs h) in
    B.neg m, m

  (* replace intervals [a;b] with max(|a|,|b|) * [-1;1] *)
  let symmetric (l:t) : t =
    map itv_symmetric  l


  (* replace interval variable coefficients with scalar ones *)
  let quasilinearize (env:env) ((i,m):t) : t =
    try
      VMap.fold
        (fun v (cl,ch) (i,m) ->
          let med = B.of_base (I.mean (cl,ch)) in
          let delta = 
            B.max (B.abs (B.sub_up cl med))
                  (B.abs (B.sub_up med ch))
          in
          I.add i (I.mul (env v) (B.neg delta,delta)),
          if B.equal med B.zero then m 
          else VMap.add v (med,med) m
        )
        m (i,VMap.empty)
    with Int.Overflow -> I.top, VMap.empty


  (* remove zero coefficients *)
  let simplify ((i,m):t) : t =
    i,
    (VMap.fold 
       (fun v i acc -> if I.equal i I.zero then acc else VMap.add v i acc)
        m VMap.empty
    )


  (* get/set coefficients *)
 
  let get_cst ((i,m):t) : itv = 
    i

  let set_cst (i:itv) ((_,m):t) : t = 
    i,m

  let get_var (v:var) ((_,m):t) : itv = 
    try VMap.find v m with Not_found -> I.zero

  let set_var (v:var) (c:itv) ((i,m):t) : t = 
    i, VMap.add v c m


  (* projection *)
  let project ((i,m):t) (vl:var list) : t =
    let mm =
      List.fold_left
        (fun mm v -> try VMap.add v (VMap.find v m) mm with Not_found -> mm)
        VMap.empty vl
    in
    i,mm


end


(* instantiations *)
(* ************** *)

module IntAffine(V:VAR) = Make(V)(Itv_int)
module RatAffine(V:VAR) = Make(V)(Itv_rat)
module FloatAffine(V:VAR) = Make(V)(Itv_float)
