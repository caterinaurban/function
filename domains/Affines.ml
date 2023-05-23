(* ********* Affine Ranking Functions Abstract Domain ************ Copyright
   (C) 2012-2014 by Caterina Urban. All rights reserved. *)

open AbstractSyntax
open Apron
open Partition
open Functions
open Numerical

module Affine (B : PARTITION) : FUNCTION = struct
  module B = B

  (**)

  let manager = Polka.manager_alloc_strict ()

  type a = Bot | Fun of Linexpr1.t | Top

  type f = {ranking: a; env: Environment.t; vars: var list}

  let v = Var.of_string "#"

  let ranking f = f.ranking

  let env f = f.env

  let vars f = f.vars

  (**)
  let reinit f =
    match f.ranking with
    | Top -> {ranking= Bot; env= f.env; vars= f.vars}
    | _ -> f

  let bot e vs = {ranking= Bot; env= e; vars= vs}

  let zero e vs =
    { ranking= Fun (Linexpr1.make (Environment.add e [|v|] [||]))
    ; env= e
    ; vars= vs }

  let top e vs = {ranking= Top; env= e; vars= vs}

  (**)

  let isBot f = match f.ranking with Bot -> true | _ -> false

  let defined f = match f.ranking with Fun _ -> true | _ -> false

  let isTop f = match f.ranking with Top -> true | _ -> false

  let isEq b f1 f2 =
    (* b = domain of first/second function, f1/f2 = value of first/second
       function *)
    match (f1.ranking, f2.ranking) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l = List.length (B.constraints b) + 1 in
        (* l = |b| + 1 *)
        let a1 = Lincons1.array_make env l
        and a2 = Lincons1.array_make env l in
        let i = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a1 !i (Lincons1.extend_environment c env) ;
            Lincons1.array_set a2 !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b) ;
        (* copying constraints from b to a1 and a2 *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a1 (l - 1) (Lincons1.make f1 Lincons1.SUPEQ) ;
        (* adding constraint # <= f1 to a1 *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a2 (l - 1) (Lincons1.make f2 Lincons1.SUPEQ) ;
        (* adding constraint # <= f2 to a2 *)
        let p1 = Abstract1.of_lincons_array manager env a1 in
        (* p1 = polyhedra represented by a1 *)
        let p2 = Abstract1.of_lincons_array manager env a2 in
        (* p2 = polyhedra represented by a2 *)
        Abstract1.is_eq manager p1 p2
    | Bot, Bot | Top, Top -> true
    | _ -> false

  let domainEq b f1 f2 =
    (* b = domain of first/second function, f1/f2 = value of first/second
       function *)
    match (f1.ranking, f2.ranking) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l = List.length (B.constraints b) + 2 in
        (* l = |b| + 2 *)
        let a = Lincons1.array_make env l in
        let i = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b) ;
        (* copying constraints from b to a *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a (l - 2) (Lincons1.make f1 Lincons1.EQ) ;
        (* adding constraint # = f1 to a *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a (l - 1) (Lincons1.make f2 Lincons1.EQ) ;
        (* adding constraint # = f2 to a *)
        let p = Abstract1.of_lincons_array manager env a in
        (* remove # special variable *)
        let p = Abstract1.change_environment manager p (B.env b) false in
        let cc = Abstract1.to_lincons_array manager p in
        let f = ref [] in
        for i = 0 to Lincons1.array_length cc - 1 do
          f := Lincons1.array_get cc i :: !f
        done ;
        B.inner (B.env b) (B.vars b) !f
    | Bot, Bot | Top, Top -> b
    | _ -> B.bot (B.env b) (B.vars b)

  let isLeq k b f1 f2 =
    (* k = kind of test, b = domain of first/second function, f1/f2 = value
       of first/second function *)
    match (f1.ranking, f2.ranking) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l = List.length (B.constraints b) + 1 in
        (* l = |b| + 1 *)
        let a1 = Lincons1.array_make env l
        and a2 = Lincons1.array_make env l in
        let i = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a1 !i (Lincons1.extend_environment c env) ;
            Lincons1.array_set a2 !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b) ;
        (* copying constraints from b to a1 and a2 *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a1 (l - 1) (Lincons1.make f1 Lincons1.SUPEQ) ;
        (* adding constraint # <= f1 to a1 *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a2 (l - 1) (Lincons1.make f2 Lincons1.SUPEQ) ;
        (* adding constraint # <= f2 to a2 *)
        let p1 = Abstract1.of_lincons_array manager env a1 in
        (* p1 = polyhedra represented by a1 *)
        let p2 = Abstract1.of_lincons_array manager env a2 in
        (* p2 = polyhedra represented by a2 *)
        Abstract1.is_leq manager p1 p2
    | Bot, Fun _ -> (
      match k with APPROXIMATION -> false | COMPUTATIONAL -> true )
    | Fun _, Bot -> (
      match k with APPROXIMATION -> true | COMPUTATIONAL -> false )
    | Bot, _ | _, Top -> true
    | _ -> false

  (**)

  let join_ranking k b f1 f2 =
    (* k = kind of join, b = domain of first/second function, f1/f2 = value
       of first/second function *)
    let aux a c =
      (* checking if constraint c belongs to set of constraints a *)
      let l = Lincons1.array_length a in
      let b = ref false in
      for i = 0 to l - 1 do
        if c = Lincons1.array_get a i then b := true
      done ;
      !b
    in
    (*REMOVE?*)
    match (f1, f2) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l = List.length (B.constraints b) + 1 in
        (* l = |b| + 1 *)
        let a = Lincons1.array_make env (l - 1) in
        (*REMOVE?*)
        let a1 = Lincons1.array_make env l
        and a2 = Lincons1.array_make env l in
        let i = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a !i (Lincons1.extend_environment c env) ;
            (*REMOVE?*)
            Lincons1.array_set a1 !i (Lincons1.extend_environment c env) ;
            Lincons1.array_set a2 !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b) ;
        (* copying constraints from b to a1 and a2 *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a1 (l - 1) (Lincons1.make f1 Lincons1.SUPEQ) ;
        (* adding constraint # <= f1 to a1 *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a2 (l - 1) (Lincons1.make f2 Lincons1.SUPEQ) ;
        (* adding constraint # <= f2 to a2 *)
        let p1 = Abstract1.of_lincons_array manager env a1 in
        (* p1 = polyhedra represented by a1 *)
        let p2 = Abstract1.of_lincons_array manager env a2 in
        (* p2 = polyhedra represented by a2 *)
        let p = Abstract1.join manager p1 p2 in
        (* p = convex-hull *)
        let p = Abstract1.to_lincons_array manager p in
        (* converting p into set of constraints *)
        let f = ref [] in
        for i = 0 to Lincons1.array_length p - 1 do
          let c = Lincons1.array_get p i in
          try
            if
              (not (Coeff.is_zero (Lincons1.get_coeff c v)))
              && (*REMOVE?*) not (aux a c)
            then f := c :: !f
          with _ -> ()
        done ;
        (* f = list of constraints on special variable # *)
        if 1 = List.length !f (* if there is only one constraint on # *) then (
          let f = Lincons1.get_linexpr1 (List.hd !f) in
          Linexpr1.set_coeff f v (Coeff.s_of_int 0) ;
          Fun f (* defined join function *) )
        else Top (* otherwise *)
    | Bot, _ -> ( match k with APPROXIMATION -> Bot | COMPUTATIONAL -> f2 )
    | _, Bot -> ( match k with APPROXIMATION -> Bot | COMPUTATIONAL -> f1 )
    | _ -> Top

  let join k b f1 f2 =
    { ranking= join_ranking k b f1.ranking f2.ranking
    ; env= f1.env
    ; vars= f1.vars }

  let widen_ranking b f1 f2 =
    (* b = domain of first/second function, f1/f2 = value of first/second
       function *)
    let aux a c =
      (* checking if constraint c belongs to set of constraints a *)
      let l = Lincons1.array_length a in
      let b = ref false in
      for i = 0 to l - 1 do
        if c = Lincons1.array_get a i then b := true
      done ;
      !b
    in
    (*REMOVE?*)
    match (f1, f2) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l = List.length (B.constraints b) + 1 in
        (* l = |b| + 1 *)
        let a = Lincons1.array_make env (l - 1) in
        (*REMOVE?*)
        let a1 = Lincons1.array_make env l
        and a2 = Lincons1.array_make env l in
        let i = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a !i (Lincons1.extend_environment c env) ;
            (*REMOVE?*)
            Lincons1.array_set a1 !i (Lincons1.extend_environment c env) ;
            Lincons1.array_set a2 !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b) ;
        (* copying constraints from b to a1 and a2 *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a1 (l - 1) (Lincons1.make f1 Lincons1.SUPEQ) ;
        (* adding constraint # <= f1 to a1 *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a2 (l - 1) (Lincons1.make f2 Lincons1.SUPEQ) ;
        (* adding constraint # <= f2 to a2 *)
        let p1 = Abstract1.of_lincons_array manager env a1 in
        (* p1 = polyhedra represented by a1 *)
        let p2 = Abstract1.of_lincons_array manager env a2 in
        (* p2 = polyhedra represented by a2 *)
        let p = Abstract1.widening manager p1 p2 in
        (* p = widening *)
        let p = Abstract1.to_lincons_array manager p in
        (* converting p into set of constraints *)
        let f = ref [] in
        for i = 0 to Lincons1.array_length p - 1 do
          let c = Lincons1.array_get p i in
          try
            if
              (not (Coeff.is_zero (Lincons1.get_coeff c v)))
              && (*REMOVE?*) not (aux a c)
            then f := c :: !f
          with _ -> ()
        done ;
        (* f = list of constraints on special variable # *)
        if 1 = List.length !f (* if there is only one constraint on # *) then (
          let f = Lincons1.get_linexpr1 (List.hd !f) in
          Linexpr1.set_coeff f v (Coeff.s_of_int 0) ;
          Fun f (* defined widening function *) )
        else Top (* otherwise *)
    | Bot, _ -> f2
    | _, Bot -> f1
    | _ -> Top

  let widen ?(jokers = 0) b f1 f2 =
    { ranking= widen_ranking b f1.ranking f2.ranking
    ; env= f1.env
    ; vars= f1.vars }

  let extend_ranking b1 b2 f1 f2 =
    match (f1, f2) with
    | Fun f1, Fun f2 ->
        let env = Environment.add (B.env b1) [|v|] [||] in
        (* adding special variable # to environment of b *)
        let l1 = List.length (B.constraints b1) + 1 in
        (* l1 = |b1| + 1 *)
        let l2 = List.length (B.constraints b2) + 1 in
        (* l2 = |b2| + 1 *)
        let a1 = Lincons1.array_make env l1
        and a2 = Lincons1.array_make env l2 in
        let i = ref 0 and j = ref 0 in
        List.iter
          (fun c ->
            Lincons1.array_set a1 !i (Lincons1.extend_environment c env) ;
            i := !i + 1 )
          (B.constraints b1) ;
        (* copying constraints from b1 to a1 *)
        List.iter
          (fun c ->
            Lincons1.array_set a2 !j (Lincons1.extend_environment c env) ;
            j := !j + 1 )
          (B.constraints b2) ;
        (* copying constraints from b2 to a2 *)
        let f1 = Linexpr1.copy f1 and f2 = Linexpr1.copy f2 in
        (* creating copies of f1 and f2 *)
        Linexpr1.set_coeff f1 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a1 (l1 - 1) (Lincons1.make f1 Lincons1.SUPEQ) ;
        (* adding constraint # <= f1 to a1 *)
        Linexpr1.set_coeff f2 v (Coeff.s_of_int (-1)) ;
        Lincons1.array_set a2 (l2 - 1) (Lincons1.make f2 Lincons1.SUPEQ) ;
        (* adding constraint # <= f2 to a2 *)
        let p1 = Abstract1.of_lincons_array manager env a1 in
        (* p1 = polyhedra represented by a1 *)
        let p2 = Abstract1.of_lincons_array manager env a2 in
        (* p2 = polyhedra represented by a2 *)
        let p = Abstract1.join manager p1 p2 in
        (* p = convex-hull *)
        let p = Abstract1.to_lincons_array manager p in
        (* converting p into set of constraints *)
        let f = ref [] in
        for i = 0 to Lincons1.array_length p - 1 do
          let c = Lincons1.array_get p i in
          try
            if not (Coeff.is_zero (Lincons1.get_coeff c v)) then f := c :: !f
          with _ -> ()
        done ;
        (* f = list of constraints on special variable # *)
        if 1 <= List.length !f (* if there is at least one constraint on # *)
        then
          let f =
            List.map
              (fun c ->
                let c = Lincons1.get_linexpr1 c in
                (* let k = Linexpr1.get_coeff c v in if Coeff.is_scalar k &&
                   (Coeff.cmp k (Coeff.s_of_int 0)) < 0 then Linexpr1.set_cst
                   c (Linexpr1.get_cst c); if Coeff.is_scalar k && (Coeff.cmp
                   k (Coeff.s_of_int 0)) < 0 then Linexpr1.iter (fun k x ->
                   Linexpr1.set_coeff c x (Coeff.neg k)) c; *)
                Linexpr1.set_coeff c v (Coeff.s_of_int 0) ;
                Fun c )
              !f
          in
          List.fold_left
            (join_ranking COMPUTATIONAL b2)
            (List.hd f) (List.tl f)
        else Top (* otherwise *)
    | _ -> f2

  let extend b1 b2 f1 f2 =
    { ranking= extend_ranking b1 b2 f1.ranking f2.ranking
    ; env= f1.env
    ; vars= f1.vars }

  (**)

  let reset f =
    { ranking= Fun (Linexpr1.make (Environment.add f.env [|v|] [||]))
    ; env= f.env
    ; vars= f.vars }

  let addScalar c1 c2 =
    match (c1, c2) with
    | Scalar.Float c1, Scalar.Float c2 -> Scalar.Float (c1 +. c2)
    | Scalar.Float c1, Scalar.Mpqf c2 -> Scalar.Float (c1 +. Mpqf.to_float c2)
    | Scalar.Float c1, Scalar.Mpfrf c2 ->
        Scalar.Float (c1 +. Mpfrf.to_float c2)
    | Scalar.Mpqf c1, Scalar.Float c2 -> Scalar.Float (Mpqf.to_float c1 +. c2)
    | Scalar.Mpqf c1, Scalar.Mpqf c2 -> Scalar.Mpqf (Mpqf.add c1 c2)
    | Scalar.Mpqf c1, Scalar.Mpfrf c2 ->
        Scalar.Mpqf (Mpqf.add c1 (Mpfrf.to_mpqf c2))
    | Scalar.Mpfrf c1, Scalar.Float c2 ->
        Scalar.Float (Mpfrf.to_float c1 +. c2)
    | Scalar.Mpfrf c1, Scalar.Mpqf c2 ->
        Scalar.Mpqf (Mpqf.add (Mpfrf.to_mpqf c1) c2)
    | Scalar.Mpfrf c1, Scalar.Mpfrf c2 ->
        Scalar.Mpfrf (Mpfrf.add c1 c2 Mpfr.Zero)

  let addCoeff c1 c2 =
    match (c1, c2) with
    | Coeff.Scalar c1, Coeff.Scalar c2 -> Coeff.Scalar (addScalar c1 c2)
    | Coeff.Scalar c1, Coeff.Interval c2 ->
        Coeff.reduce
          (Coeff.i_of_scalar
             (addScalar c1 c2.Interval.inf)
             (addScalar c1 c2.Interval.sup) )
    | Coeff.Interval c1, Coeff.Scalar c2 ->
        Coeff.reduce
          (Coeff.i_of_scalar
             (addScalar c1.Interval.inf c2)
             (addScalar c1.Interval.sup c2) )
    | Coeff.Interval c1, Coeff.Interval c2 ->
        Coeff.reduce
          (Coeff.i_of_scalar
             (addScalar c1.Interval.inf c2.Interval.inf)
             (addScalar c1.Interval.sup c2.Interval.sup) )

  let predecessor_ranking f =
    match f with
    | Fun f ->
        let f = Linexpr1.copy f in
        Linexpr1.set_cst f
          (addCoeff (Linexpr1.get_cst f) (Coeff.s_of_int (-1))) ;
        Fun f
    | _ -> f

  let predecessor f =
    {ranking= predecessor_ranking f.ranking; env= f.env; vars= f.vars}

  let successor_ranking f =
    match f with
    | Fun f ->
        let f = Linexpr1.copy f in
        Linexpr1.set_cst f (addCoeff (Linexpr1.get_cst f) (Coeff.s_of_int 1)) ;
        Fun f
    | _ -> f

  let successor f =
    {ranking= successor_ranking f.ranking; env= f.env; vars= f.vars}

  let bwdAssign_ranking f (x, e) =
    match x with
    | A_var x -> (
      match f with
      | Fun f ->
          let env = Linexpr1.get_env f in
          let e = Texpr1.of_expr env (aExp_to_apron e) in
          let f = Linexpr1.copy f in
          let a = Lincons1.array_make env 1 in
          Linexpr1.set_coeff f v (Coeff.s_of_int (-1)) ;
          Lincons1.array_set a 0 (Lincons1.make f Lincons1.SUPEQ) ;
          let p = Abstract1.of_lincons_array manager env a in
          let p =
            Abstract1.substitute_texpr manager p (Var.of_string x.varId) e
              None
          in
          let a = Abstract1.to_lincons_array manager p in
          if 1 = Lincons1.array_length a then (
            let f = Lincons1.get_linexpr1 (Lincons1.array_get a 0) in
            Linexpr1.set_coeff f v (Coeff.s_of_int 0) ;
            Linexpr1.set_cst f
              (addCoeff (Linexpr1.get_cst f) (Coeff.s_of_int 1)) ;
            Fun f )
          else Top
      | _ -> f )
    | _ -> raise (Invalid_argument "Box.fwdAssign: unexpected lvalue")

  let bwdAssign f (x, e) =
    {ranking= bwdAssign_ranking f.ranking (x, e); env= f.env; vars= f.vars}

  let filter f _ = successor f

  (**)

  let print fmt f =
    let first = ref true in
    let rec aux c v =
      match c with
      | Coeff.Scalar s ->
          if v <> "" && Scalar.sgn s = 0 then ()
          else (
            if Scalar.sgn s < 0 then
              if v <> "" && Scalar.equal_int s (-1) then
                Format.fprintf fmt "-"
              else Format.fprintf fmt "-%s" (Scalar.to_string (Scalar.neg s))
            else if !first then
              if v <> "" && Scalar.equal_int s 1 then ()
              else Format.fprintf fmt "%s" (Scalar.to_string s)
            else if v <> "" && Scalar.equal_int s 1 then
              Format.fprintf fmt "+"
            else Format.fprintf fmt "+%s" (Scalar.to_string s) ;
            if v <> "" then Format.fprintf fmt "%s" v ;
            first := false )
      | Coeff.Interval i ->
          if Scalar.equal i.Interval.inf i.Interval.sup then
            aux (Coeff.Scalar i.Interval.inf) v
          else (
            if not !first then Format.fprintf fmt "+" ;
            Format.fprintf fmt "[%s,%s]"
              (Scalar.to_string i.Interval.inf)
              (Scalar.to_string i.Interval.sup) ;
            if v <> "" then Format.fprintf fmt "%s" v ) ;
          first := false
    in
    let vars = f.vars in
    match f.ranking with
    | Fun f ->
        Linexpr1.iter
          (fun v x ->
            try
              let x =
                List.find
                  (fun y -> String.compare (Var.to_string x) y.varId = 0)
                  vars
              in
              Format.fprintf Format.str_formatter "%s{%s}" x.varId x.varName ;
              aux v (Format.flush_str_formatter ())
            with Not_found -> () )
          f ;
        aux (Linexpr1.get_cst f) ""
    | Bot -> Format.fprintf fmt "bottom"
    | Top -> Format.fprintf fmt "top"
end

module AB = Affine (B)
module AO = Affine (O)
module AP = Affine (P)
