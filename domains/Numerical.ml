(***************************************************)
(*                                                 *)
(*   Ranking Function Numerical Domain Partition   *)
(*                                                 *)
(*                  Caterina Urban                 *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Partition
open Constraints


module type NUMERICAL = sig 
  type lib
  val manager: lib Manager.t 
  val supports_underapproximation: bool
end

(** Single partition of the domain of a ranking function 
    represented by an APRON numerical abstract domain. *)
module Numerical(N: NUMERICAL)(C: CONSTRAINT): PARTITION = struct

  (** Linear constraints used to represent the partition. *)
  module C = C 

  module BanalApron = Banal_apron_domain.ApronDomain(N)

  (** An element of the numerical abstract domain. *)
  type t = { 
    constraints : C.t list; (* representation as list of constraints *)
    env : Environment.t; (* APRON environment *)
    vars : var list (* list of variables in the APRON environment *)
  }

  type apron_t = N.lib Abstract1.t

  (** The current representation as list of linear constraints. *)
  let constraints b: C.t list = List.fold_right (fun c cs -> 
      (* warning: fold_left impacts speed and result of the analysis *)
      try (* equality constraints are turned into pairs of inequalities *)
        let (c1,c2) = C.expand c in c1::c2::cs
      with Invalid_argument _ -> c::cs
    ) b.constraints [] 

  (** The current APRON environment. *)
  let env b = b.env

  (** The current list of variables in the APRON environment. *)
  let vars b = b.vars

  (** Creates an APRON manager depending on the numerical abstract domain. *)

  let manager = N.manager

  (**)

  let bot e vs = {
    constraints = [Lincons1.make_unsat e];
    env = e;
    vars = vs
  }

  let inner e vs cs = {
    constraints = cs;
    env = e;
    vars = vs
  }

  let top e vs = {
    constraints = [];
    env = e;
    vars = vs
  }

  (**)

  let isBot b =
    let a = Lincons1.array_make b.env (List.length b.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) b.constraints;
    let b = Abstract1.of_lincons_array manager b.env a in
    Abstract1.is_bottom manager b

  let isLeq b1 b2 = 
    let env = b1.env in
    let a1 = Lincons1.array_make env (List.length b1.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a1 !i c; i := !i + 1) b1.constraints;
    let b1 = Abstract1.of_lincons_array manager env a1 in
    let a2 = Lincons1.array_make env (List.length b2.constraints) in
    let j = ref 0 in
    List.iter (fun c -> Lincons1.array_set a2 !j c; j := !j + 1) b2.constraints;
    let b2 = Abstract1.of_lincons_array manager env a2 in
    Abstract1.is_leq manager b1 b2

  (**)


  let to_apron_t (t:t) : apron_t = 
    let env = t.env in
    let a = Lincons1.array_make env (List.length t.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) t.constraints;
    Abstract1.of_lincons_array manager env a 

  let of_apron_t env vars (a:apron_t) : t = 
    let a = Abstract1.to_lincons_array manager a in
    let cs = ref [] in
    for i=0 to (Lincons1.array_length a)-1 do
      cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
    done; { constraints = !cs; env = env; vars = vars }

  let join b1 b2 = 
    let env = b1.env in
    let vars = b1.vars in
    let a1 = Lincons1.array_make env (List.length b1.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a1 !i c; i := !i + 1) b1.constraints;
    let b1 = Abstract1.of_lincons_array manager env a1 in
    let a2 = Lincons1.array_make env (List.length b2.constraints) in
    let j = ref 0 in
    List.iter (fun c -> Lincons1.array_set a2 !j c; j := !j + 1) b2.constraints;
    let b2 = Abstract1.of_lincons_array manager env a2 in
    let b = Abstract1.join manager b1 b2 in
    let a = Abstract1.to_lincons_array manager b in
    let cs = ref [] in
    for i=0 to (Lincons1.array_length a)-1 do
      cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
    done; { constraints = !cs; env = env; vars = vars }

  let widen b1 b2 = 
    let env = b1.env in
    let vars = b1.vars in
    let a1 = Lincons1.array_make env (List.length b1.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a1 !i c; i := !i + 1) b1.constraints;
    let b1 = Abstract1.of_lincons_array manager env a1 in
    let a2 = Lincons1.array_make env (List.length b2.constraints) in
    let j = ref 0 in
    List.iter (fun c -> Lincons1.array_set a2 !j c; j := !j + 1) b2.constraints;
    let b2 = Abstract1.of_lincons_array manager env a2 in
    let b = Abstract1.widening manager b1 b2 in
    let a = Abstract1.to_lincons_array manager b in
    let cs = ref [] in
    for i=0 to (Lincons1.array_length a)-1 do
      cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
    done; { constraints = !cs; env = env; vars = vars }

  let meet b1 b2 = 
    let env = b1.env in
    let vars = b1.vars in
    let a1 = Lincons1.array_make env (List.length b1.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a1 !i c; i := !i + 1) b1.constraints;
    let b1 = Abstract1.of_lincons_array manager env a1 in
    let a2 = Lincons1.array_make env (List.length b2.constraints) in
    let j = ref 0 in
    List.iter (fun c -> Lincons1.array_set a2 !j c; j := !j + 1) b2.constraints;
    let b2 = Abstract1.of_lincons_array manager env a2 in
    let b = Abstract1.meet manager b1 b2 in
    let a = Abstract1.to_lincons_array manager b in
    let cs = ref [] in
    for i=0 to (Lincons1.array_length a)-1 do
      cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
    done; { constraints = !cs; env = env; vars = vars }

  (**)

  let fwdAssign b (x,e) = match x with
    | A_var x ->
      let env = b.env in
      let vars = b.vars in
      let e = Texpr1.of_expr env (aExp_to_apron e) in
      let a = Lincons1.array_make env (List.length b.constraints) in
      let i = ref 0 in
      List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) b.constraints;
      let b = Abstract1.of_lincons_array manager env a in
      let b = Abstract1.assign_texpr manager b (Var.of_string x.varId) e None in
      let a = Abstract1.to_lincons_array manager b in
      let cs = ref [] in
      for i=0 to (Lincons1.array_length a)-1 do
        cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
      done; { constraints = !cs; env = env; vars = vars }
    | _ -> raise (Invalid_argument "fwdAssign: unexpected lvalue")


  let bwdAssign_underapprox (t:t) ((x,e): aExp * aExp) : t = match x with
    | A_var x ->
      if not N.supports_underapproximation then
        raise (Invalid_argument "Underapproximation not supported by this abstract domain, use polyhedra instead");
      let env = t.env in
      let vars = t.vars in
      let at = to_apron_t t in
      let top = Abstract1.top manager (Abstract1.env at) in
      let pre = top in (* use top as pre environment *)
      let assignDest = Banal_domain.STRONG (Function_banal_converter.var_to_banal x) in
      let assignValue = Function_banal_converter.of_aExp e in
      let assigned = BanalApron.bwd_assign at () assignDest assignValue pre in
      of_apron_t env vars assigned
    | _ -> raise (Invalid_argument "bwdAssign_underapprox: unexpected lvalue")

  let bwdAssign b (x,e) = match x with
    | A_var x ->
      let env = b.env in
      let vars = b.vars in
      let e = Texpr1.of_expr env (aExp_to_apron e) in
      let a = Lincons1.array_make env (List.length b.constraints) in
      let i = ref 0 in
      List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) b.constraints;
      let b = Abstract1.of_lincons_array manager env a in
      let b = Abstract1.substitute_texpr manager b (Var.of_string x.varId) e None in
      let a = Abstract1.to_lincons_array manager b in
      let cs = ref [] in
      for i=0 to (Lincons1.array_length a)-1 do
        cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
      done; { constraints = !cs; env = env; vars = vars }
    | _ -> raise (Invalid_argument "bwdAssign: unexpected lvalue")


  let filter_underapprox (t:t) (e:bExp) : t = 
    if not N.supports_underapproximation then
      raise (Invalid_argument "Underapproximation not supported by this abstract domain, use octagons or polyhedra instead");
    let env = t.env in
    let vars = t.vars in
    let expr = Function_banal_converter.of_bExp e in
    let at = to_apron_t t in
    let top = Abstract1.top manager (Abstract1.env at) in
    let bot = Abstract1.bottom manager (Abstract1.env at) in
    let pre = top in (* use top as pre environment *)
    let filtered = BanalApron.bwd_filter at bot () expr () pre in
    let result = of_apron_t env vars filtered in 
    result


  let rec filter b e =
    match e with
    | A_TRUE -> b
    | A_MAYBE -> b
    | A_FALSE -> bot b.env b.vars
    | A_bunary (o,e) ->
      (match o with
       | A_NOT -> let (e,_) = negBExp e in filter b e)
    | A_bbinary (o,(e1,_),(e2,_)) ->
      let b1 = filter b e1 and b2 = filter b e2 in
      (match o with
       | A_AND -> meet b1 b2
       | A_OR -> join b1 b2)
    | A_rbinary (o,e1,e2) ->
      let env = b.env in
      let vars = b.vars in
      let a = Lincons1.array_make env (List.length b.constraints) in
      let i = ref 0 in
      List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) b.constraints;
      let b = Abstract1.of_lincons_array manager env a in
      (match o with
       | A_LESS ->
         let e = Texpr1.of_expr env (aExp_to_apron (A_abinary (A_MINUS,e2,e1))) in
         let c = Tcons1.make e Tcons1.SUP in
         let a = Tcons1.array_make env 1 in
         Tcons1.array_set a 0 c;
         let b = Abstract1.meet_tcons_array manager b a in
         let a = Abstract1.to_lincons_array manager b in
         let cs = ref [] in
         for i=0 to (Lincons1.array_length a)-1 do
           cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
         done; { constraints = !cs; env = env; vars = vars }
       | A_LESS_EQUAL ->
         let e = Texpr1.of_expr env (aExp_to_apron (A_abinary (A_MINUS,e2,e1))) in
         let c = Tcons1.make e Tcons1.SUPEQ in
         let a = Tcons1.array_make env 1 in
         Tcons1.array_set a 0 c;
         let b = Abstract1.meet_tcons_array manager b a in
         let a = Abstract1.to_lincons_array manager b in
         let cs = ref [] in
         for i=0 to (Lincons1.array_length a)-1 do
           cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
         done; { constraints = !cs; env = env; vars = vars }
       | A_GREATER ->
         let e = Texpr1.of_expr env (aExp_to_apron (A_abinary (A_MINUS,e1,e2))) in
         let c = Tcons1.make e Tcons1.SUP in
         let a = Tcons1.array_make env 1 in
         Tcons1.array_set a 0 c;
         let b = Abstract1.meet_tcons_array manager b a in
         let a = Abstract1.to_lincons_array manager b in
         let cs = ref [] in
         for i=0 to (Lincons1.array_length a)-1 do
           cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
         done; { constraints = !cs; env = env; vars = vars }
       | A_GREATER_EQUAL ->
         let e = Texpr1.of_expr env (aExp_to_apron (A_abinary (A_MINUS,e1,e2))) in
         let c = Tcons1.make e Tcons1.SUPEQ in
         let a = Tcons1.array_make env 1 in
         Tcons1.array_set a 0 c;
         let b = Abstract1.meet_tcons_array manager b a in
         let a = Abstract1.to_lincons_array manager b in
         let cs = ref [] in
         for i=0 to (Lincons1.array_length a)-1 do
           cs := (Lincons1.array_get a i)::!cs; (*TODO: normalization *)
         done; { constraints = !cs; env = env; vars = vars })

  (**)

  let print fmt b =
    let vars = b.vars in
    let a = Lincons1.array_make b.env (List.length b.constraints) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set a !i c; i := !i + 1) b.constraints;
    let b = Abstract1.of_lincons_array manager b.env a in
    let a = Abstract1.to_lincons_array manager b in
    let cs = ref [] in
    for i=0 to (Lincons1.array_length a)-1 do
      cs := (Lincons1.array_get a i)::!cs;
    done;
    match !cs with
    | [] -> Format.fprintf fmt "top"
    | x::_ ->
      if (C.isBot x) then Format.fprintf fmt "bottom" else
        let i = ref 1 and l = List.length !cs in
        List.iter (fun c ->
            C.print vars fmt c;
            if (!i = l) then () else Format.fprintf fmt " && ";
            i := !i + 1
          ) !cs

end

(** Single partition of the domain of a ranking function 
    represented by the boxes numerical abstract domain. *)
module B = Numerical(
  struct 
    type lib = Box.t
    let manager = Box.manager_alloc ()
    let supports_underapproximation = false
  end)(C)

(** Single partition of the domain of a ranking function 
    represented by the octagons abstract domain. *)
module O = Numerical(
  struct 
    type lib = Oct.t 
    let manager = Oct.manager_alloc () 
    let supports_underapproximation = false
  end)(C)

(** Single partition of the domain of a ranking function 
    represented by the polyhedra abstract domain. *)
module P = Numerical(
  struct 
    type lib = Polka.loose Polka.t
    let manager = Polka.manager_alloc_loose ()
    let supports_underapproximation = true
  end)(C)
