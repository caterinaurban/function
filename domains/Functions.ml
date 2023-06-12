(* ********* Ranking Functions Abstract Domain ************ Copyright (C)
   2012-2014 by Caterina Urban. All rights reserved. *)

open AbstractSyntax
open Apron
open Partition

type kind = APPROXIMATION | COMPUTATIONAL | LEARNING

module type FUNCTION = sig
  module B : PARTITION

  type f

  val env : f -> Environment.t

  val vars : f -> var list

  val bot : Environment.t -> var list -> f

  val zero : Environment.t -> var list -> f

  val top : Environment.t -> var list -> f

  val isBot : f -> bool

  val defined : f -> bool

  val isTop : f -> bool

  val isEq : B.t -> f -> f -> bool

  (* returns the domain where the two functions are equal *)
  val domainEq : B.t -> f -> f -> B.t

  val isLeq : kind -> B.t -> f -> f -> bool

  val join : kind -> B.t -> f -> f -> f

  val learn : B.t -> f -> f -> f (* conflict-driven analysis *)

  val widen : ?jokers:int -> B.t -> f -> f -> f

  val extend : B.t -> B.t -> f -> f -> f

  val reinit : f -> f

  val reset : f -> f

  val predecessor : f -> f

  val successor : f -> f

  val bwdAssign : f -> aExp * aExp -> f

  val filter : f -> bExp -> f

  val print : Format.formatter -> f -> unit
end
