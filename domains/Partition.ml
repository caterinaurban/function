(***************************************************)
(*                                                 *)
(*        Ranking Function Domain Partition        *)
(*                                                 *)
(*                  Caterina Urban                 *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Constraints

(** Signature for a single partition of the domain of a ranking function. *)
module type PARTITION = sig
  module C : CONSTRAINT

  type t
  val constraints : t -> C.t list
  val env : t -> Environment.t
  val vars : t -> var list

  val bot : Environment.t -> var list -> t
  val inner : Environment.t -> var list -> C.t list -> t
  val top : Environment.t -> var list -> t

  val isBot : t -> bool
  val isLeq : t -> t -> bool

  val join : t -> t -> t
  val widen : t -> t -> t
  val meet : t -> t -> t

  val fwdAssign : t -> aExp * aExp -> t
  val bwdAssign : t -> aExp * aExp -> t
  val bwdAssign_underapprox : t -> aExp * aExp -> t
  val filter : t -> bExp -> t
  val filter_underapprox : t -> bExp -> t

  val print : Format.formatter -> t -> unit

end
