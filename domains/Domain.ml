(***************************************************)
(*                                                 *)
(*      The Ranking Functions Abstract Domain      *)
(*                                                 *)
(*                 Caterina Urban                  *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*          ETH Zurich, Zurich, Switzerland        *)
(*                      2016                       *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Partition
open Functions

module type RANKING_FUNCTION =
sig

  module B : PARTITION

  type t

  val bot : ?domain:B.t -> Environment.t -> var list -> t
  val zero : ?domain:B.t -> Environment.t -> var list -> t
  val top : ?domain:B.t -> Environment.t -> var list -> t

  val isLeq : kind -> t -> t -> bool
  val join : kind -> t -> t -> t
  val meet : kind -> t -> t -> t
  val widen : ?jokers:int -> t -> t -> t
  val dual_widen : t -> t -> t

  val terminating : t -> bool
  val bwdAssign : ?domain:B.t -> ?underapprox:bool -> t -> aExp * aExp -> t
  val filter : ?domain:B.t -> ?underapprox:bool -> t -> bExp -> t
  val reset : ?mask:t -> t -> bExp -> t
  val refine : t -> B.t -> t

  val compress : t -> t
  val print : Format.formatter -> t -> unit

  val print_graphviz_dot : Format.formatter -> t -> unit

  val left_narrow: t -> t -> t
  val reset_until: t -> t -> t -> t

  val complement: t -> t

end
