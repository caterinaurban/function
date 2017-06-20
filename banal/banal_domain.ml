(*   
   Abstract domain signature.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_abstract_syntax
open Banal_typed_syntax

type assign_dst =
  | STRONG of var
  | WEAK of var list

type error = unit

type merge_type =
  | MERGE 
  | WIDEN

type filter_type =
  | IFTHENELSE
  | LOOP

module type ABSTRACT_DOMAIN = sig


  (* types *)
  (* ***** *)

  type t


  (* management *)
  (* ********** *)

  (* empty element, with no variable *)
  val empty: unit -> t

  (* creates a _|_ element on the same variable set as its argument *)
  val bot: t -> t

  (* creates a top element on the same variable set as its argument *)
  val top: t -> t

  (* queries *)
  (* ******* *)

  (* true is definite, false means maybe *)

  val is_bot: t -> bool

  val subseteq: t -> t -> bool

  val print: Format.formatter -> t -> unit

  val print_svg: out_channel -> ?color:bool -> ?window:float*float*float*float -> int*int -> t -> var*var -> unit
  val print_html: out_channel -> t -> unit
  val print_latex: out_channel -> t -> unit


  (* forward operators *)
  (* ***************** *)

  (* take pre as argument, return overapproximated post *)

  val fwd_add_var: t -> var -> t

  val fwd_del_var: t -> var -> t

  val fwd_join: t -> t -> merge_type -> t

  val fwd_assign: t -> assign_dst -> (expr typed) -> t * error

  (* returns first an environment where the test holds, then an environment
     where it does not
   *)
  val fwd_filter: t -> (expr typed) -> t * t * error


  (* backward operators *)
  (* ****************** *)

  (* takes refined post and pre as arguments, return underapproximated pre *)

  val bwd_add_var: t -> var -> t -> t

  val bwd_del_var: t -> var -> t -> t

  val bwd_meet: t -> t -> merge_type -> t

  val bwd_assign: t -> error -> assign_dst -> (expr typed) -> t -> t

  val bwd_filter: t -> t -> error -> (expr typed) -> filter_type -> t -> t


end

(* no error management yet *)
let join_err err1 err2 = ()
let noerr = ()
