(*   
   Semantic definitions and utilities

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_abstract_syntax
open Banal_typed_syntax

module Int = Banal_int
module Float = Banal_float
module Itv_int = Banal_itv_int
module Itv_float = Banal_itv_float


(* value domain of each type *)

let signed_set bsize =
  Finite (Int.neg (Int.shift_left Int.one (bsize-1))), 
  Finite (Int.pred (Int.shift_left Int.one (bsize-1)))

let unsigned_set bsize =
  Finite Int.zero, Finite (Int.pred (Int.shift_left Int.one bsize))

let int_type_set (t:int_type) (s:int_sign) : Itv_int.t = 
  match t,s with
  | A_CHAR,    A_SIGNED   -> signed_set    8
  | A_CHAR,    A_UNSIGNED -> unsigned_set  8
  | A_SHORT,   A_SIGNED   -> signed_set   16
  | A_SHORT,   A_UNSIGNED -> unsigned_set 16
  | A_INT,     A_SIGNED   -> signed_set   32
  | A_INT,     A_UNSIGNED -> unsigned_set 32
  | A_LONG,    A_SIGNED   -> signed_set   64
  | A_LONG,    A_UNSIGNED -> unsigned_set 64
  | A_INTEGER, A_SIGNED   -> MINF, INF
  | A_INTEGER, A_UNSIGNED -> Finite Int.zero, INF
        

let float_type_set (t:float_type) : Itv_float.t = 
  match t with
  | A_FLOAT  -> -.Float.Single.max_normal, Float.Single.max_normal
  | A_DOUBLE -> -.Float.Double.max_normal, Float.Double.max_normal
  | A_REAL   -> neg_infinity, infinity


let bool_type_set = Maybe


let type_set_expr (t:typ) : expr =
  match t with
  | A_int (i,s) -> T_int_const (int_type_set i s)
  | A_float f -> T_float_const (float_type_set f)
  | A_BOOL -> T_bool_const Maybe
