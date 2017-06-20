(*   
   Abstract sytax tree output by the parser.

   Copyright (C) 2011 Antoine Miné
*)

open Lexing
open Banal_datatypes


(************************************************************************)
(* TYPES *)
(************************************************************************)

(* position in the source file *)
type position = Lexing.position
type extent = position * position (* start/end *)

(* tree nodes are tagged with a source position *)
type 'a ext = 'a * extent

type int_type =
  | A_CHAR     (* 8-bit *)
  | A_SHORT    (* 16-bit *)
  | A_INT      (* 32-bit *)
  | A_LONG     (* 64-bit *)
  | A_INTEGER  (* arbitrary precision *)

type int_sign =
  | A_SIGNED | A_UNSIGNED

type float_type = 
  | A_FLOAT | A_DOUBLE | A_REAL

type typ =
  | A_int of int_type * int_sign
  | A_float of float_type
  | A_BOOL

type unary_op = 
  | A_UNARY_PLUS
  | A_UNARY_MINUS
  | A_NOT
  | A_cast of typ ext

and binary_op =
  | A_PLUS
  | A_MINUS
  | A_MULTIPLY
  | A_DIVIDE
  | A_MODULO
  | A_EQUAL
  | A_NOT_EQUAL
  | A_LESS
  | A_LESS_EQUAL
  | A_GREATER
  | A_GREATER_EQUAL
  | A_AND
  | A_OR

and incr =
  | A_INCR
  | A_DECR

and prepost =
  | A_PRE
  | A_POST

and binary_assign_op =
  | A_PLUS_ASSIGN
  | A_MINUS_ASSIGN
  | A_MULTIPLY_ASSIGN
  | A_DIVIDE_ASSIGN
  | A_MODULO_ASSIGN

and expr =
  | A_unary of unary_op * (expr ext)
  | A_binary of binary_op * (expr ext) * (expr ext)
  | A_assign of (lvalue ext) * (binary_assign_op option) * (expr ext)
  | A_increment of (lvalue ext) * incr * prepost
  | A_call of (string ext) * (expr ext list)
  | A_identifier of string
  | A_float_const of string
  | A_int_const of string
  | A_bool_const of bool
  | A_float_itv of (string ext) * (string ext)
  | A_int_itv of (string ext) * (string ext)

and stat =
  | A_SKIP
  | A_BREAK
  | A_expr of expr ext
  | A_if of (expr ext) * (stat ext) (* then *) * (stat ext option) (* else *)
  | A_while of (expr ext) * (stat ext) (* body *)
  | A_return of expr ext option
  | A_block of stat ext list
  | A_local of vardecl
  | A_label of string ext
  | A_assert of expr ext
  | A_assume of expr ext
  | A_print of (lvalue ext) list

and lvalue = string

and decl =
  | A_global of (vardecl ext) * global_kind
  | A_function of fundecl ext

and global_kind = A_VARIABLE | A_INPUT | A_VOLATILE

and vardecl = (typ ext) * (((string ext) * (expr ext option)) list)

and fundecl = 
  (typ ext option) * 
  (string ext) * 
  ((string ext) * (typ ext)) list *
  (stat ext list)


(************************************************************************)
(* UTILITIES *)
(************************************************************************)

let position_unknown = Lexing.dummy_pos
let extent_unknown = (position_unknown, position_unknown)

let cmp_neg = function
  | A_EQUAL -> A_NOT_EQUAL
  | A_NOT_EQUAL -> A_EQUAL
  | A_LESS -> A_GREATER_EQUAL
  | A_GREATER -> A_LESS_EQUAL
  | A_LESS_EQUAL -> A_GREATER
  | A_GREATER_EQUAL -> A_LESS
  | _ -> invalid_arg "not a comparison operator"

(* order in increasing filename, and the position in file *)
let compare_position p1 p2 =
  match compare p1.pos_fname p2.pos_fname with
  | 0 -> compare p1.pos_cnum p2.pos_cnum
  | _ -> compare p1.pos_fname p2.pos_fname

(* order in increasing start position, and then in reverse ending position *)
let compare_extent (b1,e1) (b2,e2) =
  match compare_position b1 b2 with
  | 0 -> compare_position e2 e1
  | x -> x


(************************************************************************)
(* PRINTERS *)
(************************************************************************)

let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
    
let string_of_extent (p,q) =
  if p.pos_fname = q.pos_fname then
    if p.pos_lnum = q.pos_lnum then
      if p.pos_cnum = q.pos_cnum then
        Printf.sprintf "%s:%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
      else
        Printf.sprintf "%s:%i.%i-%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) (q.pos_cnum - q.pos_bol)
    else
      Printf.sprintf "%s:%i.%i-%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) q.pos_lnum (q.pos_cnum - q.pos_bol)
  else
    Printf.sprintf "%s:%i.%i-%s:%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) q.pos_fname q.pos_lnum (q.pos_cnum - q.pos_bol)


let string_of_typ = function
  | A_BOOL -> "bool"
  | A_float A_FLOAT -> "float"
  | A_float A_DOUBLE -> "double"
  | A_float A_REAL -> "real"
  | A_int (k,s) ->
      (if s = A_UNSIGNED then "unsigned " else "") ^
      (match k with
      | A_CHAR -> "char"
      | A_SHORT -> "short"
      | A_INT -> "int"
      | A_LONG -> "long"
      | A_INTEGER -> "integer"
      )
