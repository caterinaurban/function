(*   
             ********* Intermediate Syntax ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

open Lexing

type position = Lexing.position
type extent = position * position  (* start/end *)
type 'a annotated = 'a * extent

let position_tostring p = Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

type typ =
  | I_INT

type unaryOp =
  | I_PLUS  	(* + *)
  | I_MINUS	(* - *)

type assignOp =
  | I_EQUAL     		(* = *)
  | I_PLUS_EQUAL      (* += *)
  | I_MINUS_EQUAL     (* -= *)
  | I_MULTIPLY_EQUAL  (* *= *)
  | I_DIVIDE_EQUAL    (* /= *)
  | I_MODULO_EQUAL    (* %= *)

type exp =
  | I_TRUE (* primary_exp *)
  | I_RANDOM (* primary_exp *)
  | I_FALSE (* primary_exp *)
  | I_id (* primary_exp *) of string
  | I_const (* primary_exp *) of string
  | I_interval (* primary_exp *) of (unaryOp option) * string * (unaryOp option) * string
  (**)
  | I_incr (* postfix_exp *) of (exp (* primary_exp/postfix_exp *) annotated)
  | I_decr (* postfix_exp *) of (exp (* primary_exp/postfix_exp *) annotated)
  | I_call (* postfix_exp *) of (exp (* primary_exp/postfix_exp *) annotated) * (exp annotated list)
  (**)
  | I_preincr (* unary_exp *) of (exp (* postfix_exp/unary_exp *) annotated)
  | I_predecr (* unary_exp *) of (exp (* postfix_exp/unary_exp *) annotated)
  | I_not (* unary_exp *) of (exp (* postfix_exp/unary_exp *) annotated)
  | I_unary (* unary_exp *) of unaryOp * (exp (* postfix_exp/unary_exp *) annotated)
  (**)
  | I_mul (* mul_exp *) of (exp (* unary_exp/mul_exp *) annotated) * (exp (* unary_exp *) annotated)
  | I_div (* mul_exp *) of (exp (* unary_exp/mul_exp *) annotated) * (exp (* unary_exp *) annotated)
  | I_mod (* mul_exp *) of (exp (* unary_exp/mul_exp *) annotated) * (exp (* unary_exp *) annotated)
  (**)
  | I_add (* add_exp *) of (exp (* mul_exp/add_exp *) annotated) * (exp (* mul_exp *) annotated)
  | I_minus (* add_exp *) of (exp (* mul_exp/add_exp *) annotated) * (exp (* mul_exp *) annotated)
  (**)
  | I_less (* relational_exp *) of (exp (* add_exp/relational_exp *) annotated) * (exp (* add_exp *) annotated)
  | I_leq (* relational_exp *) of (exp (* add_exp/relational_exp *) annotated) * (exp (* add_exp *) annotated)
  | I_greater (* relational_exp *) of (exp (* add_exp/relational_exp *) annotated) * (exp (* add_exp *) annotated)
  | I_geq (* relational_exp *) of (exp (* add_exp/relational_exp *) annotated) * (exp (* add_exp *) annotated)
  (**)
  | I_eq (* equality_exp *) of (exp (* relational_exp/equality_exp *) annotated) * (exp (* relational_exp *) annotated)
  | I_neq (* equality_exp *) of (exp (* relational_exp/equality_exp *) annotated) * (exp (* relational_exp *) annotated)
  (**)
  | I_and (* logical_and_exp *) of (exp (* equality_exp/logical_and_exp *) annotated) * (exp (* equality_exp *) annotated)
  (**)
  | I_or (* logical_or_exp *) of (exp (* logical_and_exp/logical_or_exp *) annotated) * (exp (* logical_and_exp *) annotated)
  (**)
  | I_assign of (exp (* unary_exp *) annotated) * assignOp * (exp annotated)

type property =
  | I_universal of (exp annotated)
  | I_particular of string * (exp annotated)

type stmt =
  | I_label of (string annotated)
  | I_SKIP
  | I_exp of (exp annotated)
  | I_assert of (exp annotated)
  | I_if of (exp annotated) * (stmt annotated) * (stmt annotated option)
  | I_while of (exp annotated) * (stmt annotated)
  | I_for_simple of (exp annotated) * (exp annotated) * (exp annotated) * (stmt annotated)
  | I_for of (globalDecl annotated) * (exp annotated) * (exp annotated) * (stmt annotated)
  | I_return of (exp annotated option)
  | I_local of (globalDecl annotated)
  | I_block of (stmt annotated) list

and decl =
  | I_global of globalDecl annotated
  | I_function of functionDecl annotated

and globalDecl = (typ annotated) * (declarator list)

and functionDecl = (typ annotated option) * (string annotated) * ((typ annotated) * (string annotated)) list * (stmt annotated) list

and declarator = (string annotated) * (exp annotated option)

type prog = (decl list) annotated
