(*   
   Linearization functions.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_mathtypes
open Banal_abstract_syntax
open Banal_typed_syntax
open Banal_semantics
open Banal_domain

module Affine = Banal_affine


module Var = struct
  type t = var
  let compare x y = compare x.var_id y.var_id
  let to_string v = v.var_name
end


module Make(I : INTERVAL) = struct

  module A = Affine.Make(Var)(I)
  module I = I
  module B = I.Bound
  module VMap = A.VMap

  module D = Float.Double
  module S = Float.Single

  type env = var -> I.t

  type linexpr = A.t

  (* constraints have the for "expr op 0";
     we also remember whether the constraint is integer, for subsequent 
     negations
   *)
  type cons_op =
    | L_EQ   (* =  0 *)
    | L_NEQ  (* != 0 *)
    | L_GEQ  (* >= 0 *)
    | L_GT   (* >  0 *)
    | L_EQ_INT
    | L_NEQ_INT
    | L_GEQ_INT
  type lincons = linexpr * cons_op



  (* UTILITIES *)
  (* ********* *)


  (* interval for a given type *)
  let type_itv (t:typ) : I.t =
    match t with
    | A_int (i,s) -> let l,h = int_type_set i s in I.of_intinf l h
    | A_float f -> let l,h = float_type_set f in I.of_floats l h
    | A_BOOL -> I.zero_one
          

  (* floating-point rounding, parameterized by type precision *)
  let round_float (min:float) (ulp:float) (l:linexpr) : linexpr =
    let rel = A.mul_cst (A.symmetric l) (I.of_float ulp)
    and abs = I.of_floats (-. min) min in
    A.add l (A.add_cst rel abs)

                    
  (* effect of cast and rounding *)
  (* TODO: bound-check *)
  let cast_expr (env:env) (src:typ) (dst:typ) (l:linexpr) : linexpr =
    match src,dst with
      
    (* float -> int (truncation) *)
    | A_float _, (A_int _ | A_BOOL) ->
        let rl,rh = A.eval env l in
        let err =
          (* adds [0;1], [-1;0] or [-1;1] depending on the sign of r *)
          if B.sign rl >= 0 then I.minus_one_zero else
          if B.sign rh <= 0 then I.zero_one else
          I.minus_one_one
        in
        A.add_cst l err
          
    (* float -> float (rounding) *)
          
    | A_float A_REAL, A_float A_DOUBLE ->
        round_float D.min_denormal D.ulp l
          
    | A_float (A_REAL | A_DOUBLE), A_float A_FLOAT ->
        round_float S.min_denormal S.ulp l
          
    (* int -> float (identity or rounding) *)

    | A_int _, A_float A_DOUBLE ->
        if B.leq (I.magnitude (A.eval env l)) (B.of_float_down D.max_exact)
        then l (* exact *)
        else round_float D.min_denormal D.ulp l
            
    | A_int _, A_float A_FLOAT ->
        if B.leq (I.magnitude (A.eval env l)) (B.of_float_down S.max_exact)
        then l (* exact *)
        else round_float S.min_denormal S.ulp l
            
    (* others: identity *)

    | _ -> l




  (* expression linearization *)
  let rec lin_expr (env:env) ((e,t,x):expr typed) : linexpr =
    (* natural type of the result, before cast *)
    let tr = match t with
    | A_int _ | A_BOOL -> A_int (A_INTEGER, A_SIGNED)
    | A_float _ -> A_float A_REAL
    in
    match e with
      
    | T_unary (op,((_,t1,_) as e1)) ->
        let l = lin_expr env e1 in
        (match op with
          
        | A_UNARY_PLUS -> l
              
        | A_UNARY_MINUS -> cast_expr env tr t (A.neg l)
              
        | A_NOT -> invalid_arg "boolean node in Linearization.lin_expr"
              
        | A_cast _ -> cast_expr env t1 t l
        )
          
    | T_binary (op,e1,e2) ->
        let l1, l2 = lin_expr env e1, lin_expr env e2 in
        (match op with
          
        | A_PLUS -> cast_expr env tr t (A.add l1 l2)
              
        | A_MINUS -> cast_expr env tr t (A.sub l1 l2)
              
        | A_MULTIPLY -> cast_expr env tr t (A.mul env l1 l2)
              
        | A_DIVIDE -> 
            (* division creates reals from ints, hence the need for a
               cast from real
             *)
            (try cast_expr env (A_float A_REAL) t (A.div env l1 l2)
            with Division_by_zero -> A.top)
              
        | A_MODULO ->
            let (m1,h1), i2 = A.eval env l1, A.eval env l2 in
            let (m2,h2) = I.abs i2 in
            (* modulo by zero *)
            if B.sign m2 = 0 then A.top else
            (* first arg fits in modulo interval *)
            if B.gt m1 (B.neg m2) && B.lt h1 m2 then l1 else
            (* does not fit => whole modulo interval *)
            let r = B.sub_up h2 (B.one) in
            if B.sign m1 >= 0 then A.cst (B.zero, r) else
            if B.sign h1 <= 0 then A.cst (r, B.zero) else
            A.cst (B.neg r, r)
              
        | A_EQUAL | A_NOT_EQUAL 
        | A_LESS | A_LESS_EQUAL | A_GREATER | A_GREATER_EQUAL
        | A_AND | A_OR ->
            (* boolean *)
            A.cst I.zero_one
        )
          
    | T_float_const (l,h) -> A.cst (I.of_floats l h)
          
    | T_int_const (l,h) -> A.cst (I.of_intinf l h)

    | T_bool_const False -> A.zero

    | T_bool_const True -> A.one

    | T_bool_const Maybe -> A.cst I.zero_one

    | T_var v -> 
        (* volatiles are replaced with their bound *)
        if v.var_scope = T_VOLATILE then A.cst (env v) else A.var v


  let is_integer (_,t,_) =
    match t with
    | A_int _ | A_BOOL -> true
    | A_float _ -> false


  (* constraint linearization 
     returns both the true and false conditions
  *)
  let rec lin_cons (env:env) ((e,t,x):expr typed) : lincons =
    match e with
    | T_binary (op,e1,e2) ->
        let l1, l2 = lin_expr env e1, lin_expr env e2 in
        (match op, is_integer e1 with
        | A_EQUAL, false -> A.sub l1 l2, L_EQ
        | A_NOT_EQUAL, false -> A.sub l1 l2, L_NEQ
        | A_LESS, false -> A.sub l2 l1, L_GT
        | A_LESS_EQUAL, false -> A.sub l2 l1, L_GEQ
        | A_GREATER, false -> A.sub l1 l2, L_GT
        | A_GREATER_EQUAL, false -> A.sub l1 l2, L_GEQ

        (* integer inequalities: x > 0 => x-1 >= 0 *)
        | A_EQUAL, true -> A.sub l1 l2, L_EQ_INT
        | A_NOT_EQUAL, true -> A.sub l1 l2, L_NEQ_INT
        | A_LESS, true -> A.sub_cst (A.sub l2 l1) I.one, L_GEQ_INT
        | A_LESS_EQUAL, true ->  A.sub l2 l1, L_GEQ_INT
        | A_GREATER, true -> A.sub_cst (A.sub l1 l2) I.one, L_GEQ_INT
        | A_GREATER_EQUAL, true -> A.sub l1 l2, L_GEQ_INT

        | _ -> invalid_arg "non-comparison node in Linearization.lin_cons"
        )
    | _ -> invalid_arg "non-comparison node in Linearization.lin_cons"



  (* MAIN FUNCTIONS *)
  (* ************** *)


  (* quasi = false: interval variable coefficients allowed
     quasi = true: only scalar variable coefficients allowed
   *)

  let linearize_expr (quasi:bool) (env:env) (e:expr typed) : linexpr =
    let l = lin_expr env e in
    let l = if quasi then A.quasilinearize env l else l in
    A.simplify l


  let linearize_cons (quasi:bool) (env:env) (e:expr typed) : lincons =
    let l,o = lin_cons env e in
    let l = if quasi then A.quasilinearize env l else l in
    A.simplify l, o


  (* inverts the assignment, which should be quasi-linear, with non-nul
     coefficient for v;
     the result can be made quasi-linear
   *)
  let invert (quasi:bool) (env:env) (l:linexpr) (v:var) : linexpr =
    let c = A.get_var v l in
    assert(not (I.equal I.zero c));
    let l = A.div_cst (A.set_var v I.one (A.neg l)) c in
    if quasi then A.quasilinearize env l else l


  let expr_to_string = A.to_string

  let cons_to_string (l,op) =
    let s = expr_to_string l in 
    match op with
    | L_EQ | L_EQ_INT -> s^" = 0"
    | L_NEQ | L_NEQ_INT -> s^" != 0"
    | L_GEQ | L_GEQ_INT -> s^" >= 0"
    | L_GT -> s^" > 0"

  (* negate the cosntraint *)
  let neg_cons ((l,op):lincons) : lincons =
    match op with
    | L_NEQ -> l, L_EQ
    | L_EQ -> l, L_NEQ
    | L_GEQ -> A.neg l, L_GT
    | L_GT -> A.neg l, L_GEQ
    | L_NEQ_INT -> l, L_EQ_INT
    | L_EQ_INT -> l, L_NEQ_INT
    | L_GEQ_INT ->  A.sub_cst (A.neg l) I.one, L_GEQ_INT


  (* split equality or disequality into two inequality *)
  let split_cons ((l,op):lincons) : lincons * lincons =
    match op with
    | L_EQ -> (l,L_GEQ), (A.neg l,L_GEQ)
    | L_EQ_INT -> (l,L_GEQ_INT), (A.neg l,L_GEQ_INT)
    | L_NEQ -> (l,L_GT), (A.neg l,L_GT)
    | L_NEQ_INT -> 
        (A.sub_cst l I.one, L_GEQ_INT), 
        (A.sub_cst (A.neg l) I.one, L_GEQ_INT)
    | _ -> invalid_arg "split_cons: not an equality or disequality constraint"


  let project_expr (l:linexpr) (vl:var list) : linexpr =
    A.project l vl

  let project_cons ((l,op):lincons) (vl:var list) : lincons =
    A.project l vl, op

end
