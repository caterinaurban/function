
open Banal_abstract_syntax
open Banal_typed_syntax
open Banal_datatypes

module FAS = AbstractSyntax


let dummy_extent : extent = 
  let dp = Lexing.dummy_pos
  in (dp, dp)

let int_type = A_int (A_INTEGER, A_SIGNED)

let var_to_banal (v:FAS.var) : var = 
  let name = v.FAS.varName in 
  let id = v.FAS.varId in 
  {
    var_name = name;
    var_extent = dummy_extent;
    var_typ = int_type;
    var_id = id;
    var_synthetic = false; 
    var_scope = T_LOCAL;
  }


let rec of_aExp_aux (aExp:FAS.aExp) : expr = 
  match aExp with 
  | FAS.A_var v -> T_var (var_to_banal v)
  | FAS.A_const i -> 
    let i = Banal_intinf.of_base @@ Banal_int.of_int i in
    T_int_const (i,i)
  | FAS.A_interval (i1,i2) ->
    let i1 = Banal_intinf.of_base @@ Banal_int.of_int i1 in
    let i2 = Banal_intinf.of_base @@ Banal_int.of_int i2 in
    T_int_const (i1, i2)
  | FAS.A_aunary (op, (e,_)) ->
    let expr = (of_aExp_aux e, int_type, dummy_extent) in
    let unOp = match op with | FAS.A_UMINUS -> A_UNARY_MINUS in
    T_unary (unOp, expr)
  | FAS.A_abinary (op, (e1,_), (e2,_)) ->
    let expr1 = (of_aExp_aux e1, int_type, dummy_extent) in
    let expr2= (of_aExp_aux e2, int_type, dummy_extent) in
    let binOp = match op with 
      | FAS.A_PLUS -> A_PLUS
      | FAS.A_MINUS -> A_MINUS
      | FAS.A_MULTIPLY -> A_MULTIPLY
      | FAS.A_DIVIDE -> A_DIVIDE
    in T_binary (binOp, expr1, expr2) 
  | FAS.A_RANDOM -> T_int_const (Banal_intinf.minus_inf, Banal_intinf.inf) 
    
let of_aExp (aExp:FAS.aExp) : expr typed = (of_aExp_aux aExp, int_type, dummy_extent)
    
let rec of_bExp_aux (bExp:FAS.bExp) : expr = 
  match bExp with 
  | FAS.A_TRUE -> T_bool_const True
  | FAS.A_MAYBE	-> T_bool_const Maybe
  | FAS.A_FALSE -> T_bool_const False
  | FAS.A_bunary (op, (e,_)) ->
    let expr = (of_bExp_aux e, A_BOOL, dummy_extent) in
    let uOp = match op with | FAS.A_NOT -> A_NOT in
    T_unary (uOp, expr)
  | FAS.A_bbinary (op, (e1,_), (e2,_)) ->
    let expr1 = (of_bExp_aux e1, A_BOOL, dummy_extent) in
    let expr2 = (of_bExp_aux e2, A_BOOL, dummy_extent) in
    let bOp = match op with
      | FAS.A_AND -> A_AND
      | FAS.A_OR -> A_OR in
    T_binary (bOp, expr1, expr2)
  | FAS.A_rbinary (op, (e1,_), (e2,_)) ->
    let expr1 = (of_aExp_aux e1, int_type, dummy_extent) in
    let expr2 = (of_aExp_aux e2, int_type, dummy_extent) in
    let bOp = match op with
      | FAS.A_LESS -> A_LESS
      | FAS.A_LESS_EQUAL -> A_LESS_EQUAL
      | FAS.A_GREATER -> A_GREATER
      | FAS.A_GREATER_EQUAL	-> A_GREATER_EQUAL in 
    T_binary (bOp, expr1, expr2)

let of_bExp (bExp:FAS.bExp) : expr typed = (of_bExp_aux bExp, A_BOOL, dummy_extent)

