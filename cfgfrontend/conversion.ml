open Apron
open AbstractSyntax
open ControlFlowGraph
open AbstractSyntaxTree

(* Some utility functions to convert from new Cfg expressions to old AbstractSyntax expressions
   NOTE: this quite a mess, ideally one would unify the two syntaxes but this is a lot of work
*)

let varId_of_var (var: ControlFlowGraph.var) = "$" ^ string_of_int var.var_id

let of_var (var: ControlFlowGraph.var): AbstractSyntax.var =
    {
      AbstractSyntax.varId = varId_of_var var;
      AbstractSyntax.varName = var.var_name;
      AbstractSyntax.varTyp  = AbstractSyntax.A_INT; (* only int for now *)
    }

let env_vars_of_cfg (cfg:cfg) =
  let var_to_apron v = Apron.Var.of_string (varId_of_var v) in
  let apron_vars = Array.map var_to_apron (Array.of_list cfg.cfg_vars) in
  let env = Environment.make apron_vars [||] in
  let vars = List.map of_var cfg.cfg_vars in
  (env, vars)

   
let of_binary_op (op:int_binary_op): AbstractSyntax.aBinOp = match op with
  | AST_PLUS -> A_PLUS
  | AST_MINUS -> A_MINUS
  | AST_MULTIPLY -> A_MULTIPLY
  | AST_DIVIDE -> A_DIVIDE
  | AST_MODULO -> raise (Invalid_argument "modulo not supported")


let rec of_int_expr expr = match expr with 
  | CFG_int_unary (AST_UNARY_PLUS, e) -> of_int_expr e 
  | CFG_int_unary (AST_UNARY_MINUS, e) -> 
    A_aunary (A_UMINUS, annotate @@ of_int_expr e) 
  | CFG_int_binary (AST_MODULO, e1, e2) -> A_RANDOM
  | CFG_int_binary (op, e1, e2) -> 
    A_abinary (
      of_binary_op op, 
      annotate @@ of_int_expr e1,
      annotate @@ of_int_expr e2
    )
  | CFG_int_var var -> A_var (of_var var)
  | CFG_int_const c -> A_const (Z.to_int c)
  | CFG_int_random -> A_RANDOM
  | CFG_int_interval (l, u) -> A_interval (Z.to_int l, Z.to_int u)
  | CFG_arr_elm (var,idx) ->
    raise (Invalid_argument "array element is not yet supported")


let rec of_bool_expr (expr:ControlFlowGraph.bool_expr): bExp = match expr with
  | CFG_bool_unary (AST_NOT, e) -> 
    A_bunary (A_NOT, annotate @@ of_bool_expr e)
  | CFG_bool_binary (AST_AND, e1, e2) -> 
    A_bbinary (A_AND, annotate @@ of_bool_expr e1, annotate @@ of_bool_expr e2)
  | CFG_bool_binary (AST_OR, e1, e2) ->
    A_bbinary (A_OR, annotate @@ of_bool_expr e1, annotate @@ of_bool_expr e2)
  | CFG_compare (AST_LESS_EQUAL, e1, e2) -> 
    A_rbinary (A_LESS_EQUAL, annotate @@ of_int_expr e1, annotate @@ of_int_expr e2)
  | CFG_compare (AST_LESS, e1, e2) -> 
    A_rbinary (A_LESS, annotate @@ of_int_expr e1, annotate @@ of_int_expr e2)
  | CFG_compare (AST_GREATER, e1, e2) -> 
    A_rbinary (A_GREATER, annotate @@ of_int_expr e1, annotate @@ of_int_expr e2)
  | CFG_compare (AST_GREATER_EQUAL, e1, e2) -> 
    A_rbinary (A_GREATER_EQUAL, annotate @@ of_int_expr e1, annotate @@ of_int_expr e2)
  | CFG_compare (AST_EQUAL, e1, e2) ->
    let leq = CFG_compare (AST_LESS_EQUAL, e1, e2) in
    let geq = CFG_compare (AST_GREATER_EQUAL, e1, e2) in
    of_bool_expr (CFG_bool_binary (AST_AND, leq, geq))
  | CFG_compare (AST_NOT_EQUAL, e1, e2) ->
    let leq = CFG_compare (AST_LESS, e1, e2) in
    let geq = CFG_compare (AST_GREATER, e1, e2) in
    of_bool_expr (CFG_bool_binary (AST_OR, leq, geq))
  | CFG_bool_const true -> A_TRUE
  | CFG_bool_const false -> A_FALSE
  | CFG_bool_rand -> A_MAYBE
