(*   
   Processed syntax tree:
   - variable scoping is resolved
   - trees are typed
   - casts are explicit
   - expressions are side-effect free
   - statements are labelled with unique identifiers

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_abstract_syntax

module Float = Banal_float
module Intinf = Banal_intinf


(************************************************************************)
(* TYPES *)
(************************************************************************)

(* constant sets *)
type int_set = Intinf.t * Intinf.t (* interval *)
type float_set = Float.t * Float.t (* interval *)
type bool_set = tbool (* 3-valued logic *)


(* expression nodes have type and source location *)
type 'a typed = 'a * typ * extent

(* side-effect free, typed expressions *)
type expr =
  | T_unary of unary_op * (expr typed)
  | T_binary of binary_op * (expr typed) * (expr typed)
  | T_float_const of float_set
  | T_int_const of int_set
  | T_bool_const of bool_set
  | T_var of var

(* variables *)
and var = {
    var_name: string;
    var_extent: extent;
    var_typ: typ;
    (* var_id: id; *)
    var_id: string;
    var_synthetic: bool; (* added by translation? *)
    var_scope: var_scope;
  }

and var_scope =
  | T_GLOBAL
  | T_LOCAL
  | T_INPUT
  | T_VOLATILE

(* statements *)
type stat =
  | T_expr of expr typed
  | T_assign of (var ext) * (expr typed)
  | T_call of func ext (* arguments and return values as local variables *)
  | T_if of (expr typed) * block * block
  | T_while of  label (* loop-invariant label *) * (expr typed) * block
  | T_add_var of var * (expr typed option)
  | T_del_var of var
  | T_RETURN (* returned value in func_return *)
  | T_BREAK
  | T_assert of expr typed
  | T_assume of expr typed
  | T_print of (var ext) list
  | T_label of string ext

and block = 
  | T_empty of label
  | T_stat of label * (stat ext) * block

and label = id * position

(* functions *)

and func = {
    func_name: string;
    func_extent: extent;
    func_id: id;
    func_return: var option;
    func_args: var list;
    func_body: block;
  }


(* whole program *)

type prog =
    block *               (* global variable creation and initialization *)
    (func StringMap.t) *  (* function declarations *)
    (var IdMap.t)         (* variables *)


(************************************************************************)
(* PRINTERS *)
(************************************************************************)


let string_of_unary_op = function
  | A_UNARY_PLUS -> "+"
  | A_UNARY_MINUS -> "-"
  | A_NOT -> "!"
  | A_cast (t,_) -> "("^(string_of_typ t)^")"

let string_of_binary_op = function
  | A_MULTIPLY -> "*"
  | A_DIVIDE -> "/"
  | A_MODULO -> "%"
  | A_PLUS -> "+"
  | A_MINUS -> "-"
  | A_EQUAL -> "=="
  | A_NOT_EQUAL -> "!="
  | A_LESS -> "<"
  | A_LESS_EQUAL -> "<="
  | A_GREATER -> ">"
  | A_GREATER_EQUAL -> ">="
  | A_AND -> "&&"
  | A_OR -> "||"


let binary_precedence = function
  | A_MULTIPLY | A_DIVIDE | A_MODULO -> 6
  | A_PLUS  | A_MINUS -> 5
  | A_EQUAL | A_NOT_EQUAL -> 4
  | A_LESS | A_LESS_EQUAL | A_GREATER | A_GREATER_EQUAL -> 3
  | A_AND -> 2
  | A_OR -> 1

let expr_precedence (e,_,_) = 
  match e with
  | T_unary (op, _) -> 99
  | T_binary(op, _, _) -> binary_precedence op
  | _ -> 100


let print_var_name fmt v =
(*  Format.fprintf fmt "%s#%s" v.var_name (string_of_id "" v.var_id)*)
  Format.fprintf fmt "%s" v.var_name

let print_func_name fmt f =
(*  Format.fprintf fmt "%s#%s" f.func_name (string_of_id "" f.func_id)*)
  Format.fprintf fmt "%s" f.func_name


let print_list f sep fmt l =
  let rec aux = function
    | [] -> ()
    | [a] -> f fmt a
    | a::b -> f fmt a; Format.pp_print_string fmt sep; aux b
  in
  aux l


let print_label fmt (l,p) =
  Format.fprintf fmt "{%s}" (string_of_id "" l)

let string_of_int_set (a,b) =
  if a=b then Intinf.to_string a 
  else Printf.sprintf "[%a;%a]" Intinf.sprint a Intinf.sprint b

let string_of_float_set (a,b) =
  if a=b then Float.to_string a 
  else Printf.sprintf "[%a;%a]" Float.sprint a Float.sprint b


let rec print_expr fmt ((ee,_,_) as e) = 
  match ee with

  | T_unary (op,e1) ->
      Format.pp_print_string fmt (string_of_unary_op op);
      if expr_precedence e1 <= expr_precedence e
      then Format.fprintf fmt " (%a)" print_expr e1
      else Format.fprintf fmt " %a" print_expr e1

  | T_binary (op,e1,e2) ->
      if expr_precedence e1 < expr_precedence e
      then Format.fprintf fmt "(%a) " print_expr e1
      else Format.fprintf fmt "%a " print_expr e1;
      Format.pp_print_string fmt (string_of_binary_op op);
      if expr_precedence e2 <= expr_precedence e
      then Format.fprintf fmt " (%a)" print_expr e2
      else Format.fprintf fmt " %a" print_expr e2
          
  | T_float_const f -> Format.pp_print_string fmt (string_of_float_set f)
        
  | T_int_const i -> Format.pp_print_string fmt (string_of_int_set i)
        
  | T_bool_const b -> Format.pp_print_string fmt (string_of_tbool b)
        
  | T_var v -> print_var_name fmt v


let rec print_stat ind fmt s = 
  match s with

  | T_expr e -> print_expr fmt e

  | T_assign ((v,_),e) ->
      Format.fprintf fmt "%a = %a" print_var_name v print_expr e

  | T_call (f,fx) ->
      (match f.func_return with
      | Some v -> Format.fprintf fmt "%a = " print_var_name v
      | None -> ());
      Format.fprintf fmt "%a(" print_func_name f;
      print_list print_var_name "," fmt f.func_args;
      Format.pp_print_string fmt ")"
 
  | T_if (e, b1, b2) ->
      Format.fprintf fmt "if (%a)@\n" print_expr e;
      print_block ind fmt b1;
      Format.fprintf fmt "%selse@\n" ind;
      print_block ind fmt b2

  | T_while (lbl,e,b) ->
      Format.fprintf fmt "while %a (%a)@\n" print_label lbl print_expr e;
      print_block ind fmt b
        
  | T_add_var (v, eo) ->
      Format.fprintf fmt "%s%s %a" 
        (match v.var_scope with
        | T_INPUT -> "input "
        | T_VOLATILE -> "volatile "
        | _ -> "")
        (string_of_typ v.var_typ) 
        print_var_name v;
      (match eo with
      | Some e -> Format.fprintf fmt " = %a" print_expr e
      | None -> ())
        
  | T_del_var v ->
      Format.fprintf fmt "delete %a" print_var_name v
        
  | T_RETURN -> Format.pp_print_string fmt "return"

  | T_BREAK -> Format.pp_print_string fmt "break"

  | T_label (s,_) -> Format.fprintf fmt "%s:" s

  | T_assert e ->
      Format.fprintf fmt "assert (%a)" print_expr e

  | T_assume e ->
      Format.fprintf fmt "assume (%a)" print_expr e

  | T_print l ->
      Format.fprintf fmt "print (%a)" (print_list print_var_name ",") 
        (List.map fst l)

        
and print_stat_end ind fmt s =
  print_stat ind fmt s;
  (match s with T_if _ | T_while _ -> () | _ -> Format.fprintf fmt ";@\n")


and print_block ind fmt s =
  let ind2 = ind ^ "  " in
  let rec aux = function
    | T_stat (l, (s,_), r) ->
        Format.fprintf fmt "%s%a %a" 
          ind2 print_label l (print_stat_end ind2) s;
        aux r
    | T_empty l -> Format.fprintf fmt "%s%a@\n" ind2 print_label l
  in
  Format.fprintf fmt "%s{@\n" ind;
  aux s;
  Format.fprintf fmt "%s}@\n" ind


let print_func fmt f =
  (match f.func_return with
  | None -> Format.pp_print_string fmt "void"
  | Some v -> Format.pp_print_string fmt (string_of_typ v.var_typ));
  Format.fprintf fmt " %a(" print_func_name f;
  print_list 
    (fun fmt v -> 
      Format.fprintf fmt "%s %a" (string_of_typ v.var_typ) print_var_name v
    ) 
    "," fmt f.func_args;
  Format.pp_print_string fmt ")@\n";
  print_block "" fmt f.func_body 
    

let print_prog fmt (init,funcs,vars) =
  print_block "" fmt init;
  StringMap.iter (fun _ -> print_func fmt) funcs;
  Format.pp_print_flush fmt ()



(************************************************************************)
(* MISC *)
(************************************************************************)

(* name of specially inserted labels *)

let break_label (id,_) =
  string_of_id "break#" id

let return_label id =
  string_of_id "return#" id
