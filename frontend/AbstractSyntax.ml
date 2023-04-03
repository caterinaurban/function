(*   
             ********* Abstract Syntax ************
             - no side-effects
             - labeled statements
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

open Apron
open IntermediateSyntax







(* types *)
type typ =
  | A_INT
  | A_ARR of int
  
  
let typ_print fmt = function
  | A_INT -> Format.fprintf fmt "int"
  | A_ARR n-> Format.fprintf fmt "int[%d]"  n
  
(* variables *)
type var = {
  varId: string;
  varName: string;
  varTyp: typ;
  
}

type arrayvar = {
  arrName: string;
  arrId: string;
  size:int;
  vars: var list
}
let var_print fmt v = Format.fprintf fmt "%s{%s}" v.varId v.varName

module VarMap = Map.Make(
  struct
    type t = Var of var | Arr of arrayvar
    let compare x1 x2 = match x1,x2  with 
                        | Arr x1, Var x2 -> compare x1.arrId x2.varId
                        | Var x1, Arr x2 -> compare x1.varId x2.arrId
                        | Arr x1, Arr x2 -> compare x1.arrId x2.arrId
                        | Var x1, Var x2 -> compare x1.varId x2.varId
  end)

(* arithmetic binary operators *)
type aBinOp =
  | A_PLUS		(* + *)
  | A_MINUS		(* - *)
  | A_MULTIPLY	(* * *)
  | A_DIVIDE		(* / *)

let aBinOp_prec = function
  | A_PLUS -> 5
  | A_MINUS -> 5
  | A_MULTIPLY -> 6
  | A_DIVIDE -> 6

let aBinOp_tostring = function
  | A_PLUS -> "+"
  | A_MINUS -> "-"
  | A_MULTIPLY -> "*"
  | A_DIVIDE -> "/"

let aBinOp_print fmt = function
  | A_PLUS -> Format.fprintf fmt "+"
  | A_MINUS -> Format.fprintf fmt "-"
  | A_MULTIPLY -> Format.fprintf fmt "*"
  | A_DIVIDE -> Format.fprintf fmt "/"

(* arithmetic unary operators *)
type aUnOp =
  | A_UMINUS	(* - *)
  
let aUnOp_tostring = function
  | A_UMINUS -> "-"

let aUnOp_print fmt = function
  | A_UMINUS -> Format.fprintf fmt "-"

(* boolean binary operators *)
type bBinOp =
  | A_AND	(* && *)
  | A_OR	(* || *)

let bBinOp_prec = function
  | A_AND -> 2
  | A_OR -> 1

let negBBinOp = function
  | A_AND -> A_OR
  | A_OR -> A_AND

let bBinOp_tostring = function
  | A_AND -> "&&"
  | A_OR -> "||"

let bBinOp_print fmt = function
  | A_AND -> Format.fprintf fmt "&&"
  | A_OR -> Format.fprintf fmt "||"

(* boolean unary operators *)
type bUnOp =
  | A_NOT	(* ! *)

let bUnOp_tostring = function
  | A_NOT -> "!"

let bUnOp_print fmt = function
  | A_NOT -> Format.fprintf fmt "!"

(* relational binary operators *)
type rBinOp =
  | A_LESS			(* < *)
  | A_LESS_EQUAL		(* <= *)
  | A_GREATER			(* > *)
  | A_GREATER_EQUAL	(* >= *)

let rBinOp_prec = function
  | A_LESS -> 4
  | A_LESS_EQUAL -> 4
  | A_GREATER -> 4
  | A_GREATER_EQUAL -> 4

let negRBinOp = function
  | A_LESS -> A_GREATER_EQUAL
  | A_LESS_EQUAL -> A_GREATER
  | A_GREATER -> A_LESS_EQUAL
  | A_GREATER_EQUAL -> A_LESS

let rBinOp_tostring = function
  | A_LESS -> "<"
  | A_LESS_EQUAL -> "<="
  | A_GREATER -> ">"
  | A_GREATER_EQUAL -> ">="

let rBinOp_print fmt = function
  | A_LESS -> Format.fprintf fmt "<"
  | A_LESS_EQUAL -> Format.fprintf fmt "<="
  | A_GREATER -> Format.fprintf fmt ">"
  | A_GREATER_EQUAL -> Format.fprintf fmt ">="

(* arithmetic expressions *)
type aExp =
  | A_RANDOM	(* ? *)
  | A_var of var
  | A_const of int
  | A_interval of int * int
  | A_aunary of aUnOp * (aExp annotated)
  | A_abinary of aBinOp * (aExp annotated) * (aExp annotated)

let rec aExp_invertible x e =
  match e with
  | A_var y -> (compare x.varId y.varId) = 0
  | A_aunary (_,(e,_)) -> aExp_invertible x e
  | A_abinary (_,(e1,_),(e2,_)) -> (aExp_invertible x e1) || (aExp_invertible x e2)
  | _ -> false

let aExp_prec = function
  | A_aunary (_,_) -> 99
  | A_abinary (o,_,_) -> aBinOp_prec o
  | _ -> 100

let rec aExp_to_apron e =
  match e with
  | A_RANDOM -> Texpr1.Cst (Coeff.Interval Interval.top)
  | A_var x -> Texpr1.Var (Var.of_string x.varId)
  | A_const i -> Texpr1.Cst (Coeff.s_of_int i)
  | A_interval (i1,i2) -> Texpr1.Cst (Coeff.i_of_int i1 i2)
  | A_aunary (o,(e,_)) ->
    let e = aExp_to_apron e in
    (match o with
     | A_UMINUS -> Texpr1.Unop (Texpr1.Neg,e,Texpr1.Int,Texpr1.Zero))
  | A_abinary (o,(e1,_),(e2,_)) ->
    let e1 = aExp_to_apron e1 in
    let e2 = aExp_to_apron e2 in
    (match o with
     | A_PLUS -> Texpr1.Binop (Texpr1.Add,e1,e2,Texpr1.Int,Texpr1.Zero)
     | A_MINUS -> Texpr1.Binop (Texpr1.Sub,e1,e2,Texpr1.Int,Texpr1.Zero)
     | A_MULTIPLY -> Texpr1.Binop (Texpr1.Mul,e1,e2,Texpr1.Int,Texpr1.Zero)
     | A_DIVIDE -> Texpr1.Binop (Texpr1.Div,e1,e2,Texpr1.Int,Texpr1.Zero))

  | _ -> failwith "nyi : array to apron"
let rec aExp_print fmt (e,_) =
  match e with
  | A_RANDOM -> Format.fprintf fmt "?"
  | A_var v -> var_print fmt v
  | A_const i -> Format.fprintf fmt "%i" i
  | A_interval (i1,i2) -> Format.fprintf fmt "[%i,%i]" i1 i2
  | A_aunary (o,e1) ->
    Format.fprintf fmt "%a" aUnOp_print o;
    if aExp_prec (fst e1) <= aExp_prec e
    then Format.fprintf fmt "(%a)" aExp_print e1
    else Format.fprintf fmt "%a" aExp_print e1
  | A_abinary (o,e1,e2) ->
    if aExp_prec (fst e1) <= aExp_prec e
    then Format.fprintf fmt "(%a) " aExp_print e1
    else Format.fprintf fmt "%a " aExp_print e1;
    Format.fprintf fmt "%a" aBinOp_print o;
    if aExp_prec (fst e2) <= aExp_prec e
    then Format.fprintf fmt " (%a)" aExp_print e2
    else Format.fprintf fmt " %a" aExp_print e2
  

(* boolean expressions *)
type bExp =
  | A_TRUE
  | A_MAYBE	(* ? *)
  | A_FALSE
  | A_bunary of bUnOp * (bExp annotated)
  | A_bbinary of bBinOp * (bExp annotated) * (bExp annotated)
  | A_rbinary of rBinOp * (aExp annotated) * (aExp annotated)

let bExp_prec = function
  | A_bunary (_,_) -> 99
  | A_bbinary (o,_,_) -> bBinOp_prec o
  | A_rbinary (o,_,_) -> rBinOp_prec o
  | _ -> 100

let rec negBExp (b,a) =
  match b with
  | A_TRUE -> (A_FALSE,a)
  | A_MAYBE -> (A_MAYBE,a)
  | A_FALSE -> (A_TRUE,a)
  | A_bunary (o,b) ->
    (match o with
     | A_NOT -> b)
  | A_bbinary (o,b1,b2) -> (A_bbinary (negBBinOp o,negBExp b1,negBExp b2),a)
  | A_rbinary (o,a1,a2) -> (A_rbinary (negRBinOp o,a1,a2),a)


let rec bExp_print_aux fmt e =
  match e with
  | A_TRUE -> Format.fprintf fmt "true"
  | A_MAYBE -> Format.fprintf fmt "?"
  | A_FALSE -> Format.fprintf fmt "false"
  | A_bunary (o,e1) ->
    Format.fprintf fmt "%a" bUnOp_print o;
    if bExp_prec (fst e1) <= bExp_prec e
    then Format.fprintf fmt "(%a)" bExp_print_aux (fst e1)
    else Format.fprintf fmt "%a" bExp_print_aux (fst e1)
  | A_bbinary (o,e1,e2) ->
    if bExp_prec (fst e1) <= bExp_prec e
    then Format.fprintf fmt "(%a) " bExp_print_aux (fst e1)
    else Format.fprintf fmt "%a " bExp_print_aux (fst e1);
    Format.fprintf fmt "%a" bBinOp_print o;
    if bExp_prec (fst e2) <= bExp_prec e
    then Format.fprintf fmt " (%a)" bExp_print_aux (fst e2)
    else Format.fprintf fmt " %a" bExp_print_aux (fst e2)
  | A_rbinary (o,e1,e2) ->
    if aExp_prec (fst e1) <= bExp_prec e
    then Format.fprintf fmt "(%a) " aExp_print e1
    else Format.fprintf fmt "%a " aExp_print e1;
    Format.fprintf fmt "%a" rBinOp_print o;
    if aExp_prec (fst e2) <= bExp_prec e
    then Format.fprintf fmt " (%a)" aExp_print e2
    else Format.fprintf fmt " %a" aExp_print e2

let bExp_print fmt b = bExp_print_aux fmt (fst b)

(* expressions *)
type exp =
  | A_arithmetic of (aExp annotated)
  | A_boolean of (bExp annotated)
  | A_stmt

(* properties *)
module StringMap = Map.Make(struct type t=string let compare=compare end)

type property = (bExp annotated) StringMap.t

let property_print fmt p =
  let (u,p) = StringMap.partition (fun l _ -> l = "") p in
  if (StringMap.cardinal p) > 0
  then StringMap.iter (fun l e -> Format.fprintf fmt "%s -> %a\n" l bExp_print e) p
  else Format.fprintf fmt "%a\n" bExp_print (StringMap.find "" u)

(* statements *)
type stmt =
  | A_label of (string annotated)
  | A_return
  | A_assign of (aExp annotated) * (aExp annotated)
  | A_assert of (bExp annotated)
  | A_if of (bExp annotated) * block * block
  | A_while of label * (bExp annotated) * block
  | A_call of string * (stmt annotated) list (* function call *)
  | A_recall of string * (stmt annotated) list (* recursive call *)

and block =
  | A_empty of label
  | A_block of label * (stmt annotated) * block

and label = int

type statements = stmt annotated list

let rec stmt_print ind fmt (s,_) =
  match s with
  | A_label (l,_) -> Format.fprintf fmt "%s%s:\n" ind l
  | A_return -> Format.fprintf fmt "%sreturn\n" ind
  | A_assign (v,e) -> Format.fprintf fmt "%s%a := %a\n" ind aExp_print v aExp_print e
  | A_assert b -> Format.fprintf fmt "%sassert( %a )\n" ind bExp_print b
  | A_if (b,s1,s2) ->
    Format.fprintf fmt "%sif ( %a ) then\n%a%s      else\n%a%s      endif\n"
      ind bExp_print b
      (block_print (ind ^ "  ")) s1 ind	
      (block_print (ind ^ "  ")) s2 ind
  | A_while (l,b,s1) ->
    Format.fprintf fmt "%swhile %a ( %a ) do\n%a%s      od\n"
      ind label_print l bExp_print b
      (block_print (ind ^ "  ")) s1 ind
  | A_call (f,ss) ->
    Format.fprintf fmt "%s%s( " ind f;
    List.iter (fun s -> Format.fprintf fmt "%a; " parameter_print s) ss;
    Format.fprintf fmt ")\n"
  | A_recall (f,ss) ->
    Format.fprintf fmt "%s%s( " ind f;
    List.iter (fun s -> Format.fprintf fmt "%a; " parameter_print s) ss;
    Format.fprintf fmt ")\n"

and parameter_print fmt (s,_) =
  match s with
  | A_assign (v,e) -> Format.fprintf fmt "%a := %a" aExp_print v aExp_print e
  | _ -> raise (Invalid_argument "parameter_print:")

and block_print ind fmt b =
  match b with
  | A_empty l -> Format.fprintf fmt "%a\n" label_print l
  | A_block (l,s,b) ->
    Format.fprintf fmt "%a %a%a" label_print l (stmt_print ind) s (block_print ind) b

and label_print fmt l = if (l < 10) then Format.fprintf fmt "[ %i:]" l else Format.fprintf fmt "[%i:]" l

let label_of_block (block:block) : label = match block with
  | A_empty l -> l
  | A_block (l, _ , _) -> l

(* functions *)
type func = {
  funcName: string;
  funcTyp: var option;
  funcArgs: var list;
  funcVars: var StringMap.t;
  funcBody: block;
}

let function_print fmt f =
  match f.funcTyp with
  | None ->
    Format.fprintf fmt "void ";
    Format.fprintf fmt "%s( " f.funcName;
    List.iter (fun v -> Format.fprintf fmt "%a %a; " typ_print v.varTyp var_print v) f.funcArgs;
    Format.fprintf fmt "):\n";
    block_print "" fmt f.funcBody
  | Some v ->
    Format.fprintf fmt "%a " typ_print v.varTyp;
    Format.fprintf fmt "%s( " f.funcName;
    List.iter (fun v -> Format.fprintf fmt "%a %a; " typ_print v.varTyp var_print v) f.funcArgs;
    Format.fprintf fmt "):\n";
    block_print "" fmt f.funcBody

(* programs *)
type prog = (var StringMap.t) * block * (func StringMap.t)

let prog_print fmt (_,b,fs) = block_print "" fmt b; StringMap.iter (fun _ f -> function_print fmt f) fs

(* utility *)

let annotate e = (e, (Lexing.dummy_pos, Lexing.dummy_pos))
