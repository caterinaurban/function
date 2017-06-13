(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
  Definition of the abstract syntax trees output by the parser.
*)


open Lexing


(* position in the source file, we use ocamllex's default type *)

type position = Lexing.position
let position_unknown = Lexing.dummy_pos


(* extents are pairs of positions *)

type extent = position * position (* start/end *)
let extent_unknown = (position_unknown, position_unknown)


(* Many parts of the syntax are tagged with an extent indicating which 
   part of the parser-file corresponds to the sub-tree.
   This is very useful for interesting error reporting!
 *)
type 'a ext = 'a * extent

(* variable identifiers are string *)
type id = string

(* types of variables: only int for now *)
type typ =
  (* mathematical integers, in Z *)
  | AST_TYP_INT


(* unary expression operators *)

type int_unary_op = 
  | AST_UNARY_PLUS     (* +e *)
  | AST_UNARY_MINUS    (* -e *)

type bool_unary_op =
  | AST_NOT            (* !e logical negation *)


(* binary expression operators *)

type int_binary_op =
  | AST_PLUS          (* e + e *)
  | AST_MINUS         (* e - e *)
  | AST_MULTIPLY      (* e * e *)
  | AST_DIVIDE        (* e / e *)
  | AST_MODULO        (* e % e *)

type compare_op =
  | AST_EQUAL         (* e == e *)
  | AST_NOT_EQUAL     (* e != e *)
  | AST_LESS          (* e < e *)
  | AST_LESS_EQUAL    (* e <= e *)
  | AST_GREATER       (* e > e *)
  | AST_GREATER_EQUAL (* e >= e *)

type bool_binary_op =
  | AST_AND           (* e && e *)
  | AST_OR            (* e || e *)


(* expressions *)

type int_expr = 
  (* unary operation *)
  | AST_int_unary of int_unary_op * (int_expr ext)

  (* binary operation *)
  | AST_int_binary of int_binary_op * (int_expr ext) * (int_expr ext)

  (* variable use *)
  | AST_int_identifier of id ext

  (* constants (integers are still in their string representation) *)
  | AST_int_const of string ext

  (* non-deterministic choice between two integers *)
  | AST_int_rand of (string ext) (* lower bound *) * 
                    (string ext) (* upper bound *)

  (* calls a function with arguments and return value *)
  | AST_expr_call of (id ext) (* function name *) * 
                     (int_expr ext list) (* arguments *)


type bool_expr =
  (* unary operation *)
  | AST_bool_unary of bool_unary_op * (bool_expr ext)

  (* binary operation *)
  | AST_bool_binary of bool_binary_op * (bool_expr ext) * (bool_expr ext)
  | AST_compare of compare_op * (int_expr ext) * (int_expr ext)

  (* constants *)
  | AST_bool_const of bool

  (* non-deterministic choice *)
  | AST_bool_rand
      

(* statements *)
type stat =

  (* block of statements { ... } *)
  | AST_block of stat ext list

  (* assignment  lvalue = expr *)
  | AST_assign of (id ext) * (int_expr ext)

  (* assignment lvalue op= expr *)
  | AST_assign_op of (id ext) * int_binary_op * (int_expr ext)

  (* increment lvalue += cst *)
  | AST_increment of (id ext) * int

  (* if-then-else; the else branch is optional *)
  | AST_if of (bool_expr ext) (* condition *) * 
              (stat ext) (* then branch *) * 
              (stat ext option) (* optional else *)

  (* while loop *)
  | AST_while of (bool_expr ext) (* condition *) * 
                 (stat ext) (* body *)

  (* for loop *)
  | AST_for of (stat ext list) (* init *) *
               (bool_expr ext option) (* condition *) *
               (stat ext list) (* increment *) *
               (stat ext) (* body *)
        
  (* assertion: fail if the boolean expression does not hold *)
  | AST_assert of bool_expr ext

  (* declaration of a local variable, live until the end of the current block *)
  | AST_local_decl of var_decl ext

  (* calls a function with arguments (no return value) *)
  | AST_stat_call of (id ext) (* function name *) * 
                     (int_expr ext list) (* arguments *)

  (* exits form the function, with optional return value *)
  | AST_return of int_expr ext option

  (* exits from the innermost while loop *)
  | AST_break of unit ext

  (* empty instruction: do nothing *)
  | AST_SKIP

  (* go to a label in the function *)
  | AST_goto of id ext

  (* destination of a goto *)
  | AST_label of id ext


(* declare some variables with a common type *)
and var_decl = (typ ext) (* type *) * (var_init list) (* variable list *)

(* each declared variable has an optional initializer *)
and var_init = (id ext) (* declared variable *) * 
               (int_expr ext option)  (* initializer *)


(* function declaration 
   (no return type, all functions return void)
 *)
type fun_decl = 
    { (* function name *)
      fun_name: id ext;

      (* formal arguments, with type *)
      fun_args: ((typ ext) * (id ext)) list;

      (* type of the returned value, if any *)
      fun_typ: typ option ext;

      (* function body *)
      fun_body: stat ext list;

      (* function location *)
      fun_ext: extent;
    }

      
      
(* top-level statements *)
type toplevel =
    
  (* global variable declaration *)
  | AST_global_decl of var_decl ext
    
  (* function declaration *)
  | AST_fun_decl of fun_decl ext


(* a program is a list of top-level statements *)
type prog = toplevel list ext
