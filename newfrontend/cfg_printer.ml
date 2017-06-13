(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
   Pretty-printer for control-flow graphs.
 *)



open Lexing
open Abstract_syntax_tree
open Cfg


  
(* locations *)
(* ********* *)

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


      
(* operators *)
(* ********* *)

let string_of_int_unary_op = function
  | AST_UNARY_PLUS -> "+"
  | AST_UNARY_MINUS -> "-"

let string_of_bool_unary_op = function        
  | AST_NOT -> "!"

let string_of_int_binary_op = function
  | AST_MULTIPLY -> "*"
  | AST_DIVIDE -> "/"
  | AST_MODULO -> "%"
  | AST_PLUS -> "+"
  | AST_MINUS -> "-"

let string_of_compare_op = function 
  | AST_EQUAL -> "=="
  | AST_NOT_EQUAL -> "!="
  | AST_LESS -> "<"
  | AST_LESS_EQUAL -> "<="
  | AST_GREATER -> ">"
  | AST_GREATER_EQUAL -> ">="

let string_of_bool_binary_op = function        
  | AST_AND -> "&&"
  | AST_OR -> "||"


        
let int_expr_precedence = function
  | CFG_int_unary (op,_) -> 99
  | CFG_int_binary ((AST_MULTIPLY | AST_DIVIDE | AST_MODULO),_,_) -> 6
  | CFG_int_binary ((AST_PLUS | AST_MINUS),_,_) -> 5
  | _ -> 100

let bool_expr_precedence = function
  | CFG_compare (_,_,_) -> 3
  | CFG_bool_binary (AST_AND,_,_) -> 2
  | CFG_bool_binary (AST_OR,_,_) -> 1
  | _ -> 100

        
(* utility to print lists *)
let print_list f sep ch l =
  let rec aux = function
    | [] -> ()
    | [a] -> f ch a
    | a::b -> f ch a; output_string ch sep; aux b
  in
  aux l

(* utility to print options *)    
let print_option f none ch l =
  match l with
  | None -> output_string ch none
  | Some v -> f ch v
    

      
(* expressions *)
(* *********** *)


let print_var ch v =
  Printf.fprintf ch "%s(%i)" v.var_name v.var_id


let string_of_type t =
  match t with
  | AST_TYP_INT -> "int"

        
let rec print_int_expr ch e = 
  match e with
    
  | CFG_int_unary (op,e1) ->
      output_string ch (string_of_int_unary_op op);
      if int_expr_precedence e1 <= int_expr_precedence e
      then Printf.fprintf ch " (%a)" print_int_expr e1
      else Printf.fprintf ch " %a" print_int_expr e1

  | CFG_int_binary (op,e1,e2) ->
      if int_expr_precedence e1 < int_expr_precedence e
      then Printf.fprintf ch "(%a) " print_int_expr e1
      else Printf.fprintf ch "%a " print_int_expr e1;
      output_string ch (string_of_int_binary_op op);
      if int_expr_precedence e2 <= int_expr_precedence e
      then Printf.fprintf ch " (%a)" print_int_expr e2
      else Printf.fprintf ch " %a" print_int_expr e2
          
  | CFG_int_const i -> output_string ch (Z.to_string i)

  | CFG_int_rand (i1,i2) ->
      Printf.fprintf ch "rand(%s,%s)" (Z.to_string i1) (Z.to_string i2)
        
  | CFG_int_var v -> print_var ch v

        
and print_bool_expr ch e = 
  match e with
    
  | CFG_bool_unary (op,e1) ->
      output_string ch (string_of_bool_unary_op op);
      if bool_expr_precedence e1 <= bool_expr_precedence e
      then Printf.fprintf ch " (%a)" print_bool_expr e1
      else Printf.fprintf ch " %a" print_bool_expr e1

  | CFG_bool_binary (op,e1,e2) ->
      if bool_expr_precedence e1 < bool_expr_precedence e
      then Printf.fprintf ch "(%a) " print_bool_expr e1
      else Printf.fprintf ch "%a " print_bool_expr e1;
      output_string ch (string_of_bool_binary_op op);
      if bool_expr_precedence e2 <= bool_expr_precedence e
      then Printf.fprintf ch " (%a)" print_bool_expr e2
      else Printf.fprintf ch " %a" print_bool_expr e2
          
  | CFG_compare (op,e1,e2) ->
      if int_expr_precedence e1 < bool_expr_precedence e
      then Printf.fprintf ch "(%a) " print_int_expr e1
      else Printf.fprintf ch "%a " print_int_expr e1;
      output_string ch (string_of_compare_op op);
      if int_expr_precedence e2 <= bool_expr_precedence e
      then Printf.fprintf ch " (%a)" print_int_expr e2
      else Printf.fprintf ch " %a" print_int_expr e2
          
  | CFG_bool_const i -> Printf.fprintf ch "%B" i

  | CFG_bool_rand -> Printf.fprintf ch "brand"

        
      
(* instructions *)
(* ************ *)


let print_inst ch i =
  match i with
  | CFG_skip msg -> Printf.fprintf ch "%s" msg
  | CFG_assign (v,e) -> Printf.fprintf ch "%a = %a" print_var v print_int_expr e
  | CFG_guard b -> Printf.fprintf ch "%a ?" print_bool_expr b
  | CFG_assert b -> Printf.fprintf ch "assert %a" print_bool_expr b
  | CFG_call f -> Printf.fprintf ch "call %s" f.func_name

     

(* programs *)
(* ******** *)

(* raw dump of the graph *)        
let print_cfg ch p =
  let pp_var ch v =
    Printf.fprintf ch "%s(%i):%s"
      v.var_name v.var_id (string_of_type v.var_type)
  in
  Printf.fprintf ch "List of variables:\n";
  List.iter
    (fun v ->
      Printf.fprintf ch "  %a at %s\n"
        pp_var v (string_of_extent v.var_pos)
    ) p.cfg_vars;
  Printf.fprintf ch "\n";
  Printf.fprintf ch "List of functions:\n";
  List.iter
    (fun f ->
      Printf.fprintf ch "  %i: %s(%a) -> %a at %s, entry: %i, exit: %i, calls:"
        f.func_id f.func_name
        (print_list pp_var ",") f.func_args
        (print_option pp_var "void") f.func_ret
        (string_of_extent f.func_pos)
        f.func_entry.node_id f.func_exit.node_id;
      List.iter
        (fun a ->
          Printf.fprintf ch " %i->%i" a.arc_src.node_id a.arc_dst.node_id
        ) f.func_calls;
      Printf.fprintf ch "\n";
    ) p.cfg_funcs;
  Printf.fprintf ch "\n";
  Printf.fprintf ch "List of nodes:\n";
  List.iter
    (fun n ->
      Printf.fprintf ch "  %i: at %s, in: "
        n.node_id  (string_of_position n.node_pos);
      List.iter (fun a -> Printf.fprintf ch "%i " a.arc_src.node_id) n.node_in;
      Printf.fprintf ch "out:";
      List.iter (fun a -> Printf.fprintf ch "%i " a.arc_dst.node_id) n.node_out;
      Printf.fprintf ch "\n";
    ) p.cfg_nodes;
  Printf.fprintf ch "\n";
  Printf.fprintf ch "List of arcs:\n";
  List.iter
    (fun a ->
      Printf.fprintf ch "  %i -> %i: %a\n"
        a.arc_src.node_id a.arc_dst.node_id print_inst a.arc_inst
    ) p.cfg_arcs;
  Printf.fprintf ch "\n"

    
(* dump to a DOT file, viewable with Graphviz *)
let output_dot name p =
  let ch = open_out name in
  Printf.fprintf ch "digraph CFG {\n";
  (* nodes and instructions *)
  List.iter
    (fun a ->
      Printf.fprintf ch " %i -> %i [label=\"%a\"];\n"
        a.arc_src.node_id a.arc_dst.node_id print_inst a.arc_inst
    ) p.cfg_arcs;
  (* function entry and exit *)
  List.iter
    (fun f ->
      Printf.fprintf ch "  entry_%s [shape=box,label=\"%s(%a) -> %a\"];\n"
        f.func_name f.func_name
        (print_list print_var ",") f.func_args
        (print_option print_var "void") f.func_ret;
      Printf.fprintf ch "  exit_%s [shape=box,label=\"exit %s\"];\n"
        f.func_name f.func_name;
      Printf.fprintf ch "  entry_%s -> %i;\n" f.func_name f.func_entry.node_id;
      Printf.fprintf ch "  %i -> exit_%s;\n" f.func_exit.node_id f.func_name
    ) p.cfg_funcs;
  (* init code entry and exit *)
  Printf.fprintf ch "  init_entry [shape=box];\n";
  Printf.fprintf ch "  init_exit [shape=box];\n";
  Printf.fprintf ch "  init_entry -> %i;\n" p.cfg_init_entry.node_id;
  Printf.fprintf ch "  %i -> init_exit;\n" p.cfg_init_exit.node_id;
  Printf.fprintf ch "}\n";
  flush ch;
  close_out ch
    
