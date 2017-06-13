(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
   Control-flow graphs (CFG).

   The CFG representation is much simpler than the tree representation:
   - complex expressions are compiled into a sequence of simpler ones;
   - variables are desambiguated;
   - the binding of formal and actual arguments is explicit (as assignments);
   - the control structures are translated into jumps between nodes.
 *)



open Lexing
open Abstract_syntax_tree

  
  
(* Variables *)
(* ********* *)


(* Each global variable, local variable and function parameter is associated a
   var structure.
   We use unique identifiers (integers) to distinguish between variables declared
   at different point with the same name.
 *)
type var =
    { var_id: int;       (* unique variable identifier *)
      var_name: id;      (* original name, in the program *)
      var_type: typ;     (* variable type *)
      var_pos: extent;   (* position of the variable declaration *)
    }
    

      
(* Expressions *)
(* *********** *)

  
(*
  Expressions in the CFG are call-free. Calls are extracted during the
  translation form AST to CFG and put in separate instructions
  (possibly introducing temporary variables).

  To simplify, we remove some all location information (ext) in expressions.

  Variable scoping is resolved in the translation: variables in CFG 
  expressions are var structures instead of plain strings.  
 *)

type int_expr = 

  (* unary operation *)
  | CFG_int_unary of int_unary_op * int_expr

  (* binary operation *)
  | CFG_int_binary of int_binary_op * int_expr * int_expr

  (* variable use *)
  | CFG_int_var of var

  (* constants *)
  | CFG_int_const of Z.t

  (* non-deterministic choice between two integers *)
  | CFG_int_rand of Z.t (* lower bound *) * Z.t (* upper bound *)

        
type bool_expr =

  (* unary operation *)
  | CFG_bool_unary of bool_unary_op * bool_expr

  (* binary operation *)
  | CFG_bool_binary of bool_binary_op * bool_expr * bool_expr
  | CFG_compare of compare_op * int_expr * int_expr

  (* constants *)
  | CFG_bool_const of bool

  (* non-deterministic choice *)
  | CFG_bool_rand
      

      
(* Instructions *)
(* ************ *)

      
(*
  Each arc between two CFG node is labelled with an instruction to
  execute to go from the source node to the destination node.
  CFG instructions are thus very simple.
*)

type inst =      

  (* go to the destination node doing nothing *)
  (* the string argument is just for printing, it give some
     information on the original instruction that caused the skip
   *)
  | CFG_skip of string
    
  (* assignment *)
  | CFG_assign of var * int_expr

  (* guard: test that must be satisfied to make a transition *)
  | CFG_guard of bool_expr

  (* assertion: it is an error if the test is not satisfied *)
  | CFG_assert of bool_expr
        
  (* function call *)
  | CFG_call of func


        
(* Functions *)
(* ********* *)

(*
  Functions have a single entry node and a single exit node.
  The execution always starts at the entry node, and the function always
  returns through the return node.

  A return instruction inside the function is compiled as a jump to the
  exit node.
  Any returned value is stored into a special variable before jumping to
  the exit node.
 *)

and func =
    { func_id: int;          (* unique function identifier *)
      func_name: string;     (* function name *)
      func_pos: extent;      (* function position in the source *)
      func_entry: node;      (* entry node *)
      func_exit: node;       (* exit node *)
      func_args: var list;   (* list of formal arguments *)
      func_ret: var option;  (* variable used to store the return value *)
      mutable func_calls: arc list; (* list of calls to the function *)
    }


      
(* Graphs *)
(* ****** *)

      
(* 
   Each CFG node is denoted by a unique (integer) identifier.
   A CFG node corresponds roughly to a position in the program, but complex
   statements and expressions can be split among many nodes.
 *)      
and node =
    { node_id:  int;               (* unique identifier *)
      node_pos: position;          (* position in the source *)
      mutable node_out: arc list;  (* arcs with this node as source *)
      mutable node_in: arc list;   (* arcs with this node as destination *)
    }

and arc =
    { arc_id:  int;    (* unique identifier *)
      arc_src: node;   (* source node *)
      arc_dst: node;   (* destination node *)
      arc_inst: inst;  (* instruction *)
    }

      

(* Sets, maps and hashtables *)
(* ************************* *)
      

(* module parameter for Hashtbl, Set and Map functors *)
      
module Node =
  struct
    type t = node
    let compare v1 v2 = compare v1.node_id v2.node_id
    let equal v1 v2 = v1.node_id = v2.node_id
    let hash v = v.node_id
  end

module Arc =
  struct
    type t = arc
    let compare v1 v2 = compare v1.arc_id v2.arc_id
    let equal v1 v2 = v1.arc_id = v2.arc_id
    let hash v = v.arc_id
  end

module Var =
  struct
    type t = var
    let compare v1 v2 = compare v1.var_id v2.var_id
    let equal v1 v2 = v1.var_id = v2.var_id
    let hash v = v.var_id
  end

module Func =
  struct
    type t = func
    let compare v1 v2 = compare v1.func_id v2.func_id
    let equal v1 v2 = v1.func_id = v2.func_id
    let hash v = v.func_id
  end
    
module NodeSet = Set.Make(Node)
module NodeMap = Mapext.Make(Node)
module NodeHash = Hashtbl.Make(Node)

module ArcSet = Set.Make(Arc)
module ArcMap = Mapext.Make(Arc)
module ArcHash = Hashtbl.Make(Arc)

module VarSet = Set.Make(Var)
module VarMap = Mapext.Make(Var)
module VarHash = Hashtbl.Make(Var)

module FuncSet = Set.Make(Func)
module FuncMap = Mapext.Make(Func)
module FuncHash = Hashtbl.Make(Func)

    

(* Program CFG *)
(* *********** *)

    
type cfg =
    { cfg_vars: var list;    (* list of all the variables *)
      cfg_funcs: func list;  (* list of all the functions *)
      cfg_nodes: node list;  (* list of all the nodes in the program *)
      cfg_arcs: arc list;    (* list of all the arcs in the program *)
      cfg_init_entry: node;  (* first node of code initializing global variables *)
      cfg_init_exit: node;   (* last node of initialization code *)
    }
      
