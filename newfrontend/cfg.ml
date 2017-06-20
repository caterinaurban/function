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
    
let unique_var_name v = Printf.sprintf "$%i{%s}" v.var_id v.var_name 

      
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

  (* constants *)
  | CFG_int_random 

  (* non-deterministic choice between two integers *)
  | CFG_int_interval of Z.t (* lower bound *) * Z.t (* upper bound *)

        
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

  (* equivalent to skip but has name of label definiton as value *)
  | CFG_label of string
    
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
      

(* Utility functions *)



(* Find function by name *)
let find_func (func_name:string) (cfg:cfg): func = 
  List.find (fun f -> String.equal func_name f.func_name) cfg.cfg_funcs 

(* Create NodeMap that maps 'value' to each node in 'cfg'*)
let const_node_map (cfg:cfg) value = 
  List.fold_left (fun map node -> NodeMap.add node value map) NodeMap.empty cfg.cfg_nodes

let negate_bool_expr (bexpr:bool_expr): bool_expr = CFG_bool_unary (AST_NOT, bexpr)


let max_arc_id cfg =
  List.fold_left 
    (fun currentMax arc -> if arc.arc_id > currentMax then arc.arc_id else currentMax) 
    0 cfg.cfg_arcs 

(* Insert 'exit' label befor exit node in 'func' *)
let insert_exit_label (cfg:cfg) (func:func) = 
  let maxNodeId = List.fold_left 
      (fun currentMax node -> if node.node_id > currentMax then node.node_id else currentMax) 
      0 cfg.cfg_nodes in
  let maxArcId = max_arc_id cfg in
  let oldExitNode = func.func_exit in
  let newExitNode = { 
    node_id = maxNodeId + 1;
    node_pos = position_unknown;
    node_out = [];
    node_in = [];
  } in
  let labelArc = {
    arc_id = maxArcId + 1;    
    arc_src = oldExitNode;   
    arc_dst = newExitNode;   
    arc_inst = CFG_label "exit";
  } in 
  let newFunc = {
    func with
    func_exit = newExitNode;
  } in
  let newCfgFuncs = List.map (fun f -> if f.func_id == newFunc.func_id then newFunc else f) cfg.cfg_funcs in
  oldExitNode.node_out <- labelArc::oldExitNode.node_out;
  newExitNode.node_in <- labelArc::newExitNode.node_in;
  {
    cfg with
    cfg_funcs = newCfgFuncs;
    cfg_nodes = newExitNode::cfg.cfg_nodes;
    cfg_arcs = labelArc::cfg.cfg_arcs
  }



(* 
   Replace each function call edge in cfg with an edge from the original call source entering the 
   function and adds one returning back from the function exit to the destination of the original call edge. 
*)
let add_function_call_arcs (cfg:cfg) =
  let arcId = ref (max_arc_id cfg + 1) in
  let nextArcId () = 
    let id = !arcId in
    incr arcId;
    id
  in
  let add_function_call_arc (cfg:cfg) (arc:arc) = 
    match arc.arc_inst with 
    | CFG_call f -> 
      (* replace function call arc with arc from source node to function entry *)
      let src_func_arc = {
        arc_id = arc.arc_id;
        arc_src = arc.arc_src;
        arc_dst = f.func_entry;
        arc_inst = CFG_skip ("call: " ^ f.func_name);
      } in
      arc.arc_src.node_out <- [src_func_arc];
      f.func_entry.node_in <- src_func_arc::f.func_entry.node_in;
      (* add edge from function exit back to arc destination *)
      let func_dst_arc = {
        arc_id = nextArcId ();
        arc_src = f.func_exit;
        arc_dst = arc.arc_dst;
        arc_inst = CFG_skip ("return: " ^ f.func_name);
      } in
      f.func_exit.node_out <- func_dst_arc::f.func_exit.node_out;
      arc.arc_dst.node_in <- [func_dst_arc];
      (* add two new arcs to cfg and remove old one *) 
      let new_arcs = func_dst_arc :: List.map 
                       (fun a -> if a.arc_id == arc.arc_id then src_func_arc else a) cfg.cfg_arcs 
      in {
        cfg with
        cfg_arcs = new_arcs;
      }
    | _ -> raise (Invalid_argument "Not a call arc")
  in 
  let callArcs = List.filter (fun a -> match a.arc_inst with | CFG_call f -> true | _ -> false) 
      cfg.cfg_arcs in
  List.fold_left add_function_call_arc cfg callArcs



