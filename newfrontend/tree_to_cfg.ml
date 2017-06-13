(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
   Converts an abstract syntax tree to a control-flow-graph.
   CFG arcs use a simpler language.
   The conversion takes care of splitting complex statements and
   expressions, and introducing temporaries if necessary.
 *)


open Lexing
open Abstract_syntax_tree
open Cfg
open Cfg_printer
  

(* map variable and function names to structures *)    
module StringMap = Map.Make(String)



    
(* constructors *)
(* ************ *)


     
let node_counter = ref 0    

let nodes = ref []
    
(* create a new node, with a fresh identifier and accumulate into nodes *)
let create_node (pos:position) =
  incr node_counter;
  let node =
    { node_id = !node_counter;
      node_pos = pos;
      node_in = [];
      node_out = [];
    }
  in
  nodes := node::(!nodes);
  node


let arcs = ref []

let arc_counter = ref 0    

(* create a new arc and accumulate it into arcs *)
let add_arc (src:node) (dst:node) (inst:inst) =
  incr arc_counter;
  let arc =
    { arc_id = !arc_counter;
      arc_src = src;
      arc_dst = dst;
      arc_inst = inst;
    }
  in
  src.node_out <- arc::src.node_out;
  dst.node_in <- arc::dst.node_in;
  (* remember call sites for call instructions *)
  (match inst with
  | CFG_call f -> f.func_calls <- arc::f.func_calls
  | _ -> ()
  );
  arcs := arc::(!arcs)


let var_counter = ref 0

(* create a variable structure, assigning it a fresh identifier *)    
let create_var (name:string) (pos:extent) (typ:typ) =
  incr var_counter;
  { var_id = !var_counter;
    var_name = name;
    var_pos = pos;
    var_type = typ;
  }


let fun_counter = ref 0

(* create a function structure, assigning it a fresh identifier *)
let create_fun (name:string) (entry:node) (exit:node) (pos:extent) (args:var list) (ret:var option) =
  incr fun_counter;
  { func_id = !fun_counter;
    func_name = name;
    func_pos = pos;
    func_entry = entry;
    func_exit = exit;
    func_args = args;
    func_ret = ret;
    func_calls = [];
  }
    
           

(* add a sequence of instructions to the CFG between two nodes *)
let rec add_inst (entry:node) (exit:node) (l:inst ext list) =
  match l with
  | [] ->
      (* entry --[skip]--> exit *)
      add_arc entry exit (CFG_skip "skip")
  | [(a,_)] ->
      (* entry --[a]--> exit *)
      add_arc entry exit a
  | (first,x)::rest ->
      (* add intermediate (next) node *)
      let next = create_node (snd x) in
      (* entry --[first]--> next *)
      add_arc entry next first;
      (* next --[rest]--> exit *)
      add_inst next exit rest  
    

(* Add a sequence of instructions to the CFG.
   The entry of the first instruction is the given node; other
   nodes are created.
   The exit node of the last instruction is returned.
 *)
let rec append_inst (entry:node) (l:inst ext list) : node =
  match l with
  | [] -> entry
  | (first,x)::rest ->
      (* add intermediate (next) node *)
      let next = create_node (snd x) in
      (* entry --[first]--> next *)
      add_arc entry next first;
      (* next --[rest]-->  *)
      append_inst next rest

  
(* Also add a sequence of instruction to the CFG.
   The exist of the first instruction is given node.
   The entry of the last instruction is returned.
 *)
let rec prepend_inst (exit:node) (l:inst ext list) : node =
  match l with
  | [] -> exit
  | (first,x)::rest ->
      (* add intermediate (prev) node *)
      let prev = create_node (fst x) in
      (* prev --[first]--> exit *)
      add_arc prev exit first;
      (* --[rest]--> prev *)
      prepend_inst prev rest

                                 


(* translation *)
(* *********** *)

    
(*
  We need to remember a lot of information during translation, such as the 
  set of variables in the scope, where to jump to after a break or a return,
  in which variable to store a returned value, etc.
  For gotos, arcs are generated at the end of the translation of each 
  procedure, to handle more easily backward gotos; hence, we must also 
  remember label and goto instructions for this later pass.
  Everything needed is wrapped in an env.
*)
type env =
    { env_vars: var StringMap.t;           (* visible variables in scope, by name *)
      env_funcs: func StringMap.t;         (* visible functions in scope, by name *)
      env_break: node option;              (* destination of a break *)
      env_exit: node option;               (* destination of a return *)
      env_return: var option;              (* variable storing the returned value *)
      env_allvars: VarSet.t;               (* set of all variables *)
      env_labels: node StringMap.t;        (* labels *)
      env_gotos: (node * string ext) list; (* gotos *)
    }


let add_to_vars (env:env) (v:var) : env =
  { env with
    env_vars = StringMap.add v.var_name v env.env_vars;
    env_allvars = VarSet.add v env.env_allvars;
  }
    
    
(*
  Expression translation.

  Also returns a list of instructions that must be executed before the
  expression can be evaluated, such as function calls that have been
  extracted from the expression.
*)
    
let rec int_expr (env:env) (expr:Abstract_syntax_tree.int_expr)
    : env * inst ext list * int_expr =
  match expr with
  | AST_int_unary (o,(e1,_)) ->
      let env1, before1, f1 = int_expr env e1 in
      env1, before1, CFG_int_unary (o,f1)
        
  | AST_int_binary (o,(e1,_),(e2,_)) ->
      let env1, before1, f1 = int_expr env e1 in
      let env2, before2, f2 = int_expr env e2 in
      env2, before1@before2, CFG_int_binary (o,f1,f2)
                
  | AST_int_identifier (id,x) ->
      let var =
        try StringMap.find id env.env_vars
        with Not_found -> failwith (Printf.sprintf "unknown variable %s at %s" id (string_of_extent x))
      in
      env, [], CFG_int_var var
        
  | AST_int_const (i,x) ->
      let v =
        try Z.of_string i
        with _ -> failwith (Printf.sprintf "invalid integer constant %s at %s" i (string_of_extent x))
      in
      env, [], CFG_int_const v

  | AST_int_rand ((i1,x1),(i2,x2)) ->
      let v1 =
        try Z.of_string i1
        with _ -> failwith (Printf.sprintf "invalid integer constant %s at %s" i1 (string_of_extent x1))
      and v2 =
        try Z.of_string i2
        with _ -> failwith (Printf.sprintf "invalid integer constant %s at %s" i2 (string_of_extent x2))
      in
      env, [], CFG_int_rand (v1,v2)
        
  | AST_expr_call ((id,x),exprs) ->
      let env1, inst, f = call env (id,x) exprs in
      (match f.func_ret with
      | None -> failwith (Printf.sprintf "function %s has no return value at %s" id (string_of_extent x))
      | Some var ->
          (* we must create a temporary to hold the returned value
             (consider the case where the function is called twice in the expression)
           *)
          let tmp = create_var ("__ret_"^id) x var.var_type in
          let ass = CFG_assign (tmp, CFG_int_var var) in
          add_to_vars env1 var, inst@[ass,x], CFG_int_var tmp
      )

        
and bool_expr (env:env) (expr:Abstract_syntax_tree.bool_expr)
    : env * inst ext list * bool_expr =
  match expr with
  | AST_bool_unary (o,(e1,_)) ->
      let env1, before1, f1 = bool_expr env e1 in
      env1, before1, CFG_bool_unary (o,f1)
        
  | AST_bool_binary (o,(e1,_),(e2,_)) ->
      let env1, before1, f1 = bool_expr env e1 in
      let env2, before2, f2 = bool_expr env e2 in
      env2, before1@before2, CFG_bool_binary (o,f1,f2)
                
  | AST_compare (o,(e1,_),(e2,_)) ->
      let env1, before1, f1 = int_expr env e1 in
      let env2, before2, f2 = int_expr env e2 in
      env2, before1@before2, CFG_compare (o,f1,f2)
                
  | AST_bool_const f ->
      env, [], CFG_bool_const f
        
  | AST_bool_rand ->
      env, [], CFG_bool_rand



(* Translate a call. *)
        
and call (env:env) ((id,x):id ext) (exprs:Abstract_syntax_tree.int_expr ext list)
    : env * inst ext list * func =
  let f =
    try StringMap.find id env.env_funcs
    with Not_found -> failwith (Printf.sprintf "unknown function %s at %s" id (string_of_extent x))
  in
  (* match formal and actual arguments *)
  let rec doargs env inst args = match args with
  | [],[] -> env, inst
  | var::rest1, (expr,x1)::rest2 ->
      (* translate argument binding to assignment *)
      let env1, before, e1 = int_expr env expr in
      let env2 = add_to_vars env1 var in
      doargs env2 (before @ [CFG_assign (var,e1), x1] @ inst) (rest1, rest2)
  | _ ->
      failwith (Printf.sprintf "wrong number of arguments for function %s at %s" id (string_of_extent x))
  in
  let env1, inst = doargs env [CFG_call f, x] (f.func_args,exprs) in
  env1, inst, f

    
(* Variable declarations.

   Create the variable structure, remember it in the environment,
   and translate initialization into assignments.
 *)
let decls (env:env) (((t,_),l):var_decl) : env * inst ext list =
  List.fold_left
    (fun (env,inst) ((id,x),init) ->
      let var = create_var id x t in
      let env1 = add_to_vars env var in
      match init with
      | None -> env1, inst
      | Some (expr,x1) ->
          let env2, before, e = int_expr env1 expr in
          env2, before @ [CFG_assign (var,e), x1] @ inst
    )
    (env,[]) l

        
                
(*
  Translate a statement.
  
  Translation creates a subgraph. The first instruction of the subgraph
  is connected to the given entry node, and the last is connected to the
  given exit node.
 *)

let rec stat (env:env) (entry:node) (exit:node) (s:stat) : env =
  match s with

  | AST_block l ->
      let env1 = stat_list env entry exit l in
      (* restore the variable scoping from the begining of the block *)
      { env1 with env_vars = env.env_vars; }

  | AST_SKIP ->
      add_arc entry exit (CFG_skip "skip");
      env

  | AST_assign ((id,x),(expr,_)) ->
      (* translate expression *)
      let env1, before, e1 = int_expr env expr in
      (* entry --[before]--> entry1 --[assign] --> exit *)
      let entry1 = append_inst entry before in
      let var =
        try StringMap.find id env1.env_vars
        with Not_found -> failwith (Printf.sprintf "unknown variable %s at %s" id (string_of_extent x))
      in
      add_arc entry1 exit (CFG_assign (var, e1));
      env1

  | AST_increment ((id,x),v) ->
      (* x++ is translated as x = x + 1 *)
      let var =
        try StringMap.find id env.env_vars
        with Not_found -> failwith (Printf.sprintf "unknown variable %s at %s" id (string_of_extent x))
      in
      add_arc entry exit
        (CFG_assign (var, (CFG_int_binary (AST_PLUS, CFG_int_var var, CFG_int_const (Z.of_int v)))));
      env

  | AST_assign_op ((id,x),op,(expr,_)) ->
      (* x +=  expr is translated as x = x + expr *)
      let env1, before, e = int_expr env expr in
      let entry1 = append_inst entry before in
      let var =
        try StringMap.find id env1.env_vars
        with Not_found -> failwith (Printf.sprintf "unknown variable %s at %s" id (string_of_extent x))
      in
      add_arc entry1 exit
        (CFG_assign (var, (CFG_int_binary (op, CFG_int_var var, e))));
      env1

   | AST_assert (expr,_) ->
      (* entry --[before]--> entry1 --[assert] --> exit *)
      let env1, before, e = bool_expr env expr in
      let entry1 = append_inst entry before in
      add_arc entry1 exit (CFG_assert e);
      env1

   | AST_break ((),x) ->
       (* break: jump outside innermost loop *)
       (* entry --[skip]--> env_break *)
       (match env.env_break with
       | Some node -> add_arc entry node (CFG_skip "skip: break")
       | None ->  failwith (Printf.sprintf "break outside loop at %s" (string_of_extent x))
       );
       env

   | AST_return None ->
       (* return: jump to the function exit *)
       (* entry --[skip]--> env_exit *)
       (match env.env_exit with
       | Some exit -> add_arc entry exit (CFG_skip "skip: return")
       | None -> failwith "no exit node for function"
       );
       env

   | AST_return (Some (expr,x)) ->
       (* return expr is translated as return = expr
          the assignment is connected directly to the function exit
        *)
       (* entry --[before]--> entry1 --[assign] --> env_exit *)
       let env1, before, e = int_expr env expr in
       let entry1 = append_inst entry before in
       let var =
         match env1.env_return with
         | Some v -> v
         | None -> failwith (Printf.sprintf "function cannot return a value at %s" (string_of_extent x))
       in
       (match env1.env_exit with
       | Some exit ->  add_arc entry1 exit (CFG_assign (var, e))
       | None -> failwith "no exit node for function"
       );
       env1

   | AST_if ((expr,_),(s1,x1),(Some (s2,x2))) ->
       (*
                                       /--[expr]---> node_t --[s1]--\
         entry --[before]--> entry1 --|                              |---> exit
                                       \--[!expr]--> node_f --[s2]--/
        *)
       let env1, before, e = bool_expr env expr in
       (* entry --[before]--> entry1 *)
       let entry1 = append_inst entry before in
       let node_t, node_f = create_node (fst x1), create_node (fst x2) in
       (* entry1 --[expr]--> node_t_t *)
       add_arc entry1 node_t (CFG_guard e);
       (* entry1 --[!expr] --> node_f *)
       add_arc entry1 node_f (CFG_guard (CFG_bool_unary (AST_NOT, e)));
       (* node_t --[s1]--> exit *)
       let env2 = stat env1 node_t exit s1 in
       (* node_f --[s2] --> exit *)
       stat env2 node_f exit s2
         
   | AST_if ((expr,_),(s1,x1),None) ->
       (*
                                       /--[expr]---> node_t --[s1]--\
         entry --[before]--> entry1 --|                              |---> exit
                                       \--[!expr]--> ---------------/
        *)
       let env1, before, e = bool_expr env expr in
       (* entry --[before]--> entry1 *)
       let entry1 = append_inst entry before in
       let node_t = create_node (fst x1) in
       (* entry1 --[expr]--> node_t *)
       add_arc entry1 node_t (CFG_guard e);
       (* entry1 --[!expr]--> exit *)
       add_arc entry1 exit (CFG_guard (CFG_bool_unary (AST_NOT, e)));
       (* node_t --[s1]--> exit *)
       stat env1 node_t exit s1
         
   | AST_while ((expr,_),(s1,x1)) ->
       (*
         similar to "if expr then s1", except that we have
           node_t --[s1]--> entry
         instead of
           node_t --[s1]--> exit
        *)
       let env1, before, e = bool_expr env expr in
       (* entry --[before]--> entry1 *)
       let entry1 = append_inst entry before in
       let node_t = create_node (fst x1) in
       (* entry1 --[expr]--> node_t *)
       add_arc entry1 node_t (CFG_guard e);
       (* entry1 --[!expr]--> node_f *)
       add_arc entry1 exit (CFG_guard (CFG_bool_unary (AST_NOT, e)));
       (* node_t --[s1]--> entry *)
       let env2 = stat { env1 with env_break = Some exit; } node_t entry s1 in
       { env2 with env_break = env1.env_break; }

   | AST_for (init,expr,incr,(s1,x1)) ->
       (* init *)
       (* entry --[init]--> head *)
       let env1, head =
         if init = []
         then env, entry
         else (
           let head = create_node (fst x1) in
           stat_list env entry head init, head
          )
       in
       (* conditional *)
       (*
         head --[before]--> head1 ---[expr]---> node_t 
                                  \--[!expr]--> exit
        *)
       let env2, before, e =
         match expr with
         | None -> env1, [], CFG_bool_const true
         | Some (expr,_) -> bool_expr env1 expr
       in
       let head1 = append_inst head before in
       let node_t = create_node (fst x1) in
       add_arc head1 node_t (CFG_guard e);
       add_arc head1 exit (CFG_guard (CFG_bool_unary (AST_NOT, e)));
       (* increment *)
       (* tail --[incr]--> head *)
       let env3, tail =
         if incr = []
         then env2, head
         else (
           let tail = create_node (snd x1) in
           stat_list env2 tail head incr, tail
          )
       in
       (* body *)
       (* node_t --[s1]--> tail *)
       let env4 = stat { env3 with env_break = Some exit; } node_t tail s1 in
       { env4 with env_break = env3.env_break; }

   | AST_local_decl (d,_) ->
       let env1, inst = decls env d in
       add_inst entry exit inst;
       env1

   | AST_stat_call (idx,exprs) ->
       let env1, inst, _ = call env idx exprs in
       add_inst entry exit inst;
       env1

   | AST_label (id,x) ->
       (* remember the node of the label *)
       if StringMap.mem id env.env_labels then
         failwith (Printf.sprintf "duplicate label %s at %s" id (string_of_extent x));
       add_arc entry exit (CFG_skip ("skip: label "^id));
       { env with env_labels = StringMap.add id entry env.env_labels; }

   | AST_goto (id,x) ->
       (* remember the goto; we will generate at the end of the function,
          when all the labels are known
        *)
       { env with env_gotos = (entry,(id,x))::env.env_gotos; }
       
       

(* Translate a sequence of statements. *)
         
and stat_list (env:env) (entry:node) (exit:node) (l:stat ext list) : env =
  match l with
  | [] ->
      (* entry --[skip]--> exit *)
      add_arc entry exit (CFG_skip "skip");
      env
  | [(s,_)] ->
      (* entry --[s]--> exit *)
      stat env entry exit s
  | (first,x)::rest ->
      (* add an intermediate (next) node *)
      let next = create_node (snd x) in
      (* entry --[first]--> next *)
      let env1 = stat env entry next first in
      (* next --[rest]--> exit *)
      stat_list env1 next exit rest



(* Translate a function *)

let func (env:env) (f:fun_decl) : env =
  (* create entry and exit nodes *)
  let entry = create_node (fst f.fun_ext) in
  let exit = create_node (snd f.fun_ext) in
  (* create variable structures for formal arguments and return *)
  let args = List.map (fun ((t,_),(id,x)) -> create_var id x t) f.fun_args in
  let ret = match f.fun_typ with
  | None, _ -> None
  | Some t, x -> Some (create_var ("__return_"^(fst f.fun_name)) f.fun_ext t)
  in
  (* create function structure *)
  let func = create_fun (fst f.fun_name) entry exit f.fun_ext args ret in
  (* populate env with formal arguments and return *)
  let env1 =
    { env with
      env_exit = Some exit;
      env_return = ret;
      env_funcs = StringMap.add func.func_name func env.env_funcs;
    }
  in
  let env2 = List.fold_left add_to_vars env1 args in
  let env3 = match ret with Some v -> add_to_vars env2 v | None -> env2 in
  (* translate body *)
  let env4 = stat_list env3 entry exit f.fun_body in
  (* generate gotos *)
  List.iter
    (fun (src,(id,x)) ->
      let dst =
        try StringMap.find id env4.env_labels
        with Not_found -> failwith (Printf.sprintf "unknown label %s at %s" id (string_of_extent x))
      in
      add_arc src dst (CFG_skip ("skip: goto "^id))
    ) env4.env_gotos;
  (* returned environment *)
  { env with
    env_funcs = env4.env_funcs;
    env_allvars = env4.env_allvars;
  }


(* Translate a whole program *)        

let prog ((t,x):toplevel list ext) : cfg =
  (* initial environment *)
  arcs := [];
  nodes := [];
  let env_init = 
    { env_vars = StringMap.empty;
      env_funcs = StringMap.empty;
      env_break = None;
      env_exit = None;
      env_return = None;
      env_allvars = VarSet.empty;
      env_labels = StringMap.empty;
      env_gotos = [];
    }
  in
  (* translate each toplevel instruction *)
  let env, revinit =
    List.fold_left
      (fun (env,revinit) t -> match t with
      | AST_fun_decl (f,_) ->
          func env f, revinit
      | AST_global_decl (d,_) ->
          let env1, inst1 = decls env d in
          env1, List.rev_append inst1 revinit
      )
      (env_init,[]) t
  in
  let init = List.rev revinit in
  (* init code *)
  let entry = create_node (fst x) in
  let exit = create_node (snd x) in
  add_inst entry exit init;
  (* extract program info *)
  let vars = List.rev (VarSet.fold (fun a acc -> a::acc) env.env_allvars []) in
  let funcs = List.rev (StringMap.fold (fun _ f acc -> f::acc) env.env_funcs []) in
  { cfg_vars = vars;
    cfg_funcs = funcs;
    cfg_init_entry = entry;
    cfg_init_exit = exit;
    cfg_nodes = List.rev !nodes;
    cfg_arcs = List.rev !arcs;
  }
    
