open Cfg

(* Computes reverse-post-order (starting from 1) for the nodes of 'cfg'
   starting from 'initial_node' *)
let reverse_postorder (initial_node : node) : int NodeMap.t =
  (* reference that counts the order in which the nodes where visited in
     post-order*)
  let c = ref 1 in
  let rec dfs (node : node) (postOrderNodes : int NodeMap.t) =
    (* Check if this node has an entry in 'postOrderNode' i.e. was already
       visited*)
    if NodeMap.mem node postOrderNodes then postOrderNodes
    else
      (* assign -1 to current node to mark it as visited *)
      let postOrderNodes' = NodeMap.add node (-1) postOrderNodes in
      (* recursively visit all neighbors *)
      let postOrderNodes'' =
        List.fold_left
          (fun acc out_arc -> dfs out_arc.arc_dst acc)
          postOrderNodes' node.node_out
      in
      let order = !c in
      c := order + 1 ;
      NodeMap.add node order postOrderNodes''
  in
  (* compute post-order starting from initial node*)
  let postOrder = dfs initial_node NodeMap.empty in
  let cc = !c in
  (* revert post-order, first visited becomes last and vice-versa *)
  NodeMap.map (fun order -> cc - order) postOrder

(* Compute dominator set for every node in mainFunc. For now this is a naive
   O(n^2) solution based on the maximal solution for the following set of
   equations:

   dom(n_init) = {n_init} dom(n) = {n} union with intersection over dom(p)
   for all p in pred(n) *)
let dominator (cfg : cfg) (mainFunc : func) : NodeSet.t NodeMap.t =
  let intersection sets =
    match sets with
    | [] -> NodeSet.empty
    | [x] -> x
    | x :: xs -> List.fold_left NodeSet.inter x xs
  in
  let dom = ref NodeMap.empty in
  let set_dom node dominators = dom := NodeMap.add node dominators !dom in
  let get_dom node = NodeMap.find node !dom in
  let funcNodes = func_nodes mainFunc in
  let funcNodesWithoutInitial =
    List.filter (fun n -> n != mainFunc.func_entry) funcNodes
  in
  let allNodes = NodeSet.of_list funcNodes in
  (* the dominator of the inital node is the initial node *)
  set_dom mainFunc.func_entry (NodeSet.singleton mainFunc.func_entry) ;
  (* set all nodes as dominator for each node except the initial one*)
  List.iter (fun node -> set_dom node allNodes) funcNodesWithoutInitial ;
  let updateDom n =
    (* Intersect all dominators of predecessor nodes of n*)
    let dom_inters =
      intersection (List.map (fun arc -> get_dom arc.arc_src) n.node_in)
    in
    (* form union with {n} *)
    let new_dom = NodeSet.union (NodeSet.singleton n) dom_inters in
    let old_dom = get_dom n in
    (* update dom(n) *)
    set_dom n new_dom ;
    (* return if anything changed *)
    not @@ NodeSet.equal new_dom old_dom
  in
  let change = ref true in
  while !change do
    (* call updateDom for each node except the inital one, keep going as long
       as 'dom' still changes *)
    change :=
      List.fold_left
        (fun acc n -> updateDom n || acc)
        false funcNodesWithoutInitial
  done ;
  !dom

(* check if 'u' dominates 'v' *)
let dominates (dom : NodeSet.t NodeMap.t) (u : node) (v : node) =
  let dom_v = NodeMap.find v dom in
  (* dominators of v *)
  NodeSet.mem u dom_v (* is u a dominator of v*)

(* an edge "u --> v" is a retreating edge if v comes before u in
   reverse-postorder *)
let is_retreating_edge (reverse_postorder : int NodeMap.t) (arc : arc) =
  let u = arc.arc_src in
  let v = arc.arc_dst in
  NodeMap.find u reverse_postorder > NodeMap.find v reverse_postorder

(* an edge "u --> v" is a back edge if its retreating and if v dominates u,
   assumes that the cfg is reducible *)
let is_back_edge (dom : NodeSet.t NodeMap.t) (arc : arc) =
  dominates dom arc.arc_dst arc.arc_src

(* A cfg is reducible if the following condition holds for all edges "u -->
   v":

   edge is retrating => v dominates u *)
let is_reducible (cfg : cfg) (mainFunc : func) (dom : NodeSet.t NodeMap.t)
    (reverse_postorder : int NodeMap.t) : bool =
  let valid_edge arc =
    let u = arc.arc_src in
    let v = arc.arc_dst in
    if NodeMap.mem u reverse_postorder && NodeMap.mem v reverse_postorder
    then (
      let res =
        (not (is_retreating_edge reverse_postorder arc))
        || dominates dom arc.arc_dst arc.arc_src
      in
      if not res then
        Printf.printf "Edge (%d, %d) invalid\n" u.node_id v.node_id ;
      res )
    else (* arc is not part of mainFunc, therefore we ignore it*)
      true
  in
  List.for_all valid_edge cfg.cfg_arcs

(* Check for each node in cfg if it is a loop head i.e. if there is an
   incoming back_edge *)
let loop_heads (cfg : cfg) (dom : NodeSet.t NodeMap.t) : bool NodeMap.t =
  let isLoopHead node =
    NodeMap.mem node dom
    && (* check if node is part of main function *)
    List.exists (is_back_edge dom)
      node.node_in (* check if node is destination of a back edge *)
  in
  List.fold_left
    (fun map n -> NodeMap.add n (isLoopHead n) map)
    NodeMap.empty cfg.cfg_nodes

(* Check if cfg is reducible and if so returns all loop heads. If cfg is not
   reducible then a map of possible loop heads is return where each node that
   is a branch point is marked as possible loop head *)
let possible_loop_heads (cfg : cfg) (mainFunc : func) : bool NodeMap.t =
  let rpo = reverse_postorder mainFunc.func_entry in
  let dom = dominator cfg mainFunc in
  let reducible = is_reducible cfg mainFunc dom rpo in
  if reducible then loop_heads cfg dom
  else
    List.fold_left
      (fun map n -> NodeMap.add n (List.length n.node_out > 2) map)
      NodeMap.empty cfg.cfg_nodes
