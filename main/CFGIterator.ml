open Cfg

(* Map module for cfg nodes *)
module NodeMap = Map.Make(Node)

(**
    Module signature that defines abstract transformers used by 'backward_analysis' 
*)
module type CFGIterator = sig

  (* type of abstract state that is computed for each node in cfg *)
  type t 

  (**
     abstract transformer that is used to processes node
  *)
  val process_node: 
    node -> (* current node that is being processed *)
    int -> (* iteration count, how many times has this node been processed *)
    t -> (* current value of abstract state for this node *)
    (t * inst) list (** 
                       list of (abstract state, instruction) pairs for all 
                       predecessors nodes that have an incoming edge from this node. 
                    *)
    -> t (* resulting new abstract state for this node *)

  (** 
     lessEqual realtion that is used to determine if the computed state of a node
     is still increasing i.e. has changed.  

     example:
     s  = abstract state at node
     s' = new abstract state at node after applying abstract transformer

     The analysis propagates s' to all predecessors of the node current if s <= s'
  *)
  val lessEqual: t -> t -> bool
end


(* abstract implementation of a backward analysis over a control flow graph *)
let backward_analysis
    (type a) (* type of the abstract state that is computed for each node in 'cfg' *)
    (module I : CFGIterator with type t = a) (* module that implements the abstract transformers for the analysis *)
    (initial_value:a NodeMap.t) (* map that assigns an initial state to each node in the cfg *)
    (main:func) (* entry point of the program *)
    (cfg:cfg) (* control flow graph *)
    : a NodeMap.t (* returns a map that assigns each node an abstract state that is the result of the backward analysis *)
  =
  let nodeCount = List.length cfg.cfg_nodes in
  (* Create empty worklist *)
  let worklist = Queue.create () in 
  (* bitmap array that keeps track of all nodes that are in the worklist *)
  let inWorklist = Array.make (nodeCount + 1) false in 
  (* Adds all predecessors of a node to the worklist *)
  let addPredecessorsToWorklist node = List.iter (fun arc -> 
      let predecessor = arc.arc_src in
      Queue.add predecessor worklist; (* add predecessor to worklist *)
      inWorklist.(predecessor.node_id) <- true (* update inWorklist array*)
    ) node.node_in
  in
  (** 
     array that counts the number of times a node as heen processed
     NOTE: this assumes that nodes have ids numbered from 1...n and that the 
     array is accessed through this id e.g. processed.(i) where 'i' is the node id
  *)
  let processed = Array.make (nodeCount + 1) 0 in 
  (** 
     auxiliary function that implements the worklist algorithm expressed in tail-recursive form.
     Takes an inital 'nodeMap' as argument that assigns an abstract state to each node and returns a final 'nodeMap'
     containing the result of the analysis.
  *)
  let rec aux (nodeMap: a NodeMap.t): a NodeMap.t = 
    let getState node = NodeMap.find node nodeMap in
    if Queue.is_empty worklist then nodeMap (* Work list is empty => exit *)
    else
      (* Take node from worklist *)
      let node = Queue.take worklist in
      (* update inWorklist array *)
      inWorklist.(node.node_id) <- false;
      (* Find current state of node *)
      let currentState = getState node in
      (* find states of all successors and convert to list of (a, inst) tuples for each successor *)
      let instStatePairs = List.map (fun arc -> (getState arc.arc_dst, arc.arc_inst)) node.node_out in
      (* number of times this node has been processed *)
      let nodeProcessed = processed.(node.node_id) in
      (* run abstract transformer for node to get new abstract state *)
      let newState = I.process_node node processed.(node.node_id) currentState instStatePairs in 
      (* update 'processed' count *)
      processed.(node.node_id) <- nodeProcessed + 1; 
      if (I.lessEqual newState currentState) then 
        (*if newState <= currentState i.e. the abstract state didn't increase then we are done for this node *)
        NodeMap.add node newState nodeMap
      else 
        (* otherwise we add all predecessors of node to the worklist, update 'worklist', update 'nodeMap' and contionue with the algorithm *)
        let newNodeMap = NodeMap.add node newState nodeMap in (* update nodeMap with new state *)
        addPredecessorsToWorklist node;
        aux newNodeMap (* continute with algorithm *)
  in
  (* Add all predecessors of exit node to worklist *)
  addPredecessorsToWorklist main.func_exit;
  (* Run analysis *)
  aux initial_value
