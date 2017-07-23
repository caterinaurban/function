(***************************************************)
(*                                                 *)
(*         CFG Based Backward Interpreter          *)
(*                                                 *)
(*            Samuel Marco Ueltschi                *)
(*           ETH Zurich, Switzerland               *)
(*                   2017                          *)
(*                                                 *)
(***************************************************)

open Cfg
open Iterator


(* log worklist algorithm *)
let trace = ref false
(* log worklist algorithm and show how states change *)
let trace_states = ref false


(** 
   type definition for abstact transformer.
   
   an abstract transformer is a function that transforms a given abstract
   w.r.t the semantics of a node in the control-flow-graph (cfg)
*)
type 't abstract_transformer  = 
  node -> (* current node that is being processed *)
  bool -> (* is current node a loop head *)
  int -> (* iteration count, how many times has this node been processed *)
  't -> (* current value of abstract state for this node *)
  ('t * inst) list (** 
                       list of (abstract state, instruction) pairs for all 
                       predecessors (successor) nodes that have an incoming (outgoing) edge from (to) this node. 
                   *)
  -> (bool * 't) (* returns a boolean flag that indicates if a fixed point was 
                    reached for this node and the updated state for this node *)
     
let print_worklist fmt queue = 
  Format.fprintf fmt "worklist = [";
  Queue.iter (fun node -> Format.fprintf fmt "%d " node.node_id) queue;
  Format.fprintf fmt "]"


type analysis_type = | FORWARD | BACKWARD 

(* abstract implementation of a backward/forward analysis over a control flow graph *)
let execute
    (type a) (* type of the abstract state that is computed for each node in 'cfg' *)
    (analysis_type:analysis_type) (* type of analysis, either forward or backward *)
    (abstract_transformer: a abstract_transformer) (* implementation of abstract transformer for this analysis *)
    (initial_value:a NodeMap.t) (* map that assigns an initial state to each node in the cfg *)
    (entry_node:node) (* entry point of the program analysis *)
    (cfg:cfg) (* control flow graph *)
    (loop_heads:bool NodeMap.t)
    : a NodeMap.t (* returns a map that assigns each node an abstract state that is the result of the analysis *)
  =
  let nodeCount = List.length cfg.cfg_nodes in
  (* Create empty worklist *)
  let worklist = Queue.create () in 
  (* bitmap array that keeps track of all nodes that are in the worklist *)
  let inWorklist = Array.make (nodeCount + 1) false in 
  (* add all predecessors of 'node' to worklist if they are not already in it*)
  let addPredecessorsToWorklist node = List.iter (fun arc -> 
      let predecessor = arc.arc_src in
      if not inWorklist.(predecessor.node_id) then begin
        Queue.add predecessor worklist; (* add predecessor to worklist *)
        inWorklist.(predecessor.node_id) <- true (* update inWorklist array*)
      end
    ) node.node_in
  in
  (* add all successors of 'node' to worklist if they are not already in it*)
  let addSuccessorsToWorklist node = List.iter (fun arc -> 
      let successor = arc.arc_dst in
      if not inWorklist.(successor.node_id) then begin
        Queue.add successor worklist; (* add successor to worklist *)
        inWorklist.(successor.node_id) <- true (* update inWorklist array*)
      end
    ) node.node_out
  in
  (* choose how to update worklist based on analysis type *)
  let updateWorklist = match analysis_type with 
    | FORWARD -> addSuccessorsToWorklist
    | BACKWARD -> addPredecessorsToWorklist
  in
  (* get all states / instructions from predecessor nodes*)
  let getIncomingStates nodeMap node = 
    List.map (fun arc -> (NodeMap.find arc.arc_src nodeMap, arc.arc_inst)) node.node_in in
  (* get all states / instructions from successor nodes*)
  let getOutgoingStates nodeMap node = 
    List.map (fun arc -> (NodeMap.find arc.arc_dst nodeMap, arc.arc_inst)) node.node_out in
  (* choose which states are passed to abstract transformer based on analysis type *)
  let getStateDependency = match analysis_type with 
    | FORWARD -> getIncomingStates
    | BACKWARD -> getOutgoingStates
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
      (* find states/instructions of successors (predecessor) nodes *)
      let instStatePairs = getStateDependency nodeMap node in
      (* number of times this node has been processed *)
      let nodeProcessed = processed.(node.node_id) in
      if !trace then begin
        Format.fprintf !fmt "### processing node %d (iter: %d): \n" node.node_id nodeProcessed; 
        Pervasives.print_newline ();
      end;
      let isLoopHead = NodeMap.find node loop_heads in
      (* run abstract transformer for node to get new abstract state *)
      let (fixedPoint, newState) = 
        abstract_transformer 
          node 
          isLoopHead 
          processed.(node.node_id) 
          currentState 
          instStatePairs 
      in 
      (* update 'processed' count *)
      processed.(node.node_id) <- nodeProcessed + 1; 
      (* add predecessors (successors) of node to worklist if either we didn't reach a fixed point 
         or if this is the first time we process this node *)
      if not fixedPoint || processed.(node.node_id) == 1 then updateWorklist node;
      if !trace then Format.fprintf !fmt "-fixedPoint: %b \n-new worklist: %a \n######### \n \n"
          fixedPoint print_worklist worklist;
      (* add newState to nodeMap and continute with algorithm *)
      aux @@ NodeMap.add node newState nodeMap 
  in
  (* Add initial node to worklist *)
  Queue.add entry_node worklist;
  if !trace then 
    begin 
      match analysis_type with 
      | BACKWARD -> 
        Format.fprintf !fmt "### starting backward analysis: %a \n" print_worklist worklist;
      | FORWARD -> 
        Format.fprintf !fmt "### starting forward analysis: %a \n" print_worklist worklist;
    end;
  (* Run analysis *)
  aux initial_value


let backward_analysis (type a) (at: a abstract_transformer) = execute BACKWARD at 

let forward_analysis (type a) (at: a abstract_transformer) = execute FORWARD at 

