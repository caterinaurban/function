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


let trace = ref false
let trace_states = ref false


(** 
   type definition for abstact transformer.
   
   an abstract transformer is a function that transforms a given abstract
   w.r.t the semantics of a node in the control-flow-graph (cfg)
*)
type 't abstract_transformer  = 
  node -> (* current node that is being processed *)
  int -> (* iteration count, how many times has this node been processed *)
  't -> (* current value of abstract state for this node *)
  ('t * inst) list (** 
                       list of (abstract state, instruction) pairs for all 
                       predecessors nodes that have an incoming edge from this node. 
                   *)
  -> (bool * 't) (* returns a boolean flag that indicates if a fixed point was 
                    reached for this node and the updated state for this node *)
     

type 't print_state = Format.formatter -> 't -> unit

let print_worklist fmt queue = 
  Format.fprintf fmt "worklist = [";
  Queue.iter (fun node -> Format.fprintf fmt "%d " node.node_id) queue;
  Format.fprintf fmt "]"

(* abstract implementation of a backward analysis over a control flow graph *)
let backward_analysis
    (type a) (* type of the abstract state that is computed for each node in 'cfg' *)
    ?(print_state:a print_state option)
    (abstract_transformer: a abstract_transformer) (* implementation of abstract transformer for this analysis *)
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
  (* Adds all predecessors of 'node' to the worklist *)
  let addPredecessorsToWorklist node = List.iter (fun arc -> 
      let predecessor = arc.arc_src in
      if not inWorklist.(predecessor.node_id) then begin
        Queue.add predecessor worklist; (* add predecessor to worklist *)
        inWorklist.(predecessor.node_id) <- true (* update inWorklist array*)
      end
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
      if !trace then Format.fprintf !fmt "### processing node %d: \n" node.node_id; 
      (* run abstract transformer for node to get new abstract state *)
      let (fixedPoint, newState) = abstract_transformer node processed.(node.node_id) currentState instStatePairs in 
      (* update 'processed' count *)
      processed.(node.node_id) <- nodeProcessed + 1; 
      (* add predecessors of node to worklist if either we didn't reach a fixed point 
         or if this is the first time we process this node *)
      if not fixedPoint || processed.(node.node_id) == 1 then addPredecessorsToWorklist node;
      if !trace then Format.fprintf !fmt "-fixedPoint: %b \n-new worklist: %a \n######### \n \n"
          fixedPoint print_worklist worklist;
      (* add newState to nodeMap and continute with algorithm *)
      aux @@ NodeMap.add node newState nodeMap 
  in
  (* Add exit node to worklist *)
  Queue.add main.func_exit worklist;
  if !trace then Format.fprintf !fmt "### starting backward analysis: %a \n" print_worklist worklist;
  (* Run analysis *)
  aux initial_value
