open ControlFlowGraph
open CFGPrinter
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator
open ForwardIterator

(* type for CTL properties, instantiated with ControlFlowGraph.bool_expr for atomic
   propositions *)
type ctl_property = ControlFlowGraph.bool_expr CTLProperty.generic_property

let rec print_ctl_property ch (property : ctl_property) =
  match property with
  | Atomic (p, None) -> Printf.printf "%a" print_bool_expr p
  | Atomic (p, Some l) -> Printf.printf "%s: %a" l print_bool_expr p
  | AX p -> Printf.printf "AX{%a}" print_ctl_property p
  | AF p -> Printf.printf "AF{%a}" print_ctl_property p
  | AG p -> Printf.printf "AG{%a}" print_ctl_property p
  | AU (p1, p2) ->
      Printf.printf "AU{%a}{%a}" print_ctl_property p1 print_ctl_property p2
  | EX p -> Printf.printf "EX{%a}" print_ctl_property p
  | EF p -> Printf.printf "EF{%a}" print_ctl_property p
  | EG p -> Printf.printf "EG{%a}" print_ctl_property p
  | EU (p1, p2) ->
      Printf.printf "EU{%a}{%a}" print_ctl_property p1 print_ctl_property p2
  | AND (p1, p2) ->
      Printf.printf "AND{%a}{%a}" print_ctl_property p1 print_ctl_property p2
  | OR (p1, p2) ->
      Printf.printf "OR{%a}{%a}" print_ctl_property p1 print_ctl_property p2
  | NOT p -> Printf.printf "NOT{%a}" print_ctl_property p

type quantifier = UNIVERSAL | EXISTENTIAL

module CTLIterator (D : RANKING_FUNCTION) = struct
  module CFGForwardIteratorB = CFGForwardIterator.CFGForwardIterator (D.B)

  type inv = D.t NodeMap.t

  type fwd_inv = D.B.t NodeMap.t

  let printFwdInv fmt (inv : D.B.t NodeMap.t) =
    let printState node a =
      Format.fprintf fmt "[%d]: %a\n" node.node_id D.B.print a
    in
    NodeMap.iter printState inv

  let printInv fmt (inv : inv) robust =
    (* TODO with robust *)
    let print = if robust then D.print else D.print in
    let inv = if !compress then NodeMap.map D.compress inv else inv in
    let printState node a =
      if !dot then
        Format.fprintf fmt "[%d:]:\n%a\nDOT: %a\n" node.node_id print a
          D.print_graphviz_dot a
      else Format.fprintf fmt "[%d:]:\n%a\n" node.node_id print a
    in
    let printState =
      if robust then fun node a ->
        if node.node_id = 1 then printState node a else ()
      else printState
    in
    NodeMap.iter printState inv

  (* assign corresponding state for atomic proposition to each node *)
  let atomic bot (cfg : cfg) (property : bool_expr) : inv =
    (* abstract state of all nodes in cfg*)
    let atomicState = D.reset bot (Conversion.of_bool_expr property) in
    (* create node map that assigns atomicState to all nodes*)
    List.fold_left
      (fun map node -> NodeMap.add node atomicState map)
      NodeMap.empty cfg.cfg_nodes

  (* assign corresponding atomic state for atomic state to each nodes with
     given label *)
  let labeled_atomic bot (cfg : cfg) (label : string) (property : bool_expr)
      : inv =
    (* node map that assigns bottom to all nodes *)
    let botNodeMap =
      List.fold_left
        (fun map n -> NodeMap.add n bot map)
        NodeMap.empty cfg.cfg_nodes
    in
    (* abstract state of all nodes with label in cfg*)
    let atomicState = D.reset bot (Conversion.of_bool_expr property) in
    (* create node map that assigns atomicState to all nodes that have an
       incoming arc with that label*)
    let aux map arc =
      match arc.arc_inst with
      | CFG_label l ->
          if String.equal l label then
            NodeMap.add arc.arc_dst atomicState map
          else map
      | _ -> map
    in
    List.fold_left aux botNodeMap cfg.cfg_arcs

  (* CTL 'or' opperator *)
  let logic_or (fp1 : inv) (fp2 : inv) : inv =
    let f _ t1 t2 = D.join COMPUTATIONAL t1 t2 in
    NodeMap.map2 f fp1 fp2

  (* CTL 'and' opperator *)
  let logic_and (fp1 : inv) (fp2 : inv) : inv =
    let f _ t1 t2 = D.meet COMPUTATIONAL t1 t2 in
    NodeMap.map2 f fp1 fp2

  (* CTL 'not' opperator *)
  let logic_not (fp : inv) : inv = NodeMap.map D.complement fp

  (* return appropriate 'join' 'assign' and 'filter' implementations based on
     given quantifier *)
  let transform_functions (quantifier : quantifier) =
    let join =
      D.join
        (if quantifier == UNIVERSAL then APPROXIMATION else COMPUTATIONAL)
    in
    let bwdAssign = D.bwdAssign ~underapprox:(quantifier == EXISTENTIAL) in
    let filter = D.filter ~underapprox:(quantifier == EXISTENTIAL) in
    (join, bwdAssign, filter)

  (* process instructions of a given arc in CFG *)
  let process_inst (quantifier : quantifier) ((out_state, inst) : D.t * inst)
      =
    let join, bwdAssign, filter = transform_functions quantifier in
    match inst with
    | CFG_skip _ -> out_state
    | CFG_label _ -> out_state
    | CFG_assign (var, expr) ->
        bwdAssign out_state
          ( AbstractSyntax.A_var (Conversion.of_var var)
          , Conversion.of_int_expr expr )
    | CFG_elm_assign (var, idx, expr) ->
        raise (Invalid_argument "array element assignment is not yet supported")
    | CFG_arr_assign (var, aexpr) ->
        raise (Invalid_argument "array assignment is not yet supported")
    | CFG_guard bexpr -> filter out_state @@ Conversion.of_bool_expr bexpr
    | CFG_assert bexpr -> filter out_state @@ Conversion.of_bool_expr bexpr
    | CFG_call bexpr ->
        raise (Invalid_argument "function calls not supported")

  (* CTL 'next' operator *)
  let next (quantifier : quantifier) (bot : D.t) (cfg : cfg) (inv : inv) :
      inv =
    let join, _, _ = transform_functions quantifier in
    let aux (nodeMap : inv) (node : node) =
      let outEdges =
        List.map
          (fun arc -> (NodeMap.find arc.arc_dst inv, arc.arc_inst))
          node.node_out
      in
      let outStates = List.map (process_inst quantifier) outEdges in
      let joindOutStates =
        match outStates with
        | [] -> bot
        | [s] -> s
        | [s1; s2] -> join s1 s2
        | s :: ss -> List.fold_left join s ss
      in
      NodeMap.add node joindOutStates nodeMap
    in
    List.fold_left aux NodeMap.empty cfg.cfg_nodes

  (* CTL 'until' operator *)
  let until (quantifier : quantifier) (fwd_inv : fwd_inv) (inv_keep : inv)
      (inv_reset : inv) : D.t CFGInterpreter.abstract_transformer =
    let join, bwdAssign, filter = transform_functions quantifier in
    let abstract_transformer
        (node : node) (* current node that is being processed *)
        (loop_head : bool) (* current node is loop head *)
        (iter_count : int)
          (* iteration count, how many times has this node been processed *)
        (current_state : D.t)
          (* current value of abstract state for this node *)
        (out_edges : (D.t * inst) list) : bool * D.t =
      let keepState = NodeMap.find node inv_keep in
      let resetState = NodeMap.find node inv_reset in
      (* process all outgoing edges and get resp. abstract states for each
         out edge after applying its instruction*)
      let outStates = List.map (process_inst quantifier) out_edges in
      (* join states *)
      let joindOutStates =
        match outStates with
        | [] -> current_state
        | [s] -> s
        | [s1; s2] -> join s1 s2
        | s :: ss -> List.fold_left join s ss
      in
      (* do optional refinement of joined states *)
      let joindOutStates =
        if !refine then D.refine joindOutStates (NodeMap.find node fwd_inv)
        else joindOutStates
      in
      (* apply 'until' operator to compute new state for this node *)
      let newState = D.until joindOutStates keepState resetState in
      if !CFGInterpreter.trace_states then
        Format.fprintf !fmt "old_state: \n%a \nnew_state: \n%a \n" D.print
          (D.compress current_state)
          D.print (D.compress newState) ;
      if List.length outStates < 2 || not loop_head then
        (* no widening if this this is not a loop head point *)
        (false, newState)
      else
        (* check if newState is fixed point *)
        let isLeqComp = D.isLeq COMPUTATIONAL newState current_state in
        let isLeqApprox = D.isLeq APPROXIMATION newState current_state in
        if isLeqComp && isLeqApprox then
          (* fixed point reached for this node *)
          (true, newState)
        else
          (* abstract state is still increasing *)
          let newState' =
            if iter_count <= !joinbwd then
              (* keep going while widening threshold not reached *)
              newState
            else if not isLeqComp then (
              (* widening threshold reached, apply widening *)
              let widenedNewState =
                D.widen current_state
                  (D.join COMPUTATIONAL current_state newState)
              in
              (* NOTE: the join might be necessary to ensure termination
                 because of a bug (???) *)
              if !CFGInterpreter.trace_states then
                Format.fprintf !fmt "widenedNewState: \n %a \n" D.print
                  (D.compress widenedNewState) ;
              widenedNewState )
            else
              let widenedNewState = D.widen current_state newState in
              (* widening threshold reached, apply widening *)
              if !CFGInterpreter.trace_states then
                Format.fprintf !fmt "widenedNewState: \n %a \n" D.print
                  (D.compress widenedNewState) ;
              widenedNewState
          in
          (false, newState')
    in
    abstract_transformer

  (* CTL 'global' operator *)
  let global (quantifier : quantifier) (fwd_inv : fwd_inv) :
      D.t CFGInterpreter.abstract_transformer =
    let join, bwdAssign, filter = transform_functions quantifier in
    let abstract_transformer
        (node : node) (* current node that is being processed *)
        (loop_head : bool) (* current node is loop head *)
        (iter_count : int)
          (* iteration count, how many times has this node been processed *)
        (current_state : D.t)
          (* current value of abstract state for this node *)
        (out_edges : (D.t * inst) list) : bool * D.t =
      (* process all outgoing edges and get resp. abstract states for each
         out edge after applying its instruction*)
      let outStates = List.map (process_inst quantifier) out_edges in
      (* join states *)
      let joindOutStates =
        match outStates with
        | [] -> current_state
        | [s] -> s
        | [s1; s2] -> join s1 s2
        | s :: ss -> List.fold_left join s ss
      in
      (* do optional refinedment of joined states *)
      let joindOutStates =
        if !refine then D.refine joindOutStates (NodeMap.find node fwd_inv)
        else joindOutStates
      in
      (* apply 'mask' operator to get new state for this node. This removes
         all partitions from the current state that are not also part of the
         newly computed state. *)
      let newState = D.mask current_state joindOutStates in
      if !CFGInterpreter.trace_states then
        Format.fprintf !fmt "old_state: \n%a \nnew_state: \n%a \n" D.print
          (D.compress current_state)
          D.print (D.compress newState) ;
      if List.length outStates < 2 || not loop_head then
        (* no widening if this this is not a loop head point *)
        (false, newState)
      else
        (* check if greatest fixed point is reached *)
        let isLeqApprox = D.isLeq APPROXIMATION current_state newState in
        let isLeqComp = D.isLeq COMPUTATIONAL current_state newState in
        if isLeqComp && isLeqApprox then
          (* greatest fixed point reached for this node *)
          (true, newState)
        else
          let newState' =
            if iter_count <= !joinbwd then
              (* keep going while widening threshold not reached *)
              newState
            else
              (* use dual_widen after widening threshold reached *)
              let widenedNewState = D.dual_widen current_state newState in
              if !CFGInterpreter.trace_states then
                Format.fprintf !fmt "widenedNewState: \n %a \n" D.print
                  widenedNewState ;
              widenedNewState
          in
          (false, newState')
    in
    abstract_transformer

  let compute (cfg : cfg) (main : ControlFlowGraph.func) (loop_heads : bool NodeMap.t)
      (domSets : NodeSet.t NodeMap.t)
      robust (property : ctl_property) : inv =
    let backwardAnalysis = CFGInterpreter.backward_analysis in
    let env, vars = Conversion.env_vars_of_cfg cfg in
    let print_inv property inv =
      if not !minimal then (
        Printf.printf "Property: %a\n\n" print_ctl_property property ;
        printInv !fmt inv robust )
    in
    let bot = D.bot env vars in
    let zero = D.zero env vars in
    let atomic_true_inv = atomic bot cfg (CFG_bool_const true) in
    let init_bot = const_node_map cfg bot in
    let fwdInv =
      if !refine then CFGForwardIteratorB.compute cfg main loop_heads domSets
      else NodeMap.empty
    in
    if (not !minimal) && !refine then
      Format.fprintf !fmt "Forwad Analysis: \n%a \n" printFwdInv fwdInv ;
    let rec inv (property : ctl_property) : inv =
      let result =
        match property with
        | Atomic (b, None) -> atomic bot cfg b
        | Atomic (b, Some l) -> labeled_atomic bot cfg l b
        | AX p -> next UNIVERSAL bot cfg (inv p)
        | AG p ->
            let pInv = inv p in
            (* exit_node is set to bot for inital value of backward
               analysis *)
            let init_value = NodeMap.add main.func_exit bot pInv in
            backwardAnalysis (global UNIVERSAL fwdInv) init_value
              main.func_exit cfg loop_heads domSets
        | AU (p1, p2) ->
            let abstractTransformer =
              until UNIVERSAL fwdInv (inv p1) (inv p2)
            in
            backwardAnalysis abstractTransformer init_bot main.func_exit cfg
              loop_heads domSets
        | AF p ->
            let abstractTransformer =
              until UNIVERSAL fwdInv atomic_true_inv (inv p)
            in
            backwardAnalysis abstractTransformer init_bot main.func_exit cfg
              loop_heads domSets
        | EX p ->
            if !ctl_existential_equivalence then
              (* use the following equivalence relation: EX(p) := not AX(not
                 p) *)
              inv (NOT (AX (NOT p)))
            else next EXISTENTIAL bot cfg (inv p)
        | EG p ->
            if !ctl_existential_equivalence then
              (* use the following equivalence realtion: EG(p) := not AF(not
                 p) *)
              inv (NOT (AF (NOT p)))
            else
              let pInv = inv p in
              (* exit_node is set to bot for inital value of backward
                 analysis *)
              let init_value = NodeMap.add main.func_exit bot pInv in
              backwardAnalysis
                (global EXISTENTIAL fwdInv)
                init_value main.func_exit cfg loop_heads domSets
        | EU (p1, p2) ->
            if !ctl_existential_equivalence then
              raise
                (Invalid_argument
                   "existential equivalence conversion not supported for \
                    'until' operator" )
            else
              let abstractTransformer =
                until EXISTENTIAL fwdInv (inv p1) (inv p2)
              in
              backwardAnalysis abstractTransformer init_bot main.func_exit
                cfg loop_heads domSets
        | EF p ->
            if !ctl_existential_equivalence then (
              (* use the following equivalence relation: EF(p) := not AG(not
                 p) *)
              (* compute: not p*)
              let inv_not_p = inv (NOT p) in
              (* compute: 'AG (not p)' starting from 'zero' instead of 'bot'
                 to capture finite traces with the 'AG' operator *)
              let init_value = NodeMap.add main.func_exit zero inv_not_p in
              let inv_ag =
                backwardAnalysis (global UNIVERSAL fwdInv) init_value
                  main.func_exit cfg loop_heads domSets
              in
              print_inv (AG (NOT p)) inv_ag ;
              (* compute: not AG(not p) *)
              logic_not inv_ag )
            else
              let abstractTransformer =
                until EXISTENTIAL fwdInv atomic_true_inv (inv p)
              in
              backwardAnalysis abstractTransformer init_bot main.func_exit
                cfg loop_heads domSets
        | AND (p1, p2) -> logic_and (inv p1) (inv p2)
        | OR (p1, p2) -> logic_or (inv p1) (inv p2)
        | NOT (Atomic (property, None)) ->
            let negatedProperty = negate_bool_expr property in
            atomic bot cfg negatedProperty
        | NOT p -> logic_not (inv p)
      in
      print_inv property result ; result
    in
    inv property

  let analyze ?(precondition = CFG_bool_const true) (cfg : cfg) robust
      (main : ControlFlowGraph.func) (loop_heads : bool NodeMap.t)
      (domSets : NodeSet.t NodeMap.t)
      (property : ctl_property) =
    let inv = compute cfg main loop_heads domSets robust property in
    let programInvariant = NodeMap.find main.func_entry inv in
    let precondition = Conversion.of_bool_expr precondition in
    D.defined ~condition:precondition programInvariant
end
