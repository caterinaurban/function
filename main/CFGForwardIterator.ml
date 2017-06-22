(*   
     ********* CFG based Forward Iterator ************
            Copyright (C) 2017 
             Samuel Ueltschi
                ETH Zurich
*)


open Cfg
open CFGInterpreter
open Partition
open Iterator

module CFGForwardIterator (B: PARTITION) = struct

  let process_inst (in_state, inst: (B.t * inst)) =
    match inst with
    | CFG_skip _ -> in_state
    | CFG_label _ -> in_state
    | CFG_assign (var, expr) -> 
      B.fwdAssign in_state (AbstractSyntax.A_var (Conversion.of_var var), Conversion.of_int_expr expr)
    | CFG_guard bexpr -> B.filter in_state @@ Conversion.of_bool_expr bexpr
    | CFG_assert bexpr -> B.filter in_state @@ Conversion.of_bool_expr bexpr
    | CFG_call bexpr -> raise (Invalid_argument "function calls not supported")


  let abstract_transformer 
      (node:node) (* current node that is being processed *)
      (iter_count:int) (* iteration count, how many times has this node been processed *)
      (current_state:B.t) (* current value of abstract state for this node *)
      (in_edges: (B.t * inst) list): (bool * B.t) =
    (* process instructions *)
    let inStates = List.map process_inst in_edges in
    (* join states *)
    let newState = match inStates with 
      | [] -> current_state
      | s::[] -> s
      | s1::s2::[] -> B.join s1 s2
      | s::ss -> List.fold_left B.join s ss
    in
    if !CFGInterpreter.trace_states then begin
      Format.fprintf !fmt "old_state: %a \nnew_state: %a \n" B.print current_state B.print newState;
    end;
    if List.length inStates < 2 then 
      (* don't check for convergence if this this is not a branch point *)
      (false, newState)
    else 
      if B.isLeq newState current_state then 
        (* fixed point*)
        (true, current_state) 
      else
        (* apply widenging after fixed iteration count *)
        let newState' = 
          if iter_count <= !joinfwd then 
            newState 
          else 
            let widenedNewState = B.widen current_state newState in
            if !CFGInterpreter.trace_states then 
              Format.fprintf !fmt "widenedNewState: \n %a \n" B.print widenedNewState;
            widenedNewState
        in
        (false, newState')


  (* compute invariant map based on forward analysis *)
  let compute (cfg:cfg) (main:func) : B.t NodeMap.t = 
    let (env, vars) = Conversion.env_vars_of_cfg cfg in
    let bot = B.bot env vars in
    let top = B.top env vars in
    let initialState = List.fold_left
        (fun map node -> NodeMap.add node bot map) 
        NodeMap.empty cfg.cfg_nodes 
    in
    let initialState = NodeMap.add cfg.cfg_init_entry top initialState in
    let fwdInv = forward_analysis abstract_transformer initialState cfg.cfg_init_entry cfg in
    let mainStartState = NodeMap.find cfg.cfg_init_exit fwdInv in
    let initialState = NodeMap.add main.func_entry mainStartState fwdInv in
    forward_analysis abstract_transformer initialState main.func_entry cfg

end
