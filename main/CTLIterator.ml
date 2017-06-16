open AbstractSyntax
open InvMap
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator
open ForwardIterator

(* type for CTL properties, instantiated with bExp for atomic propositions *)
type ctl_property = AbstractSyntax.bExp CTLProperty.generic_property

let atomic_property_of_bexp (b:bExp) = Atomic (b, None)

type quantifier = UNIVERSAL | EXISTENTIAL

let rec print_ctl_property fmt (property:ctl_property) = match property with 
  | Atomic (p, Some l) -> Format.fprintf fmt "%s: %a" l AbstractSyntax.bExp_print_aux p 
  | Atomic (p, None) -> AbstractSyntax.bExp_print_aux fmt p
  | AX p -> Format.fprintf fmt "AX{%a}" print_ctl_property p
  | AF p -> Format.fprintf fmt "AF{%a}" print_ctl_property p
  | AG p -> Format.fprintf fmt "AG{%a}" print_ctl_property p
  | AU (p1,p2) -> Format.fprintf fmt "AU{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | EX p -> Format.fprintf fmt "EX{%a}" print_ctl_property p
  | EF p -> Format.fprintf fmt "EF{%a}" print_ctl_property p
  | EG p -> Format.fprintf fmt "EG{%a}" print_ctl_property p
  | EU (p1,p2) -> Format.fprintf fmt "EU{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | AND (p1,p2) -> Format.fprintf fmt "AND{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | OR (p1,p2) -> Format.fprintf fmt "OR{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | NOT p -> Format.fprintf fmt "NOT{%a}" print_ctl_property p;


(* Bundle commonly used values (AST, Apron env. variable list) to one struct *)
type program = {
  environment: Apron.Environment.t;
  variables: AbstractSyntax.var list;
  mainFunction: AbstractSyntax.func;
  globalBlock: AbstractSyntax.block; 
}

(* Computes the set of all labels of a program *)
let labels_of_program program = 
  let rec stmtLabels s =
    match s with
    | A_if (_, s1, s2) -> List.append (blockLabels s1) (blockLabels s2)
    | A_while (l,(b,ba),s) -> l::(blockLabels s)
    | _ -> []
  and blockLabels b =
    match b with
    | A_empty l -> [l]
    | A_block (l,(s,_),b) -> l::(List.append (stmtLabels s) (blockLabels b))
  in
  (blockLabels program.globalBlock) @ (blockLabels program.mainFunction.funcBody) 


(* get label at start of block *)
let block_label block = 
  match block with
  | A_empty l -> l
  | A_block (l,_,_) -> l


(* generate map that assigns a block to each label in the program *)
let block_label_map block: block InvMap.t = 
  let rec aux (b:block) (map: block InvMap.t) = 
    let map' = InvMap.add (block_label b) b map in 
    match b with
    | A_empty _ -> map'
    | A_block (blockLabel, (stmt, _), nextBlock) ->
      let map'' = aux nextBlock map' in
      match stmt with 
      | A_if (_, bIf, bElse) -> aux bElse (aux bIf map'') 
      | A_while (whileLabel, _, loop_body) -> 
        let map''' = InvMap.add whileLabel b map'' in
        aux loop_body map'''
      | _ -> map''
  in aux block InvMap.empty


(*
   This function takes a given paresed program and introduces a new label called 'exit' 
   before each 'return' statement and at the end of the program

   The augmented program and a corresponding ctl_property 'AF{exit: true}' for termination is returned. 
   This pair can then be used with the usual CTL analysis to check for termination.
*)
let program_of_prog_with_termination (prog: AbstractSyntax.prog) (main: AbstractSyntax.StringMap.key) : (program * ctl_property) =
  let (globalVariables, globalBlock, functions) = prog in
  let mainFunction = StringMap.find main functions in
  let dummyExtent = (Lexing.dummy_pos, Lexing.dummy_pos) in
  let exitLabel = A_label ("exit", dummyExtent) in
  let id = ref (-1) in
  let nextId () = let i = !id in id := i - 1; i in
  let rec addTerminationStmt (block:block) = match block with
    | A_empty l -> A_block (nextId (), (exitLabel, dummyExtent), block)
    | A_block (l,stmt,nextBlock) -> A_block (l, stmt, addTerminationStmt nextBlock)
  in
  let rec addTerminationStmtReturn (block:block) = match block with
    | A_empty l -> block 
    | A_block (l, (A_return,a) , nextBlock) -> 
      let nextBlock = A_block (l, (A_return, a), addTerminationStmtReturn nextBlock) in
      A_block (nextId (), (exitLabel, dummyExtent), nextBlock)
    | A_block (l, stmt, nextBlock) -> A_block (l, stmt, addTerminationStmtReturn nextBlock)
  in
  let augmentedBody = addTerminationStmtReturn @@ addTerminationStmt mainFunction.funcBody in
  let v1 = snd (List.split (StringMap.bindings globalVariables)) in
  let v2 = snd (List.split (StringMap.bindings mainFunction.funcVars)) in
  let vars = List.append v1 v2 in 
  let var_to_apron v = Apron.Var.of_string v.varId in
  let apron_vars = Array.map var_to_apron (Array.of_list vars) in
  let env = Environment.make apron_vars [||] in
  let program = {
    environment = env;
    variables = vars;
    mainFunction = {
      funcName = mainFunction.funcName;
      funcTyp = mainFunction.funcTyp;
      funcArgs = mainFunction.funcArgs;
      funcVars = mainFunction.funcVars; (* add termination var to function variables *)
      funcBody = augmentedBody
    };
    globalBlock = globalBlock;
  } in
  (program, AF (Atomic (A_TRUE, Some "exit")))


let prog_of_program (program:program) : prog = 
  let funcMap = StringMap.add "main" program.mainFunction StringMap.empty in
  let varMap = List.fold_left (fun map var -> StringMap.add var.varId var map) StringMap.empty program.variables in
  (varMap, program.globalBlock, funcMap)

(* bundle values into 'program' struct *)
let program_of_prog (prog: AbstractSyntax.prog) (main: AbstractSyntax.StringMap.key) : program =
  let (globalVariables, globalBlock, functions) = prog in
  let mainFunction = StringMap.find main functions in
  let v1 = snd (List.split (StringMap.bindings globalVariables)) in
  let v2 = snd (List.split (StringMap.bindings mainFunction.funcVars)) in
  let vars = List.append v1 v2 in
  let var_to_apron v = Apron.Var.of_string v.varId in
  let apron_vars = Array.map var_to_apron (Array.of_list vars) in
  let env = Environment.make apron_vars [||] in
  {
    environment = env;
    variables = vars;
    mainFunction = mainFunction;
    globalBlock = globalBlock;
  }


module CTLIterator(D: RANKING_FUNCTION) = struct

  (*
     Fixed Point Computation:

     The fixed point for the 'unit' and 'global' operators are computed by performing a backward-analysis.

     Given a statement, we call the state that holds before the statement the 'in' state, and the state that holds
     after the statement the 'out' state (before and after according to the control-flow). 
     The backward analysis starts with an initial 'out' state at the end of the program
     and propagates it backwards to the start of the program. For each statement, the 'in' state is computed based on the 'out' state.
     At loop heads the final 'in' state is computed by iterating over the loop body until a fixed-point is reached.
  *)

  module ForwardIteratorB = ForwardIterator(D.B)

  (* 
     Type that represents an invariant/fixed-point.
     An invariant assigns an abstract state to each statement of a program
  *)
  type inv = D.t InvMap.t

  let printInv ?fwdInvOpt fmt (inv:inv) =
    let inv = 
      if !compress then 
        InvMap.map D.compress inv
      else 
        inv
    in InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\nDOT: %a\n" label_print l D.print a D.print_graphviz_dot a) inv


  let abstract_transformer (quantifier:quantifier) = match quantifier with
    | UNIVERSAL -> (D.join APPROXIMATION, D.bwdAssign ~underapprox:false, D.filter ~underapprox: false)
    | EXISTENTIAL -> (D.join COMPUTATIONAL, D.bwdAssign ~underapprox:true, D.filter ~underapprox: true)


  (* Computes fixed-point for 'until' properties: AU{inv_keep}{inv_reset} 
     inv_keep and inv_reset are fixed-point for the nested properties

     Applies the 'until' operator of the decision tree domain to each state. 
     This resets the ranking function for those parts of the domain where inv_reset is also 
     defined and it discards those parts of the ranking function where neither inv_keep nor inv_reset are defined.
  *)
  let until (quantifier:quantifier) (fwdInv:label -> D.B.t) (program:program) (inv_keep:inv) (inv_reset:inv) : inv =
    let (branch_join, bwd_assign, bwd_filter) = abstract_transformer quantifier in
    let inv = ref InvMap.empty in (* variable where the resulting invariant/fixed-point is stored *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
    let bot = D.bot program.environment program.variables in
    let start = Sys.time () in
    let rec bwd (out:D.t) (b:block) : D.t = (* recursive function that performs the backward analysis *)
      if (((Sys.time ()) -. start) > !timeout) then raise Timeout; (* check for timeout *)
      match b with
      | A_empty blockLabel ->
        let invBlockKeep = InvMap.find blockLabel inv_keep in
        let invBlockReset = InvMap.find blockLabel inv_reset in
        let out = if !refine then D.refine out (fwdInv blockLabel) else out in
        let in_state = D.until out invBlockKeep invBlockReset in
        if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print in_state;
        addInv blockLabel in_state
      | A_block (blockLabel,(stmt,_),nextBlock) ->
        let invBlockKeep = InvMap.find blockLabel inv_keep in 
        let invBlockReset = InvMap.find blockLabel inv_reset in
        let d_until = fun t -> D.until t invBlockKeep invBlockReset in
        let out_state = bwd out nextBlock in (* recursively process the rest of the program, this gives us the 'out' state for this statement *)
        (* compute 'in' state for this statement *)
        let in_state = match stmt with 
          | A_label (l,_) -> out_state
          | A_return -> bot
          | A_assign ((l,_),(e,_)) -> bwd_assign out_state (l,e)
          | A_assert (b,_) -> bwd_filter out_state b
          | A_if ((b,ba),s1,s2) ->
            let in_if = bwd out_state s1 in (* compute 'in state for if-block*)
            let in_else = bwd out_state s2 in (* compute 'in state for else-block *)
            let in_if_filtered = bwd_filter in_if b in (* filter *)
            let in_else_filtered = bwd_filter in_else (fst (negBExp (b,ba))) in (* filter *)
            branch_join in_if_filtered in_else_filtered (* join the two branches *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = bwd_filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                in_state (* 'in' state of the previous iteration *)
                out_enter (* current 'out' state when entering the loop body *)
                n = (* iteration counter *)
              let in_state' = d_until (D.join APPROXIMATION out_exit out_enter) in (* 'in' state for this iteration *)
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                Format.fprintf !fmt "out_exit: %a\n" D.print out_exit;
                Format.fprintf !fmt "in: %a\n" D.print in_state;
                Format.fprintf !fmt "out_enter: %a\n" D.print out_enter;
                Format.fprintf !fmt "in': %a\n" D.print in_state';
              end;
              let isLeqComp = D.isLeq COMPUTATIONAL in_state' in_state in
              let isLeqApprox = D.isLeq APPROXIMATION in_state' in_state in
              if isLeqComp && isLeqApprox then
                (* fixed-point reached *)
                let fixed_point = in_state in
                if !tracebwd && not !minimal then begin
                  Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l;
                  Format.fprintf !fmt "in_state: %a\n" D.print fixed_point;
                end;
                fixed_point
              else
                (*fixed-point not yet reached, continue with next iteration*)
                let in_state'' = 
                  if n <= !joinbwd then 
                    in_state' (* iteration count below widening threshold *)
                  else if (not isLeqComp) then 
                    D.widen in_state (D.join COMPUTATIONAL in_state in_state') (* widening threshold reached, apply widening *)
                      (* NOTE: the join might be necessary to ensure termination because of a bug (???) *)
                  else 
                    D.widen in_state in_state' (* widening threshold reached, apply widening *)
                in
                if !tracebwd && not !minimal then Format.fprintf !fmt "in'': %a\n" D.print in_state'';
                let out_enter' = bwd_filter (bwd in_state'' loop_body) b in (* process loop body again with updated 'in' state *)
                aux in_state'' out_enter' (n+1) (* run next iteration *)
            in
            let initial_in = bot in (* start with bottom as initial 'in' state *)
            let initial_out_enter = bwd_filter (bwd initial_in loop_body) b in (* process loop body with initial 'in' state *)
            let final_in_state = aux initial_in initial_out_enter 1 in (* compute fixed point for loop-head *)
            addInv l final_in_state 
          | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
          | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
        in
        let in_state = if !refine then D.refine in_state (fwdInv blockLabel) else in_state in
        let in_state = d_until in_state in (* reset ranking functions *)
        if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print in_state;
        addInv blockLabel in_state
    in 
    let _ = bwd bot program.mainFunction.funcBody in (* process entire program starting with bottom *)
    !inv (* return computed program invariant *)


  (*
    Computed fixed-point for 'global' operator (e.g. AG{...}), takes an existing fixed point for the nested property as argument.

    Applies the 'mask' operator to compute the greatest-fixed point starting from the given fixed point for the nested property. 
    During the backward analysis, the reachable state at each statement is computed by inspecting the 'out' states. 
    Then this reachable state is used to shrink down the given fixed-point of the nested property by using 'maks'. 
    To backward analysis for the while-loop uses dual_widening to get convergence.


    By default, the global operator only holds for infinite traces in which the property is satisfied. 
    As a consequence, it's not possible to argue about properties that hold globally until termination. 
    This problem could be solved by adding an endless loop at the end of the program. 
    The result of adding an infinite loop at the end of the program can be simulated by setting the optional 'use_sink_state' parameter to true.
    This will start the backward analysis with a zero state that is defined on the entire domain. By doing so, we avoid cutting away states on finite traces.

  *)
  let global (quantifier:quantifier) (fwdInv:label -> D.B.t) ?(use_sink_state = false) (program:program) (fixed_point:inv) : inv =
    let (branch_join, bwd_assign, bwd_filter) = abstract_transformer quantifier in
    let inv = ref (InvMap.union (fun _ _ _ -> None) fixed_point InvMap.empty) in (* initialize InvMap with given fixed-point for nested property *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
    let blockState block = InvMap.find (label_of_block block) !inv in (* returns current 'in' state of a block *)
    let zero = D.zero program.environment program.variables in 
    let bot = D.bot program.environment program.variables in 
    let start = Sys.time () in
    let rec bwd (out:D.t) (b:block) : D.t = (* recursive function that performs block-wise backward analysis *)
      if (((Sys.time ()) -. start) > !timeout) then raise Timeout; (* check for timeout *)
      let current_in = blockState b in (* current 'in' state for this block *)
      match b with
      | A_empty blockLabel -> 
        let out = if !refine then D.refine out (fwdInv blockLabel) else out in
        addInv blockLabel (D.mask current_in out) 
      | A_block (blockLabel, (stmt,_), nextBlock) ->
        let out_state = bwd out nextBlock in (* recursively process the rest of the program, this gives us the 'out' state for this statement *)
        let new_in = match stmt with 
          | A_label (l,_) -> out_state
          | A_return -> if use_sink_state then zero else bot 
          | A_assign ((l,_),(e,_)) -> D.mask current_in (bwd_assign out_state (l,e))
          | A_assert (b,_) -> D.mask current_in (bwd_filter out_state b)
          | A_if ((b,ba),s1,s2) ->
            let out_if = bwd_filter (bwd out_state s1) b in (* compute 'out' state for if-block*)
            let out_else = bwd_filter (bwd out_state s2) (fst (negBExp (b,ba))) in (* compute 'out' state for else-block *)
            D.mask current_in (branch_join out_if out_else) (* join the two branches and combine with current 'in' state using mask *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = bwd_filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                (current_in:D.t) (* 'in' state of the previous iteration *)
                (out_enter:D.t) (* current 'out' state when entering the loop body *)
                (n:int) : D.t = (* iteration counter *)
              let out_joined = branch_join out_exit out_enter in (* new 'in' state after joining the incoming branches *)
              let updated_in = D.mask current_in out_joined in (* join two branches and combine with current 'in' state using mask *)
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                (* Format.fprintf !fmt "out_exit: %a\n" D.print out_exit; *)
                (* Format.fprintf !fmt "out_enter: %a\n" D.print out_enter; *)
                Format.fprintf !fmt "out_joined: %a\n" D.print out_joined;
                Format.fprintf !fmt "current_in: %a\n" D.print current_in;
                Format.fprintf !fmt "updated_in: %a\n" D.print updated_in;
              end;
              let isLeqApprox = D.isLeq APPROXIMATION current_in updated_in in
              let isLeqComp = D.isLeq COMPUTATIONAL current_in updated_in in
              if isLeqComp && isLeqApprox then
                (* fixed point *)
                let fixed_point = current_in in 
                if !tracebwd && not !minimal then Format.fprintf !fmt "Fixed-Point reached \n";
                fixed_point
              else
                let updated_in' = 
                  if n <= !joinbwd then updated_in (* widening threshold not yet reached *)
                  else D.dual_widen current_in updated_in (* use dual_widen after widening threshold reached *)
                in
                let out_enter' = bwd_filter (bwd updated_in' loop_body) b in (* process loop body again with updated 'in' state *)
                (* next iteration *)
                aux updated_in' out_enter' (n+1)
            in
            let initial_out_enter = bwd_filter (bwd current_in loop_body) b in (* process loop body with current 'in' state at loop-head *)
            let final_in_state = aux current_in initial_out_enter 1 in (* compute fixed point for while-loop starting with current 'in' state at loop-head *)
            addInv l final_in_state 
          | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
          | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
        in
        let new_in = if !refine then D.refine new_in (fwdInv blockLabel) else new_in in
        addInv blockLabel new_in (* use mask to compute the new 'in' state for this block *)
    in 
    let _ = bwd (if use_sink_state then zero else bot) program.mainFunction.funcBody in (* run backward analysis starting from bottom *)
    !inv

  (* 
    Computes fixed-point for 'next' operator (e.g. AX{...})
    For each program label, this function joins all 'out' states and sets this value as the new invariant.
    This computatoin is straight-forward with the exception of empty blocks. 
    There we need to inject the 'out' state of the next basic block in the control-flow-graph. 
    This is done by passing in said state through the recursion of the backward analysis.
  *)
  let next (quantifier:quantifier) (program:program) (fp:inv) : inv =
    let (branch_join, bwd_assign, bwd_filter) = abstract_transformer quantifier in
    let invMap = ref InvMap.empty in
    let addInv label state = invMap := InvMap.add label state !invMap in
    let rec aux (b:block) (nextOuterState:D.t) () = (* nextOuterState is the 'out' state of the next basic block in the CFG *)
      match b with 
      | A_empty l -> addInv l nextOuterState (* here we use 'nextOuterState' because there is no 'out' state coming in from the next block *)
      | A_block (blockLabel,(stmt,_),nextBlock) -> 
        let nextBlockLabel = block_label nextBlock in
        let nextBlockState = InvMap.find nextBlockLabel fp in
        aux nextBlock nextOuterState ();
        match stmt with
        | A_if ((b,ba), bIf, bElse) -> 
          let sIf = bwd_filter (InvMap.find (block_label bIf) fp) b in
          let sElse = bwd_filter (InvMap.find (block_label bElse) fp) (fst (negBExp (b,ba))) in
          let s = branch_join sIf sElse in
          addInv blockLabel s;
          aux bElse nextBlockState ();
          aux bIf nextBlockState ()
        | A_while (whileLabel,(b,ba),whileBlock) -> 
          let blockState = InvMap.find blockLabel fp in
          let sFall = bwd_filter (InvMap.find (block_label whileBlock) fp) b in
          let sJump = bwd_filter nextBlockState (fst (negBExp (b,ba))) in
          let s = branch_join sFall sJump in
          addInv blockLabel s;
          aux whileBlock blockState ();
        | A_assign ((l,_),(e,_)) -> 
          let s = bwd_assign nextBlockState (l,e) in
          addInv blockLabel s
        | _ -> 
          addInv blockLabel nextBlockState
    in
    let bot = D.bot program.environment program.variables in
    let top = D.top program.environment program.variables in
    aux program.mainFunction.funcBody bot ();
    let zero_leafs t = D.until t top t in (* set all defined leafs of the decision trees to zero *)
    InvMap.map zero_leafs !invMap 


  (* 
    Assign atomic state to those blocks that match the label and bot to all others
  *)
  let label_atomic (program:program) (propertyLabel:string) (property:bExp) : inv = 
    let bot = D.bot program.environment program.variables in
    let labelState = D.reset bot property in
    let blockMap = block_label_map program.mainFunction.funcBody in
    let reducer (inv:D.t InvMap.t) (label, block) =
      let state = match block with
        | A_block (_, (A_label (l, _), _), _) ->
          if String.equal l propertyLabel then labelState else bot
        | _ -> bot
      in InvMap.add label state inv
    in List.fold_left reducer InvMap.empty (InvMap.bindings blockMap)


  (* 
    Assign atomic state to each label of the program.
    Atomic state is a function that returns zero for all states that satisfy the property
  *)
  let atomic (program:program) (property:bExp) : inv = 
    let bot = D.bot program.environment program.variables in
    let blockMap = block_label_map program.mainFunction.funcBody in
    let atomicState = D.reset bot property in
    let reducer (inv:D.t InvMap.t) (label, block) = InvMap.add label atomicState inv
    in List.fold_left reducer InvMap.empty (InvMap.bindings blockMap)

  let atomic_true (program:program) : inv =
    let bot = D.bot program.environment program.variables in
    let trueState = D.reset bot (A_TRUE) in
    let labels = labels_of_program program in
    List.fold_left (fun inv label -> InvMap.add label trueState inv) InvMap.empty labels


  (* CTL 'or' opperator *)
  let logic_or (fp1:inv) (fp2:inv) : inv =
    let f _ t1 t2 = Some (D.join COMPUTATIONAL t1 t2) in
    InvMap.union f fp1 fp2

  (* CTL 'and' opperator *)
  let logic_and (fp1:inv) (fp2:inv) : inv =
    let f _ t1 t2 = Some (D.meet COMPUTATIONAL t1 t2) in
    InvMap.union f fp1 fp2

  (* CTL 'not' opperator *)
  let logic_not (fp:inv) : inv = InvMap.map D.complement fp


  (*
    Recusively compute fixed-point for CTL properties 
  *)
  let compute (program:program) (property:ctl_property) : inv = 
    let atomic_true_inv = atomic_true program in
    let fwdInvMap = if !refine then (* Run forward analysis if 'refine' flag is set *)
        ForwardIteratorB.compute 
          (program.variables,
           program.globalBlock,
          (StringMap.singleton program.mainFunction.funcName program.mainFunction))
          program.mainFunction.funcName
          program.environment
      else
        InvMap.empty
    in
    let fwdInv l = InvMap.find l fwdInvMap in
    let a_until = until UNIVERSAL fwdInv in
    let e_until = until EXISTENTIAL fwdInv in
    let a_global = global UNIVERSAL fwdInv in
    let e_global = global EXISTENTIAL fwdInv in
    let a_next = next UNIVERSAL in
    let e_next = next EXISTENTIAL in
    let print_inv property inv = 
      if not !minimal then
        begin
          Format.fprintf !fmt "Property: %a\n\n" print_ctl_property property;
          printInv !fmt inv;
        end;
    in
    let rec inv (property:ctl_property) : inv = 
      let result = 
        match property with
        | Atomic (b, None) -> atomic program b
        | Atomic (b, Some l) -> label_atomic program l b
        | AX p -> a_next program (inv p)
        | AF p -> a_until program atomic_true_inv (inv p)
        | AG p -> a_global program (inv p)
        | AU (p1, p2) -> a_until program (inv p1) (inv p2)
        | EU (p1, p2) -> 
          if !ctl_existential_equivalence then 
            raise (Invalid_argument "existential equivalence conversion not supported for 'until' operator")
          else 
            e_until program (inv p1) (inv p2)
        | EF p -> 
          if !ctl_existential_equivalence then 
            (* use the following equivalence relation: EF(p) := not AG(not p)  *)
            let inv_not_p = inv (NOT p) in
            let inv_ag = a_global ~use_sink_state:true program inv_not_p in
            print_inv (AG (NOT p)) inv_ag;
            let not_inv_ag = logic_not inv_ag in
            not_inv_ag
          else
            e_until program atomic_true_inv (inv p)
        | EG p -> 
          if !ctl_existential_equivalence then
            (* use the following equivalence realtion: EG(p) := not AF(not p)  *)
            let inv_not_p = inv (NOT p) in
            let inv_af = a_until program atomic_true_inv inv_not_p in
            print_inv (AF (NOT p)) inv_af;
            let not_inv_af = logic_not inv_af in
            not_inv_af
          else
            e_global program (inv p)
        | EX p -> (* EX(p) := not AX(not p)  *)
          if !ctl_existential_equivalence then
            (* use the following equivalence relation: EX(p) := not AX(not p)  *)
            let inv_not_p = inv (NOT p) in
            let inv_ax = a_next program inv_not_p in
            print_inv (AX (NOT p)) inv_ax;
            let not_inv_ax = logic_not inv_ax in
            not_inv_ax
          else
            e_next program (inv p)
        | AND (p1, p2) -> logic_and (inv p1) (inv p2)
        | OR (p1, p2) -> logic_or (inv p1) (inv p2)
        | NOT (Atomic (b, None)) -> 
          atomic program @@ fst @@ negBExp (b, (Lexing.dummy_pos, Lexing.dummy_pos))
        | NOT p -> logic_not (inv p) 
      in
      print_inv property result;
      result
    in inv property

  let analyze ?precondition (program:program) (property:ctl_property) =  
    let inv = compute program property in
    let initialLabel = block_label program.mainFunction.funcBody in
    let programInvariant = InvMap.find initialLabel inv in
    D.defined ?condition:precondition programInvariant


end

