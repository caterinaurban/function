open AbstractSyntax
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator

(* type for CTL properties, instantiated with bExp for atomic propositions *)
type ctl_property = AbstractSyntax.bExp CTLProperty.generic_property

let rec print_ctl_property fmt (property:ctl_property) = match property with 
  | Atomic b -> AbstractSyntax.bExp_print_aux fmt b
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


module InvMap = Map.Make(struct type t=label let compare=compare end)

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



(*
   This function takes a given paresed program and introduces a new variable called 'termination' that is set to '0'
   whenever the program terminates i.e. at the end and before every return statement. 

   The augmented program and a corresponding ctl_property for termination is returned. 
   This pair can then be used with the usual CTL analysis to check for termination.
*)
let program_of_prog_with_termination (prog: AbstractSyntax.prog) (main: AbstractSyntax.StringMap.key) : (program * ctl_property) =
  let (globalVariables, globalBlock, functions) = prog in
  let mainFunction = StringMap.find main functions in
  let dummyPosition = match mainFunction.funcBody with 
    | A_block (_, (_,pos), _) -> pos
    | _ -> raise (Invalid_argument "empty main function")
  in
  let terminationVar = {varId = "$termination"; varName = "termination"; varTyp = A_INT} in
  let assignZero = (A_assign ((A_var terminationVar, dummyPosition), (A_const 0, dummyPosition)), dummyPosition) in
  let assignOne = (A_assign ((A_var terminationVar, dummyPosition), (A_const 1, dummyPosition)), dummyPosition) in
  let id = ref (-2) in
  let nextId () = let i = !id in id := i + 1; i in
  let rec addTerminationStmt (block:block) = match block with
    | A_empty l -> A_block (nextId (), assignOne, block)
    | A_block (l,stmt,nextBlock) -> A_block (l, stmt, addTerminationStmt nextBlock)
  in
  let rec addTerminationStmtReturn (block:block) = match block with
    | A_empty l -> block 
    | A_block (l, (A_return,a) , nextBlock) -> 
      let nextBlock = A_block (l, (A_return, a), addTerminationStmtReturn nextBlock) in
      A_block (nextId (), assignOne, nextBlock)
    | A_block (l, stmt, nextBlock) -> A_block (l, stmt, addTerminationStmtReturn nextBlock)
  in
  let augmentedBody = A_block (-1, assignZero, addTerminationStmtReturn @@ addTerminationStmt mainFunction.funcBody) in
  let v1 = snd (List.split (StringMap.bindings globalVariables)) in
  let v2 = snd (List.split (StringMap.bindings mainFunction.funcVars)) in
  let vars = terminationVar :: (List.append v1 v2) in (* add special terminationVar to list of variables *)
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
      funcVars = StringMap.add terminationVar.varId terminationVar mainFunction.funcVars; (* add termination var to function variables *)
      funcBody = augmentedBody
    };
    globalBlock = globalBlock
  } in
  let terminationProperty = AF (Atomic (AbstractSyntax.A_rbinary (AbstractSyntax.A_GREATER_EQUAL, (A_var terminationVar, dummyPosition), (A_const 1, dummyPosition)))) in
  (program, terminationProperty)


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
    globalBlock = globalBlock
  }


module ForwardIterator (D:RANKING_FUNCTION) = struct

  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:D.B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.B.print a) m

  (* Forward Iterator *)

  let rec fwdStm (program:program) p s =
    match s with
    | A_label _ -> p
    | A_return -> D.B.bot program.environment program.variables
    | A_assign ((l,_),(e,_)) -> D.B.fwdAssign p (l,e)
    | A_assert (b,_) -> D.B.filter p b
    | A_if ((b,ba),s1,s2) ->
      let p1 = fwdBlk program (D.B.filter p b) s1 in
      let p2 = fwdBlk program (D.B.filter p (fst (negBExp (b,ba)))) s2 in
      D.B.join p1 p2
    | A_while (l,(b,ba),s) ->
      let rec aux i p2 n =
        let i' = D.B.join p p2 in
        if !tracefwd && not !minimal then begin
          Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
          Format.fprintf !fmt "p: %a\n" D.B.print p;
          Format.fprintf !fmt "i: %a\n" D.B.print i;
          Format.fprintf !fmt "p2: %a\n" D.B.print p2;
          Format.fprintf !fmt "i': %a\n" D.B.print i';
        end;
        if D.B.isLeq i' i then i
        else
          let i'' = if n <= !joinfwd then i' else D.B.widen i i' in
          if !tracefwd && not !minimal then Format.fprintf !fmt "i'': %a\n" D.B.print i'';
          aux i'' (fwdBlk program (D.B.filter i'' b) s) (n+1)
      in
      let i = D.B.bot program.environment program.variables in
      let p2 = fwdBlk program (D.B.filter i b) s in
      let p = aux i p2 1 in
      addFwdInv l p;
      D.B.filter p (fst (negBExp (b,ba)))
    | A_call (f,ss) -> raise (Invalid_argument "fwdStm:A_recall")
    | A_recall (f,ss) -> raise (Invalid_argument "fwdStm:A_recall")

  and fwdBlk (program:program) (p:D.B.t) (b:block) : D.B.t =
    match b with
    | A_empty l ->
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "### %a ###: %a\n" label_print l D.B.print p;
      addFwdInv l p; p
    | A_block (l,(s,_),b) ->
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "### %a ###: %a\n" label_print l D.B.print p;
      addFwdInv l p; fwdBlk program (fwdStm program p s) b

  let compute (program:program) =
    let s = program.mainFunction.funcBody in
    (* Forward Analysis *)
    if !tracefwd && not !minimal then Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
    fwdInvMap := InvMap.empty; (* reset inv map *)
    let _ = fwdBlk program (D.B.top program.environment program.variables) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap;
      end;
    !fwdInvMap

end


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

  (* 
     Type that represents an invariant/fixed-point.
     An invariant assigns an abstract state to each statement of a program
  *)
  type inv = D.t InvMap.t


  (* Computes fixed-point for 'until' properties: AU{inv_keep}{inv_reset} 
     inv_keep and inv_reset are fixed-point for the nested properties

     Applies the reset_until operator to each state. 
     This resets the ranking function for those parts of the domain where inv_reset is also 
     defined and it discards those parts of the ranking function where neither inv_keep nor inv_reset are defined.
  *)
  let until ?(join_kind = APPROXIMATION) (program:program) (inv_keep:inv) (inv_reset:inv) : inv =
    let inv = ref InvMap.empty in (* variable where the resulting invariant/fixed-point is stored *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
    let join = D.join join_kind in (* defines which join should be used when joining branches *)
    let bot = D.bot program.environment program.variables in
    let start = Sys.time () in
    let rec bwd (out:D.t) (b:block) : D.t = (* recursive function that performs the backward analysis *)
      if (((Sys.time ()) -. start) > !timeout) then raise Timeout; (* check for timeout *)
      match b with
      | A_empty blockLabel ->
        let invBlockKeep = InvMap.find blockLabel inv_keep in
        let invBlockReset = InvMap.find blockLabel inv_reset in
        let in_state = D.reset_until invBlockKeep invBlockReset out in
        if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print in_state;
        addInv blockLabel in_state
      | A_block (blockLabel,(stmt,_),nextBlock) ->
        let invBlockKeep = InvMap.find blockLabel inv_keep in 
        let invBlockReset = InvMap.find blockLabel inv_reset in
        let reset_until = D.reset_until invBlockKeep invBlockReset in
        let out_state = bwd out nextBlock in (* recursively process the rest of the program, this gives us the 'out' state for this statement *)
        (* compute 'in' state for this statement *)
        let in_state = match stmt with 
          | A_label (l,_) -> out_state
          | A_return -> bot
          | A_assign ((l,_),(e,_)) -> D.bwdAssign out_state (l,e)
          | A_assert (b,_) -> D.filter out_state b
          | A_if ((b,ba),s1,s2) ->
            let out_if = D.filter (bwd out_state s1) b in (* compute 'out' state for if-block*)
            let out_else = D.filter (bwd out_state s2) (fst (negBExp (b,ba))) in (* compute 'out' state for else-block *)
            join out_if out_else (* join the two branches *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = D.filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                in_state (* 'in' state of the previous iteration *)
                out_enter (* current 'out' state when entering the loop body *)
                n = (* iteration counter *)
              let in_state' = reset_until (join out_exit out_enter) in (* 'in' state for this iteration *)
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
                let out_enter' = D.filter (bwd in_state'' loop_body) b in (* process loop body again with updated 'in' state *)
                aux in_state'' out_enter' (n+1) (* run next iteration *)
            in
            let initial_in = bot in (* start with bottom as initial 'in' state *)
            let initial_out_enter = D.filter (bwd initial_in loop_body) b in (* process loop body with initial 'in' state *)
            let final_in_state = aux initial_in initial_out_enter 1 in (* compute fixed point for loop-head *)
            addInv l final_in_state 
          | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
          | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
        in
        let in_state = reset_until in_state in (* reset ranking functions *)
        if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print in_state;
        addInv blockLabel in_state
    in 
    let _ = bwd bot program.mainFunction.funcBody in (* process entire program starting with bottom *)
    !inv (* return computed program invariant *)


  (*
    Computed fixed-point for 'global' operator (e.g. AG{...}), takes an existing fixed point for the nested property as argument.

    Applies the 'left_narrow' function to compute the greates-fixed point starting from the given fixed point for the nested property. 
    During the backward analysis, the reachable state at each statement is computed by inspecting the 'out' states. 
    Then this reachable state is used to narrow down the given fixed-point of the nested property by using 'left_narrow'. 
    To backward analysis for the while-loop uses widening to get convergence.
  *)
  let global ?(join_kind = APPROXIMATION) (program:program) (fixed_point:inv) : inv =
    let inv = ref (InvMap.union (fun _ _ _ -> None) fixed_point InvMap.empty) in (* initialize InvMap with given fixed-point for nested property *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
    let blockState block = InvMap.find (label_of_block block) !inv in (* returns current 'in' state of a block *)
    let join = D.join join_kind in (* defines which join should be used when joining branches *)
    let bot = D.bot program.environment program.variables in 
    let start = Sys.time () in
    let rec bwd (out:D.t) (b:block) : D.t = (* recursive function that performs block-wise backward analysis *)
      if (((Sys.time ()) -. start) > !timeout) then raise Timeout; (* check for timeout *)
      let current_in = blockState b in (* current 'in' state for this block *)
      match b with
      | A_empty blockLabel -> addInv blockLabel (D.left_narrow current_in out) 
      | A_block (blockLabel, (stmt,_), nextBlock) ->
        let out_state = bwd out nextBlock in (* recursively process the rest of the program, this gives us the 'out' state for this statement *)
        let new_in = match stmt with 
          | A_label (l,_) -> out_state
          | A_return -> bot
          | A_assign ((l,_),(e,_)) -> D.left_narrow current_in (D.bwdAssign out_state (l,e))
          | A_assert (b,_) -> D.left_narrow current_in (D.filter out_state b)
          | A_if ((b,ba),s1,s2) ->
            let out_if = D.filter (bwd out_state s1) b in (* compute 'out' state for if-block*)
            let out_else = D.filter (bwd out_state s2) (fst (negBExp (b,ba))) in (* compute 'out' state for else-block *)
            D.left_narrow current_in (join out_if out_else) (* join the two branches and combine with current 'in' state using left_narrow *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = D.filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                (current_in:D.t) (* 'in' state of the previous iteration *)
                (out_enter:D.t) (* current 'out' state when entering the loop body *)
                (n:int) : D.t = (* iteration counter *)
              let out_joined = join out_exit out_enter in (* new 'in' state after joining the incoming branches *)
              let updated_in = D.left_narrow current_in out_joined in (* join two branches and combine with current 'in' state using left_narrow *)
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                (* Format.fprintf !fmt "out_exit: %a\n" D.print out_exit; *)
                (* Format.fprintf !fmt "out_enter: %a\n" D.print out_enter; *)
                Format.fprintf !fmt "out_joined: %a\n" D.print out_joined;
                Format.fprintf !fmt "current_in: %a\n" D.print current_in;
                Format.fprintf !fmt "updated_in: %a\n" D.print updated_in;
              end;
              (** NOTE: left_narrow is only monotone w.r.t. the approximation order. In fact it usually decreases w.r.t the computational order as
                    leafs of the decision tree got to bottom. Therefore we only check if we reached a fixed point using the approximation order. 
              *)
              if (D.isLeq APPROXIMATION current_in updated_in) && (D.isLeq APPROXIMATION updated_in current_in) then
                (* fixed-point reached, current_in has stabilized *)
                let fixed_point = current_in in 
                if !tracebwd && not !minimal then Format.fprintf !fmt "Fixed-Point reached \n";
                fixed_point
              else
                let updated_in' = 
                  if n <= !joinbwd then updated_in (* widening threshold not yet reached *)
                  else D.widen current_in updated_in (* use widening after widening threshold met *)
                in
                let out_enter' = D.filter (bwd updated_in' loop_body) b in (* process loop body again with updated 'in' state *)
                (* next iteration *)
                aux updated_in' out_enter' (n+1)
            in
            let initial_out_enter = D.filter (bwd current_in loop_body) b in (* process loop body with current 'in' state at loop-head *)
            let final_in_state = aux current_in initial_out_enter 1 in (* compute fixed point for while-loop starting with current 'in' state at loop-head *)
            addInv l final_in_state 
          | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
          | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
        in
        addInv blockLabel new_in (* use left_narrow to compute the new 'in' state for this block *)
    in 
    let _ = bwd bot program.mainFunction.funcBody in (* run backward analysis starting from bottom *)
    !inv

  (* 
    Computes fixed-point for 'next' operator (e.g. AX{...})
    For each program label, this function joins all 'out' states and sets this value as the new invariant.
    This computatoin is straight-forward with the exception of empty blocks. 
    There we need to inject the 'out' state of the next basic block in the control-flow-graph. 
    This is done by passing in said state through the recursion of the backward analysis.
  *)
  let next ?(join_kind = APPROXIMATION) (program:program) (fp:inv) : inv =
    let invMap = ref InvMap.empty in
    let addInv label state = invMap := InvMap.add label state !invMap in
    let join = D.join join_kind in (* defines which join should be used when joining branches *)
    let rec aux (b:block) (nextOuterState:D.t) () = (* nextOuterState is the 'out' state of the next basic block in the CFG *)
      match b with 
      | A_empty l -> addInv l nextOuterState (* here we use 'nextOuterState' because there is no 'out' state coming in from the next block *)
      | A_block (blockLabel,(stmt,_),nextBlock) -> 
        let nextBlockLabel = block_label nextBlock in
        let nextBlockState = InvMap.find nextBlockLabel fp in
        aux nextBlock nextOuterState ();
        match stmt with
        | A_if ((b,ba), bIf, bElse) -> 
          let sIf = D.filter (InvMap.find (block_label bIf) fp) b in
          let sElse = D.filter (InvMap.find (block_label bElse) fp) (fst (negBExp (b,ba))) in
          let s = join sIf sElse in
          addInv blockLabel s;
          aux bElse nextBlockState ();
          aux bIf nextBlockState ()
        | A_while (whileLabel,(b,ba),whileBlock) -> 
          let blockState = InvMap.find blockLabel fp in
          let sFall = D.filter (InvMap.find (block_label whileBlock) fp) b in
          let sJump = D.filter nextBlockState (fst (negBExp (b,ba))) in
          let s = join sFall sJump in
          addInv blockLabel s;
          aux whileBlock blockState ();
        | A_assign ((l,_),(e,_)) -> 
          let s = D.bwdAssign nextBlockState (l,e) in
          addInv blockLabel s
        | _ -> 
          addInv blockLabel nextBlockState
    in
    let bot = D.bot program.environment program.variables in
    let top = D.top program.environment program.variables in
    aux program.mainFunction.funcBody bot ();
    let zero_leafs t = D.reset_until top t t in (* set all defined leafs of the decision trees to zero *)
    InvMap.map zero_leafs !invMap 

  (* 
    Assign atomic state to each label of the program.
    Atomic state is a function that returns zero for all states that satisfy the property
  *)
  let atomic (program:program) (property:bExp) : inv = 
    let bot = D.bot program.environment program.variables in
    let atomicState = D.reset bot property in
    let labels = labels_of_program program in
    List.fold_left (fun invMap label -> InvMap.add label atomicState invMap) InvMap.empty labels

  (* CTL 'or' opperator *)
  let logic_or (fp1:inv) (fp2:inv) : inv =
    let f _ t1 t2 = Some (D.join COMPUTATIONAL t1 t2) in
    InvMap.union f fp1 fp2

  (* CTL 'and' opperator *)
  let logic_and (fp1:inv) (fp2:inv) : inv =
    let f _ t1 t2 = Some (D.meet COMPUTATIONAL t1 t2) in
    InvMap.union f fp1 fp2

  let printInv ?fwdInvOpt fmt (inv:inv) =
    let inv = 
      if !compress then 
        InvMap.map D.compress inv
      else 
        inv
    in InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\nDOT: %a\n" label_print l D.print a D.print_graphviz_dot a) inv

  (*
    Recusively compute fixed-point for CTL properties 
  *)
  let compute (program:program) (property:ctl_property) : inv = 
    let rec inv (property:ctl_property) : inv = 
      let result = 
        match property with
        | Atomic b -> atomic program b
        | AX p -> next program (inv p)
        | AF p -> until program (inv (Atomic A_TRUE)) (inv p)
        | AG p -> global program (inv p)
        | AU (p1, p2) -> until program (inv p1) (inv p2)
        | EX p -> next ~join_kind:COMPUTATIONAL program (inv p)
        | EF p -> until ~join_kind:COMPUTATIONAL program (inv (Atomic A_TRUE)) (inv p)
        | EG p -> global ~join_kind:COMPUTATIONAL program (inv p)
        | EU (p1, p2) -> until ~join_kind:COMPUTATIONAL program (inv p1) (inv p2)
        | AND (p1, p2) -> logic_and (inv p1) (inv p2)
        | OR (p1, p2) -> logic_or (inv p1) (inv p2)
      in
      if not !minimal then
        begin
          Format.fprintf !fmt "Property: %a\n\n" print_ctl_property property;
          printInv !fmt result;
        end;
      result
    in inv property

  let analyze (program:program) (property:ctl_property) =  
    let inv = compute program property in
    let initialLabel = block_label program.mainFunction.funcBody in
    let programInvariant = InvMap.find initialLabel inv in
    let isTerminating = D.terminating programInvariant in
    isTerminating


end

