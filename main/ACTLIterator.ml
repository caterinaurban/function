open AbstractSyntax
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator

type ctl_property = AbstractSyntax.bExp CTLProperty.generic_property

let rec print_ctl_property fmt (property:ctl_property) = match property with 
  | Atomic b -> AbstractSyntax.bExp_print_aux fmt b
  | AX p -> Format.fprintf fmt "AX{%a}" print_ctl_property p
  | AF p -> Format.fprintf fmt "AF{%a}" print_ctl_property p
  | AG p -> Format.fprintf fmt "AG{%a}" print_ctl_property p
  | AU (p1,p2) -> Format.fprintf fmt "AU{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | AND (p1,p2) -> Format.fprintf fmt "AND{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | OR (p1,p2) -> Format.fprintf fmt "OR{%a}{%a}" print_ctl_property p1 print_ctl_property p2;


module InvMap = Map.Make(struct type t=label let compare=compare end)

(* Bundle commonly used values (AST, Apron env. variable list) to one struct*)
type program = {
  environment: Apron.Environment.t;
  variables: AbstractSyntax.var list;
  mainFunction: AbstractSyntax.func;
  globalBlock: AbstractSyntax.block; 
}

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

module ACTLIterator(D: RANKING_FUNCTION) = struct

  type inv = D.t InvMap.t

  (*
     Fixed Point Computation:

     The fixed point for the 'unit' and 'global' operators are computed by performing a backward-analysis.

     Given a statement, we call the state that holds before the statement the 'in' state, and the state that holds
     after the statement the 'out' state. The backward analysis starts with an initial 'out' state at the end of the program
     and propagates it backwards to the start of the program. For each statement, the 'in' state is computed based on the 'out' state.
     At loop heads the final 'in' state is computed by iterating over the loop body until a fixed-point is reached.

  *)


  (* Computes fixed-point for 'until' properties: AU{inv_keep}{inv_reset} 
     inv_keep and inv_reset are fixed-point for the nested properties

     Applies the reset_until operator to each state. 
     This resets the ranking function for those parts of the domain where inv_reset is also 
     defined and it discards those parts of the ranking function where neither inv_keep nor inv_reset are defined.
  *)
  let until (program:program) (inv_keep:inv) (inv_reset:inv) : inv =
    let inv = ref InvMap.empty in (* map that stores the 'in' states for each block *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
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
            D.join APPROXIMATION out_if out_else (* join the two branches *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = D.filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                in_state (* 'in' state of the previous iteration *)
                out_enter (* current 'out' state when entering the loop body *)
                n = (* iteration counter *)
              let in_state' = reset_until (D.join APPROXIMATION out_exit out_enter) in (* 'in' state for this iteration *)
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                Format.fprintf !fmt "out_exit: %a\n" D.print out_exit;
                Format.fprintf !fmt "in: %a\n" D.print in_state;
                Format.fprintf !fmt "out_enter: %a\n" D.print out_enter;
                Format.fprintf !fmt "in': %a\n" D.print in_state';
              end;
              if (D.isLeq COMPUTATIONAL in_state' in_state) then
                if (D.isLeq APPROXIMATION in_state' in_state) then
                  (* fixed-point reached*)
                  let fixed_point = in_state in
                  if !tracebwd && not !minimal then begin
                    Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l;
                    Format.fprintf !fmt "in_state: %a\n" D.print fixed_point;
                  end;
                  fixed_point
                else
                  (* apply widening after constant number of iterations *)
                  let in_state'' = if n <= !joinbwd then in_state' else D.widen in_state in_state' in
                  if !tracebwd && not !minimal then Format.fprintf !fmt "in'': %a\n" D.print in_state'';
                  let out_enter' = D.filter (bwd in_state'' loop_body) b in (* process loop body again with updated 'in' state *)
                  aux in_state'' out_enter' (n+1) (* next iteration *)
              else
                (* apply widening after constant number of iterations *)
                let in_state'' = if n <= !joinbwd then in_state' else D.widen in_state (D.join COMPUTATIONAL in_state in_state') in
                if !tracebwd && not !minimal then Format.fprintf !fmt "in'': %a\n" D.print in_state'';
                let out_enter' = D.filter (bwd in_state'' loop_body) b in (* process loop body again with updated 'in' state *)
                aux in_state'' out_enter' (n+1)
            in
            let initial_in = bot in (* start with bottom as initial 'in' state *)
            let initial_out_enter = D.filter (bwd initial_in loop_body) b in (* process loop body with initial 'in' state *)
            let final_in_state = aux initial_in initial_out_enter 1 in (* compute fixed point for while-loop starting *)
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


  let global (program:program) (fixed_point:inv) : inv =
    let inv = ref (InvMap.union (fun _ _ _ -> None) fixed_point InvMap.empty) in (* initialize InvMap with given fixed point *)
    let addInv l (a:D.t) = inv := InvMap.add l a !inv; a in (* update InvMap with new value and return new updated value *)
    let blockState block = InvMap.find (label_of_block block) !inv in (* returns current 'in' state of a block *)
    let bot = D.bot program.environment program.variables in 
    let start = Sys.time () in
    let rec bwd (out:D.t) (b:block) : D.t = (* recursive function that performs block-wise backward analysis *)
      if (((Sys.time ()) -. start) > !timeout) then raise Timeout; (* check for timeout *)
      let current_in = blockState b in (* current 'in' state for this block *)
      match b with
      | A_empty blockLabel -> addInv blockLabel (D.left_narrow current_in out) 
      | A_block (blockLabel, (stmt,_), nextBlock) ->
        (* recursively process the rest of the program, this gives us the 'out' state for this statement *)
        let out_state = bwd out nextBlock in
        let new_in = match stmt with 
          | A_label (l,_) -> out_state
          | A_return -> bot
          | A_assign ((l,_),(e,_)) -> D.left_narrow current_in (D.bwdAssign out_state (l,e))
          | A_assert (b,_) -> D.left_narrow current_in (D.filter out_state b)
          | A_if ((b,ba),s1,s2) ->
            let out_if = D.filter (bwd out_state s1) b in (* compute 'out' state for if-block*)
            let out_else = D.filter (bwd out_state s2) (fst (negBExp (b,ba))) in (* compute 'out' state for else-block *)
            D.left_narrow current_in (D.join APPROXIMATION out_if out_else) (* join the two branches and combine with current 'in' state using left_narrow *)
          | A_while (l,(b,ba),loop_body) ->
            let out_exit = D.filter out_state (fst (negBExp (b,ba))) in (* 'out' state when not entering the loop body *)
            let rec aux (* recursive function that iteratively computes fixed point for 'in' state at loop head *)
                (current_in:D.t) (* 'in' state of the previous iteration *)
                (out_enter:D.t) (* current 'out' state when entering the loop body *)
                (n:int) : D.t = (* iteration counter *)
              let out_joined = D.join APPROXIMATION out_exit out_enter in (* new 'in' state after joining the incoming branches *)
              let updated_in = D.left_narrow current_in out_joined in (* join two branches and combine with current 'in' state using left_narrow *)
              if !tracebwd && not !minimal then begin
                Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                (* Format.fprintf !fmt "out_exit: %a\n" D.print out_exit; *)
                (* Format.fprintf !fmt "out_enter: %a\n" D.print out_enter; *)
                Format.fprintf !fmt "out_joined: %a\n" D.print out_joined;
                Format.fprintf !fmt "current_in: %a\n" D.print current_in;
                Format.fprintf !fmt "updated_in: %a\n" D.print updated_in;
              end;
              if (D.isLeq APPROXIMATION current_in updated_in) && (D.isLeq APPROXIMATION updated_in current_in) then
                (* fixed-point reached, current_in has stabilized *)
                let fixed_point = current_in in 
                if !tracebwd && not !minimal then Format.fprintf !fmt "Fixed-Point reached \n";
                fixed_point
              else
                (* next iteration *)
                let out_enter' = D.filter (bwd updated_in loop_body) b in (* process loop body again with updated 'in' state *)
                aux updated_in out_enter' (n+1)
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

  let next (program:program) (fp:inv) : inv =
    let invMap = ref InvMap.empty in
    let addInv label state = invMap := InvMap.add label state !invMap in
    let rec aux (b:block) (nextOuterState:D.t) ()  =
      match b with 
      | A_empty l -> 
        addInv l nextOuterState
      | A_block (blockLabel,(stmt,_),nextBlock) -> 
        let nextBlockLabel = block_label nextBlock in
        let nextBlockState = InvMap.find nextBlockLabel fp in
        aux nextBlock nextOuterState ();
        match stmt with
        | A_if ((b,ba), bIf, bElse) -> 
          let sIf = D.filter (InvMap.find (block_label bIf) fp) b in
          let sElse = D.filter (InvMap.find (block_label bElse) fp) (fst (negBExp (b,ba))) in
          let s = D.join APPROXIMATION sIf sElse in
          addInv blockLabel s;
          aux bElse nextBlockState ();
          aux bIf nextBlockState ()
        | A_while (whileLabel,(b,ba),whileBlock) -> 
          let blockState = InvMap.find blockLabel fp in
          let sFall = D.filter (InvMap.find (block_label whileBlock) fp) b in
          let sJump = D.filter nextBlockState (fst (negBExp (b,ba))) in
          let s = D.join APPROXIMATION sFall sJump in
          addInv blockLabel s;
          aux whileBlock blockState ();
        | A_assign ((l,_),(e,_)) -> 
          let s = D.bwdAssign nextBlockState (l,e) in
          addInv blockLabel s
        | _ -> 
          addInv blockLabel nextBlockState
    in
    let bot = D.bot program.environment program.variables in
    aux program.mainFunction.funcBody bot ();
    InvMap.map D.zero_leafs !invMap 


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
    let f _ t1 t2 = Some (D.join Functions.COMPUTATIONAL t1 t2) in
    InvMap.union f fp1 fp2

  (* CTL 'and' opperator *)
  let logic_and (fp1:inv) (fp2:inv) : inv =
    let f _ t1 t2 = Some (D.leaf_preserving_meet t1 t2) in (* meet COMPUTATIONAL ... *)
    InvMap.union f fp1 fp2

  let printInv fmt (inv:inv) =
    if !compress then
      InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\n" label_print l D.print (D.compress a)) inv
    else
      InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\n" label_print l D.print a) inv

  let compute (program:program) (property:ctl_property) : inv = 
    Format.fprintf !fmt "Processing ACTL Property: %a \n" print_ctl_property property;
    let rec inv (property:ctl_property) : inv = 
      let result = 
        match property with
        | Atomic b -> atomic program b
        | AX p -> next program (inv p)
        | AF p -> until program (inv (Atomic A_TRUE)) (inv p)
        | AG p -> global program (inv p)
        | AU (p1, p2) -> until program (inv p1) (inv p2)
        | AND (p1, p2) -> logic_and (inv p1) (inv p2)
        | OR (p1, p2) -> logic_or (inv p1) (inv p2)
      in
      if not !minimal then
        begin
          Format.fprintf !fmt "========== Computed invariant for: %a =========== \n" print_ctl_property property;
          printInv !fmt result;
        end;
      result
    in inv property

  let analyze (program:program) (property:ctl_property) =  
    let _ = compute program property in
    ()


end

