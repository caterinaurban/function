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

  let until (program:program) (inv_keep:inv) (inv_reset:inv) : inv =
    let inv = ref InvMap.empty in
    let addInv l (a:D.t) = inv := InvMap.add l a !inv in
    let rec bwdBlk (p:D.t) (b:block) : D.t =
      match b with
      | A_empty blockLabel ->
        let invBlockKeep = InvMap.find blockLabel inv_keep in
        let invBlockReset = InvMap.find blockLabel inv_reset in
        let p = D.reset_until invBlockKeep invBlockReset p in
        if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print p;
        addInv blockLabel p; 
        p
      | A_block (blockLabel,(s,_),nextBlock) ->
        stop := Sys.time ();
        if ((!stop -. !start) > !timeout) then raise Timeout
        else
          let p = bwdBlk p nextBlock in
          let invBlockKeep = InvMap.find blockLabel inv_keep in
          let invBlockReset = InvMap.find blockLabel inv_reset in
          let reset = D.reset_until invBlockKeep invBlockReset in
          let p = match s with 
            | A_label (l,_) -> p
            | A_return -> D.bot program.environment program.variables
            | A_assign ((l,_),(e,_)) -> D.bwdAssign p (l,e)
            | A_assert (b,_) -> D.filter p b
            | A_if ((b,ba),s1,s2) ->
              let p1 = D.filter (bwdBlk p s1) b in
              let p2 = D.filter (bwdBlk p s2) (fst (negBExp (b,ba))) in
              D.join APPROXIMATION p1 p2
            | A_while (l,(b,ba),s) ->
              let p1 = D.filter p (fst (negBExp (b,ba))) in
              let rec aux i p2 n =
                let i' = reset (D.join APPROXIMATION p1 p2) in
                if !tracebwd && not !minimal then begin
                  Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
                  Format.fprintf !fmt "p1: %a\n" D.print p1;
                  Format.fprintf !fmt "i: %a\n" D.print i;
                  Format.fprintf !fmt "p2: %a\n" D.print p2;
                  Format.fprintf !fmt "i': %a\n" D.print i';
                end;
                if (D.isLeq COMPUTATIONAL i' i) then
                  if (D.isLeq APPROXIMATION i' i) then
                    let i = i in
                    if !tracebwd && not !minimal then begin
                      Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l;
                      Format.fprintf !fmt "i: %a\n" D.print i;
                    end;
                    i
                  else
                    let i'' = if n <= !joinbwd then i' else D.widen i i' in
                    if !tracebwd && not !minimal then Format.fprintf !fmt "i'': %a\n" D.print i'';
                    aux i'' (D.filter (bwdBlk i'' s) b) (n+1)
                else
                  let i'' = if n <= !joinbwd then i' else D.widen i (D.join COMPUTATIONAL i i') in
                  if !tracebwd && not !minimal then Format.fprintf !fmt "i'': %a\n" D.print i'';
                  aux i'' (D.filter (bwdBlk i'' s) b) (n+1)
              in
              let i = D.bot program.environment program.variables in
              let p2 = D.filter (bwdBlk i s) b in
              let p = aux i p2 1 in
              addInv l p; 
              p
            | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
            | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
          in
          let p = reset p in
          if !tracebwd && not !minimal then Format.fprintf !fmt "### %a ###:\n%a\n" label_print blockLabel D.print p;
          addInv blockLabel p;
          p
    in 
    let _ = bwdBlk (D.bot program.environment program.variables) program.mainFunction.funcBody in
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
    let f _ t1 t2 = Some (D.leaf_preserving_meet t1 t2) in
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
        | AU (p1, p2) -> until program (inv p1) (inv p2)
        | AND (p1, p2) -> logic_and (inv p1) (inv p2)
        | OR (p1, p2) -> logic_or (inv p1) (inv p2)
        | _ -> raise (Invalid_argument "ACTL formula not yet supported")
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

