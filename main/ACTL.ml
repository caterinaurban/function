open AbstractSyntax
open Apron
open Domain
open Partition
open Functions
open Iterator

type formula =
  | ACTL_atomic of AbstractSyntax.bExp
  | ACTL_future of AbstractSyntax.bExp
  | ACTL_next of formula
  | ACTL_until of (formula * formula)
  | ACTL_global of (formula * formula)
  | ACTL_and of (formula * formula)
  | ACTL_or of (formula * formula)

module InvMap = Map.Make(struct type t=label let compare=compare end)

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


module ForwardIterator(B: PARTITION) = struct

  module B = B

  let print fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l B.print a) m

  let compute (program : program) : B.t InvMap.t =
    let invMap = ref InvMap.empty in
    let addFwdInv l (a:B.t) = invMap := InvMap.add l a !invMap in
    let rec fwdStm (p:B.t) (s:stmt) : B.t =
      match s with
      | A_label _ -> p
      | A_return -> B.bot program.environment program.variables
      | A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
      | A_assert (b,_) -> B.filter p b
      | A_if ((b,ba),s1,s2) ->
        let p1 = fwdBlk (B.filter p b) s1 in
        let p2 = fwdBlk (B.filter p (fst (negBExp (b,ba)))) s2 in
        B.join p1 p2
      | A_while (l,(b,ba),s) ->
        let rec aux i p2 n =
          let i' = B.join p p2 in
          if !tracefwd && not !minimal then
            begin
              Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
              Format.fprintf !fmt "p: %a\n" B.print p;
              Format.fprintf !fmt "i: %a\n" B.print i;
              Format.fprintf !fmt "p2: %a\n" B.print p2;
              Format.fprintf !fmt "i': %a\n" B.print i'
            end;
          if B.isLeq i' i then i
          else
            let i'' = if n <= !joinfwd then i' else B.widen i i' in
            if !tracefwd && not !minimal then Format.fprintf !fmt "i'': %a\n" B.print i'';
            aux i'' (fwdBlk (B.filter i'' b) s) (n+1)
        in
        let i = B.bot program.environment program.variables in
        let p2 = fwdBlk (B.filter i b) s in
        let p = aux i p2 1 in
        addFwdInv l p;
        B.filter p (fst (negBExp (b,ba)))
      | A_call (f,ss) -> raise (Invalid_argument "fwdStm:A_call")
      | A_recall (f,ss) -> raise (Invalid_argument "fwdStm:A_recall")
    and fwdBlk (p:B.t) (b:block) : B.t =
      match b with
      | A_empty l ->
        if !tracefwd && not !minimal then Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p;
        addFwdInv l p; 
        p
      | A_block (l,(s,_),b) ->
        if !tracefwd && not !minimal then Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p;
        addFwdInv l p; 
        fwdBlk (fwdStm p s) b
    in
    if !tracefwd && not !minimal then Format.fprintf !fmt "\nForward Analysis Trace:\n";

    let startfwd = Sys.time () in
    let top = B.top program.environment program.variables in
    let pGlobal = fwdBlk top program.globalBlock in
    let _ = fwdBlk pGlobal program.mainFunction.funcBody in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        print !fmt !invMap;
      end;
    !invMap

end

module AtomicIterator(D: RANKING_FUNCTION) = struct

  (* 
    Assign atomic state to each label of the program.
    Atomic state is a function that returns zero for all states that satisfy the property
  *)
  let compute (program:program) (property:bExp) : D.t InvMap.t = 
    let bot = D.bot program.environment program.variables in
    let atomicState = D.reset bot property in
    let labels = labels_of_program program in
    List.fold_left (fun invMap label -> InvMap.add label atomicState invMap) InvMap.empty labels

end


module NextIterator(D: RANKING_FUNCTION) = struct

  let compute (program:program) (property:bExp) (fp:D.t InvMap.t) : D.t InvMap.t =
    let bot = D.bot program.environment program.variables in
    let rec bwd (b:block) invMap =
      match b with 
      | A_empty l -> InvMap.add l bot invMap
      | A_block (blockLabel,(stmt,_),nextBlock) -> 
        let nextBlockLabel = block_label nextBlock in
        let nextP = InvMap.find nextBlockLabel invMap in
        match stmt with
        | A_if ((b,ba), bIf, bElse) -> 
          let p1 = D.filter (InvMap.find (block_label bIf) invMap) b in
          let p2 = D.filter (InvMap.find (block_label bElse) invMap) (fst (negBExp (b,ba))) in
          let p = D.join APPROXIMATION p1 p2 in
          InvMap.add blockLabel p invMap
        | A_while (whileLabel,(b,ba),whileBlock) -> 
          let p1 = D.filter (InvMap.find (block_label whileBlock) invMap) b in
          let p2 = D.filter (InvMap.find nextBlockLabel invMap) (fst (negBExp (b,ba))) in
          let p = D.join APPROXIMATION p1 p2 in
          InvMap.add whileLabel p (InvMap.add blockLabel p invMap) (* add this to both the block and the while label*)
        | A_assign ((l,_),(e,_)) -> 
          let p = D.bwdAssign nextP (l,e) in
          InvMap.add blockLabel p invMap
        | _ -> InvMap.add blockLabel nextP invMap
    in
    bwd program.mainFunction.funcBody fp

end







module FutureIterator(D: RANKING_FUNCTION) = struct

  let print fmt m =
    if !compress then
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print (D.compress a)) m
    else
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print a) m


  let compute (program:program) (property:bExp) : D.t InvMap.t =
    let invMap = ref InvMap.empty in
    let addInv l (a:D.t) = invMap := InvMap.add l a !invMap in
    let rec bwdStm (p:D.t) (s:stmt) =
      match s with
      | A_label (l,_) ->
        let p = try D.reset p property with Not_found -> p in p
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
          let i' = D.reset (D.join APPROXIMATION p1 p2) property in
          if !tracebwd && not !minimal then
            begin
              Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
              Format.fprintf !fmt "p1: %a\n" D.print p1;
              Format.fprintf !fmt "i: %a\n" D.print i;
              Format.fprintf !fmt "p2: %a\n" D.print p2;
              Format.fprintf !fmt "i': %a\n" D.print i';
            end;
          if (D.isLeq COMPUTATIONAL i' i)
          then
            if (D.isLeq APPROXIMATION i' i)
            then
              let i = i in
              if !tracebwd && not !minimal then
                begin
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
            if !tracebwd && not !minimal then
              Format.fprintf !fmt "i'': %a\n" D.print i'';
            aux i'' (D.filter (bwdBlk i'' s) b) (n+1)
        in
        let i = D.bot program.environment program.variables in
        let p2 = D.filter (bwdBlk i s) b in
        let p = aux i p2 1 in
        addInv l p; 
        p
      | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
      | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")
    and bwdBlk (p:D.t) (b:block) : D.t =
      match b with
      | A_empty l ->
        let p = D.reset p property in
        if !tracebwd && not !minimal then
          Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p;
        addInv l p; 
        p
      | A_block (l,(s,_),b) ->
        stop := Sys.time ();
        if ((!stop -. !start) > !timeout)
        then raise Timeout
        else
          let b = bwdBlk p b in
          let p = bwdStm b s in
          let p = D.reset p property in
          if !tracebwd && not !minimal then
            Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p;
          addInv l p; 
          p
    in 
    if !tracebwd && not !minimal then Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let bot = D.bot program.environment program.variables in
    let _ = bwdBlk (bwdBlk bot program.mainFunction.funcBody) program.globalBlock in
    !invMap

end



module ACTLIterator(D: RANKING_FUNCTION) = struct

  module ForwardIteratorB = ForwardIterator(D.B)
  module AtomicIteratorD = AtomicIterator(D)
  module FutureIteratorD = FutureIterator(D)

  let printForwardInvMap fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.B.print a) m

  let printInvMap fmt m =
    if !compress then
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print (D.compress a)) m
    else
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print a) m

  let compute (program:program) (formula:formula) : D.t InvMap.t = 
    (*let fwdInvMap = ForwardIteratorB.compute program in*)
    let bwdInvMap = 
      match formula with
      | ACTL_atomic property -> 
        AtomicIteratorD.compute program property
      | ACTL_future property -> 
        FutureIteratorD.compute program property
      | _ -> raise (Invalid_argument "ACTL formula not yet suppoerted")
   in
   if not !minimal then
     begin
       Format.fprintf !fmt "\nBackward Analysis:\n";
       printInvMap !fmt bwdInvMap;
     end;
   bwdInvMap


end





