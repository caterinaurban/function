open AbstractSyntax
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator

type ctl_property = AbstractSyntax.bExp CTLProperty.generic_formula

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


module OrIterator(D: RANKING_FUNCTION) = struct

  let compute (program:program) (fp1:D.t InvMap.t) (fp2:D.t InvMap.t) : D.t InvMap.t =
    let join _ t1 t2 = Some (D.join Functions.COMPUTATIONAL t1 t2) in
    InvMap.union join fp1 fp2

end


module AndIterator(D: RANKING_FUNCTION) = struct

  let compute (program:program) (fp1:D.t InvMap.t) (fp2:D.t InvMap.t) : D.t InvMap.t =
    let join _ t1 t2 = Some (D.join Functions.APPROXIMATION t1 t2) in
    InvMap.union join fp1 fp2

end


module NextIterator(D: RANKING_FUNCTION) = struct

  let compute (program:program) (fp:D.t InvMap.t) : D.t InvMap.t =
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

  module AtomicIteratorD = AtomicIterator(D)
  module FutureIteratorD = FutureIterator(D)
  module NextIteratorD = NextIterator(D)

  let printForwardInvMap fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.B.print a) m

  let printInvMap fmt m =
    if !compress then
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print (D.compress a)) m
    else
      InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print a) m

  let compute (program:program) (property:ctl_property) : D.t InvMap.t = 
    let bwdInvMap = 
      match property with
      | Atomic b -> AtomicIteratorD.compute program b
      | AX (Atomic b) -> 
        let invMap = AtomicIteratorD.compute program b in
        NextIteratorD.compute program invMap
      | _ -> raise (Invalid_argument "ACTL formula not yet suppoerted")
   in
   if not !minimal then
     begin
       Format.fprintf !fmt "\nBackward Analysis:\n";
       printInvMap !fmt bwdInvMap;
     end;
   bwdInvMap

end

