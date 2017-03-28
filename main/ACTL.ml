open AbstractSyntax
open Apron
open Domain
open Partition
open Functions
open Iterator

type formula =
  | ACTL_atomic of AbstractSyntax.bExp
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


module type AbstractSematic = sig
  
  type t

  val compute : program -> t InvMap.t

end


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


  let compute (program:program) (fwdInvMap: D.B.t InvMap.t) (property:bExp) : D.t InvMap.t = 
    let bot = D.bot program.environment program.variables in
    let atomicState = D.reset bot property in
    let mapState = 
      if !refine then
        (fun a -> D.refine atomicState a)
      else 
        (fun _ -> atomicState) 
    in
    let invMap = InvMap.map mapState fwdInvMap in

    invMap


end


module ACTLIterator(D: RANKING_FUNCTION) = struct

  module ForwardIteratorB = ForwardIterator(D.B)
  module AtomicIteratorD = AtomicIterator(D)

  let print fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l D.print a) m

  let compute (program:program) (formula:formula) : D.t InvMap.t = 
    let fwdInvMap = ForwardIteratorB.compute program in
    let bwdInvMap = 
      match formula with
      | ACTL_atomic property -> 
        AtomicIteratorD.compute program fwdInvMap property
      | _ -> raise (Invalid_argument "ACTL formula not yet suppoerted")
   in
   if not !minimal then
     begin
       Format.fprintf !fmt "\nBackward Analysis:\n";
       print !fmt bwdInvMap;
     end;
   bwdInvMap


end





