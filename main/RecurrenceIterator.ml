(*   
     ********* Forward/Backward Recurrence Iterator ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

open AbstractSyntax
open Apron
open Domain
open Functions
open Iterator

module RecurrenceIterator (D: RANKING_FUNCTION) =
struct

  module D = D

  module B = D.B

  (* Invariant Map *)

  module InvMap = Map.Make(struct type t=label let compare=compare end)

  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l B.print a) m

  let bwdInvMap = ref InvMap.empty

  let addBwdInv l (a:D.t) = bwdInvMap := InvMap.add l a !bwdInvMap

  let bwdMap_print fmt m =
    if !compress then
      InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\n" label_print l D.print (D.compress a)) m
    else
      InvMap.iter (fun l a -> Format.fprintf fmt "%a:\n%a\n" label_print l D.print a) m

  (* Forward Iterator *)

  let rec fwdStm funcs env vars p s =
    match s with
    | A_label _ -> p
    | A_return -> B.bot env vars
    | A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
    | A_assert (b,_) -> B.filter p b
    | A_if ((b,ba),s1,s2) ->
      let p1 = fwdBlk funcs env vars (B.filter p b) s1 in
      let p2 = fwdBlk funcs env vars (B.filter p (fst (negBExp (b,ba)))) s2 in
      B.join p1 p2
    | A_while (l,(b,ba),s) ->
      let rec aux i p2 n =
        let i' = B.join p p2 in
        if !tracefwd && not !minimal then
          Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
        if !tracefwd && not !minimal then
          Format.fprintf !fmt "p: %a\n" B.print p;
        if !tracefwd && not !minimal then
          Format.fprintf !fmt "i: %a\n" B.print i;
        if !tracefwd && not !minimal then
          Format.fprintf !fmt "p2: %a\n" B.print p2;
        if !tracefwd && not !minimal then
          Format.fprintf !fmt "i': %a\n" B.print i';
        if B.isLeq i' i then i
        else
          let i'' = if n <= !joinfwd then i' else B.widen i i' in
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "i'': %a\n" B.print i'';
          aux i'' (fwdBlk funcs env vars (B.filter i'' b) s) (n+1)
      in
      let i = B.bot env vars in
      let p2 = fwdBlk funcs env vars (B.filter i b) s in
      let p = aux i p2 1 in
      addFwdInv l p;
      B.filter p (fst (negBExp (b,ba)))
    | A_call (f,ss) ->
      let f = StringMap.find f funcs in
      let p = List.fold_left (fun ap (s,_) -> fwdStm funcs env vars p s) p ss in
      fwdBlk funcs env vars p f.funcBody
    | A_recall (f,ss) -> raise (Invalid_argument "fwdStm:A_recall")

  and fwdBlk funcs env vars (p:B.t) (b:block) : B.t =
    match b with
    | A_empty l ->
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p;
      addFwdInv l p; p
    | A_block (l,(s,_),b) ->
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p;
      addFwdInv l p; fwdBlk funcs env vars (fwdStm funcs env vars p s) b

  (* Backward Iterator *)

  let rec bwdStm property funcs env vars p s =
    match s with
    | A_label (l,_) ->
      let p = try D.reset p (fst (StringMap.find l property)) with Not_found -> p in p (* TODO: is this OK? *)
    | A_return -> D.bot env vars
    | A_assign ((l,_),(e,_)) -> D.bwdAssign p (l,e)
    | A_assert (b,_) -> D.filter p b
    | A_if ((b,ba),s1,s2) ->
      let p1 = D.filter (bwdBlk property funcs env vars p s1) b in
      let p2 = D.filter (bwdBlk property funcs env vars p s2) (fst (negBExp (b,ba))) in
      D.join APPROXIMATION p1 p2
    | A_while (l,(b,ba),s) ->
      let p1 = D.filter p (fst (negBExp (b,ba))) in
      let rec aux m o =
        let rec auxaux i p2 n =
          let i' = D.reset ~mask:m (D.join APPROXIMATION p1 p2) (fst (StringMap.find "" property)) in
          if !tracebwd && not !minimal then begin
            Format.fprintf !fmt "### %a-INNER:%i ###:\n" label_print l n;
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
                Format.fprintf !fmt "### %a-INNER:FIXPOINT ###:\n" label_print l;
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i: %a\n" D.print i;
              i
            else
              let i'' = if n <= !joinbwd then i' else D.widen i i' in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i'': %a\n" D.print i'';
              auxaux i'' (D.filter (bwdBlk property funcs env vars i'' s) b) (n+1)
          else
            let i'' = if n <= !joinbwd then i' else D.widen i (D.join COMPUTATIONAL i i') in
            if !tracebwd && not !minimal then
              Format.fprintf !fmt "i'': %a\n" D.print i'';
            auxaux i'' (D.filter (bwdBlk property funcs env vars i'' s) b) (n+1)
        in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "### %a-OUTER:%i ###:\n" label_print l o;
        if !tracebwd && not !minimal then
          Format.fprintf !fmt "m: %a\n" D.print m;

        let p2 = D.filter (D.bot env vars) b in
        let p = auxaux (D.bot env vars) p2 1 in					
        let p = D.join APPROXIMATION p1 (D.filter (bwdBlk property funcs env vars p s) b) in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "p: %a\n" D.print p;

        let m' = D.meet APPROXIMATION m p in

        if !tracebwd && not !minimal then
          Format.fprintf !fmt "m': %a\n" D.print m';

        if (D.isLeq COMPUTATIONAL m' m) && (D.isLeq COMPUTATIONAL m m')
        then
          let p = p in
          if !tracebwd && not !minimal then begin
            Format.fprintf !fmt "### %a-OUTER:FIXPOINT ###:\n" label_print l;
            Format.fprintf !fmt "m: %a\n" D.print m;
          end;
          p
        else
          let m'' = if o <= !meetbwd then m' else D.dual_widen m m' in
          if !tracebwd && not !minimal then
            Format.fprintf !fmt "m'': %a\n" D.print m'';
          aux m'' (o+1)
      in
      let p = aux (D.top env vars) 1 in
      addBwdInv l p; p
    | A_call (f,ss) -> raise (Invalid_argument "bwdStm:A_call")
    | A_recall (f,ss) -> raise (Invalid_argument "bwdStm:A_recall")

  and bwdBlk property funcs env vars (p:D.t) (b:block) : D.t =
    match b with
    | A_empty l ->
      let a = InvMap.find l !fwdInvMap in
      let p = if !refine then D.refine p a else p in
      let m = if !refine then D.meet APPROXIMATION (D.refine (D.top env vars) a) p else D.meet APPROXIMATION (D.top env vars) p in
      let p = D.reset ~mask:m p (fst (StringMap.find "" property)) in
      if !tracebwd && not !minimal then
        Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p;
      addBwdInv l p; p			  
    | A_block (l,(s,_),b) ->
      stop := Sys.time ();
      if ((!stop -. !start) > !timeout)
      then raise Timeout
      else
        let b = bwdBlk property funcs env vars p b in
        let p = bwdStm property funcs env vars b s in
        let a = InvMap.find l !fwdInvMap in
        let p = if !refine then D.refine p a else p in
        let m = if !refine then D.meet APPROXIMATION (D.refine (D.top env vars) a) p else D.meet APPROXIMATION (D.top env vars) p in
        let p = D.reset ~mask:m p (fst (StringMap.find "" property)) in
        if !tracebwd && not !minimal then
          Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p;
        addBwdInv l p; p

  (* Analyzer *)

  let analyze property (vars,stmts,funcs) main =
    let rec aux xs env =
      match xs with
      | [] -> env
      | x::xs -> aux xs (Environment.add env [|(Var.of_string x.varId)|] [||])
    in
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    let env = aux vars (Environment.make [||] [||]) in
    let s = f.funcBody in
    (* Forward Analysis *)
    if !tracefwd && not !minimal then
      Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
    let _ = fwdBlk funcs env vars (fwdBlk funcs env vars (B.top env vars) stmts) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap;
      end;
    (* Backward Analysis *)
    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
    let i = bwdBlk property funcs env vars (bwdBlk property funcs env vars (D.bot env vars) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then
      begin
        if !timebwd then
          Format.fprintf !fmt "\nBackward Analysis (Time: %f s):\n" (stopbwd-.startbwd)
        else
          Format.fprintf !fmt "\nBackward Analysis:\n";
        bwdMap_print !fmt !bwdInvMap;
      end;
    D.terminating i

end
