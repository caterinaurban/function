(* ********* Forward Iterator ************ Copyright (C) 2012-2014 by
   Caterina Urban. All rights reserved. *)

open AbstractSyntax
open InvMap
open Apron
open Functions
open Iterator
open Domain
open Partition

module ForwardIterator (B : PARTITION) = struct
  (* compute invariant map based on forward analysis *)
  let compute (vars, stmts, funcs) main env =
    let fwdInvMap = ref InvMap.empty in
    let addFwdInv l (a : B.t) = fwdInvMap := InvMap.add l a !fwdInvMap in
    let rec fwdStm p s =
      match s with
      | A_label _ -> p
      | A_return -> B.bot env vars
      | A_assign ((l, _), (e, _)) -> B.fwdAssign p (l, e)
      | A_assert (b, _) -> B.filter p b
      | A_if ((b, ba), s1, s2) ->
          let p1 = fwdBlk (B.filter p b) s1 in
          let p2 = fwdBlk (B.filter p (fst (negBExp (b, ba)))) s2 in
          B.join p1 p2
      | A_while (l, (b, ba), s) ->
          let rec aux i p2 n =
            let i' = B.join p p2 in
            if !tracefwd && not !minimal then (
              Format.fprintf !fmt "### %a:%i ###:\n" label_print l n ;
              Format.fprintf !fmt "p: %a\n" B.print p ;
              Format.fprintf !fmt "i: %a\n" B.print i ;
              Format.fprintf !fmt "p2: %a\n" B.print p2 ;
              Format.fprintf !fmt "i': %a\n" B.print i' ) ;
            if B.isLeq i' i then i
            else
              let i'' = if n <= !joinfwd then i' else B.widen i i' in
              if !tracefwd && not !minimal then
                Format.fprintf !fmt "i'': %a\n" B.print i'' ;
              aux i'' (fwdBlk (B.filter i'' b) s) (n + 1)
          in
          let i = B.bot env vars in
          let p2 = fwdBlk (B.filter i b) s in
          let p = aux i p2 1 in
          addFwdInv l p ;
          B.filter p (fst (negBExp (b, ba)))
      | A_call (f, ss) ->
          let f = StringMap.find f funcs in
          let p = List.fold_left (fun ap (s, _) -> fwdStm p s) p ss in
          fwdBlk p f.funcBody
      | A_recall (f, ss) -> raise (Invalid_argument "fwdStm:A_recall")
    and fwdBlk (p : B.t) (b : block) : B.t =
      match b with
      | A_empty l ->
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p ;
          addFwdInv l p ;
          p
      | A_block (l, (s, _), b) ->
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p ;
          addFwdInv l p ;
          fwdBlk (fwdStm p s) b
    in
    let f = StringMap.find main funcs in
    let s = f.funcBody in
    let _ = fwdBlk (fwdBlk (B.top env vars) stmts) s in
    !fwdInvMap
end
