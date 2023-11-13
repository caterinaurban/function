(* ********* Forward/Backward Guarantee Iterator ************ Copyright (C)
   2012-2014 by Caterina Urban. All rights reserved. *)

open AbstractSyntax
open InvMap
open ForwardIterator
open Apron
open Domain
open Functions
open Iterator

module GuaranteeIterator (D : RANKING_FUNCTION) = struct
  module D = D
  module B = D.B
  module ForwardIteratorB = ForwardIterator (D.B)

  let fwdInvMap = ref InvMap.empty

  let fwdMap_print fmt m =
    InvMap.iter
      (fun l a -> Format.fprintf fmt "%a: %a\n" label_print l B.print a)
      m

  let bwdInvMap = ref InvMap.empty

  let addBwdInv l (a : D.t) = bwdInvMap := InvMap.add l a !bwdInvMap

  let bwdMap_robust fmt m =
    let print fmt join =
      Format.printf "@[" ;
      List.iter
        (fun (l, nl, cns, j) ->
          Format.printf "@[" ;
          let _ =
            match l with
            | [] -> Format.printf "robust finished \n "
            | _ ->
                let _ =
                  Format.printf "@[ --   uncontrolled  -- \n " ;
                  List.iter
                    (fun x -> Format.printf "%s{%s}-" x.varId x.varName)
                    l ;
                  Format.printf "@]\n"
                in
                let _ =
                  Format.printf "@[ --   controlled  -- \n " ;
                  List.iter
                    (fun x -> Format.printf "%s{%s}-" x.varId x.varName)
                    nl ;
                  Format.printf "@]\n"
                in
                let _ = Format.printf "@[  -- constraints   --  \n" in
                let _ =
                  Array.iter
                    (fun c ->
                      Abstract1.print Format.std_formatter c ;
                      Format.printf "\n" )
                    cns
                in
                Format.print_newline () ;
                Format.printf "@[ -- Join constraint --  " ;
                let _ = Abstract1.print Format.std_formatter j in
                Format.printf "@]\n"
          in
          Format.printf "@]" ; Format.print_newline () )
        join ;
      Format.printf "@]"
    in
    let m = InvMap.find 2 m in
    let m = if !compress then D.robust (D.compress m) else D.robust m in
    print fmt m

  let bwdMap_print robust fmt m =
    if robust then bwdMap_robust fmt m
    else if !compress then
      InvMap.iter
        (fun l a ->
          Format.fprintf fmt "%a:\n%a\n" label_print l D.print (D.compress a)
          )
        m
    else
      InvMap.iter
        (fun l a -> Format.fprintf fmt "%a:\n%a\n" label_print l D.print a)
        m

  (* Backward Iterator *)

  let rec bwdStm property funcs env vars p s =
    match s with
    | A_label (l, _) ->
        let p =
          try D.reset p (fst (StringMap.find l property))
          with Not_found -> p
        in
        p
    | A_return -> D.bot env vars
    | A_assign ((l, _), (e, _)) -> D.bwdAssign p (l, e)
    | A_assert (b, _) -> D.filter p b
    | A_if ((b, ba), s1, s2) ->
        let p1 = D.filter (bwdBlk property funcs env vars p s1) b in
        let p2 =
          D.filter
            (bwdBlk property funcs env vars p s2)
            (fst (negBExp (b, ba)))
        in
        D.join APPROXIMATION p1 p2
    | A_while (l, (b, ba), s) ->
        let p1 = D.filter p (fst (negBExp (b, ba))) in
        let rec aux i p2 n =
          let i' =
            D.reset
              (D.join APPROXIMATION p1 p2)
              (fst (StringMap.find "" property))
          in
          if !tracebwd && not !minimal then (
            Format.fprintf !fmt "### %a:%i ###:\n" label_print l n ;
            Format.fprintf !fmt "p1: %a\n" D.print p1 ;
            Format.fprintf !fmt "i: %a\n" D.print i ;
            Format.fprintf !fmt "p2: %a\n" D.print p2 ;
            Format.fprintf !fmt "i': %a\n" D.print i' ) ;
          if D.isLeq COMPUTATIONAL i' i then (
            if D.isLeq APPROXIMATION i' i then (
              let i = i in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l ;
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i: %a\n" D.print i ;
              i )
            else
              let i'' = if n <= !joinbwd then i' else D.widen i i' in
              if !tracebwd && not !minimal then
                Format.fprintf !fmt "i'': %a\n" D.print i'' ;
              aux i''
                (D.filter (bwdBlk property funcs env vars i'' s) b)
                (n + 1) )
          else
            let i'' =
              if n <= !joinbwd then i'
              else D.widen i (D.join COMPUTATIONAL i i')
            in
            if !tracebwd && not !minimal then
              Format.fprintf !fmt "i'': %a\n" D.print i'' ;
            aux i''
              (D.filter (bwdBlk property funcs env vars i'' s) b)
              (n + 1)
        in
        let i = D.bot env vars in
        let p2 = D.filter (bwdBlk property funcs env vars i s) b in
        let p = aux i p2 1 in
        addBwdInv l p ; p
    | A_call (f, ss) -> raise (Invalid_argument "bwdStm:A_call")
    | A_recall (f, ss) -> raise (Invalid_argument "bwdStm:A_recall")

  and bwdBlk property funcs env vars (p : D.t) (b : block) : D.t =
    match b with
    | A_empty l ->
        let a =
          try InvMap.find l !fwdInvMap
          with Not_found ->
            let l = string_of_int l in
            failwith l
        in
        let p = if !refine then D.refine p a else p in
        let p = D.reset p (fst (StringMap.find "" property)) in
        if !tracebwd && not !minimal then
          Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p ;
        addBwdInv l p ;
        p
    | A_block (l, (s, _), b) ->
        stop := Sys.time () ;
        if !stop -. !start > !timeout then raise Timeout
        else
          let b = bwdBlk property funcs env vars p b in
          let p = bwdStm property funcs env vars b s in
          let a = InvMap.find l !fwdInvMap in
          let p = if !refine then D.refine p a else p in
          let p = D.reset p (fst (StringMap.find "" property)) in
          if !tracebwd && not !minimal then
            Format.fprintf !fmt "### %a ###:\n%a\n" label_print l D.print p ;
          addBwdInv l p ;
          p
  (* CDA *)

  let cda_recursive robust property funcs env vars s stmts main =
    let compress () =
      bwdInvMap := InvMap.map (fun a -> D.compress a) !bwdInvMap
    in
    let reinit () =
      bwdInvMap := InvMap.map (fun a -> D.reinit a) !bwdInvMap
    in
    let rec aux b p n =
      (* Forward Analysis *)
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "\nForward Analysis[%i] Trace:\n" n ;
      let startfwd = Sys.time () in
      fwdInvMap := ForwardIteratorB.compute (vars, stmts, funcs) main env ;
      let stopfwd = Sys.time () in
      if not !minimal then (
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis[%i] (Time: %f s):\n" n
            (stopfwd -. startfwd)
        else Format.fprintf !fmt "\nForward Analysis[%i]:\n" n ;
        fwdMap_print !fmt !fwdInvMap ) ;
      (* Backward Analysis *)
      if !tracebwd && not !minimal then
        Format.fprintf !fmt "\nBackward Analysis[%i] Trace:\n" n ;
      start := Sys.time () ;
      let startbwd = Sys.time () in
      let i =
        bwdBlk property funcs env vars
          (bwdBlk property funcs env vars (D.zero env vars) s)
          stmts
      in
      let stopbwd = Sys.time () in
      if not !minimal then (
        if !timebwd then
          Format.fprintf !fmt "\nBackward Analysis[%i] (Time: %f s):\n" n
            (stopbwd -. startbwd)
        else Format.fprintf !fmt "\nBackward Analysis[%i]:\n" n ;
        bwdMap_print robust !fmt !bwdInvMap ) ;
      if not !minimal then
        if D.terminating i then
          Format.fprintf !fmt "Analysis[%i] Result: TRUE\n" n
        else Format.fprintf !fmt "Analysis[%i] Result: UNKNOWN\n" n ;
      if D.terminating i || n > !size then
        (* End if we can already infer termination or if max number of
           iteration is reached*)
        D.learn p (D.compress i)
      else (
        learn := true ;
        (* Cumulate the constraints along the path too an undefined piece of
           the ranking function *)
        let bs = D.conflict i in
        if not !minimal then (
          Format.fprintf !fmt "CONFLICTS: { " ;
          List.iter (fun b -> Format.fprintf !fmt "%a; " B.print b) bs ;
          Format.fprintf !fmt "}\n" ) ;
        let i =
          List.fold_left
            (fun ai ab ->
              (* ai is a tree, ab is a contraint toward an undefined part of
                 the ranking function *)
              if
                B.isLeq b ab
                (* Check if we are already precise enough if not we divide
                   the constraint and relaunch the analysis *)
              then (
                let b1, b2 = B.assume ab in
                reinit () ;
                compress () ;
                if not !minimal then
                  Format.fprintf !fmt "\nASSUME-1: %a\n" B.print b1 ;
                (* restart with b1 *)
                let i = aux b1 ai (n + 1) in
                reinit () ;
                compress () ;
                if not !minimal then
                  Format.fprintf !fmt "\nASSUME-2: %a\n" B.print b2 ;
                (* continue with b2*)
                aux b2 i (n + 1) )
              else (
                reinit () ;
                compress () ;
                if not !minimal then
                  Format.fprintf !fmt "\nASSUME: %a\n" B.print ab ;
                aux ab ai (n + 1) ) )
            i bs
        in
        D.learn p (D.compress i) )
    in
    aux (B.top env vars) (D.bot env vars) 1

  (* Analyzer *)

  let analyze robust property (vars, stmts, funcs) main =
    let rec aux xs env =
      match xs with
      | [] -> env
      | x :: xs ->
          aux xs (Environment.add env [|Var.of_string x.varId|] [||])
    in
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    let env = aux vars (Environment.make [||] [||]) in
    let s = f.funcBody in
    (* Forward Analysis *)
    if !cda then (
      let i = cda_recursive robust property funcs env vars s stmts main in
      if not !minimal then
        Format.fprintf !fmt "\nConflict-Driven Analysis Result: %a@." D.print
          i ;
      if robust then bwdMap_robust fmt (InvMap.add 2 i InvMap.empty) ;
      D.defined i )
    else (
      if !tracefwd && not !minimal then
        Format.fprintf !fmt "\nForward Analysis Trace:\n" ;
      let startfwd = Sys.time () in
      fwdInvMap := ForwardIteratorB.compute (vars, stmts, funcs) main env ;
      let stopfwd = Sys.time () in
      if not !minimal then (
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n"
            (stopfwd -. startfwd)
        else Format.fprintf !fmt "\nForward Analysis:\n" ;
        fwdMap_print !fmt !fwdInvMap ) ;
      (* Backward Analysis *)
      if !tracebwd && not !minimal then
        Format.fprintf !fmt "\nBackward Analysis Trace:\n" ;
      start := Sys.time () ;
      let startbwd = Sys.time () in
      let i =
        bwdBlk property funcs env vars
          (bwdBlk property funcs env vars (D.bot env vars) s)
          stmts
      in
      let stopbwd = Sys.time () in
      if not !minimal then
        if !timebwd then
          Format.fprintf !fmt "\nBackward Analysis (Time: %f s):\n"
            (stopbwd -. startbwd)
        else bwdMap_print robust !fmt !bwdInvMap ;
      D.defined i )
end
