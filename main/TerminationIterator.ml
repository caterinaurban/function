(***************************************************)
(*                                                 *)
(*      Forward/Backward Termination Iterator      *)
(*                                                 *)
(*                  Caterina Urban                 *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Domain
open Functions
open Iterator

module TerminationIterator (D: RANKING_FUNCTION) =
	struct

		module D = D

		module B = D.B

		(* Invariant Map *)

		module InvMap = Map.Make(struct type t=label let compare=compare end)

		let fwdInvMap = ref InvMap.empty

		let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

		let fwdMap_print fmt m = InvMap.iter (fun l a -> 
			Format.fprintf fmt "%a: %a\n" label_print l B.print a) m

		let bwdInvMap = ref InvMap.empty

		let addBwdInv l (a:D.t) = bwdInvMap := InvMap.add l a !bwdInvMap

		let bwdMap_print fmt m = InvMap.iter (fun l a -> 
			Format.fprintf fmt "%a: %a\n" label_print l D.print a) m

		(* Forward Iterator *)

		let rec fwdStm funcs env vars p s =
			match s with
			| A_label _ -> p
			| A_return -> B.bot env vars
			| A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
			| A_assert (b,_) -> B.filter p b
			| A_if ((b,ba),s1,s2) ->
				let p1' = fwdBlk funcs env vars (B.filter p b) s1 in
				let p2 = B.filter p (fst (negBExp (b,ba))) in
				let p2' = fwdBlk funcs env vars p2 s2 in
				B.join p1' p2'
			| A_while (l,(b,ba),s) ->
				let rec aux i p2 n =
					let i' = B.join p p2 in
					if !tracefwd && not !minimal then begin
						Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
						Format.fprintf !fmt "p1: %a\n" B.print p;
						Format.fprintf !fmt "i: %a\n" B.print i;
						Format.fprintf !fmt "p2: %a\n" B.print p2;
						Format.fprintf !fmt "i': %a\n" B.print i'
					end;
					if B.isLeq i' i then i else
						let i'' = if n <= !joinfwd then i' else 
							B.widen i (B.join i i') in
						if !tracefwd && not !minimal then
							Format.fprintf !fmt "i'': %a\n" B.print i'';
						aux i'' (fwdBlk funcs env vars (B.filter i'' b) s) (n+1)
				in
				let i = B.bot env vars in
				let p2 = fwdBlk funcs env vars (B.filter i b) s in
				let p = aux i p2 1 in
				addFwdInv l p; B.filter p (fst (negBExp (b,ba)))
			| A_call (f,ss) ->
				let f = StringMap.find f funcs in
				let p = List.fold_left (fun ap (s,_) -> 
					fwdStm funcs env vars p s) p ss in
				fwdBlk funcs env vars p f.funcBody
			| A_recall (f,ss) -> B.top env vars (* TODO *)

		and fwdBlk funcs env vars (p:B.t) (b:block) : B.t =
			let result_print l p =
				Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p
			in
			match b with
			| A_empty l ->
				if !tracefwd && not !minimal then result_print l p;
				addFwdInv l p; p
			| A_block (l,(s,_),b) ->
				if !tracefwd && not !minimal then result_print l p;
				addFwdInv l p; 
				fwdBlk funcs env vars (fwdStm funcs env vars p s) b

		(* Backward Iterator + Recursion *)

		let rec bwdStm ?domain funcs env vars (p,r,flag) s =
			match s with
			| A_label _ -> (p,r,flag)
			| A_return -> D.zero ?domain:domain env vars, r, flag
			| A_assign ((l,_),(e,_)) ->
				D.bwdAssign ?domain:domain p (l, e), r, flag
			| A_assert (b,_) ->
				D.filter ?domain:domain p b, r, flag
			| A_if ((b,ba),s1,s2) ->
				let (p1, _, flag1) = bwdBlk funcs env vars (p, r, flag) s1 in
				let p1 = D.filter ?domain:domain p1 b in					
				let (p2, _, flag2) = bwdBlk funcs env vars (p, r, flag) s2 in
				let p2 = D.filter ?domain:domain p2 (fst (negBExp (b, ba))) in
				if !tracebwd && not !minimal then begin
					Format.fprintf !fmt "p1: %a\n" D.print p1;
					Format.fprintf !fmt "p2: %a\n" D.print p2
				end;
				(D.join APPROXIMATION p1 p2, r, flag1 || flag2)
			| A_while (l, (b, ba), s) ->
				let a = InvMap.find l !fwdInvMap in
				let dm = if !refine then Some a else None in
				let p1 = D.filter ?domain:dm p (fst (negBExp (b, ba))) in
				let rec aux (i, _, _) (p2, _, flag2) n =
					if !abort then raise Abort else
					let i' = D.join APPROXIMATION p1 p2 in
					if !tracebwd && not !minimal then begin
						Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
						Format.fprintf !fmt "p1: %a\n" D.print p1;
						Format.fprintf !fmt "i: %a\n" D.print i;
						Format.fprintf !fmt "p2: %a\n" D.print p2;
						Format.fprintf !fmt "i': %a\n" D.print i'
					end;
					let jokers = max 0 (!retrybwd * (!Ordinals.max + 1) - n + !joinbwd) in
					if (D.isLeq COMPUTATIONAL i' i) then
						if (D.isLeq APPROXIMATION i' i) then begin
							if !tracebwd && not !minimal then begin
								Format.fprintf !fmt "### %a:FIXPOINT ###:\n" label_print l;
								Format.fprintf !fmt "i: %a\n" D.print i
							end;
							(i, r, flag2)
						end else begin
							let i'' =
								if n <= !joinbwd then
									i'
								else
									D.widen ~jokers:jokers i i'
							in
							if !tracebwd && not !minimal then
								Format.fprintf !fmt "i'': %a\n" D.print i'';
							let (p2, _, flag2) = 
								bwdBlk funcs env vars (i'', r, flag2) s in
							let p2' = D.filter ?domain:dm p2 b in
							aux (i'', r, flag2) (p2', r, flag2) (n + 1)
						end
					else
						let i'' =
							if n <= !joinbwd then
								i'
							else
								D.widen ~jokers:jokers i
										(D.join COMPUTATIONAL i i')
						in
						if !tracebwd && not !minimal then
							Format.fprintf !fmt "i'': %a\n" D.print i'';
						let (p2, _, flag2) = 
							bwdBlk funcs env vars (i'', r, flag2) s in
						let p2' = D.filter ?domain:dm p2 b in
						aux (i'', r, flag2) (p2', r, flag2) (n + 1)
				in
				let i = (D.bot ?domain:dm env vars, r, flag) in
				let (p2, _, flag2) = bwdBlk funcs env vars i s in
				let p2' = D.filter ?domain:dm p2 b in
				let (p, r, flag) = aux i (p2', r, flag2) 1 in
				addBwdInv l p;
				if !refine then
					(D.refine p a, r, flag)
				else
					(p, r, flag)
			| A_call (f, ss) ->
				let f = StringMap.find f funcs in
				let p = bwdRec funcs env vars p f.funcBody in
				List.fold_left (fun (ap, ar, aflag) (s, _) ->
					bwdStm ?domain:domain funcs env vars (ap, ar, aflag) s
				) (p, r, flag) ss
			| A_recall (f, ss) ->
				(match domain with
				 | None ->
					List.fold_left (fun (ap, ar, aflag) (s, _) ->
						bwdStm funcs env vars (ap, ar, aflag) s
					) (D.join APPROXIMATION p r, r, true) ss
				 | Some domain ->
					List.fold_left (fun (ap, ar, aflag) (s, _) ->
						bwdStm ~domain:domain funcs env vars (ap, ar, aflag) s
					) (r, r, true) ss)

		and bwdBlk funcs env vars (p,r,flag) (b:block) : D.t * D.t * bool =
			let result_print l p =
				Format.fprintf !fmt "### %a ###:\n%a@." label_print l D.print p
			in
			match b with
			| A_empty l ->
				let a = InvMap.find l !fwdInvMap in
				let p = if !refine then D.refine p a else p in
				if !tracebwd && not !minimal then result_print l p;
				addBwdInv l p; (p,r,flag)
			| A_block (l,(s,_),b) ->
				stop := Sys.time ();
				if ((!stop -. !start) > !timeout)
				then raise Timeout
				else
					let (b,rb,flagb) = bwdBlk funcs env vars (p,r,flag) b in
					let a = InvMap.find l !fwdInvMap in
					let (p,r,flag) = if !refine then 
						bwdStm ~domain:a funcs env vars (b,rb,flagb) s 
					else bwdStm funcs env vars (b,rb,flagb) s in
					let p = if !refine then D.refine p a else p in
					if !tracebwd && not !minimal then result_print l p;
					addBwdInv l p; (p,r,flag)

		and bwdRec funcs env vars (p:D.t) (b:block) : D.t =
			let rec aux r n =
				let (r',_,_) = bwdBlk funcs env vars (r,r,true) b in
				if !tracebwd && not !minimal then begin
					Format.fprintf !fmt "r: %a@." D.print r;
					Format.fprintf !fmt "r': %a@." D.print r'
				end;
				if (D.isLeq COMPUTATIONAL r' r)
				then
					if (D.isLeq APPROXIMATION r' r)
					then
						let r = r in
						if !tracebwd && not !minimal then begin
							Format.fprintf !fmt "### REC-FIXPOINT ###:@.";
							Format.fprintf !fmt "r: %a@." D.print r
						end;
						r
					else
						let r'' = if n <= !joinbwd then r' else D.widen r r' in
						if !tracebwd && not !minimal then
							Format.fprintf !fmt "r'': %a@." D.print r'';
						aux r'' (n+1)
				else
					let r'' = if n <= !joinbwd then r' else 
						D.widen r (D.join COMPUTATIONAL r r') in
					if !tracebwd && not !minimal then
						Format.fprintf !fmt "r'': %a@." D.print r'';
					aux r'' (n+1)
			in
			let (i,_,flag) = bwdBlk funcs env vars (p,D.bot env vars,false) b in
			if flag then aux i 1
			else i

		(* Analyzer *)

		let rec initStm env vars s =
			match s with
			| A_if (_,s1,s2) -> initBlk env vars s1; initBlk env vars s2
			| A_while (l,_,s) -> 
				addBwdInv l (D.bot env vars); initBlk env vars s
			| _ -> ()

		and	initBlk env vars b =
			match b with
			| A_empty l -> addBwdInv l (D.bot env vars)
			| A_block (l,(s,_),b) -> addBwdInv l (D.bot env vars); 
				initStm env vars s; initBlk env vars b

		let analyze (vars,stmts,funcs) main =
			let rec aux xs env = match xs with
			| [] -> env
			| x::xs -> 
				aux xs (Environment.add env [|(Var.of_string x.varId)|] [||])
			in
			let f = StringMap.find main funcs in
			let v1 = snd (List.split (StringMap.bindings vars)) in
			let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
			let vars = List.append v1 v2 in
			let env = aux vars (Environment.make [||] [||]) in
			let s = f.funcBody in
			initBlk env vars stmts; initBlk env vars s; 
			(* TODO: handle functions calls *)
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
			let i = bwdRec funcs env vars (bwdRec funcs env vars (D.zero env vars) s) stmts in
			let stopbwd = Sys.time () in
			if not !minimal then begin
				if !timebwd then
					Format.fprintf !fmt "\nBackward Analysis (Time: %f s):\n" (stopbwd-.startbwd)
				else
					Format.fprintf !fmt "\nBackward Analysis:\n";
					bwdMap_print !fmt !bwdInvMap;
			end;
			D.terminating i

	end
