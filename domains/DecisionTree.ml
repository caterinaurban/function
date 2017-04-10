(***************************************************)
(*                                                 *)
(*      The Ranking Functions Abstract Domain      *)
(*                                                 *)
(*                 Caterina Urban                  *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*          ETH Zurich, Zurich, Switzerland        *)
(*                      2016                       *)
(*                                                 *)
(*              with contributions of              *)
(*                Nathanaël Courant                *)
(*     École Normale Supérieure, Paris, France     *)
(*                      2016                       *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Affines
open Apron
open Domain
open Functions
open Ordinals

let tracebwd = ref false
let retrybwd = ref 5

(** The ranking functions abstract domain is an abstract domain functor T. 
    It is parameterized by an auxiliary abstract domain for linear constraints 
    C, and an auxiliary abstract domains for functions F, both parameterized by 
    an auxiliary numerical abstract domain B. *)
module DecisionTree (F: FUNCTION) : RANKING_FUNCTION =
struct

  module B = F.B	(* auxiliary numerical abstract domain *)
  module C = B.C	(* auxiliary linear constraints abstract domain *)

  module CMap = Map.Make(
    struct
      type t = C.t
      let compare = C.compare
    end)

  module L =
  struct
    type t = C.t * C.t
    let compare (c1,nc1) (c2,nc2) =
      if (C.isLeq nc1 c1) then
        if (C.isLeq nc2 c2) then 
          C.compare c1 c2 
        else C.compare c1 nc2
      else if (C.isLeq nc2 c2) 
      then C.compare nc1 c2 
      else C.compare nc1 nc2
  end

  module LSet = Set.Make(L)

  (** The abstract domain manipulates piecewise-defined partial functions. 
      These are represented by decision trees, where the decision nodes are 
      labeled by linear constraints over the program variables, and the leaf 
      nodes are labeled by functions of the program variables. The decision 
      nodes recursively partition the space of possible values of the program 
      variables and the functions at the leaves provide the corresponding 
      upper bounds on the number of program execution steps to termination. *)
  type tree = Bot | Leaf of F.f | Node of L.t * tree * tree

  (** An element of the ranking functions abstract domain. *)
  type t = {
    domain : B.t option;	(* current reachable program states *)
    tree : tree;			(* current piecewise-defined ranking function *)
    env : Environment.t;	(* current APRON environment *)
    vars : var list			(* current list of program variables *)
  }


  (** The current decision tree. *)
  let tree t = t.tree

  (** Prints the current decision tree. *)
  let print_tree vars fmt t =
    let rec aux ind fmt t =
      match t with
      | Bot -> Format.fprintf fmt "\n%sNIL" ind
      | Leaf f ->  Format.fprintf fmt "\n%sLEAF %a" ind F.print f
      | Node ((c,_),l,r) -> Format.fprintf fmt "\n%sNODE %a%a%a" ind 
                              (C.print vars) c (aux (ind ^ "  ")) l (aux (ind ^ "  ")) r
    in aux "" fmt t

  (** Collects the linear constraints labeling the current decision tree. *)
  let tree_labels t =
    let ls = ref LSet.empty in
    let rec aux t =
      match t with
      | Bot | Leaf _ -> ()
      | Node (c,l,r) -> aux l; aux r; ls := LSet.add c !ls
    in aux t; !ls

  (** Sorts (and normalizes the constraints within) a decision tree `t`. 

      Let x_1,...,x_k be program variables. We consider all linear 
      constraints in a decision tree to have the following normal form:
      m_1*x_1 + ... + m_k*x_k + q >= 0
      where m_1,...,m_k,q are integer coefficients. Moreover, in order to 
      ensure a canonical representation of the linear constraints, we require
      gcd(|m_1|,...,|m_k|,|q|) = 1
      We then impose a total order on the linear constraints. In particular, 
      we define such order to be the lexicographic order on the coefficients 
      m_1,...,m_k and constant q of the linear constraints. *)
  let rec sort_tree t =
    let rec swap_tree t =
      match t with
      | Node((c,nc),l,r) ->
        let sl = swap_tree l in
        let sr = swap_tree r in
        if (C.isLeq nc c)
        then (* t is normalized *)
          (match sl, sr with
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isEq c1 c2) (* c1 = c2 *) ->
             if (C.isLeq c c1)
             then (* c <= c1 = c2 *)
               if (C.isEq c c1)
               then (* c = c1 = c2 *) Node((c,nc),l1,r2)
               else (* c < c1 = c2 *) Node((c,nc),sl,sr)
             else (* c > c1 = c2 *)
             if (C.similar c c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2)) 
             else
               let rt = (c,nc) in 
               Node((c1,nc1),Node(rt,l1,l2),Node(rt,r1,r2))
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c1 c2) (* c1 < c2 *) ->
             if (C.isLeq c c1)
             then (* c <= c1 < c2 *)
               if (C.isEq c c1)
               then (* c = c1 < c2 *) Node((c,nc),l1,sr)
               else (* c < c1 < c2 *) Node((c,nc),sl,sr)
             else (* c > c1 < c2 *)
             if (C.isLeq c c2)
             then (* c1 < c <= c2 *)
               if (C.isEq c c2)
               then (* c1 < c = c2 *)
                 if (C.similar c c1) 
                 then Node((c1,nc1),l1,Node((c,nc),r1,r2)) 
                 else
                   let rt = (c,nc) in
                   let rt1 = (c1,nc1) in
                   Node(rt1,Node(rt,l1,r2),Node(rt,r1,r2))
               else (* c1 < c < c2 *)
               if (C.similar c2 c) && (C.similar c c1) 
               then Node((c1,nc1),l1,Node((c,nc),r1,sr)) 
               else
                 let rt = (c,nc) in
                 let rt1 = (c1,nc1) in
                 Node(rt1,Node(rt,l1,sr),Node(rt,r1,sr))
             else (* c1 < c2 < c *)
             if (C.similar c c2) && (C.similar c2 c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2))
             else
               let rt = (c,nc) in
               let rt2 = (c2,nc2) in 
               Node((c1,nc1),
                    Node(rt2,Node(rt,l1,l2),Node(rt,l1,r2)),
                    Node(rt2,Node(rt,r1,l2),Node(rt,r1,r2))
                   )
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c2 c1) (* c1 > c2 *) ->
             if (C.isLeq c c2)
             then (* c <= c2 < c1 *)
               if (C.isEq c c2)
               then (* c = c2 < c1 *) Node((c,nc),sl,r2)
               else (* c < c2 < c1 *) Node((c,nc),sl,sr)
             else (* c > c2 < c1 *)
             if (C.isLeq c c1)
             then (* c2 < c <= c1 *)
               if (C.isEq c c1)
               then (* c2 < c = c1 *)
                 if (C.similar c c2) 
                 then Node((c,nc),l1,r2) 
                 else
                   let rt = (c,nc) in
                   let rt2 = (c2,nc2) in
                   Node(rt2,Node(rt,l1,l2),Node(rt,l1,r2))
               else (* c2 < c < c1 *)
               if (C.similar c1 c) && (C.similar c c2) 
               then Node((c,nc),l1,r2) 
               else
                 let rt = (c,nc) in 
                 let rt2 = (c2,nc2) in 
                 Node(rt2,Node(rt,sl,l2),Node(rt,sl,r2))
             else (* c2 < c1 < c *)
             if (C.similar c c1) && (C.similar c1 c2) 
             then Node((c1,nc1),l1,Node((c,nc),r1,r2))
             else
               let rt = (c,nc) in
               let rt1 = (c1,nc1) in  
               Node((c2,nc2),
                    Node(rt1,Node(rt,l1,l2),Node(rt,r1,l2)),
                    Node(rt1,Node(rt,l1,r2),Node(rt,r1,r2))
                   )
           | Node((c1,nc1),l1,r1), _ ->
             if (C.isLeq c c1)
             then (* c <= c1 *)
               if (C.isEq c c1)
               then (* c = c1 *) Node((c,nc),l1,sr)
               else (* c < c1 *) Node((c,nc),sl,sr)
             else (* c > c1 *)
             if (C.similar c c1) 
             then Node((c1,nc1),l1,Node((c,nc),r1,sr)) 
             else
               let rt = (c,nc) in 
               Node((c1,nc1),Node(rt,l1,sr),Node(rt,r1,sr))
           | _, Node((c2,nc2),l2,r2) ->
             if (C.isLeq c c2)
             then (* c <= c2 *)
               if (C.isEq c c2)
               then (* c = c2 *) Node((c,nc),sl,r2)
               else (* c < c2 *) Node((c,nc),sl,sr)
             else (* c > c2 *)
             if (C.similar c c2) 
             then Node((c,nc),sl,r2) 
             else
               let rt = (c,nc) in 
               Node((c2,nc2),Node(rt,sl,l2),Node(rt,sl,r2))
           | _ -> Node((c,nc),sl,sr) (* same *))
        else (* t is not normalized *)
          (match sl,sr with
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isEq c1 c2) (* c1 = c2 *) ->
             if (C.isLeq nc c1)
             then (* nc <= c1 = c2 *)
               if (C.isEq nc c1)
               then (* nc = c1 = c2 *) Node((nc,c),l2,r1)
               else (* nc < c1 = c2 *) Node((nc,c),sr,sl)
             else (* nc > c1 = c2 *)
             if (C.similar nc c1) 
             then Node((c1,nc1),l2,Node((nc,c),r2,r1)) 
             else
               let rt = (nc,c) in
               let rt1 = (c1,nc1) in
               Node(rt1,Node(rt,l2,l1),Node(rt,r2,r1))
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c1 c2) (* c1 < c2 *) ->
             if (C.isLeq nc c1)
             then (* nc <= c1 < c2 *)
               if (C.isEq nc c1)
               then (* nc = c1 < c2 *) Node((nc,c),sr,r1)
               else (* nc < c1 < c2 *) Node((nc,c),sr,sl)
             else (* nc > c1 < c2 *)
             if (C.isLeq nc c2)
             then (* c1 < nc <= c2 *)
               if (C.isEq nc c2)
               then (* c1 < nc = c2 *)
                 if (C.similar nc c1) 
                 then Node((nc,c),l2,r1) 
                 else
                   let rt = (nc,c) in 
                   let rt1 = (c1,nc1) in	
                   Node(rt1,Node(rt,l2,l1),Node(rt,l2,r1))
               else (* c1 < nc < c2 *)
               if (C.similar c2 nc) && (C.similar nc c1) 
               then Node((nc,c),l2,r1) 
               else
                 let rt = (nc,c) in 
                 let rt1 = (c1,nc1) in
                 Node(rt1,Node(rt,sr,l1),Node(rt,sr,r1))
             else (* c1 < c2 < nc *)
             if (C.similar nc c2) && (C.similar c2 c1) 
             then Node((c2,nc2),l2,Node((nc,c),r2,r1))
             else
               let rt = (nc,c) in
               let rt2 = (c2,nc2) in 
               Node((c1,nc1),
                    Node(rt2,Node(rt,l2,l1),Node(rt,r2,l1)),
                    Node(rt2,Node(rt,l2,r1),Node(rt,r2,r1))
                   )
           | Node((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
             when (C.isLeq c2 c1) (* c1 > c2 *) ->
             if (C.isLeq nc c2)
             then (* nc <= c2 < c1 *)
               if (C.isEq nc c2)
               then (* nc = c2 < c1 *) Node((nc,c),l2,sl)
               else (* nc < c2 < c1 *) Node((nc,c),sr,sl)
             else (* nc > c2 < c1 *)
             if (C.isLeq nc c1)
             then (* c2 < nc <= c1 *)
               if (C.isEq nc c1)
               then (* c2 < nc = c1 *)
                 if (C.similar nc c2) 
                 then Node((c2,nc2),l2,Node((nc,c),r2,r1)) 
                 else
                   let rt = (nc,c) in
                   let rt2 = (c2,nc2) in
                   Node(rt2,Node(rt,l2,r1),Node(rt,r2,r1))
               else (* c2 < nc < c1 *)
               if (C.similar c1 nc) && (C.similar nc c2) 
               then Node((c2,nc2),l2,Node((nc,c),r2,sl)) 
               else
                 let rt = (nc,c) in
                 let rt2 = (c2,nc2) in 
                 Node(rt2,Node(rt,l2,sl),Node(rt,r2,sl))
             else (* c2 < c1 < nc *)
             if (C.similar nc c1) && (C.similar c1 c2) 
             then Node((c2,nc2),l2,Node((nc,c),r2,r1))
             else
               let rt = (nc,c) in
               let rt1 = (c1,nc1) in 
               Node((c2,nc2),
                    Node(rt1,Node(rt,l2,l1),Node(rt,l2,r1)),
                    Node(rt1,Node(rt,r2,l1),Node(rt,r2,r1))
                   )
           | Node((c1,nc1),l1,r1), _ ->
             if (C.isLeq nc c1)
             then (* nc <= c1 *)
               if (C.isEq nc c1)
               then (* nc = c1 *) Node((nc,c),sr,r1)
               else (* nc < c1 *) Node((nc,c),sr,sl)
             else (* nc > c1 *)
             if (C.similar nc c1) then Node((nc,c),sr,r1) 
             else
               let rt = (nc,c) in 		
               Node((c1,nc1),Node(rt,sr,l1),Node(rt,sr,r1))
           | _, Node((c2,nc2),l2,r2) ->
             if (C.isLeq nc c2)
             then (* nc <= c2 *)
               if (C.isEq nc c2)
               then (* nc = c2 *) Node((nc,c),l2,sl)
               else (* nc < c2 *) Node((nc,c),sr,sl)
             else (* nc > c2 *)
             if (C.similar nc c2)
             then Node((c2,nc2),l2,Node((nc,c),r2,sl)) 
             else
               let rt = (nc,c) in 	
               Node((c2,nc2),Node(rt,l2,sl),Node(rt,r2,sl))
           | _ -> Node((nc,c),sr,sl) (* it stays the same *))
      | _ -> t
    in
    let st = swap_tree t in (* root(st) is the smallest constraint in t *)
    match st with
    | Node(c,l,r) ->
      let sl = sort_tree l in
      let sr = sort_tree r in
      Node(c,sl,sr)
    | _ -> st

  (** The bottom element of the abstract domain. The totally undefined 		function, i.e., a decision tree with a single `bottom` leaf. *)
  let bot ?domain e vs = 
    { domain = domain; tree = Leaf (F.bot e vs); env = e; vars = vs }

  (** The total function equal to zero, i.e., a decision tree with a single 
      leaf with value zero. *)
  let zero ?domain e vs = 
    { domain = domain; tree = Leaf (F.zero e vs); env = e; vars = vs }

  (** The top element of the abstract domain. The totally unknown 		function, i.e., a decision tree with a single `top` leaf. *)
  let top ?domain e vs = 
    { domain = domain; tree =  Leaf (F.top e vs); env = e; vars = vs }

  (** BINARY OPERATORS *)

  let tree_unification_aux t1 t2 env vars cs = 
    let rec aux (t1,t2) cs =
      match t1,t2 with
      | Bot,Bot -> (t1,t2)
      | Bot,Leaf _ | Leaf _,Bot | Leaf _,Leaf _ ->
        if B.isBot (B.inner env vars cs) then (Bot,Bot) else (t1,t2)
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) 
        when (C.isEq c1 c2) (* c1 = c2 *) ->
        let (ul1,ul2) = aux (l1,l2) (c1::cs) in
        let (ur1,ur2) = aux (r1,r2) (nc1::cs) in
        (Node((c1,nc1),ul1,ur1),Node((c2,nc2),ul2,ur2))
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) 
        when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let (ul1,ul2) = aux (l1,t2) (c1::cs) in
            let (ur1,ur2) = aux (r1,t2) (nc1::cs) in
            (Node((c1,nc1),ul1,ur1),Node((c1,nc1),ul2,ur2))
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) 
        when (C.isLeq c2 c1) (* c1 > c2 *) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let (ul1,ul2) = aux (t1,l2) (c2::cs) in
            let (ur1,ur2) = aux (t1,r2) (nc2::cs) in
            (Node((c2,nc2),ul1,ur1),Node((c2,nc2),ul2,ur2))
      | Node ((c1,nc1),l1,r1),_ ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let (ul1,ul2) = aux (l1,t2) (c1::cs) in
            let (ur1,ur2) = aux (r1,t2) (nc1::cs) in
            (Node((c1,nc1),ul1,ur1),Node((c1,nc1),ul2,ur2))
      | _,Node((c2,nc2),l2,r2) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let (ul1,ul2) = aux (t1,l2) (c2::cs) in
            let (ur1,ur2) = aux (t1,r2) (nc2::cs) in
            (Node((c2,nc2),ul1,ur1),Node((c2,nc2),ul2,ur2))
    in aux (t1,t2) cs

  (** The decision tree orderings and binary operators rely on tree 
      unification to find a common labeling for the decision trees. Given two 
      decision trees t1 and t2 the unification accumulates into a set `cs` 
      the linear constraints encountered along the paths of the decision 
      trees, possibly adding decision nodes or removing constraints that are 
      redundant or whose negation is redundant with respect to `cs`. 

      The implementation assumes that t1 and t2 are sorted and normalized. *)	
  let tree_unification t1 t2 env vars = 
    tree_unification_aux t1 t2 env vars [] 

  (** The decision tree ordering is parameterized by the choice of the 
      ordering `k` between leaf nodes, i.e., approximation or computational 
      ordering. Given two decision trees t1 and t2, the ordering accumulates 
      into a set `cs` the linear constraints encountered along the paths of 
      the decision tree up to the leaf nodes, which are compared by means of 
      the chosen leaf node ordering `k`. 

      The implementation assumes that t1 and t2 are defined over the same 
      reachable states, the same APRON envorinment and the same list of 
      program variables. *)
  let isLeq k t1 t2 =
    let domain = t1.domain in (* assuming t1.domain = t2.domain *)
    let env = t1.env in (* assuming t1.env = t2.env *)
    let vars = t1.vars in (* assuming t1.vars = t2.vars *)
    let rec aux (t1,t2) cs = match t1,t2 with
      | Bot, Bot -> true
      | Bot, _ | _, Bot ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> 
            B.meet (B.inner env vars cs) domain in B.isBot b
      | Leaf f1, Leaf f2 ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> 
            B.meet (B.inner env vars cs) domain in
        if (B.isBot b) then true
        else
          (match k with
           | APPROXIMATION ->
             if not (F.defined f2) || (F.defined f1) 
             then (* dom(f1) \supseteq dom(f2) *)
               if (F.defined f1) && (F.defined f2)
               then (* forall x: f1(x) <= f2(x) *)
                 F.isLeq k b f1 f2 
               else true
             else false
           | COMPUTATIONAL -> 
             F.isLeq k b f1 f2 (* forall x: f1(x) <= f2(x) *))
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
        (aux (l1,l2) (c1::cs)) && (aux (r1,r2) (nc1::cs))
      | _ -> raise (Invalid_argument "isLeq:")
    in aux (tree_unification t1.tree t2.tree env vars) []



  let tree_join k (t1,t2) domain env vars =
    let rec aux (t1,t2) cs = match t1,t2 with
      | Bot,Bot -> Bot
      | Leaf _,Bot ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain in
        if (B.isBot b) then Bot else t1
      | Bot,Leaf _ ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain in
        if (B.isBot b) then Bot else t2
      | Leaf f1,Leaf f2 ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain in
        if (B.isBot b) then Bot else Leaf (F.join k b f1 f2)
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) 
        when (C.isEq c1 c2) (* c1 = c2 *) ->
        let l = aux (l1,l2) (c1::cs) in
        let r = aux (r1,r2) (nc1::cs) in
        Node ((c1,nc1),l,r)
      | Node ((c1,nc1),l1,r1),Node((c2,_),_,_) 
        when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = aux (l1,t2) (c1::cs) in
            let r = aux (r1,t2) (nc1::cs) in
            Node ((c1,nc1),l,r)
      | Node ((c1,_),_,_),Node((c2,nc2),l2,r2) 
        when (C.isLeq c2 c1) (* c1 > c2 *) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let l = aux (t1,l2) (c2::cs) in
            let r = aux (t1,r2) (nc2::cs) in
            Node ((c2,nc2),l,r)
      | Node ((c1,nc1),l1,r1),_ ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = aux (l1,t2) (c1::cs) in
            let r = aux (r1,t2) (nc1::cs) in
            Node ((c1,nc1),l,r)
      | _,Node((c2,nc2),l2,r2) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let l = aux (t1,l2) (c2::cs) in
            let r = aux (t1,r2) (nc2::cs) in
            Node ((c2,nc2),l,r)
    in aux (t1,t2) []

  (** The decision tree join is parameterized by the choice of the 
      join `k` between leaf nodes, i.e., approximation or computational 
      join. Given two decision trees t1 and t2, the join accumulates 
      into a set `cs` the linear constraints encountered along the paths of 
      the decision tree up to the leaf nodes, which are joined by means of 
      the chosen leaf node join `k`. 

      The implementation assumes that t1 and t2 are defined over the same 
      reachable states, the same APRON envorinment and the same list of 
      program variables. *)
  let join k t1 t2 = {
    domain = t1.domain;	(* assuming t1.domain = t2.domain *)
    tree = tree_join k (t1.tree,t2.tree) t1.domain t1.env t1.vars; 
    env = t1.env;	(* assuming t1.env = t2.env *)
    vars = t1.vars	(* assuming t1.vars = t2.vars *)
  }

  (** Given two decision trees t1 and t2, the decision tree meet accumulates 
      into a set `cs` the linear constraints encountered along the paths of 
      the decision tree up to the leaf nodes, which are joined by means of 
      the leaf node meet. 

      The implementation assumes that t1 and t2 are defined over the same 
      reachable states, the same APRON envorinment and the same list of 
      program variables. *)
  let meet t1 t2 =
    let domain = t1.domain in (* assuming t1.domain = t2.domain *)
    let env = t1.env in (* assuming t1.env = t2.env *)
    let vars = t1.vars in (* assuming t1.vars = t2.vars *)
    let rec aux (t1,t2) cs = match t1,t2 with
      | Bot, _ | _, Bot -> Bot
      | Leaf f1, Leaf f2 ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain in
        if B.isBot b then Bot else Leaf (F.join APPROXIMATION b f1 f2)
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) 
        when (C.isEq c1 c2) (* c1 = c2 *) ->
        let l = aux (l1,l2) (c1::cs) in
        let r = aux (r1,r2) (nc1::cs) in
        Node ((c1,nc1),l,r)
      | Node ((c1,nc1),l1,r1), Node((c2,_),_,_) 
        when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = aux (l1,t2) (c1::cs) in
            let r = aux (r1,t2) (nc1::cs) in
            Node ((c1,nc1),l,r)
      | Node ((c1,_),_,_), Node((c2,nc2),l2,r2) 
        when (C.isLeq c2 c1) (* c1 > c2 *) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let l = aux (t1,l2) (c2::cs) in
            let r = aux (t1,r2) (nc2::cs) in
            Node ((c2,nc2),l,r)
      | Node ((c1,nc1),l1,r1), _ ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) aux (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1)
          then (* nc1 is redundant *) aux (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = aux (l1,t2) (c1::cs) in
            let r = aux (r1,t2) (nc1::cs) in
            Node ((c1,nc1),l,r)
      | _, Node((c2,nc2),l2,r2) ->
        let bcs = B.inner env vars cs in
        let bc2 = B.inner env vars [c2] in
        if (B.isLeq bcs bc2)
        then (* c2 is redundant *) aux (t1,l2) cs
        else (* c2 is not redundant *)
          let bnc2 = B.inner env vars [nc2] in
          if (B.isLeq bcs bnc2)
          then (* nc2 is redundant *) aux (t1,r2) cs
          else (* nc2 is not redundant *)
            let l = aux (t1,l2) (c2::cs) in
            let r = aux (t1,r2) (nc2::cs) in
            Node ((c2,nc2),l,r)
    in { 
      domain = domain; 
      tree = aux (t1.tree,t2.tree) []; 
      env = env; vars = vars 
    }

  let left_unification t1 t2 domain env vars =
    let ls1 = tree_labels t1 in
    let ls2 = tree_labels t2 in
    let ls = LSet.diff ls2 ls1 in
    (* Checks whether constraint c is redundant, given the constraints cs *)
    let is_redundant c cs =
      let bcs = B.inner env vars cs in
      let bc = B.inner env vars [c] in
      B.isLeq bcs bc
    in
    (* Compare l1 and l2, with labels not in t1 being greater
       * than all others, and thus will go to the bottom of the
       * tree
       *)
    let cmp l1 l2 =
      match (LSet.mem l1 ls, LSet.mem l2 ls) with
      | (false, false) -> L.compare l1 l2
      | (true, true) -> L.compare l1 l2
      | (false, true) -> -1
      | (true, false) -> 1
    in
    (* Removes redundant constraints in t *)
    let rec remove_redundant t cs =
      match t with
      | Bot | Leaf _ -> t
      | Node ((c, nc), l, r) ->
        if is_redundant c cs then
          remove_redundant l cs
        else if is_redundant nc cs then
          remove_redundant r cs
        else
          let ll = remove_redundant l (c :: cs) in
          let rr = remove_redundant r (nc :: cs) in
          Node ((c, nc), ll, rr)
    in
    let add_node (c, nc) (l, r) cs =
      if is_redundant c cs then
        l
      else if is_redundant nc cs then
        r
      else
        Node ((c, nc), l, r)
    in
    (* Creates a node, putting it in the right place so the tree
       * stays sorted
       *)
    let rec make_node (c, nc) (l, r) cs =
      let smallest t cc = match t with
        | Bot | Leaf _ -> cc
        | Node (cc1, l1, r1) ->
          if cmp cc cc1 > 0 then cc1 else cc
      in
      if is_redundant c cs then
        l
      else if is_redundant nc cs then
        r
      else
        let sc = smallest l (smallest r (c, nc)) in
        match (l, r) with
        | Node ((cl, ncl), ll, rl), Node ((cr, ncr), lr, rr) when
            cmp (cl, ncl) sc = 0 && cmp (cr, ncr) sc = 0 ->
          Node ((cl, ncl), make_node (c, nc) (ll, lr) (cl :: cs),
                make_node (c, nc) (rl, rr) (ncl :: cs))
        | Node ((cl, ncl), ll, rl), _ when cmp (cl, ncl) sc = 0 ->
          Node ((cl, ncl), make_node (c, nc) (ll, r) (cl :: cs),
                make_node (c, nc) (rl, r) (ncl :: cs))
        | _, Node ((cr, ncr), lr, rr) when cmp (cr, ncr) sc = 0 ->
          Node ((cr, ncr), make_node (c, nc) (l, lr) (cr :: cs),
                make_node (c, nc) (l, rr) (ncr :: cs))
        | _, _ -> Node ((c, nc), l, r)
    in
    (* Sort the tree completely; adding the new nodes *)
    let rec rebalance_tree t cs =
      match t with
      | Bot | Leaf _ -> t
      | Node ((c, nc), l, r) ->
        let ll = rebalance_tree l (c :: cs) in
        let rr = rebalance_tree r (nc :: cs) in
        make_node (c, nc) (ll, rr) cs
    in
    (* Collapse all leaves of t into a single one, making sure
       * all labels that are to be removed are deleted
       *)
    let rec collapse t cs =
      match t with
      | Bot | Leaf _ -> t
      | Node ((c, nc), l, r) ->
        assert (LSet.mem (c, nc) ls);
        if is_redundant c cs then
          collapse l cs
        else if is_redundant nc cs then
          collapse r cs
        else
          let ll = collapse l (c :: cs) in
          let rr = collapse r (nc :: cs) in
          match ll, rr with
          | _, Bot -> ll
          | Bot, _ -> rr
          | Leaf f1, Leaf f2 ->
            let b = match domain with
              | None -> B.inner env vars cs
              | Some domain -> B.meet (B.inner env vars cs) domain
            in
            Leaf (F.join COMPUTATIONAL b f1 f2)
          | _, _ -> assert false
    in
    (* Finish t1 and t2 unification by doing a tree unification step
       * for labels that are in t1, and collapsing the others.
       *)
    let rec lunify t1 t2 cs =
      match (t1, t2) with
      | (Bot | Leaf _), (Bot | Leaf _) -> t2
      | Node ((c1, nc1), l1, r1), (Bot | Leaf _) ->
        add_node (c1, nc1) (lunify l1 t2 (c1 :: cs), lunify r1 t2 (nc1 :: cs)) cs
      | (Bot | Leaf _), Node ((c2, nc2), l2, r2) ->
        if LSet.mem (c2, nc2) ls then
          collapse t2 cs
        else
          add_node (c2, nc2) (lunify t1 l2 (c2 :: cs), lunify t1 r2 (nc2 :: cs)) cs
      | Node ((c1, nc1), l1, r1), Node ((c2, nc2), l2, r2) ->
        let w = cmp (c1, nc1) (c2, nc2) in
        if w = 0 then
          add_node (c1, nc1) (lunify l1 l2 (c1 :: cs), lunify r1 r2 (nc1 :: cs)) cs
        else if w < 0 then
          add_node (c1, nc1) (lunify l1 t2 (c1 :: cs), lunify r1 t2 (nc1 :: cs)) cs
        else (
          assert (not (LSet.mem (c2, nc2) ls));
          add_node (c2, nc2) (lunify t1 l2 (c2 :: cs), lunify t1 r2 (nc2 :: cs)) cs
        )
    in
    let t2 = lunify t1 (remove_redundant (rebalance_tree t2 []) []) [] in
    (* TODO: domain widening *)

    (* let t2 = left_unification t2 [] in *)
    (* Format.fprintf Format.std_formatter "\nt2[left_unification]: %a\n" (print_tree vars) t2;
       domain_widen t1 *) t2

  let widen ?(jokers=0) t1 t2 =
    let domain = t1.domain in
    let env = t1.env in
    let vars = t1.vars in
    let t1 = t1.tree and t2 = t2.tree in
    let rec widen_right (t1,t2) cs =
      match t1,t2 with
      | Leaf f1,Leaf f2 ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain in
        if F.isLeq COMPUTATIONAL b f1 f2 then t2 
        else Leaf (F.top env vars)
      | Node((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) (* c1 = c2 *) ->
        let l = widen_right (l1,l2) (c1::cs) in
        let r = widen_right (r1,r2) (nc1::cs) in
        Node((c2,nc2),l,r)
      | Node((c1,nc1),l1,r1),Node((c2,_),_,_) when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) widen_right (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1) then (* nc1 is redundant *) widen_right (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = widen_right (l1, t2) (c1 :: cs) in
            let r = widen_right (r1, t2) (nc1 :: cs) in
            Node ((c1, nc1), l, r)
      | Node((c1,_),_,_),Node((c2,nc2),l2,r2) when (C.isLeq c2 c1) (* c1 > c2 *) ->
        let l = widen_right (t1,l2) (c2::cs) in
        let r = widen_right (t1,r2) (nc2::cs) in
        Node((c2,nc2),l,r)

      | Node ((c1,nc1),l1,r1),_ ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) widen_right (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1) then (* nc1 is redundant *) widen_right (r1,t2) cs
          else (* nc1 is not redundant *)
            let l = widen_right (l1, t2) (c1 :: cs) in
            let r = widen_right (r1, t2) (nc1 :: cs) in
            Node ((c1, nc1), l, r)
      | _,Node((c2,nc2),l2,r2) ->
        let l = widen_right (t1,l2) (c2::cs) in
        let r = widen_right (t1,r2) (nc2::cs) in
        Node((c2,nc2),l,r)
      | _ -> t2
    in
    let rec widen_up (t1,t2) cs =
      match t1,t2 with
      | Bot,Bot -> Bot
      | Leaf f1,Leaf f2 ->
        let b = match domain with | None -> B.inner env vars cs | Some domain -> B.meet (B.inner env vars cs) domain in
        Leaf (F.widen ~jokers:(if !retrybwd > 0 then
                                 (jokers + !retrybwd - 1) / !retrybwd
                               else 0)
                b f1 f2)
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) (* c1 = c2 *) -> Node ((c1,nc1),widen_up (l1,l2) (c1::cs),widen_up (r1,r2) (nc1::cs))
      | Node ((c1,nc1),l1,r1),Node((c2,_),_,_) when (C.isLeq c1 c2) (* c1 < c2 *) ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) widen_up (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1) then (* nc1 is redundant *) widen_up (r1,t2) cs
          else (* nc1 is not redundant *)
            Node ((c1, nc1), widen_up (l1, t2) (c1 :: cs), widen_up (r1, t2) (nc1 :: cs))
      | Node ((c1,_),_,_),Node((c2,nc2),l2,r2) when (C.isLeq c2 c1) (* c1 > c2 *) -> Node((c2,nc2),widen_up (t1,l2) (c2::cs),widen_up (t1,r2) (nc2::cs))
      | Node ((c1,nc1),l1,r1),_ ->
        let bcs = B.inner env vars cs in
        let bc1 = B.inner env vars [c1] in
        if (B.isLeq bcs bc1)
        then (* c1 is redundant *) widen_up (l1,t2) cs
        else (* c1 is not redundant *)
          let bnc1 = B.inner env vars [nc1] in
          if (B.isLeq bcs bnc1) then (* nc1 is redundant *) widen_up (r1,t2) cs
          else (* nc1 is not redundant *) Node ((c1, nc1), widen_up (l1, t2) (c1 :: cs), widen_up (r1, t2) (nc1 :: cs))
      | _,Node((c2,nc2),l2,r2) -> Node((c2,nc2),widen_up (t1,l2) (c2::cs),widen_up (t1,r2) (nc2::cs))
      | Bot, _ | _, Bot -> Bot
    in
    let widen (t1,t2) =
      let prev = t1 in
      let lbl = LSet.elements (tree_labels t2) in
      let rec leaves p t ls cs =
        match t,ls with
        | Bot,_ -> []
        | Leaf f,[] ->
          let b = match domain with | None -> B.inner env vars cs | Some domain -> B.meet (B.inner env vars cs) domain in
          if (F.defined f) && not (B.isBot b) && not (F.isEq b f (F.reset f)) then [b,f] else []
        | Leaf _,(c,nc)::ls ->
          let h = List.hd p in
          if (h = 1) then leaves (List.tl p) t ls (c::cs)
          else if (h = 2) then leaves (List.tl p) t ls (nc::cs)
          else leaves (List.tl p) t ls cs
        | Node ((c1,nc1),l1,r1),(c,_)::ls when (C.isEq c1 c) ->
          let h = List.hd p in
          if (h = 2) then leaves (List.tl p) r1 ls (nc1::cs)
          else leaves (List.tl p) l1 ls (c1::cs)
        | Node ((c1,_),_,_),(c,nc)::ls when (C.isLeq c c1) ->
          let h = List.hd p in
          if (h = 1) then leaves (List.tl p) t ls (c::cs)
          else if (h = 2) then leaves (List.tl p) t ls (nc::cs)
          else leaves (List.tl p) t ls cs
        | _ -> raise (Invalid_argument "widen:leaves:")
      in
      let rec adjacent p1 p2 =
        match p2 with
        | [] -> leaves p1 prev lbl []
        (* List.iter (fun p -> Format.fprintf Format.std_formatter "%s " (string_of_int p)) p1;
           Format.fprintf Format.std_formatter "\n";
           leaves p1 prev lbl [] *)
        | h::ps ->
          (match h with
           | 1 -> (leaves (p1@[2]@ps) prev lbl []) @ (adjacent (p1@[1]) ps)
           (* List.iter (fun p -> Format.fprintf Format.std_formatter "%s " (string_of_int p)) (p1@[2]@ps);
              Format.fprintf Format.std_formatter "\n";
              (leaves (p1@[2]@ps) prev lbl []) @ (adjacent (p1@[1]) ps) *)
           | 2 -> (leaves (p1@[1]@ps) prev lbl []) @ (adjacent (p1@[2]) ps)
           (* List.iter (fun p -> Format.fprintf Format.std_formatter "%s " (string_of_int p)) (p1@[1]@ps);
              Format.fprintf Format.std_formatter "\n";
              (leaves (p1@[1]@ps) prev lbl []) @ (adjacent (p1@[2]) ps) *)
           | _ -> adjacent (p1@[0]) ps)
      in
      let rec extend (b2,f2) bfs =
        match bfs with
        | [] -> f2
        | (b1,f1)::bfs ->
          if !tracebwd then
            begin
              Format.fprintf Format.std_formatter "EXTEND\n";
              Format.fprintf Format.std_formatter "%a? %a\n" B.print b1 F.print f1;
              Format.fprintf Format.std_formatter "%a? %a\n" B.print b2 F.print f2;
              Format.fprintf Format.std_formatter "%a? %a\n\n" B.print b2 F.print (F.extend b1 b2 f1 f2)
            end;
          F.join COMPUTATIONAL b2 (F.extend b1 b2 f1 f2) (extend (b2,f2) bfs)
      in
      let rec merge (t1,t2) cs =
        match t1,t2 with
        | _,Bot -> t1
        | Bot,_ -> t2
        | Leaf f1,Leaf f2 -> Leaf (F.join COMPUTATIONAL (B.inner env vars cs) f1 f2)
        | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) (* c1 = c2 *) ->
          let l = merge (l1,l2) (c1::cs) in
          let r = merge (r1,r2) (nc1::cs) in
          Node ((c1,nc1),l,r)
        | _ -> raise (Invalid_argument "widen:merge:")
      in
      let rec aux p (* path *) ls (* labels *) (t1,t2) cs =
        match t1, t2, ls with
        | Bot, Bot, _ -> Bot
        | Leaf f1, Leaf f2, _ ->
          let b = match domain with | None -> B.inner env vars cs | Some domain -> B.meet (B.inner env vars cs) domain in
          if (B.isBot b) then Bot
          else if (F.isEq b f1 f2) then t2
          else
            let p = List.rev p @ List.map (fun _ -> 0) ls in
            let bfs = adjacent [] p in
            Leaf (extend (b, f2) bfs)
        | Node ((c1, nc1), l1, r1),
          Node ((c2, nc2), l2, r2),
          (c, _) :: ls
          when C.isEq c1 c2 && C.isEq c1 c ->
          let l = aux (1::p) ls (l1, l2) (c1 :: cs) in
          let r = aux (2::p) ls (r1, r2) (nc1 :: cs) in
          Node ((c1,nc1),l,r)
        | Node ((c1, _), _, _),
          Node ((c2, _), _, _),
          (c, nc) :: ls
          when C.isEq c1 c2 && C.isLeq c c1 ->
          let bcs = B.inner env vars cs in
          let bc = B.inner env vars [c] in
          if (B.isLeq bcs bc)
          then (* c is redundant *)
            aux (0 :: p) ls (t1, t2) cs
          else (* c is not redundant *)
            merge (aux (1 :: p) ls (t1, t2) (c :: cs),
                   aux (2 :: p) ls (t1, t2) (nc :: cs)) cs
        | Bot, _, _ | _, Bot, _ | _, _, _ -> Bot
      in
      aux [] lbl (t1, t2) []
    in
    if !tracebwd then
      begin
        Format.fprintf Format.std_formatter "WIDENING\n";
        Format.fprintf Format.std_formatter "t1: %a\n" (print_tree vars) t1;
        Format.fprintf Format.std_formatter "\nt2: %a\n" (print_tree vars) t2
      end;
    let t2 = widen_right (t1,t2) [] in
    if !tracebwd then
      Format.fprintf Format.std_formatter "\nt2[widen_right]: %a\n" (print_tree vars) t2;
    let t2 = left_unification t1 t2 domain env vars in
    if !tracebwd then
      Format.fprintf Format.std_formatter "\nt2[left_unification]: %a\n" (print_tree vars) t2;
    let (t1,t2) = tree_unification t1 t2 env vars in
    if !tracebwd then
      begin
        Format.fprintf Format.std_formatter "\nt1[tree_unification]: %a\n" (print_tree vars) t1;
        Format.fprintf Format.std_formatter "\nt2[tree_unification]: %a\n" (print_tree vars) t2
      end;
    let t2 = widen_up (t1,t2) [] in
    if !tracebwd then
      Format.fprintf Format.std_formatter "\nt2[widen_up]: %a\n" (print_tree vars) t2;
    { domain = domain; tree = widen (t1, t2); env = env; vars = vars }

  let dual_widen t1 t2 = raise (Invalid_argument "TODO")

  (**)

  let terminating t =
    let domain = t.domain in
    let env = t.env in
    let vars = t.vars in
    let rec aux t cs =
      match t with
      | Bot ->
        let b = match domain with 
          | None -> B.inner env vars cs 
          | Some domain -> B.meet (B.inner env vars cs) domain 
        in B.isBot b
      | Leaf f ->
        (match domain with
         | None -> F.defined f || B.isBot (B.inner env vars cs)
         | Some domain -> F.defined f || B.isBot (B.meet (B.inner env vars cs) domain))
      | Node ((c,nc),l,r) -> (aux l (c::cs)) && (aux r (nc::cs))
    in aux t.tree []

  let bwdAssign ?domain t e = 
    let cache = ref CMap.empty in
    let pre = domain in
    let post = t.domain in
    let env = t.env in
    let vars = t.vars in
    let merge t1 t2 cs =
      let rec aux (t1,t2) cs =
        match t1,t2 with
        | _,Bot -> t1
        | Bot,_ -> t2
        | Leaf f1,Leaf f2 ->
          let b = match pre with | None -> B.inner env vars cs | Some pre -> B.meet (B.inner env vars cs) pre in
          Leaf (F.join APPROXIMATION b f1 f2)
        | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) ->
          Node((c1,nc1),aux (l1,l2) (c1::cs),aux (r1,r2) (nc1::cs))
        | _ -> raise (Invalid_argument "bwdAssign:merge:")
      in aux (tree_unification_aux t1 t2 env vars cs) cs
    in
    let rec build t cs =
      match cs with
      | [] -> t
      | x::xs ->
        let nx = C.negate x in
        if (C.isLeq nx x)
        then (* x is normalized *) Node((x,nx),build t xs,Bot)
        else (* x is not normalized *) Node((nx,x),Bot,build t xs)
    in
    let rec aux t cs =
      match t with
      | Bot -> Bot
      | Leaf f -> Leaf (F.bwdAssign f e)
      | Node((c,nc),l,r) -> match (fst e) with
        | A_var variable ->
          if (C.var variable c)
          then
            let filter_constraints cs dom =
              List.fold_left (fun cs c ->
                  let b = B.inner env vars [c] in
                  if not (C.isBot c) && (B.isLeq dom b || B.isBot (B.meet dom b)) then
                    cs
                  else
                    c :: cs
                ) [] cs
            in
            let c, nc = try
                CMap.find c !cache
              with Not_found ->
                (match pre, post with
                 | Some pre, Some post ->
                   let key = c in
                   let c = B.constraints (B.bwdAssign (B.meet
                                                         (B.inner env vars [c]) post) e) in
                   let c = filter_constraints c pre in
                   let nc = B.constraints (B.bwdAssign (B.meet
                                                          (B.inner env vars [nc]) post) e) in
                   let nc = filter_constraints nc pre in
                   cache := CMap.add key (c,nc) !cache;
                   (c, nc)
                 | _ ->
                   let key = c in
                   let c = B.constraints (B.bwdAssign (B.inner env vars [c]) e) in
                   let nc = B.constraints (B.bwdAssign (B.inner env vars [nc]) e) in
                   cache := CMap.add key (c,nc) !cache;
                   (c, nc)
                ) in
            (match c, nc with
             | [],[] -> merge (aux l cs) (aux r cs) cs
             | [],[y] when (C.isBot y) -> aux l cs
             | [x],[] when (C.isBot x) -> aux r cs
             | [x],[y] ->
               let nx = C.negate x in
               let ny = C.negate y in
               let ll = aux l (x::cs) in
               let rr = aux r (y::cs) in
               if (C.isEq nx y) then
                 sort_tree (Node((x,nx),ll,rr))
               else
                 merge
                   (sort_tree (Node((x,nx),ll,rr)))
                   (sort_tree (Node((y,ny),rr,ll)))
                   cs
             | _ ->
               let ll = aux l (c@cs) in
               let rr = aux r (nc@cs) in
               merge (sort_tree (build ll c)) (sort_tree (build rr nc)) cs)
          else
            let l = aux l (c::cs) in
            let r = aux r (nc::cs) in
            Node((c,nc),l,r)
        | _ -> raise (Invalid_argument "DecisionTree.bwdAssign: unexpected lvalue")
    in
    { domain = pre;
      tree = sort_tree (aux t.tree []);
      env = env;
      vars = vars }

  let rec filter ?domain t e =
    let pre = domain in
    let post = t.domain in
    let env = t.env in
    let vars = t.vars in
    let rec aux t bs cs =
      let bcs = match pre with
        | None -> B.inner env vars cs
        | Some pre -> B.meet (B.inner env vars cs) pre
      in
      match bs with
      | [] ->
        (match t with
         | Bot -> Bot
         | Leaf f -> Leaf (F.filter f e)
         | Node((c,nc),l,r) ->
           let bc = B.inner env vars [c] in
           if (B.isLeq bcs bc)
           then (* c is redundant *) aux l bs cs
           else (* c is not redundant *)
             (* if (B.isBot (B.meet bc bcs))
                then (* c is conflicting *) aux r bs cs
                else *)
             let l = aux l bs (c::cs) in
             let r = aux r bs (nc::cs) in
             (match l,r with
              | Bot,Bot -> Bot
              | Bot,Node(_,Bot,_) -> r
              | _ -> Node((c,nc),l,r)))
      | (x,nx)::xs ->
        let bx = B.inner env vars [x] in
        if (B.isLeq bcs bx)
        then (* x is redundant *) aux t xs cs
        else (* x is not redundant *)
        if (B.isBot (B.meet bx bcs))
        then (* x is conflicting *) Bot
        else
        if (C.isLeq nx x)
        then (* x is normalized *)
          (match t with
           | Node ((c,nc),l,r) when (C.isEq c x) (* c = x *) ->
             let l = aux l xs (c::cs) in
             (match l with
              | Bot -> Bot
              | _ -> Node((c,nc),l,Bot))
           | Node ((c,nc),l,r) when (C.isLeq c x) (* c < x *) ->
             let bc = B.inner env vars [c] in
             if (B.isLeq bcs bc)
             then (* c is redundant *) aux l bs cs
             else (* c is not redundant *)
               (* if (B.isBot (B.meet bc bcs))
                  then (* c is conflicting *) aux r bs cs
                  else *)
               let l = aux l bs (c::cs) in
               let r = aux r bs (nc::cs) in
               (match l,r with
                | Bot,Bot -> Bot
                | Bot,Node(_,Bot,_) -> r
                | _ -> Node((c,nc),l,r))
           | _ ->
             let l = aux t xs (x::cs) in
             (match l with
              | Bot -> Bot
              | _ -> Node((x,nx),l,Bot)))
        else (* x is not normalized *)
          (match t with
           | Node ((c,nc),l,r) when (C.isEq c nx) (* c = nx *) ->
             let r = aux r xs (nc::cs) in
             (match r with
              | Bot -> Bot
              | _ -> Node((c,nc),Bot,r))
           | Node ((c,nc),l,r) when (C.isLeq c nx) (* c < nx *) ->
             let bc = B.inner env vars [c] in
             if (B.isLeq bcs bc)
             then (* c is redundant *) aux l bs cs
             else (* c is not redundant *)
               (* if (B.isBot (B.meet bc bcs))
                  then (* c is conflicting *) aux r bs cs
                  else *)
               let l = aux l bs (c::cs) in
               let r = aux r bs (nc::cs) in
               (match l,r with
                | Bot,Bot -> Bot
                | Bot,Node(_,Bot,_) -> r
                | _ -> Node((c,nc),l,r))
           | _ ->
             let r = aux t xs (x::cs) in
             (match r with
              | Bot -> Bot
              | _ -> Node((nx,x),Bot,r)))
    in
    match e with
    | A_TRUE | A_MAYBE -> { domain = pre; tree = aux t.tree [] []; env = env; vars = vars }
    | A_FALSE -> { domain = pre; tree = Bot; env = env; vars = vars }
    | A_bunary (o,e) ->
      (match o with
       | A_NOT -> let (e, _) = negBExp e in filter ?domain:pre t e)
    | A_bbinary (o,(e1,_),(e2,_)) ->
      let t1 = filter ?domain:pre t e1 and t2 = filter ?domain:pre t e2 in
      (match o with
       | A_AND -> meet t1 t2
       | A_OR -> join APPROXIMATION t1 t2)
    | A_rbinary (_,_,_) ->
      let bp = match post with
        | None -> B.inner env vars []
        | Some post -> B.meet (B.inner env vars []) post
      in
      let bs = List.map (fun c -> let nc = C.negate c in (c,nc)) (B.constraints (B.filter bp e)) in
      let bs = List.sort L.compare bs in
      { domain = pre; tree = aux t.tree bs []; env = env; vars = vars }

  let reset ?mask t e =
    let domain = t.domain in
    let env = t.env in
    let vars = t.vars in
    let t1 = t.tree in
    let rec reset flag t =
      match t with
      | Bot -> Bot
      | Leaf f -> if flag && F.isBot f then Leaf f else Leaf (F.reset f)
      | Node (c,l,r) -> Node(c,reset flag l,reset flag r)
    in
    let t2 =
      match mask with
      | None -> reset false (tree (filter t e))
      | Some mask -> reset true (tree (filter mask e))
    in
    let rec aux (t1,t2) =
      match t1,t2 with
      | _,Bot | Bot,_ -> t1
      | Leaf f1,Leaf f2 -> Leaf f2
      | Node ((c1,nc1),l1,r1),Node((c2,nc2),l2,r2) when (C.isEq c1 c2) -> Node ((c1,nc1),aux (l1,l2),aux (r1,r2))
      | _ -> raise (Invalid_argument "reset:")
    in { domain = domain; tree = aux (tree_unification t1 t2 env vars); env = env; vars = vars }

  let refine t b = { domain = Some b; tree = t.tree; env = t.env; vars = t.vars }

  (**)

  let compress t =
    let domain = t.domain in
    let env = t.env in
    let vars = t.vars in
    let rec aux t cs =
      match t with
      | Bot | Leaf _ -> t
      | Node((c,nc),l,r) ->
        let l = aux l (c::cs) in
        let r = aux r (nc::cs) in
        match l,r with
        | Bot,Bot -> Bot
        | Leaf f1,Leaf f2 when (F.isBot f1) && (F.isBot f2) -> Leaf f1
        | Leaf f1,Leaf f2 when (F.defined f1 && F.defined f2) ->
          let b1 = match domain with | None -> B.inner env vars (c::cs) | Some domain -> B.meet (B.inner env vars (c::cs)) domain in
          if (F.isEq b1 f1 f2) then Leaf f2 else
            let b2 = match domain with | None -> B.inner env vars (nc::cs) | Some domain -> B.meet (B.inner env vars (nc::cs)) domain in
            if (F.isEq b2 f1 f2) then Leaf f1 else Node((c,nc),l,r)
        | Leaf f1,Leaf f2 when (F.isTop f1) && (F.isTop f2) -> Leaf f1
        | Leaf f1,Node((c2,nc2),Leaf f2,r2) when (F.isBot f1) && (F.isBot f2) -> aux (Node((c2,nc2),Leaf f1,r2)) cs
        | Leaf f1,Node((c2,nc2),Leaf f2,r2) when (F.defined f1) && (F.defined f2) ->
          (* e.g., NODE( y >= 2, LEAF 3y+2, NODE( y >= 1, LEAF 5, LEAF 1 )) *)
          let b2 = match domain with | None -> B.inner env vars (c2::nc::cs) | Some domain -> B.meet (B.inner env vars (c2::nc::cs)) domain in
          if (F.isEq b2 f1 f2) then aux (Node((c2,nc2),Leaf f1,r2)) cs else Node((c,nc),l,r)
        | Leaf f1,Node((c2,nc2),Leaf f2,r2) when (F.isTop f1) && (F.isTop f2) -> aux (Node((c2,nc2),Leaf f1,r2)) cs
        | Node((c1,nc1),Leaf f1,Leaf f2),Node((c2,nc2),Node((c3,nc3),Leaf f3,Leaf f4),r2) when (C.isEq c1 c3) && (F.defined f1) && (F.defined f2) && (F.defined f3) && (F.defined f4) ->
          (* e.g., NODE( x >= 2, NODE( y >= 1, LEAF 7x+3y-5, LEAF 1 ), NODE( x >= 1, NODE( y >= 1, LEAF 3y+2, LEAF 1 ), LEAF 1 ) *)
          let b3 = match domain with | None -> B.inner env vars (c3::c2::nc::cs) | Some domain -> B.meet (B.inner env vars (c3::c2::nc::cs)) domain in
          let b4 = match domain with | None -> B.inner env vars (nc3::c2::nc::cs) | Some domain -> B.meet (B.inner env vars (nc3::c2::nc::cs)) domain in
          if (F.isEq b3 f1 f3) && (F.isEq b4 f2 f4) then aux (Node((c2,nc2),Node((c3,nc3),Leaf f1,Leaf f2),r2)) cs else Node((c,nc),l,r)
        | _ -> Node((c,nc),l,r)
    in { domain = domain; tree = aux t.tree []; env = env; vars = vars }

  let rec print fmt t =
    let domain = t.domain in
    let env = t.env in
    let vars = t.vars in
    let print_domain fmt domain =
      match domain with
      | None -> ()
      | Some domain -> B.print fmt domain
    in
    let rec aux t cs =
      match t with
      | Bot ->
        let b = match domain with | None -> B.inner env vars cs | Some domain -> B.meet (B.inner env vars cs) domain in
        if B.isBot b then () else Format.fprintf fmt "%a ? BOT\n" B.print b
      | Leaf f ->
        let b = match domain with | None -> B.inner env vars cs | Some domain -> B.meet (B.inner env vars cs) domain in
        if B.isBot b then () else Format.fprintf fmt "%a ? %a\n" B.print b F.print f
      | Node((c,nc),l,r) -> aux r (nc::cs); aux l (c::cs)
    in aux t.tree []; Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree
    (* in Format.fprintf fmt "\nDOMAIN = {%a}%a\n" print_domain domain (print_tree vars) t.tree *)


  let map_tree_leafs f (t:t) =
    let rec aux (t:tree) = 
      match t with 
      | Bot -> Bot
      | Leaf l -> f l
      | Node (l, t1, t2) -> 
        let t1' = aux t1 in
        let t2' = aux t2 in
        Node (l, t1', t2')
    in
    {
      domain = t.domain;
      tree = aux t.tree;
      env = t.env;
      vars = t.vars
    }

  (* Takes a decision tree as argument and replaces all non-bottom leafs with a constant function that returns zero *)
  let zero_leafs (t:t) =
    let mapLeaf l = 
      let f = if F.isBot l then l else F.reset l 
      in Leaf f 
    in
    map_tree_leafs mapLeaf t


  let tree_join_helper fBotLeft fBotRight fLeaf tree1 tree2 env vars =
    let rec aux (t1, t2) cs = match t1, t2 with
      | (Bot, Bot) -> Bot
      | (Leaf f, Bot) -> fBotRight cs f 
      | (Bot,Leaf f) -> fBotLeft cs f
      | (Leaf f1, Leaf f2) -> fLeaf cs f1 f2
      | Node ((c1,nc1),l1,r1), Node((c2,nc2),l2,r2) ->
        if not (C.isEq c1 c2) then 
          raise (Invalid_argument "tree_join_helper: invalid tree structure, constraints don't match");
        let l = aux (l1,l2) (c1::cs) in
        let r = aux (r1,r2) (nc1::cs) in
        Node ((c1,nc1),l,r)
      | _ -> raise (Invalid_argument "tree_join_helper: invalid tree structure")
    in
    aux (tree_unification tree1 tree2 env vars) []

  (* Equivalent to usual meet but does not generate NIL nodes. Instead, all NIL nodes are converted to bot. leafs*)
  let leaf_preserving_meet t1 t2 =
    let domain = t1.domain in 
    let env = t1.env in 
    let vars = t1.vars in 
    let fBotLeftRight _ _ = Leaf (F.bot env vars) in
    let fLeaf cs l1 l2 = Leaf (F.join APPROXIMATION (B.inner env vars cs) l1 l2) in
    { 
      domain = domain; 
      tree = tree_join_helper fBotLeftRight fBotLeftRight fLeaf t1.tree t2.tree env vars; 
      env = env; 
      vars = vars 
    }

  let reset_until t_keep t_reset t =
    let domain = t.domain in 
    let env = t.env in 
    let vars = t.vars in 
    let isDefined f = not (F.isBot f || F.isTop f) in
    let rec reset (t, t_res) = match (t,t_res) with
      | (Bot, _) | (_, Bot) -> t
      | (Leaf f1, Leaf f2) -> let f = if isDefined f2 then F.reset f1 else f1 in Leaf f (* reset leaf if there is an actual value in f2 e.g. not top or bottom*)
      | (Node (c,l1,r1), Node (_,l2,r2)) -> Node (c, reset (l1, l2), reset (r1, r2))
      | _ -> raise (Invalid_argument "reset_until: Invalid Tree shape")
    in
    let reset_tree = reset (tree_unification t.tree t_reset.tree env vars) in
    let result = { 
      domain = domain; 
      tree = reset_tree; 
      env = env; 
      vars = vars 
    } in
    result
          

end

module TSAB = DecisionTree(AB)
module TSOB = DecisionTree(OB)
module TSAO = DecisionTree(AO)
module TSOO = DecisionTree(OO)
module TSAP = DecisionTree(AP)
module TSOP = DecisionTree(OP)
