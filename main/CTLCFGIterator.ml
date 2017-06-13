open Cfg
open AbstractSyntax
open InvMap
open CTLProperty
open Apron
open Domain
open Partition
open Functions
open Iterator
open ForwardIterator


let env_vars (cfg:cfg) =
  let var_to_apron v = Apron.Var.of_string (Printf.sprintf "%s(%i)" v.var_name v.var_id) in
  let apron_vars = Array.map var_to_apron (Array.of_list cfg.cfg_vars) in
  let env = Environment.make apron_vars [||] in
  let vars = List.map Conversion.of_var cfg.cfg_vars in
  (env, vars)



(* type for CTL properties, instantiated with bExp for atomic propositions *)
type ctl_property = (AbstractSyntax.bExp StringMap.t) CTLProperty.generic_property

type quantifier = UNIVERSAL | EXISTENTIAL


let rec print_ctl_property fmt (property:ctl_property) = match property with 
  | Atomic p -> 
    let propertyBindings = StringMap.bindings p in
    if List.length propertyBindings == 1 then AbstractSyntax.bExp_print_aux fmt (StringMap.find "" p)
    else StringMap.iter (fun key b -> if String.length key > 0 then Format.fprintf fmt "%s: %a" key AbstractSyntax.bExp_print_aux b else ()) p
  | AX p -> Format.fprintf fmt "AX{%a}" print_ctl_property p
  | AF p -> Format.fprintf fmt "AF{%a}" print_ctl_property p
  | AG p -> Format.fprintf fmt "AG{%a}" print_ctl_property p
  | AU (p1,p2) -> Format.fprintf fmt "AU{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | EX p -> Format.fprintf fmt "EX{%a}" print_ctl_property p
  | EF p -> Format.fprintf fmt "EF{%a}" print_ctl_property p
  | EG p -> Format.fprintf fmt "EG{%a}" print_ctl_property p
  | EU (p1,p2) -> Format.fprintf fmt "EU{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | AND (p1,p2) -> Format.fprintf fmt "AND{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | OR (p1,p2) -> Format.fprintf fmt "OR{%a}{%a}" print_ctl_property p1 print_ctl_property p2;
  | NOT p -> Format.fprintf fmt "NOT{%a}" print_ctl_property p;




module CTLCFGIterator(D: RANKING_FUNCTION) = struct

  type inv = D.t NodeMap.t

  let printInv ?fwdInvOpt fmt (inv:inv) =
    let inv = 
      if !compress then 
        NodeMap.map D.compress inv
      else 
        inv
    in NodeMap.iter (fun node a -> 
        Format.fprintf fmt "%d:\n%a\nDOT: %a\n" node.node_id D.print a D.print_graphviz_dot a) inv

  let atomic env vars (cfg:cfg) (property:bExp StringMap.t) : inv = 
    let bot = D.bot env vars in
    (* get global property i.e. property not associated with a label *)
    let globalProperty = StringMap.find "" property in 
    (* abstract state of all nodes in cfg*)
    let atomicState = D.reset bot globalProperty in
    (* create node map that assigns atomicState to all nodes*)
    let nodeMap = List.fold_left 
        (fun map node -> NodeMap.add node atomicState map) 
        NodeMap.empty cfg.cfg_nodes in
    (* set state for nodes with a labels if property is defined for label *)
    let labelReducer (inv:inv) arc = 
      let state = match arc.arc_inst  with 
        | CFG_label l -> 
          (try D.reset bot (StringMap.find l property) with 
             Not_found -> atomicState)
        | _ -> atomicState 
      in NodeMap.add arc.arc_src state inv
    in List.fold_left labelReducer nodeMap cfg.cfg_arcs


end

