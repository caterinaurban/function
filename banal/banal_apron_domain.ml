(*   
   Abstract domain based on the APRON library.

   Copyright (C) 2011 Antoine Miné
*)

open Banal_datatypes
open Banal_abstract_syntax
open Banal_typed_syntax
open Banal_semantics
open Banal_domain
open Apron

module Itv_rat = Banal_itv_rat
module Linearization = Banal_linearization

let trace_remove = false
let trace_bwd = false


(* GENERIC FUNCTOR *)
(* *************** *)

module type NUMERICAL = sig 
  type lib
  val manager: lib Manager.t 
end

module Lin = Linearization.Make(Itv_rat)

module ApronDomain(Param: NUMERICAL) = struct

  let man = Param.manager

  type t = Param.lib Abstract1.t

  (*************)
  (* UTILITIES *)
  (*************)


  (* apron name of a variable *)
  let apron_of_var (v:var) : Var.t = Var.of_string v.var_id

  let add_var_to_env env v =
    let i,r = match v.var_typ with
    | A_int _ | A_BOOL -> [|apron_of_var v|], [||]
    | A_float _ -> [||], [|apron_of_var v|]
    in
    Environment.add env i r



  (* to Apron types *)
  (* ************** *)

  let apron_of_typ (t:typ) : Texpr1.typ * Texpr1.round =
    match t with
    | A_int _ | A_BOOL -> Texpr1.Int, Texpr1.Zero
    | A_float A_FLOAT -> Texpr1.Single, Texpr1.Near
    | A_float A_DOUBLE -> Texpr1.Double, Texpr1.Near
    | A_float A_REAL -> Texpr1.Real, Texpr1.Near


  let apron_of_lin env ((i,m):Lin.linexpr) : Linexpr1.t =
    let r = Linexpr1.make env in
    Linexpr1.set_cst r (Coeff.Interval (Lin.I.to_apron i));
    Lin.VMap.iter 
      (fun v i ->
        Linexpr1.set_coeff r 
          (apron_of_var v) (Coeff.Interval (Lin.I.to_apron i))
      )
      m;
    r

  let apron_of_cons_op = function 
    | Lin.L_EQ | Lin.L_EQ_INT -> Lincons1.EQ
    | Lin.L_NEQ | Lin.L_NEQ_INT -> Lincons1.DISEQ
    | Lin.L_GEQ | Lin.L_GEQ_INT -> Lincons1.SUPEQ
    | Lin.L_GT  -> Lincons1.SUP


   let apron_of_lincons env ((l,o):Lin.lincons) : Lincons1.t =
     Lincons1.make (apron_of_lin env l) (apron_of_cons_op o)
         

   let apron_of_lincons_ar env (c:Lin.lincons) : Lincons1.earray =
     let a = Lincons1.array_make env 1 in
     Lincons1.array_set a 0 (apron_of_lincons env c);
     a
    

  (* creates a linearization environment (variable to interval) *)
  let lin_env (a:t) : Lin.env =
    fun v -> Lin.I.of_apron (Abstract1.bound_variable man a (apron_of_var v))


  (* Apron types utilities *)
  (* ********************* *)

  (* constructs a new contraint in opposite direction *)
  let neg_lincons (d:Lincons1.t) : Lincons1.t =
    let d = Lincons1.copy d in
    Lincons1.set_cst d (Coeff.neg (Lincons1.get_cst d));
    Lincons1.iter (fun c v -> Lincons1.set_coeff d v (Coeff.neg c)) d;
    d

  (* normalize equalities as pairs of inequalities *)
  let list_of_lincons_array (ar:Lincons1.earray) : Lincons1.t list =
    let l = ref [] in
    for i=0 to Lincons1.array_length ar - 1 do
      let d = Lincons1.array_get ar i in
      match Lincons1.get_typ d with
      | Lincons1.EQ ->
          Lincons1.set_typ d Lincons1.SUPEQ;
          l := d::(neg_lincons d)::(!l)
      | _ -> l := d::(!l)
    done;
    !l

  let array_of_lincons_list env (l:Lincons1.t list) : Lincons1.earray =
    let ar = Lincons1.array_make env (List.length l) in
    let i = ref 0 in
    List.iter (fun c -> Lincons1.array_set ar !i c; incr i) l;
    ar

  (* normalize equalities as pairs of inequalities *)
  let normalize_lincons_array (ar:Lincons1.earray) : Lincons1.earray =
    array_of_lincons_list (Lincons1.array_get_env ar) (list_of_lincons_array ar)

  (* construct abstract element from constraint list *)
  let abs_of_lincons_list env (l:Lincons1.t list) : t =
    Abstract1.of_lincons_array man env (array_of_lincons_list env l)

  (* normalized constraint list from abstract element *)
  let lincons_list_of_abs (a:t) : Lincons1.t list =
    list_of_lincons_array (Abstract1.to_lincons_array man a)



  (* constructs a new generator in opposite direction *)
  let neg_generator (d:Generator1.t) : Generator1.t =
    let d = Generator1.copy d in
    Generator1.iter (fun c v -> Generator1.set_coeff d v (Coeff.neg c)) d;
    d

  (* normalize lines as pairs of rays *)
  let list_of_generator_array (ar:Generator1.earray) : Generator1.t list =
    let l = ref [] in
    for i=0 to Generator1.array_length ar - 1 do
      let d = Generator1.array_get ar i in
      match Generator1.get_typ d with
      | Generator1.LINE ->
          Generator1.set_typ d Generator1.RAY;
          l := d::(neg_generator d)::(!l)
      | Generator1.LINEMOD ->
          Generator1.set_typ d Generator1.RAYMOD;
          l := d::(neg_generator d)::(!l)
      | _ -> l := d::(!l)
    done;
    !l
 
  (* generator list -> generator array *)
  let array_of_generator_list env (l:Generator1.t list) : Generator1.earray =
    let ar = Generator1.array_make env (List.length l) in
    let i = ref 0 in
    List.iter (fun c -> Generator1.array_set ar !i c; incr i) l;
    ar

  (* normalized constraint list from abstract element *)
  let generator_list_of_abs (a:t) : Generator1.t list =
    list_of_generator_array (Abstract1.to_generator_array man a)


  (* similar to add_ray, but also works for vertices *)
  let add_generators (a:t) (g:Generator1.t list) : t =
    let env = Abstract1.env a in
    (* categorize *)
    let vs,rs =
      List.fold_left
        (fun (v,r) g ->
          if Generator1.get_typ g = Generator1.VERTEX then g::v,r else v,g::r
        )
        ([],[]) g
    in
    (* add vertices *)
    let vi,vr = Environment.vars env in
    let vars = Array.append vi vr in
    let a =
      List.fold_left
        (fun a g ->
          let vals = 
            Array.init (Array.length vars)
              (fun i ->
                match Generator1.get_coeff g vars.(i) with
                | Coeff.Interval c -> c
                | Coeff.Scalar c -> Interval.of_scalar c c
              )
          in
          Abstract1.join man a (Abstract1.of_box man env vars vals)
        )
        a vs
    in
    (* add rays *)
    if rs = [] || Abstract1.is_bottom man a then a
    else Abstract1.add_ray_array man a (array_of_generator_list env rs)


  (* whether a contains the generator g *)
  let sat_generator (a:t) (g:Generator1.t) : bool =
    Abstract1.is_eq man a (add_generators a [g])



  (* useful transfer functions *)
  (* ************************* *)

  (* bounds v within i *)
  let bound_var (a:t) (v:var) ((l,h):Lin.I.t) : t =
    if not (Lin.B.is_finite l || Lin.B.is_finite h) then a else
    let env = Abstract1.env a in
    let x,y = Lin.B.to_apron (Lin.B.neg l), Lin.B.to_apron h in
    let v = apron_of_var v in
    let ar = Lincons1.array_make env 2 in
    if Lin.B.is_finite l then (
      let c = Lincons1.make( Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c [Coeff.s_of_int 1, v] (Some (Coeff.Scalar x));
      Lincons1.array_set ar 0 c
     );
    if Lin.B.is_finite h then (
      let c = Lincons1.make( Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c [Coeff.s_of_int (-1), v] (Some (Coeff.Scalar y));
      Lincons1.array_set ar 1 c
     );
    Abstract1.meet_lincons_array man a ar

  (* assigns the interval i to v *)
  let set_var_itv (a:t) (v:var) (i:Lin.I.t) : t =
    bound_var (Abstract1.forget_array man a [|apron_of_var v|] false) v i

  (* adds a constant interval i to v (c must be bounded) *)
  let shift_var_itv (a:t) (v:var) (i:Lin.I.t) : t =
    assert(Lin.B.is_finite (fst i) && Lin.B.is_finite (snd i));
    if Lin.I.equal i Lin.I.zero then a else
    let i = Lin.I.to_apron i in
    let v = apron_of_var v in
    let e = Linexpr1.make (Abstract1.env a) in
    Linexpr1.set_list e [Coeff.s_of_int 1, v] (Some (Coeff.Interval i));
    Abstract1.assign_linexpr man a v e None


  (* backward version of shift_var *)
  let bwd_shift_var_itv (a:t) (v:var) ((l,h) as i:Lin.I.t) : t =
    if Lin.I.equal i Lin.I.zero then a else
    (* shift v by -l and -h, then intersect *)
    (* all operations must be under-approximated (i.e., exact!) *)
    let sl,sh = Lin.B.neg l, Lin.B.neg h in
    let a1,a2 = shift_var_itv a v (sl,sl), shift_var_itv a v (sh,sh) in
    Abstract1.meet man a1 a2


  (* backward version of V := [-oo;+oo] *)
  let bwd_forget_var (a:t) (v:var) : t =
    if Abstract1.is_variable_unconstrained man a (apron_of_var v)
    then a else Abstract1.bottom man (Abstract1.env a)

 
  (* backward version of set_var *)
  let bwd_set_var_itv (a:t) (v:var) ((l,h) as i:Lin.I.t) : t =
    (* all operations must be under-approximated (i.e., exact!) *)
    match Lin.B.is_finite l, Lin.B.is_finite h with
    | true, true -> 
        (* bound v in i *)
        let a = bound_var a v i in
        (* shift *)
        let a = bwd_shift_var_itv a v i in
        (* forget v *)
        Abstract1.forget_array man a [|apron_of_var v|] false
    | _ ->
        (* TODO: more precise handling of half-infinite intervals *)
        bwd_forget_var a v

    
  (* backward version of the copy assignment v:=w *)
  let bwd_copy_var (a:t) (v:var) (w:var) : t =
    (* all operations must be under-approximated (i.e., exact!) *)
    let v,w = apron_of_var v, apron_of_var w in
    (* adds the constaint V=W *)
    let ar = Lincons1.array_make (Abstract1.env a) 1 in
    let c = Lincons1.array_get ar 0 in
    Lincons1.set_list c [Coeff.s_of_int 1, v; Coeff.s_of_int (-1), w] None;
    let a = Abstract1.meet_lincons_array man a ar in
    (* forget v *)
    Abstract1.forget_array man a [|v|] false


  (* first try to replace constraints in a with those in pool (or c), 
     then remove the contraint c
   *)
  let remove_constraint (a:t) (pool:Lincons1.t list) (c:Lin.lincons) : t =
    let env = Abstract1.env a in
    let c = apron_of_lincons env c in
    let ar = normalize_lincons_array (Abstract1.to_lincons_array man a) in
    (* for each constraint c in pool U { c } *)
    List.iter 
      (fun c ->
        (* for each constraint oldc in a *)
        for i=0 to Lincons1.array_length ar - 1 do
          let oldc = Lincons1.array_get ar i in
          if c <> oldc then (
            (* try to replace oldc with c *)
            Lincons1.array_set ar i c;
            let aa = Abstract1.of_lincons_array man env ar in
            if not (Abstract1.is_eq man a aa) then
              Lincons1.array_set ar i oldc
            else if trace_remove then  
              Format.printf "### remove_constraint replace %a with %a ###@\n" Lincons1.print oldc Lincons1.print c
           )
        done
      )
      (pool@[c]);
    (* now, remove c *)
    let l = list_of_lincons_array ar in
    let ll = List.filter (fun cc -> c <> cc) l in
    if trace_remove && List.length l <> List.length ll then 
      Format.printf "### remove_constraint remove: %a ###@\n" Lincons1.print c;
    abs_of_lincons_list env ll


  (* extrudes a along the lines (and rays) from dir *)
  let prism (a:t) (dir:t) : t =
    let gar = Abstract1.to_generator_array man dir in
    let l = 
      (* get all lines, and rays transformed into lines *)
      List.fold_left (* for each ray or line *)
        (fun acc g ->
          match Generator1.get_typ g with
          | Generator1.LINE | Generator1.RAY ->
              Generator1.set_typ g Generator1.LINE;
              g::acc
          | Generator1.LINEMOD | Generator1.RAYMOD ->
              Generator1.set_typ g Generator1.LINEMOD;
              g::acc
          | Generator1.VERTEX -> acc
        )
        [] (list_of_generator_array gar)
    in
    let gar = array_of_generator_list (Abstract1.env dir) l in
    Abstract1.add_ray_array man a gar


  (* add to a rays from dir that satisfy the constraint  *)
  let add_cone (a:t) (dir:Generator1.t list) (e:Lin.linexpr) : t =
    (* homogenous expression *)
    let e = Lin.A.set_cst Lin.I.zero e in
    let ea = (apron_of_lin (Abstract1.env a) e).Linexpr1.linexpr0 in
    let rays =
      (* filter rays *)
      List.fold_left
        (fun acc g ->
          match Generator1.get_typ g with
          | Generator1.RAY | Generator1.RAYMOD ->
              let env v = 
                Lin.I.of_coeff (Generator1.get_coeff g (apron_of_var v))
              in
              (* prevent from adding the ray defining the contraint itself *)
              (* this is required to analyze preciselw bwd_loop7! *)
              (* TODO: better heuristic to find which ray to add *)
              if Linexpr0.compare ea g.Generator1.generator0.Generator0.linexpr0 = 0 then acc else
              (* sign of scalar product *)
              let x = Lin.B.sign (fst (Lin.A.eval env e)) in
              if x >= 0 then g::acc else acc
          | _ -> acc
        )
        [] dir
    in

    (* add filtered rays *)
    let gar = array_of_generator_list (Abstract1.env a) rays in
    Abstract1.add_ray_array man a gar


(*
  (* alternate add_cone: for each generator that satisfy the constraint, make
     a new polyhedron by adding it, then intersect all these polyhedra
     DOES NOT WORK AS WELL
   *)
  let add_cone (a:t) (dir:Generator1.t list) (e:Lin.linexpr) : t =
    (* homogenous expression *)
    let e = Lin.A.set_cst Lin.I.zero e in
    let cones =
      List.fold_left
        (fun acc g ->
          match Generator1.get_typ g with
          | Generator1.RAY | Generator1.RAYMOD ->
              let env v = 
                Lin.I.of_coeff (Generator1.get_coeff g (apron_of_var v))
              in
              (* sign of scalar product *)
              let x = Lin.B.sign (fst (Lin.A.eval env e)) in
              if x >= 0 then
                (* new cone *)
                let gg = array_of_generator_list (Abstract1.env a) [g] in
                (Abstract1.add_ray_array man a gg)::acc
              else acc
          | _ -> acc
        )
        [] dir
    in
    (* intersect *)
    if cones = [] then a
    else List.fold_left (Abstract1.meet man) (List.hd cones) (List.tl cones)
*)


  (********************)
  (* GRAPHICAL OUTPUT *)
  (********************)

  type gobj =
    | G_abs of t
    | G_lincons of Lin.lincons
    | G_lincons1 of Lincons1.t
    | G_point of Lin.linexpr
    | G_linexpr1 of Linexpr1.t

  type glabel = string

  type gstyle = string

  type gelem = gobj * glabel * gstyle

  module PtMap = 
    Mapext.Make(struct type t=float*float let compare=compare end)

  module LineMap = 
    Mapext.Make(struct type t=float*float*float let compare=compare end)


  let simpler_var_name s =
    let s = Var.to_string s in
    try String.sub s 0 (String.index s '#') with Not_found -> s


  let svg_grid = true
  let svg_font = "sans"

  let print_svg (ch:out_channel) ?(color=false) ?window ((w,h):int*int) (a:t) ((x,y):var*var) =

    (* text processing drawing;
       put a white frame and try not put two texts at the same position
    *)
    let drawn = ref [] in
    let vtresh, htresh = 10., 8. in
    let textpos x y len =
      let x0,y0,len = x,y,float_of_int len in
      let rec doit x y =
        try
          let x',y' = List.find (fun (x',y') -> y<=y'+.vtresh && y'<=y+.vtresh && x<=x'+.len*.htresh && x'<=x+.len*.htresh) !drawn in
          let x = if x < x' then x -. htresh else x +. htresh in
          let y = 
            if y < y0 then y' -. vtresh*.1.4
            else if y > y0 then y' +. vtresh*.1.4
            else if y <= y' then y' -. vtresh*.1.4
            else y' +. vtresh*.1.4
          in
          doit x y
        with Not_found -> drawn := (x,y)::(!drawn); x,y
      in
      doit x y
    in
    let text x y anchor col s =
      let x,y = textpos x y (String.length s + 2) in
      Printf.fprintf ch "<text x='%g' y='%g' fill='none' stroke='white' stroke-width='3px' font-size='14px' font-weight='bold' font-family='%s' text-anchor='%s'>%s</text>\n" x y svg_font anchor s;
      Printf.fprintf ch "<text x='%g' y='%g' fill='%s' font-size='14px' font-weight='bold' font-family='%s' text-anchor='%s'>%s</text>\n" x y (if color then col else "black") svg_font anchor s
    in
    
    let xx,yy = apron_of_var x, apron_of_var y in
    (* extract linexpr coeff *)
    let get_coord l = 
      fst (Itv_float.of_coeff (Linexpr1.get_cst l)),
      fst (Itv_float.of_coeff (Linexpr1.get_coeff l xx)), 
      fst (Itv_float.of_coeff (Linexpr1.get_coeff l yy))
    in
    (* project on (x,y) *)
    let env = add_var_to_env (add_var_to_env (Environment.make [||] [||]) x) y 
    in
    let a = Abstract1.change_environment man a env true in
    (* get & convert constraints *)
    let lin = Abstract1.to_lincons_array man a in
    let l = 
      Array.init
        (Lincons1.array_length lin)
        (fun i ->
          let v = Lincons1.array_get lin i in
          get_coord (Lincons1.get_linexpr1 v), Lincons1.get_typ v
        )
    in
    (* compute interesting box & scale (based on vertices) *)
    let gen = Abstract1.to_generator_array man a in
    let b = match window with
    | None ->
        let m = ref 0. in
        let fact = ref 1.5 in
        for i=0 to Generator1.array_length gen - 1 do
          let g = Generator1.array_get gen i in
          if Generator1.get_typ g <> Generator1.VERTEX then fact := 3. else
          let _,x,y = get_coord (Generator1.get_linexpr1 g) in
          m := max !m (max (abs_float x) (abs_float y));
        done;
        if !m = 0. then m := 1.;
        !m *. !fact
    | Some (x0,y0,x1,y1) -> max (max (abs_float x0) (abs_float y0)) (max (abs_float x1) (abs_float y1))
    in
    let scale = 10. ** (floor (log10 b -. 0.3)) in
    let b = (ceil (b /. scale)) *. scale in
    let x0,y0,x1,y1 = match window with
    | None -> -. b, -. b, b, b 
    | Some (x0,y0,x1,y1) -> x0,y0,x1,y1
    in
    (* bound *)
    let b = bound_var a x (Lin.I.of_floats x0 x1) in
    let b = bound_var b y (Lin.I.of_floats y0 y1) in
    (* get & convert generators *)
    let gen' = Abstract1.to_generator_array man b in
    let g = 
      Array.to_list 
        (Array.init
           (Generator1.array_length gen')
           (fun i -> 
             get_coord (Generator1.get_linexpr1 (Generator1.array_get gen' i))
        ))
    in
    (* sort generators *)
    let l_to_r = 
      List.sort
        (fun (_,x,y) (_,x',y') -> if x=x' then compare y' y else compare x x')
        g 
    in
    let (_,_,u) = try List.hd l_to_r with _ -> 0.,0.,0. in
    let up = List.filter (fun (_,_,y) -> y>=u) l_to_r
    and down = List.rev (List.filter (fun (_,_,y) -> y<u) l_to_r)
    in
    (* coordinate transform *)
    let xx x = (float_of_int w) *. (x-.x0) /. (x1-.x0)
    and yy y = (float_of_int h) *. (y-.y1) /. (y0-.y1)
    in
    (* output header *)
    Printf.fprintf ch "<svg width='%ipx' height='%ipx'>\n" w h;
    (* output background *)
    Printf.fprintf ch "<rect fill='white' stroke='none' x='%i' y='%i' width='%i' height='%i'/>\n" 0 0 w h;
    (* output polygon *)
    Printf.fprintf ch "<polygon fill='lime' points='";
    List.iter (fun (_,x,y) -> Printf.fprintf ch "%g,%g " (xx x) (yy y)) (up@down);
    Printf.fprintf ch "'/>\n";
    (* output coordinate system *)
    Printf.fprintf ch "<line x1='%i' y1='%f' x2='%i' y2='%g' stroke='gray' stroke-width='1px'/>\n" 0 (yy 0.) w (yy 0.);
    Printf.fprintf ch "<line x1='%g' y1='%i' x2='%g' y2='%i' stroke='gray' stroke-width='1px'/>\n" (xx 0.) 0 (xx 0.) h;
    if svg_grid then (
      let c = ref scale in
      while !c <= x1 do
        Printf.fprintf ch "<line x1='%i' y1='%f' x2='%i' y2='%g' stroke='lightgray' stroke-width='1px'/>\n" 0 (yy !c) w (yy !c);
        Printf.fprintf ch "<line x1='%i' y1='%f' x2='%i' y2='%g' stroke='lightgray' stroke-width='1px'/>\n" 0 (yy (-. !c)) w (yy (-. !c));
        Printf.fprintf ch "<line x1='%g' y1='%i' x2='%g' y2='%i' stroke='lightgray' stroke-width='1px'/>\n" (xx !c) 0 (xx !c) h;
        Printf.fprintf ch "<line x1='%g' y1='%i' x2='%g' y2='%i' stroke='lightgray' stroke-width='1px'/>\n" (xx (-. !c)) 0 (xx (-. !c)) h;
        c := !c +. scale
      done
     );
    text (float_of_int (w-10)) ((yy 0.)-.4.) "end" "gray" x.var_name;
    text ((xx 0.)+.4.) 10. "start" "gray" y.var_name;
    (* output constraints *)
    Array.iter
      (fun ((c,a,b),op) ->
        let p = [(-.c-.b*.y1)/.a, y1;  x1, (-.c-.a*.x1)/.b;
                 (-.c-.b*.y0)/.a, y0;  x0, (-.c-.a*.x0)/.b]
        in
        (* constraint line *)
        let p = List.sort (fun a b -> compare (fst a) (fst b)) p in
        let p = List.filter (fun (x,y) -> x>=x0 && x<=x1 && y>=y0 && y<=y1) p in
        Printf.fprintf ch "<polyline points='";
        List.iter (fun (x,y) -> Printf.fprintf ch "%g,%g " (xx x) (yy y)) p;
        Printf.fprintf ch "' stroke='blue' stroke-width='1px' fill='none'/>\n";
        (* constraint test *)
        if List.length p > 0 then
          let u,v = List.hd p in
          let u = 0.95 *. u +. 0.05*.(x0+.x1)/.2.
          and v = yy v +. (if v >= (y0+.y1)/.2. then 12. else -. 6.)
          in
          let ll = Lin.A.set_var x (Lin.I.of_float a) (Lin.A.set_var y (Lin.I.of_float b) (Lin.A.cst (Lin.I.of_float c))) in
          text  (xx u) v (if u < (x0+.x1)/.2. then "start" else "end") "midnightblue" ((Lin.A.to_string ll)^" "^(Lincons1.string_of_typ op)^" 0")
      )
      l;
    Printf.fprintf ch "<polygon fill='none' stroke='%s' stroke-width='1pt' points='" (if color then "blue" else "black");
    List.iter (fun (_,x,y) -> Printf.fprintf ch "%g,%g " (xx x) (yy y)) (up@down);
    Printf.fprintf ch "'/>\n";
    (* output vertices *)
    for i=0 to Generator1.array_length gen - 1 do
      let g = Generator1.array_get gen i in
      if Generator1.get_typ g <> Generator1.VERTEX then () else
      let _,x,y = get_coord (Generator1.get_linexpr1 g) in
      (* vertex dot *)
      Printf.fprintf ch "<circle cx='%f' cy='%f' r='3' fill='%s' stroke='%s' stroke-width='2px'/>\n" (xx x) (yy y) "white" (if color then "darkgreen" else "black");
      (* vertex coord *)
      text (xx x) (if y >= u then yy y -. 8. else yy y +. 14.) "middle" "darkgreen" (Printf.sprintf "(%g,%g)" (x+.0.) (y+.0.));
    done;
    (* output footer *)
    Printf.fprintf ch "</svg>\n"



  (**********************)
  (* HTML, LaTeX OUTPUT *)
  (**********************)

  (* HTML *)
  let print_html o a = raise (Invalid_argument "not implemented")

  (* LaTeX *)
  let print_latex o a =
    if Abstract1.is_bottom man a then output_string o "\\bottom" else
    if Abstract1.is_top man a then output_string o "\\top" else
    let cons = Abstract1.to_lincons_array man a in
    let first = ref true in
    let rec pp_coeff c v = match c with
    | Coeff.Scalar s -> 
        if v <> "" && Scalar.sgn s = 0 then () else (
        if Scalar.sgn s < 0 then 
          if v <> "" && Scalar.equal_int s (-1) then output_string o "-" else
          Printf.fprintf o "-%s" (Scalar.to_string (Scalar.neg s))
        else if !first then 
          if v <> "" && Scalar.equal_int s 1 then () else
          output_string o (Scalar.to_string s)
        else 
          if v <> "" && Scalar.equal_int s 1 then output_string o " + " else
          Printf.fprintf o "+%s" (Scalar.to_string s);
        if v <> "" then Printf.fprintf o "{\\tt %s}" v;
        first := false
       )
    | Coeff.Interval i ->
        if Scalar.equal i.Interval.inf i.Interval.sup then
          pp_coeff (Coeff.Scalar i.Interval.inf) v
        else (
          if not !first then output_string o "+";
          Printf.fprintf o "{}[%s,%s]" 
            (Scalar.to_string i.Interval.inf)
            (Scalar.to_string i.Interval.sup);
          if v<>"" then Printf.fprintf o "{\tt %s}" v
         );
        first := false
    in
    for i=0 to Lincons1.array_length cons - 1 do
      if i > 0 then output_string o "\\wedge ";
      let l1 = Lincons1.array_get cons i in
      first := true;
      Lincons1.iter (fun c v -> pp_coeff c (simpler_var_name v)) l1;
      let c = Coeff.neg (Lincons1.get_cst l1) in
      if !first then output_string o "0";
      first := true;
      (match Lincons1.get_typ l1 with
      | Lincons1.EQ -> output_string o " = "; pp_coeff c ""
      | Lincons1.SUPEQ -> output_string o "  \\geq "; pp_coeff c ""
      | Lincons1.SUP -> output_string o "  > "; pp_coeff c ""
      | Lincons1.DISEQ -> output_string o "  \\neq "; pp_coeff c ""
      | Lincons1.EQMOD s -> output_string o "  = "; pp_coeff c ""; Printf.fprintf o "\\;[%s]" (Scalar.to_string s));
    done

    



  (* ********************* *)
  (* CREATION & INSPECTION *)
  (* ********************* *)


  let empty () =
    Abstract1.top man (Environment.make [||] [||])

  let bot a =
    Abstract1.bottom man (Abstract1.env a)

  let top a =
    Abstract1.top man (Abstract1.env a)

  let is_bot a =
    Abstract1.is_bottom man a
      
  let subseteq a b =
    Abstract1.is_leq man a b
      
  let print fmt a =
    Abstract1.print fmt a
      


  (* ***************** *)
  (* FORWARD OPERATORS *)
  (* ***************** *)


  let fwd_join a b m =
    match m with
    | MERGE -> Abstract1.join man a b
    | WIDEN -> Abstract1.widening man a b


  let fwd_add_var a v =
    let env = add_var_to_env (Abstract1.env a) v in
    let a = Abstract1.change_environment man a env false in
    bound_var a v (Lin.type_itv v.var_typ)

  let fwd_del_var a v =
    let env = Environment.remove (Abstract1.env a) [|apron_of_var v|] in
    Abstract1.change_environment man a env false


  let fwd_assign a dst e =
    let l = Lin.linearize_expr true (lin_env a) e in
    let ee = apron_of_lin (Abstract1.env a) l in
    let assign v = Abstract1.assign_linexpr man a (apron_of_var v) ee None
    in
    (match dst with
    | STRONG v ->
        if v.var_scope = T_VOLATILE || v.var_scope = T_INPUT
        then set_var_itv a v (Lin.A.eval (lin_env a) l)
        else assign v
    | WEAK vl ->
        List.fold_left (fun a v -> Abstract1.join man a (assign v)) a vl
    ), noerr


  let fwd_filter a e =
    let env = Abstract1.env a in
    let c1 = Lin.linearize_cons true (lin_env a) e in
    let c2 = Lin.neg_cons c1 in
    Abstract1.meet_lincons_array man a (apron_of_lincons_ar env c1),
    Abstract1.meet_lincons_array man a (apron_of_lincons_ar env c2),
    noerr



  (* ****************** *)
  (* BACKWARD OPERATORS *)
  (* ****************** *)


  let bwd_trace msg f x =
    let r = f x in
    if trace_bwd then Format.printf "**** %s => %a@\n" msg print r;
    r


  let bwd_add_var post v pre =
    let (l,h) = Lin.type_itv v.var_typ in
    if trace_bwd then Format.printf "** bwd_add_var %s %s@\n" v.var_name (Lin.I.to_string (l,h));
    let post = bwd_trace "bwd_set_var_itv" (bwd_set_var_itv post v) (l,h) in
    let post = bwd_trace "fwd_del_var" (fwd_del_var post) v in
    bwd_trace "meet" (Abstract1.meet man pre) post
      

  let bwd_del_var post v pre =
    if trace_bwd then Format.printf "** bwd_del_var %s@\n" v.var_name;
    let post = bwd_trace "fwd_add_var" (fwd_add_var post) v in
    bwd_trace "meet" (Abstract1.meet man pre) post


  let bwd_meet a b m =
    match m with
    | MERGE -> Abstract1.meet man a b
    | WIDEN ->
        if subseteq a b then a else (
        (* filter generators from a *)
        let gens =
          List.fold_left
            (fun acc g ->
              if sat_generator b g then g::acc else acc
            )
            [] (generator_list_of_abs a)
        in
        (* make abstract element from remaining generators *)
        let r = add_generators (Abstract1.bottom man (Abstract1.env a)) gens in
        if trace_bwd then Format.printf "** bwd lower widening@\n**** arg1 = %a@\n**** arg2 = %a@\n**** res= %a@\n" print a print b print r;
        r
       )


  (* helper for bwd_filter and non-invertible bwd_assign;
     cpool and rpool are constraint and ray pools to guide the operation
   *)
  let add_space post cpool rpool c =
    let env = Abstract1.env post in
    if trace_bwd then Format.printf "**** add_space %s@\n" (Lin.cons_to_string (Lin.neg_cons c));
    (* intersect with constraint *)
    let cons = apron_of_lincons_ar env c in
    let post = bwd_trace "** meet_constraint" 
        (Abstract1.meet_lincons_array man post) cons
    in
(*
    (* equality *)
    let cc = 
      Abstract1.of_lincons_array man env 
        (apron_of_lincons_ar env (fst c,Lin.L_EQ))
    in
*)
    if is_bot post then
      (* does not interect c => build element with the negation of c *)
      let c = apron_of_lincons_ar env (Lin.neg_cons c) in
      bwd_trace "** of_lincons" (Abstract1.of_lincons_array man env) c
    else (*if Abstract1.is_leq man post cc then
      (* saturate c => add cone *)
      bwd_trace "** add_cone" (add_cone post rpool) (fst (Lin.neg_cons c))
    else*)
      (* try adding cone *)
      let coned = bwd_trace "** add_cone" (add_cone post rpool) (fst (Lin.neg_cons c))
      in
      (* keep with cone only if sound *)
      let post = 
        if Abstract1.is_eq man post (Abstract1.meet_lincons_array man coned cons) 
        then coned
        else post
      in
      (* remove constraint *)
      bwd_trace "** remove_constraint" (remove_constraint post cpool) c
        

  let bwd_filter post1 post2 err e typ pre =
    let do_constraint post cpool rpool ((((l,h),m),op) as c) =
      match op with
      | Lin.L_GT | Lin.L_GEQ | Lin.L_GEQ_INT ->
          (* aff + [l;h] >= 0 => aff + h >= 0 *)
          add_space post cpool rpool (((h,h),m),op)
      | Lin.L_EQ | Lin.L_EQ_INT ->
          (* aff + [l;h] = 0 => aff + h >= 0 \/ aff + l <= 0 *)
          let (((_,h1),m1),op1), (((_,h2),m2),op2) = Lin.split_cons c in
          let post = add_space post cpool rpool (((h1,h1),m1),op1) in
          add_space post cpool rpool (((h2,h2),m2),op2)
      | Lin.L_NEQ | Lin.L_NEQ_INT -> 
          if not (Lin.B.equal l h) then Abstract1.meet man pre post else
          (* aff + c = 0 => aff + c <= 0 /\ aff + c >= 0 *)
          let c1,c2 = Lin.split_cons c in
          let post1 = add_space post cpool rpool c1 in
          let post2 = add_space post cpool rpool c2 in
          bwd_trace "meet_neq" (Abstract1.meet man post1) post2
    in
    let c1 = Lin.linearize_cons true (lin_env pre) e in
    let c2 = Lin.neg_cons c1 in
    if trace_bwd then Format.printf "** bwd_filter post1 %a@\n" print post1;
    if trace_bwd then Format.printf "** bwd_filter true %s@\n" (Lin.cons_to_string c1);
    let cpool1 = lincons_list_of_abs post2 in
    let rpool1 = List.map neg_generator (generator_list_of_abs post2) in
    let mid1 = do_constraint post1 cpool1 rpool1 c1 in

    if trace_bwd then Format.printf "** bwd_filter post2 %a@\n" print post2;
    if trace_bwd then Format.printf "** bwd_filter false %s@\n" (Lin.cons_to_string c2);
    let cpool2 = lincons_list_of_abs mid1 in
    let rpool2 = List.map neg_generator (generator_list_of_abs mid1) in
    let mid2 = do_constraint post2 cpool2 rpool2 c2 in
    let both = bwd_trace "meet_both" (Abstract1.meet man mid1) mid2 in
    bwd_trace "meet_pre" (Abstract1.meet man pre) both

  (* improvement on bwd_filter: if it fails, tries to force a branch *)
  let bwd_filter post1 post2 err e typ pre =
    (* try both branches *)
    let r = bwd_filter post1 post2 err e typ pre in
    if not (is_bot r) then r else
    (* try to force the true branch *)  (
    if trace_bwd then Format.printf "** bwd_filter force true branch@\n";
    let r = bwd_filter post1 (bot post2) err e typ pre in
    if not (is_bot r) then r else (
    (* try to force the false branch *)
    if trace_bwd then Format.printf "** bwd_filter force false branch@\n";
    bwd_filter (bot post1) post2 err e typ pre))


  (* special version of add_space that tries to maximize v *)
  let add_space_var post v l =
    let env = Abstract1.env post in
    let av = apron_of_var v in
    (* get the equality section *)
    let eq = apron_of_lincons_ar env (l,Lin.L_EQ) in
    let sec = Abstract1.meet_lincons_array man post eq in
    (* extrude along v *)
    let prism = bwd_trace "prism" (Abstract1.forget_array man sec [|av|]) false
    in
    (* meet with post *)
    let post = bwd_trace "meet_prism" (Abstract1.meet man post) prism in
    (* build line for the variable *)
    let lv = Linexpr1.make env in
    Linexpr1.set_coeff lv av (Coeff.s_of_int 1);
    let gv = Generator1.make lv Generator1.RAY in
    (* finally, add space *)
    add_space post [] [gv; neg_generator gv] (l,Lin.L_GEQ)


  let bwd_assign post err dst e pre =
    let env = Abstract1.env pre in
    let i,m = Lin.linearize_expr true (lin_env pre) e in
    let post = match dst with 
    | WEAK _ -> failwith "bwd_assign WEAK: TODO"
    | STRONG v ->
        if trace_bwd then Format.printf "** bwd_assign %s := %s@\n" v.var_name (Lin.expr_to_string (i,m));
        if Lin.VMap.is_empty m then
          (* constant case *)
          bwd_trace "bwd_set_var" (bwd_set_var_itv post v) i
        else if Lin.VMap.mem v m then
          (* invertible case *)
          let post = bwd_trace "bwd_shift_var" (bwd_shift_var_itv post v) i in
          let l = Lin.invert true (lin_env pre) (Lin.I.zero,m) v in
          if trace_bwd then Format.printf "**** inverted %s@\n" (Lin.expr_to_string l);
          let e = apron_of_lin env l in
          bwd_trace "assign_linexpr" (Abstract1.assign_linexpr man post (apron_of_var v) e) None
        else
          (* non-invertible case: handled as forget then add equality *)
          (* backward equality *)
          let ((l,h),m) = Lin.A.set_var v Lin.I.minus_one (i,m) in
          let post = add_space_var post v ((h,h),m) in
          let post = add_space_var post v (Lin.A.neg ((l,l),m)) in
   if trace_bwd then Format.printf "** !! bwd_assign post %a@\n" print post;
         (* backward forget *)
          bwd_trace "bwd_forget_var" (bwd_forget_var post) v
          
    in
    bwd_trace "meet" (Abstract1.meet man pre) post



end



(* INSTANTIATIONS *)
(* ************** *)

module PolkaDomain =
  ApronDomain
    (struct
      type lib = Polka.strict Polka.t
      let manager = Polka.manager_alloc_strict ()
    end)

module OctagonDomain =
  ApronDomain
    (struct
      type lib = Oct.t
      let manager = Oct.manager_alloc ()
    end)

