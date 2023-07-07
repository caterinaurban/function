open Banal_datatypes
open Banal_domain
open Banal_mathtypes
open Banal_typed_syntax
open Apron

module type APRON_PARAM = sig
  type lib

  val manager : lib Manager.t
end

module Make (Param: APRON_PARAM) (Itv: INTERVAL) = struct
  module Lin = Banal_linearization.Make (Itv)
  type t = Param.lib Abstract1.t
  let man = Param.manager

  (* To/from APRON types *)
  (* ******************* *)

  (* apron name of a variable *)
  let apron_of_var (v : var) : Var.t = Var.of_string v.var_id

  let add_var_to_env env v =
    let i, r =
      match v.var_typ with
      | A_int _ | A_BOOL -> ([| apron_of_var v |], [||])
      | A_float _ -> ([||], [| apron_of_var v |])
    in
    Environment.add env i r

  (* linear expressions *)
  let lin_of_blin env ((i, m) : Lin.linexpr) : Linexpr1.t =
    let r = Linexpr1.make env in
    Linexpr1.set_cst r (Coeff.Interval (Lin.I.to_apron i));
    Lin.VMap.iter
      (fun v i ->
        Linexpr1.set_coeff r (apron_of_var v)
          (Coeff.Interval (Lin.I.to_apron i)))
      m;
    r

  let blin_of_lin (vset: VSet.t) (le: Linexpr1.t) : Lin.linexpr =
    let cst = Linexpr1.get_cst le |> Lin.I.of_coeff in
    let vmap = VSet.to_seq vset
      |> Seq.map (fun v -> v, apron_of_var v)
      |> Seq.map (fun (v,av) -> v, Linexpr1.get_coeff le av |> Lin.I.of_coeff)
      |> Lin.VMap.of_seq
    in cst,vmap

  (* linear constraints *)
  let cons_op_of_bcons_op = function
    | Lin.L_EQ | Lin.L_EQ_INT -> Lincons1.EQ
    | Lin.L_NEQ | Lin.L_NEQ_INT -> Lincons1.DISEQ
    | Lin.L_GEQ | Lin.L_GEQ_INT -> Lincons1.SUPEQ
    | Lin.L_GT -> Lincons1.SUP

  let lincons_of_blincons env ((l, o) : Lin.lincons) : Lincons1.t =
    Lincons1.make (lin_of_blin env l) (cons_op_of_bcons_op o)

  let lincons_array_of_lincons env (c : Lincons1.t) : Lincons1.earray =
    let a = Lincons1.array_make env 1 in
    Lincons1.array_set a 0 c;
    a

  let lincons_array_of_blincons env (c : Lin.lincons) : Lincons1.earray =
    lincons_array_of_lincons env (lincons_of_blincons env c)

  (* creates a linearization environment (variable to interval) *)
  let lin_env (a : t) : Lin.env =
   fun v -> Lin.I.of_apron (Abstract1.bound_variable man a (apron_of_var v))

  (* Lincons construction/extraction and normalization *)
  (* ************************************************* *)

  (* negate a linexpr *)
  let neg_linexpr (e : Linexpr1.t) : Linexpr1.t =
    let e = Linexpr1.copy e in
    Linexpr1.set_cst e (Coeff.neg (Linexpr1.get_cst e));
    Linexpr1.iter (fun c v -> Linexpr1.set_coeff e v (Coeff.neg c)) e;
    e

  (* constructs a new contraint in opposite direction *)
  let neg_lincons (d : Lincons1.t) : Lincons1.t =
    Lincons1.make (Lincons1.get_linexpr1 d |> neg_linexpr) (Lincons1.get_typ d)

  (* normalize equalities as pairs of inequalities *)
  let normalize_lincons (d:Lincons1.t) : Lincons1.t list =
    match Lincons1.get_typ d with
    | Lincons1.EQ ->
        Lincons1.set_typ d Lincons1.SUPEQ;
        [d; neg_lincons d]
    | DISEQ
    | EQMOD _ -> failwith (__LOC__^": unexpected lincons kind")
    | _ -> [d]

  let seq_of_lincons_array (ar: Lincons1.earray) : Lincons1.t Seq.t =
    Seq.ints 0
      |> Seq.take (Lincons1.array_length ar)
      |> Seq.map (fun i -> Lincons1.array_get ar i)
      |> Seq.flat_map (fun c -> normalize_lincons c |> List.to_seq)

  let list_of_lincons_array (ar : Lincons1.earray) : Lincons1.t list =
    seq_of_lincons_array ar |> List.of_seq

  let array_of_lincons_list env (l : Lincons1.t list) : Lincons1.earray =
    let ar = Lincons1.array_make env (List.length l) in
    let i = ref 0 in
    List.iter
      (fun c ->
        Lincons1.array_set ar !i c;
        incr i)
      l;
    ar

  let array_of_lincons_seq env (s : Lincons1.t Seq.t) : Lincons1.earray =
    let ar = Lincons1.array_make env (Seq.length s) in
    let i = ref 0 in
    Seq.iter
      (fun c ->
        Lincons1.array_set ar !i c;
        incr i)
      s;
    ar

  let lincons_list_of_abs (a : t) : Lincons1.t list =
    list_of_lincons_array (Abstract1.to_lincons_array man a)

  let lincons_seq_of_abs (a : t) : Lincons1.t Seq.t =
    seq_of_lincons_array (Abstract1.to_lincons_array man a)

  let lincons_array_of_abs (a : t) : Lincons1.earray =
    array_of_lincons_list (Abstract1.env a) (lincons_list_of_abs a)

  let safe_abs_of_lincons_array env (l : Lincons1.earray) =
    (* Workaround a bug in APRON which causes a segfault when a a constraint of
       the form `lin_expr +- inf >= 0` is converted into an abstract value.

       Workaround also an imprecision in APRON which returns top as the
       abstract element corresponding to the constraints `x-inf>=0`.
     *)

    let s_inf = Coeff.(Scalar (Scalar.of_infty 1)) in
    let i_inf = Coeff.(Interval {inf=Scalar.of_infty 1;sup=Scalar.of_infty 1}) in
    let s_minf = Coeff.(Scalar (Scalar.of_infty (-1))) in
    let i_minf = Coeff.(Interval {inf=Scalar.of_infty (-1);sup=Scalar.of_infty (-1)}) in

    for i=0 to Lincons1.array_length l - 1 do
      let cons = Lincons1.array_get l i in

      (* ensure that all the coefficients of the variables are not infinite *)
      let all_finite =  Array.to_seq (fst (Environment.vars env))
        |> Seq.interleave (Array.to_seq (snd (Environment.vars env)))
        |> Seq.for_all (fun v ->
          match Lincons1.get_coeff cons v with
          | Scalar s -> Scalar.is_infty s = 0
          | Interval {inf; sup} -> Scalar.is_infty inf = 0 && Scalar.is_infty sup = 0
        )
      in assert all_finite;

      let cst = Lincons1.get_cst cons in

      if Coeff.equal s_inf cst || Coeff.equal i_inf cst then begin
        (* inf >= 0 is always true, replace it with 0 >= 0 *)
        Lincons1.array_set l i (Lincons1.make (Linexpr1.make env) Lincons1.SUPEQ);
      end;

      if Coeff.equal s_minf cst || Coeff.equal i_minf cst then begin
        (* minf >= 0 is never true, replace it with -1 >= 0 *)
        let le = Linexpr1.make env in
        Linexpr1.set_cst le (Coeff.Scalar (Scalar.of_int (-1)));
        Lincons1.array_set l i (Lincons1.make le Lincons1.SUPEQ);
      end
    done;

    Abstract1.of_lincons_array man env l

  (* construct abstract element from constraint list *)
  let abs_of_lincons_list env (l : Lincons1.t list) : t =
    safe_abs_of_lincons_array env (array_of_lincons_list env l)

  (* construct abstract element from constraint seq *)
  let abs_of_lincons_seq env (l : Lincons1.t Seq.t) : t =
    safe_abs_of_lincons_array env (array_of_lincons_seq env l)

  (* construct an abstract element from a constraint *)
  let abs_of_lincons env (c: Lincons1.t) : t =
    let ar = Lincons1.array_make env 1 in
    Lincons1.array_set ar 0 c;
    safe_abs_of_lincons_array env ar

  let abs_of_blincons env (c: Lin.lincons) : t =
    abs_of_lincons env (lincons_of_blincons env c)

  (* Generators construction/extraction and normalization *)
  (* **************************************************** *)

  (* constructs a new generator in opposite direction *)
  let neg_generator (g : Generator1.t) : Generator1.t =
    Generator1.make (Generator1.get_linexpr1 g |> neg_linexpr) (Generator1.get_typ g)

  (* normalize generators. Vertexs and rays are left unmodified,
     lines are converted into a pair of rays (thus the output is
     a list)
  *)
  let normalize_gen (g : Generator1.t) : Generator1.t list =
    match Generator1.get_typ g with
    | Generator1.LINE ->
        let g = Generator1.copy g in
        Generator1.set_typ g Generator1.RAY;
        [ g; neg_generator g ]
    | Generator1.LINEMOD ->
        let g = Generator1.copy g in
        Generator1.set_typ g Generator1.RAYMOD;
        [ g; neg_generator g ]
    | _ -> [ g ]

  (* normalize lines as pairs of rays *)
  let list_of_generator_array (ar : Generator1.earray) : Generator1.t list =
    let l = ref [] in
    for i = 0 to Generator1.array_length ar - 1 do
      let d = Generator1.array_get ar i in
      l := normalize_gen d @ !l
    done;
    !l

  (* generator list -> generator array *)
  let array_of_generator_list env (l : Generator1.t list) : Generator1.earray =
    let ar = Generator1.array_make env (List.length l) in
    let i = ref 0 in
    List.iter
      (fun c ->
        Generator1.array_set ar !i c;
        incr i)
      l;
    ar

  (* normalized constraint list from abstract element *)
  let generator_seq_of_abs (a : t) : Generator1.t Seq.t =
    let ar = Abstract1.to_generator_array man a in
    Seq.ints 0
      |> Seq.take (Generator1.array_length ar)
      |> Seq.map (fun i -> Generator1.array_get ar i)
      |> Seq.flat_map (fun g -> normalize_gen g |> List.to_seq)

  let generator_list_of_abs (a : t) : Generator1.t list =
    list_of_generator_array (Abstract1.to_generator_array man a)

  let vertex_seq_of_abs (a:t) : Generator1.t Seq.t =
    generator_seq_of_abs a
      |> Seq.filter (fun g -> Generator1.get_typ g = VERTEX)

  let vertex_list_of_abs (a:t) : Generator1.t list =
    List.of_seq (vertex_seq_of_abs a)

  let ray_seq_of_abs (a:t) : Generator1.t Seq.t =
    generator_seq_of_abs a
      |> Seq.filter (fun g -> Generator1.get_typ g = RAY || Generator1.get_typ g = RAYMOD)

  let ray_list_of_abs (a:t) : Generator1.t list =
    List.of_seq (ray_seq_of_abs a)

  (* Modifiers *)
  (* ********* *)

  let meet_lincons env (a:t) (c:Lincons1.t) : t =
    Abstract1.meet_lincons_array man a (lincons_array_of_lincons env c)

  let meet_blincons env (a:t) (c:Lin.lincons) : t =
    Abstract1.meet_lincons_array man a (lincons_array_of_blincons env c)

  (* similar to add_ray, but also works for vertices *)
  let add_generators (a : t) (g : Generator1.t list) : t =
    let env = Abstract1.env a in
    (* categorize *)
    let vs, rs =
      List.fold_left
        (fun (v, r) g ->
          if Generator1.get_typ g = Generator1.VERTEX then (g :: v, r)
          else (v, g :: r))
        ([], []) g
    in
    (* add vertices *)
    let vi, vr = Environment.vars env in
    let vars = Array.append vi vr in
    let a =
      List.fold_left
        (fun a g ->
          let vals =
            Array.init (Array.length vars) (fun i ->
                let le = Linexpr1.make env in
                Linexpr1.set_cst le (Generator1.get_coeff g vars.(i));
                le)
          in

          (* Note: use `assign_linexpr` as `of_box` does not work with fractionary coordinates. *)
          let vtx = Abstract1.assign_linexpr_array man (Abstract1.top man env) vars vals None in
          Abstract1.join man a vtx)
        a vs
    in
    (* add rays *)
    if rs = [] || Abstract1.is_bottom man a then a
    else Abstract1.add_ray_array man a (array_of_generator_list env rs)

  let abs_of_generators_list (env: Environment.t) (g: Generator1.t list) : t =
    add_generators (Abstract1.bottom man env) g

  (* try to remove a constraint. returns `Some a` if the contraint was removed,
     `None` otherwise *)
  let remove_lincons (c:Lincons1.t) (a:t) : t option =
    let lincons_list, found = lincons_seq_of_abs a
      |> Seq.fold_left (fun (acc_lst, found) cc ->
          if c = cc then acc_lst, true else cc::acc_lst, found
        )
        ([], false)
    in if found then Some (abs_of_lincons_list (Abstract1.env a) lincons_list) else None

  let remove_blincons (c:Lin.lincons) (a:t) : t option =
    remove_lincons (lincons_of_blincons (Abstract1.env a) c) a

  (* first try to replace constraints in a with those in pool (or c),
     then remove the contraint c
   *)
  let remove_lincons_and_harmonize (a : t) (pool : Lincons1.t list) (c : Lin.lincons) : t =
    let env = Abstract1.env a in
    let c = lincons_of_blincons env c in
    let ar = lincons_array_of_abs a in
    (* for each constraint c in pool U { c } *)
    List.iter
      (fun c ->
        (* for each constraint oldc in a *)
        for i = 0 to Lincons1.array_length ar - 1 do
          let oldc = Lincons1.array_get ar i in
          if c <> oldc then (
            (* try to replace oldc with c *)
            Lincons1.array_set ar i c;
            let aa = safe_abs_of_lincons_array env ar in
            if not (Abstract1.is_eq man a aa) then Lincons1.array_set ar i oldc
              )
        done)
      (pool @ [ c ]);
    (* now, remove c *)
    let l = list_of_lincons_array ar in
    let ll = List.filter (fun cc -> c <> cc) l in
    abs_of_lincons_list env ll

  (* add to a rays from dir that satisfy the constraint  *)
  let add_cone (a : t) (dir : Generator1.t list) (e : Lin.linexpr) : t =
    (* homogenous expression *)
    let e = Lin.A.set_cst Lin.I.zero e in
    let ea = (lin_of_blin (Abstract1.env a) e).Linexpr1.linexpr0 in
    let rays =
      (* filter rays *)
      List.fold_left
        (fun acc g ->
          match Generator1.get_typ g with
          | Generator1.RAY | Generator1.RAYMOD ->
              let g = neg_generator g in
              let env v =
                Lin.I.of_coeff (Generator1.get_coeff g (apron_of_var v))
              in
              (* prevent from adding the ray defining the contraint itself *)
              (* this is required to analyze precisely bwd_loop7! *)
              (* TODO: better heuristic to find which ray to add *)
              if
                Linexpr0.compare ea g.Generator1.generator0.Generator0.linexpr0
                = 0
              then acc
              else
                (* sign of scalar product *)
                let x = Lin.B.sign (fst (Lin.A.eval env e)) in
                if x >= 0 then g :: acc else acc
          | _ -> acc)
        [] dir
    in

    (* add filtered rays *)
    let gar = array_of_generator_list (Abstract1.env a) rays in
    Abstract1.add_ray_array man a gar


  (* Predicates *)
  (* ********** *)

  (* whether a contains the generator g *)
  let sat_generator (a : t) (g : Generator1.t) : bool =
    Abstract1.is_eq man a (add_generators a [ g ])

  let is_closed (a:t) : bool =
    lincons_seq_of_abs a
      |> Seq.for_all (fun c ->
        match Lincons1.get_typ c with
        | EQ
        | SUPEQ -> true
        | SUP -> false
        | EQMOD _ -> failwith "unexpected EQMOD constraint"
        | DISEQ -> failwith "unexpected DISEQ constraint"
        )

  (* find all the generators saturating a constraint *)
  let gen_saturating_cons cons vset a =
    let le = blin_of_lin vset (Lincons1.get_linexpr1 cons) in
    let le_no_cst = Lin.A.set_cst Itv.zero le in

    generator_seq_of_abs a
      |> Seq.fold_left (fun acc g ->
        let env = fun v -> Generator1.get_coeff g (apron_of_var v) |> Itv.of_coeff in
        if (Generator1.get_typ g = VERTEX && Lin.A.eval env le = Itv.zero)
           || Generator1.get_typ g = RAY && Lin.A.eval env le_no_cst = Itv.zero then
          g::acc
        else
          acc
        )
        []

  (* Misc *)
  (* **** *)

  (* check if a lincons is of the form `v>=c` or `v<=c`. If it is, returns
     `Some (sign, v)`  where sign is `true` if `>=` or `false` if `<=`. If
      the lincons has a different form, returns `None`.
   *)
  let lincons_single_var (vset: VSet.t) (lincons: Lincons1.t) : (bool * var) option =
    let lb = VSet.fold (fun v acc ->
      if Coeff.equal_int (Lincons1.get_coeff lincons (apron_of_var v)) 1 then
      v::acc else acc) vset []
    in
    let ub = VSet.fold (fun v acc ->
      if Coeff.equal_int (Lincons1.get_coeff lincons (apron_of_var v)) (-1) then
      v::acc else acc) vset []
    in
    match lb, ub with
    | [v], [] -> Some (true, v)
    | [], [v] -> Some (false, v)
    | _ -> None

  (* bounds of integer variable *)
  let int_var_bnds (var:var) : (Int.t * Int.t) option =
    let inf, sup = match var.var_typ with
    | A_int (i, s) -> Banal_semantics.int_type_set i s
    | A_BOOL -> Finite Int.zero, Finite (Int.of_int 2)
    | _ -> invalid_arg "bnd on a non-integer variable"
    in
    match inf, sup with
    | Finite var_inf, Finite var_sup -> Some (var_inf, Int.(var_sup-var_inf+one))
    | _ -> None

  (* compute a sequence a quadrants containing the poly *)
  let get_quadrants (vset: VSet.t) (a: t) : var Seq.t * Int.t VMap.t Seq.t =
    if Abstract1.is_bottom man a then Seq.empty, Seq.empty else
    let unbound, bound = VSet.to_seq vset
      |> Seq.filter (fun v -> match v.var_typ with
        | A_int _ | A_BOOL -> true
        | _ -> false)
      |> Seq.map (fun v ->
        match int_var_bnds v with
        | None -> v, Seq.return Int.zero
        | Some (var_inf, var_len) ->

          let (inf,sup) = Abstract1.bound_variable man a (apron_of_var v)
            |> Banal_itv_int.of_apron
          in
          match inf, sup with
          | Finite inf, Finite sup ->
            let inf_id = Int.(fdiv (inf-var_inf) var_len) in
            let sup_id = Int.(fdiv (sup-var_inf) var_len) in
            assert (sup_id >= inf_id);

            if Int.(sup_id - inf_id > of_int 2) then begin
              v, Seq.empty
            end else v, Seq.ints 0
              |> Seq.map (fun i -> Int.(of_int i + inf_id))
              |> Seq.take_while (fun i -> i <= sup_id)
          | _ -> v, Seq.empty
          )
      |> Seq.partition (fun (_, idxs) -> Seq.is_empty idxs)
    in
    let unbound = Seq.map (fun (v,_) -> v) unbound in

    (* compute the quadrant indexs *)
    let rec doit seq vars = match Seq.uncons vars with
    | None -> seq
    | Some ((v,bnd), next) -> let seq = Seq.flat_map (fun idx -> Seq.map (fun id -> VMap.add v id idx) bnd) seq in
      doit seq next
    in
    let idxs = doit (Seq.return VMap.empty) bound in

    unbound, idxs

  (* compute the directions of a poly *)
  let get_directions (a: t) : Linexpr1.t list =
    lincons_seq_of_abs a
      |> Seq.map (Lincons1.get_linexpr1)
      |> Seq.map (fun lc -> Linexpr1.set_cst lc (Coeff.s_of_int 0); lc)
      |> List.of_seq

  (* Refinement utils *)
  (* **************** *)

  let refine_singleton vset post1 post2 =
    let get_singleton post =
      let arr = Abstract1.to_lincons_array man post in
      Seq.ints 0
        |> Seq.take (Lincons1.array_length arr)
        |> Seq.map (Lincons1.array_get arr)
        |> Seq.fold_left (fun acc lc ->
          if Lincons1.get_typ lc <> EQ then acc else
          match lincons_single_var vset lc with
          | Some (_, v) -> VMap.add v (Lincons1.get_cst lc) acc
          | None -> acc
          ) VMap.empty
    in

    let singl1 = get_singleton post1 in
    let singl2 = get_singleton post2 in

    (* ensure that at least one singleton component differ by +-1 *)
    if VMap.exists2o
      (fun _ _ -> false)
      (fun _ _ -> false)
      (fun v c1 c2 ->
        match v.var_typ with
        | A_int _ ->
          let c1 = Banal_itv_rat.of_coeff c1 |> Banal_itv_rat.to_int_exact_opt |> Option.get in
          let c2 = Banal_itv_rat.of_coeff c2 |> Banal_itv_rat.to_int_exact_opt |> Option.get in
          Int.(abs (c2 - c1) = one)
        | _ -> false)
      singl1 singl2
    then
      (* ensure that the arguments have the same rays *)
      let rays1 = ray_list_of_abs post1 in
      let rays2 = ray_list_of_abs post2 in

      if List.for_all (fun r1 -> List.exists (fun r2 -> r1 = r2) rays2) rays1 &&
         List.for_all (fun r2 -> List.exists (fun r1 -> r1 = r2) rays1) rays2
      then Abstract1.join man post1 post2 |> Option.some
      else None
    else None

  let refine_behind_cons vset post1 post2 =
    let env = Abstract1.env post1 in
    let int_v, real_v = Environment.vars env in

    lincons_seq_of_abs post1
      |> Seq.find_map (fun cons ->
        (* consider only constraints relating integral variables with
           integral coefficients *)
        let nz_real = Array.exists (fun v ->
          Lincons1.get_coeff cons v |> Coeff.is_zero |> not
        ) real_v in
        let nint_int = Array.for_all (fun v ->
          Lincons1.get_coeff cons v
            |> Banal_itv_rat.of_coeff
            |> Banal_itv_rat.to_int_exact_opt
            |> Option.is_none
        ) int_v in
        let nint_cst = Lincons1.get_cst cons
          |> Banal_itv_rat.of_coeff
          |> Banal_itv_rat.to_int_exact_opt
          |> Option.is_none
        in
        if nz_real || nint_int || nint_cst then None else

        let () = assert (Lincons1.get_typ cons = SUPEQ) in

        (* find c' (note: c' here has opposite polarity compared to
           the thm) *)
        let lin_inc = Lincons1.get_linexpr1 cons |> Linexpr1.copy in
        Linexpr1.set_cst lin_inc (Interval (Linexpr1.get_cst lin_inc |> Banal_itv_rat.of_coeff |> Banal_itv_rat.add Banal_itv_rat.one |> Banal_itv_rat.to_apron));
        let cons_inc = Lincons1.make lin_inc SUPEQ |> neg_lincons in

        (* instead of refining post1 with plain post2, meet post2 with
           c' first to increase the chances of finding some generators
           of post2 saturating c' *)
        let post2_cons_inc = meet_lincons env post2 cons_inc in
        if Abstract1.is_bottom man post2_cons_inc then None else

        (* find the generators of post1 and post2 saturating respectively
           c and c'. Keep only the common rays *)
        let post1_sat = gen_saturating_cons cons vset post1 in
        let post2_sat = gen_saturating_cons cons_inc vset post2_cons_inc
          |> List.filter (fun g1 ->
              Generator1.get_typ g1 = VERTEX || List.exists (fun g2 -> g1 = g2) post1_sat)
        in
        let post1_sat = List.filter (fun g1 ->
          Generator1.get_typ g1 = VERTEX || List.exists (fun g2 -> g1 = g2) post2_sat) post1_sat
        in

        let () = assert (List.length post1_sat > 0) in
        if List.length post2_sat = 0 then None else

        (* p1_nc *)
        let post1_no_cons = remove_lincons cons post1 |> Option.get in

        (* pm *)
        let middle_poly = add_generators (Abstract1.bottom man env) (post1_sat@post2_sat) in

        (* c' *)
        let top_cons = Lincons1.make lin_inc SUPEQ in

        let rhs = match remove_lincons top_cons middle_poly with
        | None -> Abstract1.meet_array man [|post1_no_cons; middle_poly; post2_cons_inc |]
        | Some middle_poly_no_top -> Abstract1.meet_array man [|post1_no_cons; middle_poly_no_top; post2_cons_inc|]
        in

        (* ensure that all the rays of `post` are also in `rhs` *)
        let ray_post1 = ray_list_of_abs post1 in
        let ray_rhs = ray_list_of_abs rhs in
        if List.exists (fun g1 -> not (List.exists (fun g2 -> g1 = g2) ray_rhs)) ray_post1
        then None else

        let p = Abstract1.join man post1 rhs in
        if Abstract1.is_leq man p post1 then None else Some p
      )

end
