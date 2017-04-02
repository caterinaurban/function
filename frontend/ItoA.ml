(***************************************************)
(*                                                 *)
(*      Intermediate Syntax to Abstract Syntax     *)
(*                                                 *)
(*                  Caterina Urban                 *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*                                                 *)
(***************************************************)

open IntermediateSyntax
open AbstractSyntax

(* labeling *)
let id = ref 0
let dummyId = -1
let zeroId () = id := 0
let newId () = id := !id + 1; !id

(* exceptions *)
exception ItoA_error of string
exception Modulo

(* environments *)
type env = {
  globals: var StringMap.t; (* global variables *)
  locals: var StringMap.t; (* local variables *)
  funcs: func StringMap.t; (* functions *)
  return: var option;
  constants: aExp VarMap.t; (* constants *)
}

let emptyEnv = {
  globals = StringMap.empty;
  locals = StringMap.empty;
  funcs  = StringMap.empty;
  return = None;
  constants = VarMap.empty;
}

(* contexts *)
type ctx = {
  ctxName: string;
  ctxTyp: var option;
  ctxArgs: var list;
}

(* identifiers resolution *)
let getVar env (v: string) (x: extent): var =
  let unknown (v: string) (x: extent) = 
    Printf.sprintf "%s: Unknown Identifier %s" (position_tostring (fst x)) v
  in
  try StringMap.find v env.locals
  with Not_found ->
  try StringMap.find v env.globals
  with Not_found -> 
    raise (ItoA_error (unknown v x))

let getFunc env (v: string) (x: extent): func =
  let unknown (v: string) (x: extent) =
    Printf.sprintf "%s: Unknown Function %s" (position_tostring (fst x)) v
  in
  try StringMap.find v env.funcs
  with Not_found -> raise (ItoA_error (unknown v x))

(* types *)
let typ_itoa ((t: IntermediateSyntax.typ), (_: extent)): AbstractSyntax.typ =
  match t with
  | I_INT -> A_INT

(* expressions *)
let rec exp_itoa 
    (ctx: ctx) (* calling context *) 
    (env: env) (* environment *)
    (pre: statements) (* statements that should go before the expression *) 
    (post: statements) (* statements that should go after the expression *) 
    ((exp: IntermediateSyntax.exp),(a: extent)) (* annotated expressions *)
  : (AbstractSyntax.exp * env * statements * statements) = try
    match exp with
    | I_TRUE -> (A_boolean (A_TRUE, a), env, pre, post)
    | I_RANDOM -> (A_arithmetic (A_RANDOM, a), env, pre, post)
    | I_FALSE -> (A_boolean (A_FALSE, a), env, pre, post)
    | I_id x -> (A_arithmetic (A_var (getVar env x a), a), env, pre, post)
    | I_const i -> (A_arithmetic (A_const (int_of_string i), a),env,pre,post)
    | I_interval ((o1: unaryOp option), i1, (o2: unaryOp option), i2) ->
      let aux (o: unaryOp option) (i: string) = match o with
        | Some I_PLUS -> int_of_string i
        | Some I_MINUS -> - (int_of_string i)
        | None -> int_of_string i
      in (A_arithmetic (A_interval (aux o1 i1, aux o2 i2), a), env, pre, post)
    | I_incr (e, ea) | I_decr (e, ea) -> (* post-increment/decrement *)
      let aux (e: IntermediateSyntax.exp) = match e with
        | I_incr _ -> A_PLUS
        | I_decr _ -> A_MINUS
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in (match e with
          | I_id x -> 
            let v = getVar env x ea in
            let lhs = (A_var v, ea) in
            let rhs = (A_abinary (aux exp, lhs, (A_const 1, ea)), ea) in
            (A_arithmetic (A_var v,a), env, pre, post@[A_assign (lhs,rhs),a])
          | _ -> raise (Invalid_argument "exp_itoa:I_incr/I_decr"))


    | I_call ((e, ea), ee) ->
      (match e with
       | I_id x when x = ctx.ctxName ->
         let (stmt,env) = call_itoa ctx env (exp,a) in
         (match ctx.ctxTyp with
          | None -> raise (Invalid_argument "exp_itoa:I_call (recursion)")
          | Some v -> (A_arithmetic (A_var v,a),env,pre@stmt,post))
       | I_id x ->
         (try
            let f = getFunc env x ea in 
            let (stmt,env) = call_itoa ctx env (exp,a) in
            (match f.funcTyp with
             | None -> raise (Invalid_argument "exp_itoa:I_call")
             | Some v -> (A_arithmetic (A_var v,a),env,pre@stmt,post))
          with ItoA_error _ -> (* non-determinisms approximates calls to unknown functions *) (A_arithmetic (A_RANDOM,a),env,pre,post))
       | _ -> raise (Invalid_argument "exp_itoa:I_call"))


    | I_preincr (e, ea) | I_predecr (e, ea) -> (* pre-increment/decrement *)
      let aux (e: IntermediateSyntax.exp) = match e with
        | I_preincr _ -> A_PLUS
        | I_predecr _ -> A_MINUS
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in (match e with
          | I_id x -> 
            let v = getVar env x ea in
            let lhs = (A_var v, ea) in
            let rhs = (A_abinary (aux exp, lhs, (A_const 1, ea)), ea) in
            (A_arithmetic (A_var v,a), env, pre@[A_assign (lhs,rhs),a], post)
          | _ -> raise (Invalid_argument "exp_itoa:I_preincr/I_predecr"))
    | I_not e ->
      let (e,env,pre,post) = exp_itoa ctx env pre post e in
      (match e with
       | A_boolean e -> (A_boolean (A_bunary (A_NOT,e), a), env, pre, post)
       | A_arithmetic (e, ea) ->
         let zero = (A_const 0, ea) in
         let lhs = (A_rbinary (A_LESS_EQUAL, (e, ea), zero), ea) in
         let rhs = (A_rbinary (A_GREATER_EQUAL, (e, ea), zero), ea) in
         (A_boolean (A_bbinary (A_AND, lhs, rhs), a), env, pre, post)
       | _ -> raise (Invalid_argument "exp_itoa:I_not"))		
    | I_unary (o, e) ->
      let (e, env, pre, post) = exp_itoa ctx env pre post e in
      (match e with
       | A_arithmetic (e,ea) -> (match o with
           | I_PLUS -> (A_arithmetic (e,ea), env, pre, post)
           | I_MINUS -> (match e with
               | A_RANDOM -> (A_arithmetic (A_RANDOM,a), env, pre, post)
               | A_const i -> (A_arithmetic (A_const (-i), a), env, pre, post)
               | _ -> 
                 let one = (A_const (-1), ea) in
                 let mul = (A_abinary (A_MULTIPLY, one, (e, ea)), a) in
                 (A_arithmetic mul, env, pre, post)))
       | _ -> raise (Invalid_argument "exp_itoa:I_unary"))				
    | I_mul (e1, e2) | I_div (e1, e2) | I_add (e1, e2) | I_minus (e1, e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      let aux (e: IntermediateSyntax.exp) = match e with
        | I_mul _ -> A_MULTIPLY
        | I_div _ -> A_DIVIDE
        | I_add _ -> A_PLUS
        | I_minus _ -> A_MINUS
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in
      let eval e c1 c2 = match e with
        | I_mul _ -> A_const (c1 * c2)
        | I_div _ -> A_const (c1 / c2)
        | I_add _ -> A_const (c1 + c2)
        | I_minus _ -> A_const (c1 - c2)
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in
      (match e1, e2 with
       | A_arithmetic (e1, ea1), A_arithmetic (e2, ea2) -> (match e1, e2 with
           | A_RANDOM,_ | _,A_RANDOM -> 
             (A_arithmetic (A_RANDOM, a), env, pre, post)
           | A_const i1,A_const i2 -> 
             (A_arithmetic (eval exp i1 i2, a), env, pre, post)
           | _ -> 
             let e = (A_abinary (aux exp, (e1,ea1), (e2,ea2)), a) in
             (A_arithmetic e,env,pre,post))
       | _ -> raise (Invalid_argument "exp_itoa:I_mul/I_div/I_add/I_minus"))
    | I_mod _ -> raise Modulo (* modulo is approximated with non-determinism *)
    | I_less (e1, e2) | I_leq (e1, e2) | I_greater (e1, e2) | I_geq (e1, e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      let aux (e: IntermediateSyntax.exp) = match e with
        | I_less _ -> A_LESS
        | I_leq _ -> A_LESS_EQUAL
        | I_greater _ -> A_GREATER
        | I_geq _ -> A_GREATER_EQUAL
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in
      let eval e c1 c2 = match e with
        | I_less _ -> if c1 < c2 then A_TRUE else A_FALSE
        | I_leq _ -> if c1 <= c2 then A_TRUE else A_FALSE
        | I_greater _ -> if c1 > c2 then A_TRUE else A_FALSE
        | I_geq _ -> if c1 >= c2 then A_TRUE else A_FALSE
        | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
      in
      (match e1, e2 with
       | A_arithmetic (e1, ea1), A_arithmetic (e2, ea2) -> (match e1, e2 with
           | A_RANDOM,_ | _,A_RANDOM -> 
             (A_boolean (A_MAYBE, a), env, pre, post)
           | A_const i1,A_const i2 -> 
             (A_boolean (eval exp i1 i2, a), env, pre, post)
           | _ -> 
             let e = (A_rbinary (aux exp, (e1,ea1), (e2,ea2)), a) in
             (A_boolean e,env,pre,post))
       | _ -> raise (Invalid_argument "exp_itoa:I_less/I_leq/I_greater/I_geq"))
    | I_eq (e1, e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      (match e1, e2 with
       | A_arithmetic (e1, ea1), A_arithmetic (e2, ea2) -> (match e1, e2 with
           | A_RANDOM,_ | _,A_RANDOM -> 
             (A_boolean (A_MAYBE, a), env, pre, post)
           | A_const i1,A_const i2 -> 
             let eval = if i1 = i2 then A_TRUE else A_FALSE in
             (A_boolean (eval, a), env, pre, post)
           | _ -> 
             let lhs = (A_rbinary (A_LESS_EQUAL,(e1,ea1),(e2,ea2)), a) in
             let rhs = (A_rbinary (A_GREATER_EQUAL,(e1,ea1),(e2,ea2)), a) in
             (A_boolean (A_bbinary (A_AND, lhs, rhs), a), env, pre, post))
       | _ -> raise (Invalid_argument "exp_itoa:I_eq"))
    | I_neq (e1, e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      (match e1,e2 with
       | A_arithmetic (e1,ea1), A_arithmetic (e2,ea2) -> (match e1, e2 with
           | A_RANDOM,_ | _,A_RANDOM -> 
             (A_boolean (A_MAYBE, a), env, pre, post)
           | A_const i1,A_const i2 -> 
             let eval = if i1 <> i2 then A_TRUE else A_FALSE in
             (A_boolean (eval, a), env, pre, post)
           | _ -> 
             let lhs = (A_rbinary (A_LESS,(e1,ea1),(e2,ea2)), a) in
             let rhs = (A_rbinary (A_GREATER,(e1,ea1),(e2,ea2)), a) in
             (A_boolean (A_bbinary (A_OR, lhs, rhs), a), env, pre, post))
       | _ -> raise (Invalid_argument "exp_itoa:I_neq"))	 
    | I_and (e1,e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      (match e1,e2 with
       | A_arithmetic (A_RANDOM,_), A_arithmetic (A_RANDOM,_) -> 
         (A_boolean (A_MAYBE, a), env, pre, post)
       | A_arithmetic (A_RANDOM,_), A_boolean (e2,_) -> 
         (A_boolean (e2, a), env, pre, post)
       | A_boolean (e1,_), A_arithmetic (A_RANDOM,_) -> 
         (A_boolean (e1, a), env, pre, post)
       | A_boolean (e1,ea1), A_boolean (e2,ea2) -> 
         (A_boolean (A_bbinary (A_AND,(e1,ea1),(e2,ea2)), a), env, pre, post)
       | _ -> raise (Invalid_argument "exp_itoa:I_and"))
    | I_or (e1,e2) ->
      let (e1, env, pre, post) = exp_itoa ctx env pre post e1 in
      let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
      (match e1,e2 with
       | A_arithmetic (A_RANDOM,_),_ | _,A_arithmetic (A_RANDOM,_) -> 
         (A_boolean (A_MAYBE, a), env, pre, post)
       | A_boolean (e1,ea1), A_boolean (e2,ea2) -> 
         (A_boolean (A_bbinary (A_OR,(e1,ea1),(e2,ea2)),a), env, pre, post)
       | _ -> raise (Invalid_argument "exp_itoa:I_or"))
    | I_assign ((e1, ea1), o, e2) -> 
      (match e1 with
       | I_id x -> 
         let v = getVar env x ea1 in
         let (e2, env, pre, post) = exp_itoa ctx env pre post e2 in
         let aux (o: IntermediateSyntax.assignOp) = match o with
           | I_PLUS_EQUAL -> A_PLUS
           | I_MINUS_EQUAL -> A_MINUS
           | I_MULTIPLY_EQUAL -> A_MULTIPLY
           | I_DIVIDE_EQUAL -> A_DIVIDE
           | _ -> raise (Invalid_argument "exp_itoa:this should not happen")
         in
         (match e2 with
          | A_arithmetic (e2,ea2) -> (match o with
              | I_EQUAL -> 
                let ctn = pre@[A_assign ((A_var v, ea1), (e2, ea2)), a] in
                (A_stmt, env, ctn, post)							   
              | I_PLUS_EQUAL | I_MINUS_EQUAL 
              | I_MULTIPLY_EQUAL | I_DIVIDE_EQUAL ->
                let lhs = (A_var v, ea1) in
                let rhs = (A_abinary(aux o, lhs, (e2, ea2)), ea2) in
                let ctn = pre@[A_assign (lhs, rhs), a] in
                (A_stmt, env, ctn, post)
              | I_MODULO_EQUAL -> 
                let lhs = (A_var v, ea1) in
                let ctn = pre@[A_assign (lhs, (A_RANDOM, ea2)), a] in
                (A_stmt,env,ctn,post))
          | A_boolean (e2,ea2) ->
            let lhs = (A_var v, ea1) in
            let s1 = (A_assign (lhs, (A_const 0, ea2)),a) in
            let b1 = A_block (dummyId, s1, A_empty dummyId) in
            let s2 = (A_assign (lhs, (A_const 1, ea2)),a) in
            let b2 = A_block (dummyId, s2, A_empty dummyId) in
            (A_stmt, env, pre@[A_if ((e2, ea2), b1, b2), a], post)
          | _ -> 
            raise (Invalid_argument "exp_itoa:I_assign"))				 
       | _ -> raise (Invalid_argument "exp_itoa:I_assign"))

  with Modulo -> (A_arithmetic (A_RANDOM,a), env, pre, post)

and call_itoa ctx (* string *) env (exp,a) =
  match exp with
  | I_call ((exp,ea),exps) ->
    (match exp with
     | I_id x when x = ctx.ctxName ->
       let (stmt,env) = List.fold_left2 (fun (astmts,aenv) avar (aexp,aea) ->
           let (aexp,aenv,apre,apost) = exp_itoa ctx aenv astmts [] (aexp,aea) in
           (match aexp with
            | A_arithmetic (aexp,aea) ->
              (apre@[A_assign ((A_var avar,aea),(aexp,aea)),a]@apost,aenv)
            | _ -> raise (Invalid_argument "call_itoa: I_call (recursion)"))
         ) ([],env) ctx.ctxArgs exps in
       ([A_recall (ctx.ctxName,stmt),a],env)
     | I_id x ->
       let f = getFunc env x ea in
       let (stmt,env) = List.fold_left2 (fun (astmts,aenv) avar (aexp,aea) ->
           let (aexp,aenv,apre,apost) = exp_itoa ctx aenv astmts [] (aexp,aea) in
           (match aexp with
            | A_arithmetic (aexp,aea) ->
              (apre@[A_assign ((A_var avar,aea),(aexp,aea)),a]@apost,aenv)
            | _ -> raise (Invalid_argument "call_itoa: I_call"))
         ) ([],env) f.funcArgs exps in
       let locals = StringMap.fold (fun _ var vm -> StringMap.add var.varId var vm) f.funcVars env.locals in
       let env = { env with locals = locals; } in
       let unblock,env = unblock_itoa f.funcBody env in
       let unblock = List.rev unblock in
       (match unblock with
        | [] -> (stmt,env)
        | (A_return,_)::xs -> (stmt@List.rev xs,env)
        | _ -> (stmt@List.rev unblock,env))
     (* ([A_call (f.funcName,stmt),a],env) *)
     | _ -> raise (Invalid_argument "call_itoa: I_call"))
  | _ -> raise (Invalid_argument "call_itoa")

and unblock_itoa block (* Asyntax.block *) env =
  match block with
  | A_empty _ -> ([],env)
  | A_block (_,stmt (* Asyntax.stmt annotated *),block (* Asyntax.block *)) ->
    let (block,env) = unblock_itoa block env in
    (stmt::block,env)

let property_itoa ctx env (property,a) =
  match property with
  | I_universal exp ->
    let (exp,_,_,_) = exp_itoa ctx env [] [] exp in
    (match exp with
     | A_boolean exp -> StringMap.add "" exp StringMap.empty
     | _ -> raise (Invalid_argument "property_itoa:I_universal"))
  | I_particular (lbl,exp) ->
    let (exp,_,_,_) = exp_itoa ctx env [] [] exp in
    (match exp with
     | A_boolean (exp,a) -> StringMap.add lbl (exp,a) (StringMap.add "" (A_FALSE,a) StringMap.empty)
     | _ -> raise (Invalid_argument "property_itoa:I_particular"))

(* variable declarations *)
let declarator_itoa ctx (* ctx *) gs (* var StringMap.t *) ls (* var StringMap.t *) scope fs (* func StringMap.t *) cs (* int VarMap.t *) typ (* Isyntax.typ annotated *) ((x (* string *),xa),exp (* Isyntax.exp annotated option *)) =
  let v = { varId = "$" ^ string_of_int (newId()); varName = x; varTyp = typ_itoa typ } in
  let vm = if scope then StringMap.add x v ls (* local scope: e.locals is updated *) else StringMap.add x v gs (* global scope: e.globals is updated *) in
  let env =
    if scope then { globals = gs; locals = vm; funcs = fs; return = None; constants = cs } (* local scope: e.locals is updated *)
    else { globals = vm; locals = ls; funcs = fs; return = None; constants = cs } (* global scope: e.globals is updated *)
  in
  let (stmts,env) =
    (match exp with
     | Some (e,ea) ->
       let (e (* exp *),env,pre (* Asyntax.stmt list *),post (* Asyntax.stmt list *)) = exp_itoa ctx env [] (* Asyntax.stmt list *) [] (* Asyntax.stmt list *) (e,ea) in
       (match e with
        | A_arithmetic e (* aExp annotated *) ->
          (pre@[A_assign ((A_var v,xa),e),(fst xa,snd ea)]@post,env)					  
        | _ -> raise (Invalid_argument "declarator_itoa"))
     | None -> ([],env))
  in
  if scope then (env.locals (* TODO: why not vm? *),env.constants,stmts) else (env.globals (* TODO: was vm *),env.constants,stmts)

let globalDecl_itoa ctx (* ctx *) env (* env *) scope stmts (* Asyntax.stmt list *) ((typ (* Isyntax.typ annotated *),decls (* Isyntax.declarator list *)),_) =
  let vmss = if scope then (env.locals,env.constants,stmts) else (env.globals,env.constants,stmts) in
  List.fold_left (fun (avm (* var StringMap.t *),acs (* aExp VarMap.t *),astmts (* Asyntax.stmt list *)) decl (* Isyntax.declarator *) ->
      let (vm,cs,stmts) =
        if scope then declarator_itoa ctx env.globals avm true env.funcs acs typ decl (* local scope: e.locals is updated *)
        else declarator_itoa ctx avm env.locals false env.funcs acs typ decl (* global scope: e.globals is updated *)
      in (vm,cs,List.append astmts stmts)
    ) vmss decls

let arg_itoa (typ (* Isyntax.typ annotated *),(x (* string *),xa)) = { varId = "$" ^ string_of_int (newId()); varName = x; varTyp = typ_itoa typ }

(* statements *)
let rec stmt_itoa
    (ctx: ctx) (* calling context *) 
    (env: env) (* environment *)
    ((stmt: IntermediateSyntax.stmt),(a: extent)) (* annotated statement *)
  : (statements * env) =
  match stmt with
  | I_label lbl -> ([A_label lbl, a], env)
  | I_SKIP -> ([], env)
  | I_exp (exp,a) ->
    (match exp with
     | I_call ((e,_),_) ->
       (match e with
        | I_id _ -> call_itoa ctx env (exp,a)
        | _ -> raise (Invalid_argument "stmt_itoa: I_exp"))
     | _ ->
       let (exp, env, pre, post) = exp_itoa ctx env [] [] (exp,a) in
       (match exp with
        | A_arithmetic (A_var _, _) | A_stmt -> (pre@post, env)
        | _ -> raise (Invalid_argument "stmt_itoa: I_exp")))
  | I_assert exp ->
    let (exp, env, pre, post) = exp_itoa ctx env [] [] exp in
    (match exp with
     | A_boolean exp -> (pre@[A_assert exp,a]@post, env)
     | A_arithmetic (A_RANDOM,ea) -> (pre@post, env)
     | A_arithmetic (e,ea) -> 
       let lhs = (A_rbinary (A_LESS, (e, ea), (A_const 0, ea)), ea) in
       let rhs = (A_rbinary (A_GREATER, (e, ea), (A_const 0, ea)), ea) in
       (pre@[A_assert (A_bbinary (A_OR, lhs ,rhs), ea), a]@post, env)
     | _ -> raise (Invalid_argument "stmt_itoa: I_assert"))
  | I_if (exp,stmt1,stmt2) -> (* TODO: fix scope *)
    let (exp, lenv, pre, post) = exp_itoa ctx env [] [] exp in
    let (stmt1, lenv1) = stmt_itoa ctx lenv stmt1 in
    let thn = block_itoa (post@stmt1) in
    (match stmt2 with
     | Some stmt2 ->
       let (stmt2, lenv2) = stmt_itoa ctx lenv1 stmt2 in
       let els = block_itoa (post@stmt2) in
       (match exp with
        | A_boolean exp -> (pre@[A_if (exp, thn, els), a], lenv2)
        | A_arithmetic (A_RANDOM, ea) ->
          (pre@[A_if ((A_MAYBE, ea), thn, els), a], lenv2)
        | A_arithmetic (e, ea) ->
          let lhs = (A_rbinary (A_LESS, (e,ea), (A_const 0,ea)), ea) in
          let rhs = (A_rbinary (A_GREATER, (e,ea), (A_const 0,ea)), ea) in
          (pre@[A_if ((A_bbinary (A_OR,lhs,rhs),ea), thn, els), a], lenv2)
        | _ -> raise (Invalid_argument "stmt_itoa: I_if"))
     | None ->
       let els = block_itoa (post@[]) in
       (match exp with
        | A_boolean exp ->
          (pre@[A_if (exp, thn, els), a], lenv1)
        | A_arithmetic (A_RANDOM, ea) ->
          (pre@[A_if ((A_MAYBE, ea), thn, els), a], lenv1)
        | A_arithmetic (e, ea) ->
          let lhs = (A_rbinary (A_LESS, (e,ea), (A_const 0,ea)), ea) in
          let rhs = (A_rbinary (A_GREATER, (e,ea), (A_const 0,ea)), ea) in
          (pre@[A_if ((A_bbinary (A_OR,lhs,rhs),ea), thn, els), a], lenv1)
        | _ -> raise (Invalid_argument "stmt_itoa: I_if")))
  | I_while (exp,stmt) -> (* TODO: fix scope *)
    let (exp, lenv, pre, post) = exp_itoa ctx env [] [] exp in
    let (stmt, lenv) = stmt_itoa ctx lenv stmt in
    let body = block_itoa (post@stmt@pre) in
    (match exp with
     | A_boolean exp -> (pre@[A_while (dummyId, exp, body), a]@post, lenv)
     | A_arithmetic (A_RANDOM,ea) -> 
       (pre@[A_while (dummyId, (A_MAYBE,ea), body), a]@post, lenv)
     | A_arithmetic (e,ea) -> 
       let lhs = (A_rbinary (A_LESS, (e,ea), (A_const 0,ea)), ea) in
       let rhs = (A_rbinary (A_GREATER, (e,ea), (A_const 0,ea)), ea) in
       let tst = (A_bbinary (A_OR, lhs, rhs), ea) in
       (pre@[A_while (dummyId, tst, body), a]@post, lenv)		 
     | _ -> raise (Invalid_argument "stmt_itoa: I_while"))



  | I_for_simple (e1,e2,e3,s) -> (* TODO: fix scope *)
    let (e1,lenv,pre1,post1) = exp_itoa ctx env [] [] e1 in
    let (e2,lenv,pre2,post2) = exp_itoa ctx lenv [] [] e2 in
    let (e3,lenv,pre3,post3) = exp_itoa ctx lenv [] [] e3 in
    let (s,lenv) = stmt_itoa ctx lenv s in
    (match e1,e2,e3 with
     | A_stmt, A_boolean e2, A_stmt -> (pre1@post1@pre2@[A_while (dummyId,e2,block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_stmt, A_boolean e2, A_arithmetic (A_var _,_) -> (pre1@post1@pre2@[A_while (dummyId,e2,block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_arithmetic (A_var _,_), A_boolean e2, A_stmt -> (pre1@post1@pre2@[A_while (dummyId,e2,block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_arithmetic (A_var _,_), A_boolean e2, A_arithmetic (A_var _,_) -> (pre1@post1@pre2@[A_while (dummyId,e2,block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)		 
     | A_stmt, A_arithmetic (A_RANDOM,ea2), A_stmt -> (pre1@post1@pre2@[A_while (dummyId,(A_MAYBE,ea2),block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_stmt, A_arithmetic (A_RANDOM,ea2), A_arithmetic (A_var _,_) -> (pre1@post1@pre2@[A_while (dummyId,(A_MAYBE,ea2),block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_arithmetic (A_var _,_), A_arithmetic (A_RANDOM,ea2), A_stmt -> (pre1@post1@pre2@[A_while (dummyId,(A_MAYBE,ea2),block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | A_arithmetic (A_var _,_), A_arithmetic (A_RANDOM,ea2), A_arithmetic (A_var _,_) -> (pre1@post1@pre2@[A_while (dummyId,(A_MAYBE,ea2),block_itoa (post2@s@pre2@pre3@post3)),a]@post2,lenv)
     | _ -> raise (Invalid_argument "stmt_itoa: I_for_simple"))
  | I_for (decl,exp1,exp2,stmt) -> (* TODO: fix scope *)
    let (locals (* var StringMap.t *),constants,stmts) = globalDecl_itoa ctx env true (* local scope *) [] decl in
    let lenv = { env with locals = locals; constants = constants; } in
    let (e1,lenv,pre1,post1) = exp_itoa ctx lenv [] [] exp1 in
    let (e2,lenv,pre2,post2) = exp_itoa ctx lenv [] [] exp2 in
    let (stmt,lenv) = stmt_itoa ctx lenv stmt in
    (match e1,e2 with
     | A_boolean e1, A_stmt -> (stmts@pre1@[A_while (dummyId,e1,block_itoa (post1@stmt@pre1@pre2@post2)),a]@post1,lenv)
     | A_boolean e1, A_arithmetic (A_var _,_) -> (stmts@pre1@[A_while (dummyId,e1,block_itoa (post1@stmt@pre1@pre2@post2)),a]@post1,lenv)
     | A_arithmetic (A_RANDOM,ea1), A_stmt -> (stmts@pre1@[A_while (dummyId,(A_MAYBE,ea1),block_itoa (post1@stmt@pre1@pre2@post2)),a]@post1,lenv)
     | A_arithmetic (A_RANDOM,ea1), A_arithmetic (A_var _,_) -> (stmts@pre1@[A_while (dummyId,(A_MAYBE,ea1),block_itoa (post1@stmt@pre1@pre2@post2)),a]@post1,lenv)
     | _ -> raise (Invalid_argument "stmt_itoa: I_for"))

  | I_return None -> ([A_return, a], env)
  | I_return (Some exp) ->
    let (exp, env, pre, post) = exp_itoa ctx env [] [] exp in
    (match exp, env.return with
     | A_arithmetic (exp,ea), Some var -> 
       (pre@[A_assign ((A_var var,ea), (exp,ea)), a ; A_return, a]@post, env)
     | _ -> raise (Invalid_argument "stmt_itoa: I_return"))

  | I_local decl (* Isyntax.globalDecl annotated *) -> 
    let (locals (* var StringMap.t *),constants,stmts) = globalDecl_itoa ctx env true (* local scope *) [] decl in
    let env = { env with locals = locals; constants = constants; } in
    (stmts,env)
  | I_block stmts -> List.fold_left (fun (astmts,aenv) astmt -> 
      let (aastmts,aaenv) = stmt_itoa ctx aenv astmt in
      (astmts@aastmts,aaenv)) ([],env) stmts

and block_itoa stmts (* (Asyntax.stmt annotated) list *) = 
  match stmts with
  | [] -> A_empty (dummyId)
  | stmt (* Asyntax.stmt annotated *)::stmts -> 
    A_block (dummyId,stmt,block_itoa stmts)

let rec label_stmt s = (* statements labeling *)
  match s with
  | A_if (b,s1,s2) -> let s1 = label_block s1 in A_if (b,s1,label_block s2)
  | A_while (_,b,s) -> let id = newId() in A_while (id,b,label_block s)
  | _ -> s

and label_block b = (* blocks labeling *)
  match b with
  | A_empty _ -> A_empty (newId())
  | A_block (_,(s,a),b) ->
    let id = newId() in
    let s = label_stmt s in
    A_block (id,(s,a),label_block b)

(* function declarations *)
let functionDecl_itoa env (* env *) ((typ (* Isyntax.typ annotated option *),(x (* string *),xa),args (* (Isyntax.typ annotated * string annotated) list *),stmts (* (Isyntax.stmt annotated) list *)),_) =
  try
    let f = StringMap.find x env.funcs in
    let env = { env with locals = f.funcVars; return = f.funcTyp } in
    let ctx = { ctxName = f.funcName; ctxTyp = f.funcTyp; ctxArgs = f.funcArgs } in
    let (stmts,env) = List.fold_left (fun (astmts,aenv) astmt -> let (aastmts,aaenv) = stmt_itoa ctx aenv astmt in (astmts@aastmts,aaenv)) ([],env) stmts in
    let f = { f with funcVars = env.locals; funcBody = block_itoa stmts } in
    StringMap.add x f env.funcs
  with Not_found ->
    let return,locals =
      match typ with
      | None -> None,env.locals
      | Some typ ->
        let i = newId() in
        let v = { varId = "$" ^ string_of_int i; varName = "$" ^ string_of_int i; varTyp = typ_itoa typ } in
        Some v,StringMap.add v.varName v env.locals
    in
    let vars = List.fold_left (fun vars arg -> vars@[arg_itoa arg]) [] args in
    let locals = List.fold_left (fun alocals avar -> StringMap.add avar.varName avar alocals) locals vars in
    let env = { env with locals = locals; return = return } in
    let ctx = { ctxName = x; ctxTyp = return; ctxArgs = vars } in
    let (stmts,env) = List.fold_left (fun (astmts,aenv) astmt -> let (aastmts,aaenv) = stmt_itoa ctx aenv astmt in (astmts@aastmts,aaenv)) ([],env) stmts in
    let f = { funcName = x; funcTyp = return; funcArgs = vars; funcVars = env.locals; funcBody = block_itoa stmts } in
    StringMap.add x f env.funcs

(* declarations *)
let decl_itoa env (* env *) stmts (* Asyntax.stmt list *) decl =
  match decl with
  | I_global decl (* Isyntax.globalDecl annotated *) ->
    let ctx = { ctxName = ""; ctxTyp = None; ctxArgs = [] } in
    let (globals (* var StringMap.t *),constants,stmts) = globalDecl_itoa ctx env false (* global scope *) stmts decl in
    let env = { env with globals = globals; constants = constants; } in
    (env,stmts)
  | I_function decl (* Isyntax.functionDecl annotated *) ->
    let funcs (* fun StringMap.t *) = functionDecl_itoa env decl in
    let env = { env with funcs = funcs; } in
    (env,stmts)

(* programs *)
let prog_itoa ?property (decls (* Isyntax.decl list *),_) =
  let (env,stmts) = List.fold_left (fun (aenv (* env *),astmts (* (Asyntax.stmt annotated) list *)) adecl -> decl_itoa aenv astmts adecl) (emptyEnv,[]) decls in
  zeroId();
  let block = label_block (block_itoa stmts) in
  let aux f = { f with funcBody = label_block f.funcBody; } in
  let program = (env.globals,block,StringMap.map aux env.funcs) in
  match property with
  | None -> (program, None)
  | Some (main,property) ->
    let (globals,_,funcs) = program in
    let f = StringMap.find main funcs in
    let locals = f.funcVars in
    let env = { globals = globals; locals = locals; funcs = StringMap.empty; return = None; constants = VarMap.empty } in
    let ctx = { ctxName = main; ctxTyp = f.funcTyp; ctxArgs = f.funcArgs } in
    let property = property_itoa ctx env property in
    (program,Some property)
