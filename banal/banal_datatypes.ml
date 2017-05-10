(* 
   Useful datatypes and functor instantiations.

   Copyright (C) 2011 Antoine Miné
*)


(* unique identifiers *)
(* ****************** *)

type id = Z.t

let cur_id = ref Z.zero

let new_id () =
  cur_id := Z.succ !cur_id; !cur_id

let string_of_id prefix id =
  prefix^(Z.to_string id)

let compare_id (x:id) (y:id) = compare x y

let dummy_id = Z.minus_one


(* maps and sets *)
(* ************* *)


module StringSet = Set.Make(struct type t=string let compare=compare end)
module StringMap = Mapext.Make(struct type t=string let compare=compare end)

module IdSet = Set.Make(struct type t=id let compare=compare_id end)
module IdMap = Mapext.Make(struct type t=id let compare=compare_id end)


(* 3-valued logic *)
(* ************** *)

type tbool = True | False | Maybe

let tnot = function
  | True -> False 
  | False -> True
  | Maybe -> Maybe

let tor a b = 
  match a,b with
  | True,_ | _,True -> True
  | Maybe,_ | _,Maybe -> Maybe
  | False,False -> False

let tand a b = 
  match a,b with
  | False,_ | _,False -> False
  | Maybe,_ | _,Maybe -> Maybe
  | True,True -> True

let tbool_of_bool = function
  | true -> True
  | false -> False

let string_of_tbool = function
  | True -> "true"
  | False -> "false"
  | Maybe -> "maybe"


(* infinities *)
(* ********** *)

type 'a inf =
  | Finite of 'a
  | INF  (* +oo *)
  | MINF (* -oo *)

(* see Intinf for operators on Int.t inf *)


(* bot *)
(* *** *)

type 'a bot = Bot | Nb of 'a

let strict_bot f x = 
  match x with Bot -> Bot | Nb x -> f x

let lift_bot f x = 
  match x with Bot -> Bot | Nb x -> Nb (f x)
      
let merge_bot2 x y = 
  match x,y with Bot,_ | _,Bot -> Bot | Nb a, Nb b -> Nb (a,b)

let join_bot2 f x y = 
  match x,y with Bot,a | a,Bot -> a | Nb a,Nb b -> Nb (f a b)


exception Bot_found

let debot = 
  function Nb x -> x | Bot -> raise Bot_found

let rebot f x = 
  try f x with Bot_found -> Bot

let bot_to_string f = function Bot -> "_|_" | Nb x -> f x
