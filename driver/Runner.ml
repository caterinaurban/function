(***************************************************)
(*                                                 *)
(*                        Main                     *)
(*                                                 *)
(*                  Caterina Urban                 *)
(*     École Normale Supérieure, Paris, France     *)
(*                   2012 - 2015                   *)
(*                                                 *)
(***************************************************)

(* parsing *)
open Iterators

let analysis = ref "ctl"

let robust = ref false

let domain = ref "boxes"

let filename = ref ""

let fmt = ref Format.std_formatter

let main = ref "main"

let minimal = ref false

let ordinals = ref false

let property = ref "AF{exit: true}"

let precondition = ref "true"

let time = ref false

let ctl_analysis_type = ref "cfg"

let noinline = ref false

let parseFile filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <-
      {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} ;
    let r = Parser.file Lexer.start lex in
    close_in f ; r
  with
  | Parser.Error ->
      Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"
  | Failure e ->
      if e == "lexing: empty token" then (
        Printf.eprintf "Parse Error (Invalid Token) near %s\n"
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
        failwith "Parse Error" )
      else failwith e

let parsePropertyString str =
  let lex = Lexing.from_string str in
  try PropertyParser.file PropertyLexer.start lex with
  | PropertyParser.Error ->
      Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"
  | Failure e ->
      if e == "lexing: empty token" then (
        Printf.eprintf "Parse Error (Invalid Token) near %s\n"
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
        failwith "Parse Error" )
      else failwith e

let parseProperty filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <-
      {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} ;
    let r = PropertyParser.file PropertyLexer.start lex in
    close_in f ; r
  with
  | PropertyParser.Error ->
      Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"
  | Failure e ->
      if e == "lexing: empty token" then (
        Printf.eprintf "Parse Error (Invalid Token) near %s\n"
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
        failwith "Parse Error" )
      else failwith e

let parseCTLProperty filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <-
      {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} ;
    let res = CTLPropertyParser.prog CTLPropertyLexer.read lex in
    close_in f ;
    CTLProperty.map (fun p -> fst (parsePropertyString p)) res
  with
  | CTLPropertyParser.Error ->
      Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"
  | Failure e ->
      if e == "lexing: empty token" then (
        Printf.eprintf "Parse Error (Invalid Token) near %s\n"
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
        failwith "Parse Error" )
      else failwith e

let parseCTLPropertyString_plain (property : string) =
  let lex = Lexing.from_string property in
  try
    lex.Lexing.lex_curr_p <-
      {lex.Lexing.lex_curr_p with Lexing.pos_fname= "string"} ;
    CTLPropertyParser.prog CTLPropertyLexer.read lex
  with
  | CTLPropertyParser.Error ->
      Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"
  | Failure e ->
      if e == "lexing: empty token" then (
        Printf.eprintf "Parse Error (Invalid Token) near %s\n"
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
        failwith "Parse Error" )
      else failwith e

let parseCTLPropertyString (property : string) =
  CTLProperty.map (fun p -> fst (parsePropertyString p))
  @@ parseCTLPropertyString_plain property

let parse_args () =
  let rec doit args =
    match args with
    (* General arguments -------------------------------------------*)
    | "-domain" :: x :: r ->
        (* abstract domain: boxes|octagons|polyhedra *)
        domain := x ;
        doit r
    | "-timeout" :: x :: r ->
        (* analysis timeout *)
        Iterator.timeout := float_of_string x ;
        doit r
    | "-joinfwd" :: x :: r ->
        (* widening delay in forward analysis *)
        Iterator.joinfwd := int_of_string x ;
        doit r
    | "-joinbwd" :: x :: r ->
        (* widening delay in backward analysis *)
        Iterator.joinbwd := int_of_string x ;
        doit r
    | "-main" :: x :: r ->
        (* analyzer entry point *)
        main := x ;
        doit r
    | "-meetbwd" :: x :: r ->
        (* dual widening delay in backward analysis *)
        Iterator.meetbwd := int_of_string x ;
        doit r
    | "-minimal" :: r ->
        (* analysis result only *)
        minimal := true ;
        Iterator.minimal := true ;
        doit r
    | "-ordinals" :: x :: r ->
        (* ordinal-valued ranking functions *)
        ordinals := true ;
        Ordinals.max := int_of_string x ;
        doit r
    | "-refine" :: r ->
        (* refine in backward analysis *)
        Iterator.refine := true ;
        doit r
    | "-retrybwd" :: x :: r ->
        Iterator.retrybwd := int_of_string x ;
        DecisionTree.retrybwd := int_of_string x ;
        doit r
    | "-evolve" :: x :: r ->
        Iterator.evolve := true ;
        DecisionTree.evolve := true ;
        Iterator.evolvethr := int_of_string x ;
        DecisionTree.evolvethr := int_of_string x ;
        doit r
    | "-tracebwd" :: r ->
        (* backward analysis trace *)
        Iterator.tracebwd := true ;
        DecisionTree.tracebwd := true ;
        CFGInterpreter.trace := true ;
        CFGInterpreter.trace_states := true ;
        doit r
    | "-tracefwd" :: r ->
        (* forward analysis trace *)
        Iterator.tracefwd := true ;
        doit r
    (* Termination arguments -------------------------------*)
    | "-termination" :: r ->
        (* guarantee analysis *)
        analysis := "termination" ;
        doit r
    (* Recurrence / Guarantee arguments -------------------------------*)
    | "-guarantee" :: x :: r ->
        (* guarantee analysis *)
        analysis := "guarantee" ;
        property := x ;
        doit r
    | "-robust_ctl" :: x :: r ->
        analysis := "ctl" ;
        property := x ;
        robust := true ;
        doit r
    | "-robust" :: x :: r ->
        analysis := "guarantee" ;
        property := x ;
        robust := true ;
        doit r
    | "-robust_termination" :: r ->
        analysis := "termination" ;
        robust := true ;
        doit r
    | "-recurrence" :: x :: r ->
        (* recurrence analysis *)
        analysis := "recurrence" ;
        property := x ;
        doit r
    | "-time" :: r ->
        (* track analysis time *)
        time := true ;
        doit r
    | "-timebwd" :: r ->
        (* track backward analysis time *)
        Iterator.timebwd := true ;
        doit r
    | "-timefwd" :: r ->
        (* track forward analysis time *)
        Iterator.timefwd := true ;
        doit r
        (* CTL arguments
           ---------------------------------------------------*)
    | "-ctl" :: x :: r ->
        (* CTL analysis *)
        analysis := "ctl" ;
        property := x ;
        doit r
    | "-ast" :: r ->
        (* use AST instead of CFG for analysis *)
        ctl_analysis_type := "ast" ;
        doit r
    | "-dot" :: r ->
        (* print CFG and decision trees in 'dot' format *)
        Iterator.dot := true ;
        doit r
    | "-precondition" :: c :: r ->
        (* optional precondition that holds at the start of the program,
           default = true *)
        precondition := c ;
        doit r
    | "-ctl_existential_equivalence" :: r ->
        (* use CTL equivalence relations to convert existential to universal
           CTL properties *)
        Iterator.ctl_existential_equivalence := true ;
        doit r
    | "-noinline" :: r ->
        (* don't inline function calls, only for CFG based analysis *)
        noinline := true ;
        doit r
    | "-cda" :: x :: r ->
        Iterator.cda := true ;
        Iterator.size := int_of_string x ;
        Iterator.refine := true ;
        doit r
    | x :: r ->
        filename := x ;
        doit r
    | [] -> ()
  in
  let _ = List.map print_endline (Array.to_list Sys.argv) in
  doit (List.tl (Array.to_list Sys.argv))

(* do all *)

let result = ref false

let run_analysis analysis_function program () =
  try
    let start = Sys.time () in
    let result = analysis_function program !main in
    let ret = result in
    let stop = Sys.time () in
    if not !minimal then Format.fprintf !fmt "Analysis Result: " ;
    let result = if result then "TRUE" else "UNKNOWN" in
    if not !minimal then Format.fprintf !fmt "%s\n" result ;
    if !time then Format.fprintf !fmt "Time: %f s\n" (stop -. start) ;
    if not !minimal then Format.fprintf !fmt "\nDone.\n" ;
    ret
  with Iterator.Timeout ->
    Format.fprintf !fmt "\nThe Analysis Timed Out!\n" ;
    Format.fprintf !fmt "\nDone.\n" ;
    false

let termination () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified") ;
  let sources = parseFile !filename in
  let program, _ = ItoA.prog_itoa sources in
  if not !minimal then (
    Format.fprintf !fmt "\nAbstract Syntax:\n" ;
    AbstractSyntax.prog_print !fmt program ) ;
  let analysis_function =
    match !domain with
    | "boxes" ->
        if !ordinals then TerminationBoxesOrdinals.analyze
        else TerminationBoxes.analyze
    | "octagons" ->
        if !ordinals then TerminationOctagonsOrdinals.analyze
        else TerminationOctagons.analyze
    | "polyhedra" ->
        if !ordinals then TerminationPolyhedraOrdinals.analyze
        else TerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  run_analysis (fun a b -> analysis_function a b !robust) program ()

let guarantee () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified") ;
  if !property = "" then raise (Invalid_argument "No Property File Specified") ;
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let program, property =
    ItoA.prog_itoa ~property:(!main, property) sources
  in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then (
    Format.fprintf !fmt "\nAbstract Syntax:\n" ;
    AbstractSyntax.prog_print !fmt program ;
    Format.fprintf !fmt "\nProperty: " ;
    AbstractSyntax.property_print !fmt property ) ;
  let analysis_function =
    match !domain with
    | "boxes" ->
        if !ordinals then GuaranteeBoxesOrdinals.analyze
        else GuaranteeBoxes.analyze
    | "octagons" ->
        if !ordinals then GuaranteeOctagonsOrdinals.analyze
        else GuaranteeOctagons.analyze
    | "polyhedra" ->
        if !ordinals then GuaranteePolyhedraOrdinals.analyze
        else GuaranteePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  run_analysis (analysis_function !robust property) program ()

let recurrence () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified") ;
  if !property = "" then raise (Invalid_argument "No Property File Specified") ;
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let program, property =
    ItoA.prog_itoa ~property:(!main, property) sources
  in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then (
    Format.fprintf !fmt "\nAbstract Syntax:\n" ;
    AbstractSyntax.prog_print !fmt program ;
    Format.fprintf !fmt "\nProperty: " ;
    AbstractSyntax.property_print !fmt property ) ;
  let analysis_function =
    match !domain with
    | "boxes" ->
        if !ordinals then RecurrenceBoxesOrdinals.analyze
        else RecurrenceBoxes.analyze
    | "octagons" ->
        if !ordinals then RecurrenceOctagonsOrdinals.analyze
        else RecurrenceOctagons.analyze
    | "polyhedra" ->
        if !ordinals then RecurrencePolyhedraOrdinals.analyze
        else RecurrencePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  run_analysis (analysis_function property) program ()

let ctl () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified") ;
  if !property = "" then raise (Invalid_argument "No Property Specified") ;
  let starttime = Sys.time () in
  let cfg, getProperty =
    ASTtoCFG.prog (FileParser.parse_file !filename) !main
  in
  let mainFunc = ControlFlowGraph.find_func !main cfg in
  let cfg = ControlFlowGraph.insert_entry_exit_label cfg mainFunc in
  (* add exit/entry labels to main function *)
  let cfg =
    if !noinline then cfg else ControlFlowGraph.inline_function_calls cfg
  in
  (* inline all non recursive functions unless -noinline is used *)
  let cfg = ControlFlowGraph.add_function_call_arcs cfg in
  (* insert function call edges for remaining function calls *)
  let ctlProperty =
    CTLProperty.map FileParser.parse_bool_expression
    @@ parseCTLPropertyString_plain !property
  in
  let ctlProperty = CTLProperty.map getProperty ctlProperty in
  let precondition =
    getProperty @@ FileParser.parse_bool_expression !precondition
  in
  let analyze =
    match !domain with
    | "boxes" ->
        if !ordinals then CTLBoxesOrdinals.analyze else CTLBoxes.analyze
    | "octagons" ->
        if !ordinals then CTLOctagonsOrdinals.analyze
        else CTLOctagons.analyze
    | "polyhedra" ->
        if !ordinals then CTLPolyhedraOrdinals.analyze
        else CTLPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  if not !minimal then (
    Printf.printf "\nCFG:\n" ;
    Printf.printf "%a" CFGPrinter.print_cfg cfg ;
    Printf.printf "\n" ) ;
  if (not !minimal) && !Iterator.dot then (
    Printf.printf "CFG_DOT:\n %a" CFGPrinter.output_dot cfg ;
    Printf.printf "\n" ) ;
  let mainFunc = ControlFlowGraph.find_func !main cfg in
  let possibleLoopHeads = Loop_detection.possible_loop_heads cfg mainFunc in
  let domSets = Loop_detection.dominator cfg mainFunc in
  let result =
    analyze ~precondition cfg !robust mainFunc possibleLoopHeads domSets
      ctlProperty
  in
  ( if !time then
    let stoptime = Sys.time () in
    Format.fprintf !fmt "\nTime: %f" (stoptime -. starttime) ) ;
  let _ =
    if not !minimal then
      if result then Format.fprintf !fmt "\nAnalysis Result: TRUE\n"
      else Format.fprintf !fmt "\nAnalysis Result: UNKNOWN\n"
  in
  result

(* DEPRECATED STUFF BELOW *)

let ctl_ast () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified") ;
  if !property = "" then raise (Invalid_argument "No Property Specified") ;
  let starttime = Sys.time () in
  let parsedPrecondition = parsePropertyString !precondition in
  let parsedProperty = parseCTLPropertyString !property in
  let prog, property =
    ItoA.ctl_prog_itoa parsedProperty !main (parseFile !filename)
  in
  let precondition =
    fst
    @@ AbstractSyntax.StringMap.find ""
    @@ ItoA.property_itoa_of_prog prog !main parsedPrecondition
  in
  if not !minimal then (
    Format.fprintf !fmt "\nAbstract Syntax:\n" ;
    AbstractSyntax.prog_print !fmt prog ;
    Format.fprintf !fmt "\n" ) ;
  let program = ASTCTLIterator.program_of_prog prog !main in
  let analyze =
    match !domain with
    | "boxes" ->
        if !ordinals then ASTCTLBoxesOrdinals.analyze
        else ASTCTLBoxes.analyze
    | "octagons" ->
        if !ordinals then ASTCTLOctagonsOrdinals.analyze
        else ASTCTLOctagons.analyze
    | "polyhedra" ->
        if !ordinals then ASTCTLPolyhedraOrdinals.analyze
        else ASTCTLPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  let result = analyze ~precondition program property in
  ( if !time then
    let stoptime = Sys.time () in
    Format.fprintf !fmt "\nTime: %f" (stoptime -. starttime) ) ;
  if result then Format.fprintf !fmt "\nAnalysis Result: TRUE\n"
  else Format.fprintf !fmt "\nAnalysis Result: UNKNOWN\n"
