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

let analysis = ref "termination"
let domain = ref "boxes"
let filename = ref ""
let fmt = ref Format.std_formatter
let main = ref "main"
let minimal = ref false
let ordinals = ref false
let property = ref ""
let precondition = ref "true"
let time = ref false

let parseFile filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p
                               with Lexing.pos_fname = filename; };
    let r = Parser.file Lexer.start lex in
    close_in f; r
  with
  | Parser.Error ->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
      (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e

let parsePropertyString str =
  let lex = Lexing.from_string str in
  try
    PropertyParser.file PropertyLexer.start lex 
  with
  | PropertyParser.Error ->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e


let parseProperty filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p
                               with Lexing.pos_fname = filename; };
    let r = PropertyParser.file PropertyLexer.start lex in
    close_in f; r
  with
  | PropertyParser.Error -> Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e


let parseCTLProperty filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p with Lexing.pos_fname = filename; };
    let res = CTLPropertyParser.prog CTLPropertyLexer.read lex in
    close_in f; 
    CTLProperty.map (fun p -> fst (parsePropertyString p)) res 
  with
  | CTLPropertyParser.Error->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" 
      (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" 
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e


let parseCTLPropertyString_plain (property:string) =
  let lex = Lexing.from_string property in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p with Lexing.pos_fname = "string"; };
    CTLPropertyParser.prog CTLPropertyLexer.read lex 
  with
  | CTLPropertyParser.Error->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n" 
      (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" 
          (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e


let parseCTLPropertyString (property:string) =
    CTLProperty.map (fun p -> fst (parsePropertyString p)) @@ parseCTLPropertyString_plain property 

let parse_args () =
  let rec doit args =
    match args with
    | "-domain"::x::r -> (* abstract domain: boxes|octagons|polyhedra *)
      domain := x; doit r
    | "-guarantee"::x::r -> (* guarantee analysis *)
      analysis := "guarantee"; property := x; doit r
    | "-joinfwd"::x::r -> (* widening delay in forward analysis *)
      Iterator.joinfwd := int_of_string x; doit r
    | "-joinbwd"::x::r -> (* widening delay in backward analysis *)
      Iterator.joinbwd := int_of_string x; doit r
    | "-main"::x::r -> (* analyzer entry point *) main := x; doit r
    | "-meetbwd"::x::r -> (* dual widening delay in backward analysis *)
      Iterator.meetbwd := int_of_string x; doit r
    | "-minimal"::r -> (* analysis result only *)
      minimal := true; Iterator.minimal := true; doit r
    | "-ordinals"::x::r -> (* ordinal-valued ranking functions *)
      ordinals := true; Ordinals.max := int_of_string x; doit r
    | "-recurrence"::x::r -> (* recurrence analysis *)
      analysis := "recurrence"; property := x; doit r
    | "-actl"::x::r -> (* TODO: legacy name, delete me *)
      analysis := "actl"; property := x; doit r
    | "-actl_termination"::r -> (*TODO: legacy name, delete me*)
      analysis := "actl_termination"; doit r
    | "-ctl"::x::r -> (* CTL analysis *)
      analysis := "ctl"; property := x; doit r
    | "-ctl_str"::x::r -> (* CTL analysis with property passed as string *)
      analysis := "ctl_str"; property := x; doit r
    | "-ctl_termination"::r -> (* CTL analysis for termination *)
      analysis := "ctl_termination"; doit r
    | "-ctl_cfg"::x::r -> (* CTL analysis for termination *)
      analysis := "ctl_cfg"; property := x; doit r
    | "-precondition"::c::r -> (* optional precondition that holds at the start of the program, default = true *)
      precondition := c; doit r
    | "-ctl_existential_equivalence"::r ->
        Iterator.ctl_existential_equivalence := true; doit r
    | "-refine"::r -> (* refine in backward analysis *)
      Iterator.refine := true; doit r
    | "-retrybwd"::x::r ->
      Iterator.retrybwd := int_of_string x;
      DecisionTree.retrybwd := int_of_string x;
      doit r
    | "-time"::r -> (* track analysis time *)
      time := true; doit r
    | "-timebwd"::r -> (* track backward analysis time *)
      Iterator.timebwd := true; doit r
    | "-timefwd"::r -> (* track forward analysis time *)
      Iterator.timefwd := true; doit r
    | "-timeout"::x::r -> (* analysis timeout *)
      Iterator.timeout := float_of_string x; doit r
    | "-tracebwd"::r -> (* backward analysis trace *)
      Iterator.tracebwd := true;
      DecisionTree.tracebwd := true;
      doit r
    | "-tracefwd"::r -> (* forward analysis trace *)
      Iterator.tracefwd := true; doit r
    | "-traceworklist"::r -> (* backward analysis trace *)
      BackwardInterpreter.trace := true;
      doit r
    | "-traceworkliststates"::r -> (* backward analysis trace *)
      BackwardInterpreter.trace := true;
      BackwardInterpreter.trace_states := true;
      doit r
    | x::r -> filename := x; doit r
    | [] -> ()
  in
  doit (List.tl (Array.to_list Sys.argv))

(* do all *)

module TerminationBoxes =
  TerminationIterator.TerminationIterator(DecisionTree.TSAB)
module TerminationBoxesOrdinals =
  TerminationIterator.TerminationIterator(DecisionTree.TSOB)
module TerminationOctagons =
  TerminationIterator.TerminationIterator(DecisionTree.TSAO)
module TerminationOctagonsOrdinals =
  TerminationIterator.TerminationIterator(DecisionTree.TSOO)
module TerminationPolyhedra =
  TerminationIterator.TerminationIterator(DecisionTree.TSAP)
module TerminationPolyhedraOrdinals =
  TerminationIterator.TerminationIterator(DecisionTree.TSOP)

module GuaranteeBoxes =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSAB)
module GuaranteeBoxesOrdinals =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSOB)
module GuaranteeOctagons =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSAO)
module GuaranteeOctagonsOrdinals =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSOO)
module GuaranteePolyhedra =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSAP)
module GuaranteePolyhedraOrdinals =
  GuaranteeIterator.GuaranteeIterator(DecisionTree.TSOP)

module RecurrenceBoxes =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSAB)
module RecurrenceBoxesOrdinals =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSOB)
module RecurrenceOctagons =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSAO)
module RecurrenceOctagonsOrdinals =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSOO)
module RecurrencePolyhedra =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSAP)
module RecurrencePolyhedraOrdinals =
  RecurrenceIterator.RecurrenceIterator(DecisionTree.TSOP)

module CTLBoxes = CTLIterator.CTLIterator(DecisionTree.TSAB)
module CTLBoxesOrdinals = CTLIterator.CTLIterator(DecisionTree.TSOB)
module CTLOctagons = CTLIterator.CTLIterator(DecisionTree.TSAO)
module CTLOctagonsOrdinals = CTLIterator.CTLIterator(DecisionTree.TSOO)
module CTLPolyhedra = CTLIterator.CTLIterator(DecisionTree.TSAP)
module CTLPolyhedraOrdinals = CTLIterator.CTLIterator(DecisionTree.TSOP)

module CTLCFGBoxes = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSAB)
module CTLCFGBoxesOrdinals = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSOB)
module CTLCFGOctagons = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSAO)
module CTLCFGOctagonsOrdinals = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSOO)
module CTLCFGPolyhedra = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSAP)
module CTLCFGPolyhedraOrdinals = CTLCFGIterator.CTLCFGIterator(DecisionTree.TSOP)


let result = ref false

let run_analysis analysis_function program () =
  try 
    let start = Sys.time () in
    let terminating = analysis_function program !main in
    let stop = Sys.time () in
    Format.fprintf !fmt "Analysis Result: ";
    let result = if terminating then "TRUE" else "UNKNOWN" in
    Format.fprintf !fmt "%s\n" result;
    if !time then
      Format.fprintf !fmt "Time: %f s\n" (stop-.start);
    Format.fprintf !fmt "\nDone.\n"
  with
  | Iterator.Timeout ->
    Format.fprintf !fmt "\nThe Analysis Timed Out!\n";
    Format.fprintf !fmt "\nDone.\n"

let termination () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then TerminationBoxesOrdinals.analyze else TerminationBoxes.analyze
    | "octagons" -> if !ordinals then TerminationOctagonsOrdinals.analyze else TerminationOctagons.analyze
    | "polyhedra" -> if !ordinals then TerminationPolyhedraOrdinals.analyze else TerminationPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis analysis_function program ()


let guarantee () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program, property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then GuaranteeBoxesOrdinals.analyze else GuaranteeBoxes.analyze
    | "octagons" -> if !ordinals then GuaranteeOctagonsOrdinals.analyze else GuaranteeOctagons.analyze
    | "polyhedra" -> if !ordinals then GuaranteePolyhedraOrdinals.analyze else GuaranteePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()


let recurrence () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property File Specified");
  let sources = parseFile !filename in
  let property = parseProperty !property in
  let (program,property) =
    ItoA.prog_itoa ~property:(!main,property) sources in
  let property =
    match property with
    | None -> raise (Invalid_argument "Unknown Property")
    | Some property -> property
  in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
      Format.fprintf !fmt "\nProperty: ";
      AbstractSyntax.property_print !fmt property;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> if !ordinals then RecurrenceBoxesOrdinals.analyze else RecurrenceBoxes.analyze
    | "octagons" -> if !ordinals then RecurrenceOctagonsOrdinals.analyze else RecurrenceOctagons.analyze
    | "polyhedra" -> if !ordinals then RecurrencePolyhedraOrdinals.analyze else RecurrencePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in run_analysis (analysis_function property) program ()


(* run termination analysis in CTL *)
let ctl_termination () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let parsedConditionalTerminationProperty = parseCTLPropertyString !precondition in
  let (prog, conditionalTerminationProperty) = ItoA.ctl_prog_itoa parsedConditionalTerminationProperty !main (parseFile !filename) in
  let conditionalTerminationProperty = 
    match conditionalTerminationProperty with  (* use ctl_prog_itoa to parse the conditional termination property*)
    | CTLProperty.Atomic (prop, None) -> prop 
    | _ -> raise (Invalid_argument "ctl_prog_itoa returned invalid property") in
  let (program, property) = 
    CTLIterator.program_of_prog_with_termination prog !main in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt (CTLIterator.prog_of_program program);
      Format.fprintf !fmt "\n";
    end;
  let analyze =
    match !domain with
    | "boxes" -> if !ordinals then CTLBoxesOrdinals.analyze else CTLBoxes.analyze
    | "octagons" -> if !ordinals then CTLOctagonsOrdinals.analyze else CTLOctagons.analyze
    | "polyhedra" -> if !ordinals then CTLPolyhedraOrdinals.analyze else CTLPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  let terminating = analyze ~precondition:conditionalTerminationProperty program property in
  if terminating then 
    Format.fprintf !fmt "\nAnalysis Result: TRUE\n"
  else 
    Format.fprintf !fmt "\nAnalysis Result: UNKNOWN\n"

    
let ctl ?(property_as_string = false) () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property Specified");
  let parsedPrecondition = parsePropertyString !precondition in
  let parsedProperty = (if property_as_string then parseCTLPropertyString else parseCTLProperty) !property in
  let (prog, property) = ItoA.ctl_prog_itoa parsedProperty !main (parseFile !filename) in
  let precondition = fst @@ AbstractSyntax.StringMap.find "" @@ ItoA.property_itoa_of_prog prog !main parsedPrecondition in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt prog;
      Format.fprintf !fmt "\n";
    end;
  let program = CTLIterator.program_of_prog prog !main in
  let analyze =
    match !domain with
    | "boxes" -> if !ordinals then CTLBoxesOrdinals.analyze else CTLBoxes.analyze
    | "octagons" -> if !ordinals then CTLOctagonsOrdinals.analyze else CTLOctagons.analyze
    | "polyhedra" -> if !ordinals then CTLPolyhedraOrdinals.analyze else CTLPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  let result = analyze ~precondition:precondition program property in
  if result then 
    Format.fprintf !fmt "\nAnalysis Result: TRUE\n"
  else 
    Format.fprintf !fmt "\nAnalysis Result: UNKNOWN\n"


let ctl_cfg () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  if !property = "" then raise (Invalid_argument "No Property Specified");
  let (cfg, getProperty) = Tree_to_cfg.prog (File_parser.parse_file !filename) !main in
  let mainFunc = Cfg.find_func !main cfg in
  let cfg = Cfg.insert_exit_label cfg mainFunc in
  let ctlProperty = CTLProperty.map File_parser.parse_bool_expression @@ parseCTLPropertyString_plain !property in
  let ctlProperty = CTLProperty.map getProperty ctlProperty in
  let precondition = getProperty @@ File_parser.parse_bool_expression !precondition in
  let analyze =
    match !domain with
    | "boxes" -> if !ordinals then CTLCFGBoxesOrdinals.analyze else CTLCFGBoxes.analyze
    | "octagons" -> if !ordinals then CTLCFGOctagonsOrdinals.analyze else CTLCFGOctagons.analyze
    | "polyhedra" -> if !ordinals then CTLCFGPolyhedraOrdinals.analyze else CTLCFGPolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in
  if not !minimal then
    begin
      Printf.printf "\nCFG:\n";
      Printf.printf "%a" Cfg_printer.print_cfg cfg;
      Printf.printf "CFG_DOT:\n %a" Cfg_printer.output_dot cfg;
      Printf.printf "\n";
    end;
  let mainFunc = Cfg.find_func !main cfg in
  let result = analyze ~precondition:precondition cfg mainFunc ctlProperty in
  if result then 
    Format.fprintf !fmt "\nAnalysis Result: TRUE\n"
  else 
    Format.fprintf !fmt "\nAnalysis Result: UNKNOWN\n"





(*Main entry point for application*)
let doit () =
  parse_args ();
  match !analysis with
  | "termination" -> termination ()
  | "guarantee" -> guarantee ()
  | "recurrence" -> recurrence ()
  | "actl" -> ctl ()
  | "actl_termination" -> ctl_termination ()
  | "ctl" -> ctl ()
  | "ctl_cfg" -> ctl_cfg ()
  | "ctl_str" -> ctl ~property_as_string:true ()
  | "ctl_termination" -> ctl_termination ()
  | _ -> raise (Invalid_argument "Unknown Analysis")

let _ = doit () 
