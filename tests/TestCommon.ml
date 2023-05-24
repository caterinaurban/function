open OUnit2

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
  | Failure "lexing: empty token" ->
      Printf.eprintf "Parse Error (Invalid Token) near %s\n"
        (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p) ;
      failwith "Parse Error"

let make_analyser common_setup ?(setup = []) filename expected =
  filename
  >:: fun test_ctxt ->
  assert_command ~exit_code:(Unix.WEXITED 0)
    ~foutput:(fun out ->
      let terminate = ref false in
      let ok_string = "\nAnalysis Result: TRUE" in
      let index = ref 0 in
      (* Format.fprintf Format.std_formatter "%s " filename; *)
      (* List.iter(fun s -> Format.fprintf Format.std_formatter "%s " s)
         common_setup; *)
      Stream.iter
        (fun c ->
          (* Format.fprintf Format.std_formatter "%c" c; *)
          if !index = String.length ok_string then terminate := true
          else if c = ok_string.[!index] then incr index
          else index := if c = '\n' then 1 else 0 )
        out ;
      assert_equal ~printer:string_of_bool ~msg:filename expected !terminate
      )
    "timeout"
    (["120s"; "./function"] @ common_setup @ setup @ [filename])
