(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Opens and parses a file given as argument.
  
    Antoine Miné 2015
    Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open AbstractSyntaxTree
open CFGPrinter
open Lexing

(* parsing, with nice error messages *)

let parse_file (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    ProgramParser.file ProgramLexer.token lex
  with
  | ProgramParser.Error ->
      Printf.eprintf "Parse error (invalid syntax) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"

  | Failure e ->
      if String.equal e "lexing: empty token" then begin
        Printf.eprintf "Parse error (invalid token) near %s\n" 
          (string_of_position lex.lex_start_p); 
        failwith "Parse error"
      end else failwith e

let parse_bool_expression (bexp:string) =
  let lex = from_string bexp in
  try fst @@ ProgramParser.expression ProgramLexer.token lex
  with
  | ProgramParser.Error ->
      Printf.eprintf "Parse error (invalid syntax) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"
  | Failure e ->
      if String.equal e "lexing: empty token" then begin
        Printf.eprintf "Parse error (invalid token) near %s\n" 
          (string_of_position lex.lex_start_p); 
        failwith "Parse error"
      end else failwith e


