
{
open Lexing
open CTLPropertyParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* let atomic = ['a'-'z' 'A'-'Z' '0'-'9' '(' ')' '_' ':' '<' '>' '=' '!' '&' '|' '+' '-' '/' '\t']+ *)
let atomic = ['a'-'z' 'A'-'Z' '0'-'9' '(' ')' '_' '<' '>' '=' '!' '&' '|' '+' '-' '/' ' ' '\t']+
let label = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* ':'

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "AX"   { AX }
  | "AF"   { AF }
  | "AG"   { AG }
  | "AU"   { AU }
  | "EX"   { EX }
  | "EF"   { EF }
  | "EG"   { EG }
  | "EU"   { EU }
  | "AND"   { AND }
  | "OR"   { OR }
  | "NOT"   { NOT }
  | "{"   { LEFT_BRACE }
  | "}"   { RIGHT_BRACE }
  | atomic {ATOMIC (Lexing.lexeme lexbuf)}
  | label {LABEL (Lexing.lexeme lexbuf)}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

