(*   
                ********* OCamllex Lexer ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

{
open Lexing
open IntermediateSyntax
open PropertyParser

let keyword = Hashtbl.create 2
let _ = List.iter (fun (a,b) -> Hashtbl.add keyword a b)
	[
		"false", TOK_FALSE;
		"true", TOK_TRUE;
	]
}

let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n"

let digit = ['0'-'9']
let integer = digit digit*

rule start = parse
	| space	{ start lexbuf }
	| ""	{ token lexbuf }

and token = parse
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
		{ try Hashtbl.find keyword id with Not_found -> TOK_id id }

	| "("						{ TOK_LPAREN }
	| ")"						{ TOK_RPAREN }
	| "["						{ TOK_LBRACKET }
	| "]"						{ TOK_RBRACKET }
	| ","						{ TOK_COMMA }
	| ":"						{ TOK_COLON }
	| "&&"						{ TOK_AND }
	| "||"						{ TOK_OR }
	| "!"						{ TOK_NOT }
	| "<"						{ TOK_LESS }
	| "<="						{ TOK_LESS_EQUAL }
	| "=="						{ TOK_EQUAL_EQUAL }
	| "!="						{ TOK_NOT_EQUAL }
	| ">"						{ TOK_GREATER }
	| ">="						{ TOK_GREATER_EQUAL }
	| "+"						{ TOK_PLUS }
	| "-"						{ TOK_MINUS }
	| "*"						{ TOK_MULTIPLY } 
	| "/"						{ TOK_DIVIDE }
	| "%"						{ TOK_MODULO }

	| integer as c				{ TOK_const c }

	| "/*"						{ comment lexbuf; token lexbuf }
	| "//" [^ '\n' '\r']*		{ token lexbuf }
	| "extern" [^ '\n' '\r']*	{ token lexbuf }
	| newline					{ new_line lexbuf; start lexbuf }
	| space						{ token lexbuf }

	| eof						{ TOK_EOF }

and comment = parse
	| "*/"			{ () }
	| [^ '\n' '\r']	{ comment lexbuf }
	| newline		{ new_line lexbuf; comment lexbuf }
