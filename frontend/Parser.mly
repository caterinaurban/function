(*   
                ********* Menhir Parser ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

%{
open IntermediateSyntax
%}

%token TOK_ASSERT;
%token TOK_ELSE;
%token TOK_FALSE;
%token TOK_IF;
%token TOK_INT;
%token TOK_FOR;
%token TOK_RETURN;
%token TOK_TRUE;
%token TOK_VOID;
%token TOK_WHILE;

%token <string> TOK_id

%token TOK_LPAREN;
%token TOK_RPAREN;
%token TOK_LBRACKET;
%token TOK_RBRACKET;
%token TOK_LCURLY;
%token TOK_RCURLY;
%token TOK_COMMA;
%token TOK_COLON;
%token TOK_SEMICOLON;
%token TOK_AND;
%token TOK_OR;
%token TOK_NOT;
%token TOK_LESS;
%token TOK_LESS_EQUAL;
%token TOK_EQUAL_EQUAL;
%token TOK_NOT_EQUAL;
%token TOK_GREATER;
%token TOK_GREATER_EQUAL;
%token TOK_PLUS;
%token TOK_PLUS_PLUS;
%token TOK_MINUS;
%token TOK_MINUS_MINUS;
%token TOK_MULTIPLY;
%token TOK_DIVIDE;
%token TOK_MODULO;
%token TOK_EQUAL;
%token TOK_PLUS_EQUAL;
%token TOK_MINUS_EQUAL;
%token TOK_MULTIPLY_EQUAL;
%token TOK_DIVIDE_EQUAL;
%token TOK_MODULO_EQUAL;
%token TOK_RANDOM;

%token <string> TOK_const


%token TOK_EOF

%start <IntermediateSyntax.decl list IntermediateSyntax.annotated> file

%%

file: t = annotate(list(decl)) TOK_EOF { t }

decl:
	| d = annotate(global_decl)		{ I_global d }
	| d = annotate(function_decl)	{ I_function d }

global_decl:
	| t = annotate(global_typ) g = separated_list(TOK_COMMA,declarator) TOK_SEMICOLON					{ t, g }
	| t = annotate(global_typ) TOK_MULTIPLY g = separated_list(TOK_COMMA,declarator) TOK_SEMICOLON		{ t, g }
	| t = annotate(global_typ) a = array_typ				               							    { t, a } 





	 

global_typ:
	| TOK_INT	{ I_INT } 

array_typ:
	| d= annotate (TOK_id) TOK_LBRACKET c = TOK_const TOK_RBRACKET   TOK_SEMICOLON	 {let rec f  n = if n = -1 then []  else (bind d (fun s -> s^"_"^(string_of_int n)),None):: (f (n-1)) in f ((int_of_string c) -1)}  
declarator:
    	
	| d = annotate(TOK_id)										{ d, None }
	| d = annotate(TOK_id) TOK_EQUAL e = annotate(exp)			{ d, Some e }

function_decl:
	| t = function_typ f = annotate(TOK_id) TOK_LPAREN TOK_VOID TOK_RPAREN TOK_SEMICOLON									{ t, f, [], [] }
	| t = function_typ f = annotate(TOK_id) TOK_LPAREN TOK_VOID TOK_RPAREN b = block_stmt									{ t, f, [], b }
	| t = function_typ f = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,parameter) TOK_RPAREN TOK_SEMICOLON		{ t, f, p, [] }
	| t = function_typ f = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,parameter) TOK_RPAREN b = block_stmt	{ t, f, p, b }

%inline function_typ:
	| t = annotate(global_typ)	{ Some t }
	| TOK_VOID					{ None }

parameter:
	| t = annotate(global_typ) d = annotate(TOK_id)										{ t, d }

stmt:
	| s = labeled_stmt		{ s }
	| s = exp_stmt			{ s }
	| s = selection_stmt	{ s }
	| s = iteration_stmt	{ s }
	| s = jump_stmt			{ s }
	| s = declaration_stmt	{ s }
	| s = block_stmt		{ I_block s }

labeled_stmt:
	| l = annotate(TOK_id) TOK_COLON	{ I_label l }

exp_stmt:
	| TOK_SEMICOLON														{ I_SKIP }
	| e = annotate(exp) TOK_SEMICOLON									{ I_exp e }
	| TOK_ASSERT TOK_LPAREN e=annotate(exp) TOK_RPAREN TOK_SEMICOLON	{ I_assert e }

selection_stmt:
	| TOK_IF TOK_LPAREN e = annotate(exp) TOK_RPAREN s = annotate(stmt)									{ I_if (e,s,None) }
	| TOK_IF TOK_LPAREN e = annotate(exp) TOK_RPAREN s1 = annotate(stmt) TOK_ELSE s2 = annotate(stmt)	{ I_if (e,s1,Some s2) }

iteration_stmt:
	| TOK_WHILE TOK_LPAREN e = annotate(exp) TOK_RPAREN s = annotate(stmt)																			{ I_while (e,s) }
	| TOK_FOR TOK_LPAREN e1 = annotate(exp) TOK_SEMICOLON e2 = annotate(exp) TOK_SEMICOLON e3 = annotate(exp) TOK_RPAREN s = annotate(stmt)			{ I_for_simple (e1,e2,e3,s) }
	| TOK_FOR TOK_LPAREN d = annotate(global_decl) e1 = annotate(exp) TOK_SEMICOLON e2 = annotate(exp) TOK_RPAREN s = annotate(stmt)	{ I_for (d,e1,e2,s) }

jump_stmt:
	| TOK_RETURN e=option(annotate(exp)) TOK_SEMICOLON	{ I_return e }

declaration_stmt:
	| d = annotate(global_decl)	{ I_local d }
	
block_stmt:
	| TOK_LCURLY s = list(annotate(stmt)) TOK_RCURLY	{ s }

exp:
	| e = logical_or_exp										{ e }
	| e1 = annotate(unary_exp) o = assign_op e2 = annotate(exp)	{ I_assign (e1,o,e2) }


assign_op:
	| TOK_EQUAL				{ I_EQUAL }
	| TOK_PLUS_EQUAL		{ I_PLUS_EQUAL }
	| TOK_MINUS_EQUAL		{ I_MINUS_EQUAL }
	| TOK_MULTIPLY_EQUAL	{ I_MULTIPLY_EQUAL }
	| TOK_DIVIDE_EQUAL		{ I_DIVIDE_EQUAL }
	| TOK_MODULO_EQUAL		{ I_MODULO_EQUAL }

logical_or_exp:
	| e = logical_and_exp													{ e }
	| e1 = annotate(logical_or_exp) TOK_OR e2 = annotate(logical_and_exp)	{ I_or (e1,e2) }

logical_and_exp:
	| e = equality_exp														{ e }
	| e1 = annotate(logical_and_exp) TOK_AND e2 = annotate(equality_exp)	{ I_and (e1,e2) }

equality_exp:
	| e = relational_exp															{ e }
	| e1 = annotate(equality_exp) TOK_EQUAL_EQUAL e2 = annotate(relational_exp)		{ I_eq (e1,e2) }
	| e1 = annotate(equality_exp) TOK_NOT_EQUAL e2 = annotate(relational_exp)		{ I_neq (e1,e2) }

relational_exp:
	| e = add_exp																{ e }
	| e1 = annotate(relational_exp) TOK_LESS e2 = annotate(add_exp)				{ I_less (e1,e2) }
	| e1 = annotate(relational_exp) TOK_LESS_EQUAL e2 = annotate(add_exp)		{ I_leq (e1,e2) }
	| e1 = annotate(relational_exp) TOK_GREATER e2 = annotate(add_exp)			{ I_greater (e1,e2) }
	| e1 = annotate(relational_exp) TOK_GREATER_EQUAL e2 = annotate(add_exp)	{ I_geq (e1,e2) }

add_exp:
	| e = mul_exp												{ e }
	| e1 = annotate(add_exp) TOK_PLUS e2 = annotate(mul_exp)	{ I_add (e1,e2) }
	| e1 = annotate(add_exp) TOK_MINUS e2 = annotate(mul_exp)	{ I_minus (e1,e2) }

mul_exp:
	| e = unary_exp													{ e }
	| e1 = annotate(mul_exp) TOK_MULTIPLY e2 = annotate(unary_exp)	{ I_mul (e1,e2) }
	| e1 = annotate(mul_exp) TOK_DIVIDE e2 = annotate(unary_exp)	{ I_div (e1,e2) }
	| e1 = annotate(mul_exp) TOK_MODULO e2 = annotate(unary_exp)	{ I_mod (e1,e2) }

unary_exp:
	| e = postfix_exp							{ e }
	| TOK_PLUS_PLUS e = annotate(unary_exp)		{ I_preincr e }
	| TOK_MINUS_MINUS e = annotate(unary_exp)	{ I_predecr e }
	| TOK_NOT e = annotate(unary_exp)			{ I_not e }
	| o = unary_op e = annotate(unary_exp)		{ I_unary (o,e) }

unary_op:
	| TOK_PLUS	{ I_PLUS }
	| TOK_MINUS	{ I_MINUS }

postfix_exp:
	| e = primary_exp																				{ e }
	| e = annotate(postfix_exp) TOK_PLUS_PLUS														{ I_incr e }
	| e = annotate(postfix_exp) TOK_MINUS_MINUS														{ I_decr e }
	| e = annotate(postfix_exp) TOK_LPAREN p = separated_list(TOK_COMMA,annotate(exp)) TOK_RPAREN	{ I_call (e,p) }
	| e= TOK_id  TOK_LBRACKET c = TOK_const TOK_RBRACKET { I_id (e^"_"^c)}

primary_exp:
	| TOK_TRUE																										{ I_TRUE }
	| TOK_RANDOM																									{ I_RANDOM }
	| TOK_FALSE																										{ I_FALSE }
	| e = TOK_id																									{ I_id e }
	| TOK_MULTIPLY e = TOK_id																						{ I_id e }
	| e = TOK_const																									{ I_const e }
	| TOK_LBRACKET o1 = option(unary_op) e1 = TOK_const TOK_COMMA o2=option(unary_op) e2 = TOK_const TOK_RBRACKET	{ I_interval (o1,e1,o2,e2) }
	| TOK_LPAREN e = exp TOK_RPAREN																					{ e }

annotate(X): 
	| x = X		{ x, ($startpos, $endpos) }

%%
