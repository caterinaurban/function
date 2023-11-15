(*   
                ********* Menhir Parser ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

%{
open Abstract_syntax_tree
%}

%token TOK_ASSERT;
%token TOK_BREAK;
%token TOK_ELSE;
%token TOK_FALSE;
%token TOK_GOTO;
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
%token TOK_AND_AND;
%token TOK_BAR_BAR;
%token TOK_EXCLAIM;
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
%token TOK_STAR;
%token TOK_DIVIDE;
%token TOK_PERCENT;
%token TOK_EQUAL;
%token TOK_PLUS_EQUAL;
%token TOK_MINUS_EQUAL;
%token TOK_STAR_EQUAL;
%token TOK_DIVIDE_EQUAL;
%token TOK_PERCENT_EQUAL;
%token TOK_QUESTIONMARK;

%token <string> TOK_int

%token TOK_EOF

//%left TOK_BAR_BAR
//%left TOK_AND_AND
//%left TOK_EXCLAIM
//%left TOK_PLUS TOK_MINUS
//%left TOK_STAR TOK_DIVIDE TOK_PERCENT

%start<Abstract_syntax_tree.toplevel list Abstract_syntax_tree.ext> file
%start<Abstract_syntax_tree.expr Abstract_syntax_tree.ext> expression

%%

file: t = annotate(list(decl)) TOK_EOF          { t }
expression: t=annotate(logical_or_exp) TOK_EOF  { t }

decl:
	| d = annotate(global_decl)		{ AST_global_decl d }
	| d = annotate(function_decl)	{ AST_fun_decl d }

global_decl:
	| t = annotate(global_typ) g = separated_list(TOK_COMMA,declarator) TOK_SEMICOLON	{ t, g }

global_typ:
	| TOK_INT	{ AST_TYP_INT }

declarator:
	| d = annotate(TOK_id)										{ d, None }
	| d = annotate(TOK_id) TOK_EQUAL e = annotate(exp)			{ d, Some e }

function_decl:
	| t = annotate(function_typ) f = annotate(TOK_id) TOK_LPAREN TOK_VOID TOK_RPAREN TOK_SEMICOLON									{
	{ Abstract_syntax_tree.fun_name = f;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = [];
      Abstract_syntax_tree.fun_body = [];
      Abstract_syntax_tree.fun_ext = ($startpos, $endpos); }
	 }
	| t = annotate(function_typ) f = annotate(TOK_id) TOK_LPAREN TOK_VOID TOK_RPAREN b = block_stmt									{
	{ Abstract_syntax_tree.fun_name = f;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = [];
      Abstract_syntax_tree.fun_body = b;
      Abstract_syntax_tree.fun_ext = ($startpos, $endpos); }
	 }
	| t = annotate(function_typ) f = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,parameter) TOK_RPAREN TOK_SEMICOLON		{
	{ Abstract_syntax_tree.fun_name = f;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = p;
      Abstract_syntax_tree.fun_body = [];
      Abstract_syntax_tree.fun_ext = ($startpos, $endpos); }
	 }
	| t = annotate(function_typ) f = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,parameter) TOK_RPAREN b = block_stmt	{
	{ Abstract_syntax_tree.fun_name = f;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = p;
      Abstract_syntax_tree.fun_body = b;
      Abstract_syntax_tree.fun_ext = ($startpos, $endpos); }
	 }

%inline function_typ:
	| t = global_typ	{ Some t }
	| TOK_VOID			{ None }

parameter:
	| t = annotate(global_typ) d = annotate(TOK_id)										{ t, d }

stmt:
	| s = labeled_stmt		{ s }
	| s = exp_stmt			{ s }
	| s = selection_stmt	{ s }
	| s = iteration_stmt	{ s }
	| s = jump_stmt			{ s }
	| s = declaration_stmt	{ s }
	| s = block_stmt		{ AST_block s }

labeled_stmt:
	| l = annotate(TOK_id) TOK_COLON	{ AST_label l }

exp_stmt:
	| TOK_SEMICOLON														            { AST_SKIP }
	| e = assign_stmt TOK_SEMICOLON								                    { e }
	| e1 = annotate(TOK_id) o = assign_op e2 = annotate(exp)	TOK_SEMICOLON       { AST_assign_op (e1,o,e2) }
	| e = annotate(TOK_id) TOK_PLUS_PLUS TOK_SEMICOLON					            { AST_increment (e,1) }
	| e = annotate(TOK_id) TOK_MINUS_MINUS TOK_SEMICOLON					        { AST_increment (e,-1) }
	| TOK_ASSERT TOK_LPAREN e=annotate(logical_or_exp) TOK_RPAREN TOK_SEMICOLON	    { AST_assert e }
    | e = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,annotate(exp)) TOK_RPAREN TOK_SEMICOLON	{ AST_stat_call (e,p) }

assign_stmt:
    | e1 = annotate(TOK_id) TOK_EQUAL e2 = annotate(exp)    { AST_assign (e1, e2) }

assign_list:
    l = separated_list(TOK_COMMA,annotate(assign_stmt))     { l }

selection_stmt:
	| TOK_IF TOK_LPAREN e = annotate(exp) TOK_RPAREN s = annotate(stmt)									{ AST_if (e,s,None) }
	| TOK_IF TOK_LPAREN e = annotate(exp) TOK_RPAREN s1 = annotate(stmt) TOK_ELSE s2 = annotate(stmt)	{ AST_if (e,s1,Some s2) }

iteration_stmt:
	| TOK_WHILE TOK_LPAREN e = annotate(exp) TOK_RPAREN s = annotate(stmt)																			            { AST_while (e,s) }
	| TOK_FOR TOK_LPAREN e1 = assign_list TOK_SEMICOLON e2 = option(annotate(logical_or_exp)) TOK_SEMICOLON e3 = assign_list TOK_RPAREN s = annotate(stmt)		{ AST_for (e1,e2,e3,s) }

jump_stmt:
	| TOK_RETURN e=option(annotate(add_exp)) TOK_SEMICOLON	{ AST_return e }
	| e=annotate(TOK_BREAK) TOK_SEMICOLON                   { AST_break e }
	| TOK_GOTO e=annotate(TOK_id) TOK_SEMICOLON             { AST_goto e }

declaration_stmt:
	| d = annotate(global_decl)	{ AST_local_decl d }

block_stmt:
	| TOK_LCURLY s = list(annotate(stmt)) TOK_RCURLY	{ s }

exp:
	| e = logical_or_exp	{ e }

assign_op:
	| TOK_PLUS_EQUAL		{ AST_PLUS }
	| TOK_MINUS_EQUAL		{ AST_MINUS }
	| TOK_STAR_EQUAL	    { AST_MULTIPLY }
	| TOK_DIVIDE_EQUAL		{ AST_DIVIDE }
	| TOK_PERCENT_EQUAL		{ AST_MODULO }

logical_or_exp:
	| e = logical_and_exp													{ e }
	| e1 = annotate(logical_or_exp) TOK_BAR_BAR e2 = annotate(logical_and_exp)	{ AST_bool_binary (AST_OR, e1,e2) }

logical_and_exp:
	| e = equality_exp														{ e }
	| e1 = annotate(logical_and_exp) TOK_AND_AND e2 = annotate(equality_exp)	{ AST_bool_binary (AST_AND, e1,e2) }

equality_exp:
	| e = relational_exp															{ e }
	| e1 = annotate(equality_exp) TOK_EQUAL_EQUAL e2 = annotate(relational_exp)		{ AST_compare (AST_EQUAL, e1,e2) }
	| e1 = annotate(equality_exp) TOK_NOT_EQUAL e2 = annotate(relational_exp)		{ AST_compare (AST_NOT_EQUAL, e1,e2) }

relational_exp:
	| e = add_exp																{ e }
	| e1 = annotate(relational_exp) TOK_LESS e2 = annotate(add_exp)				{ AST_compare (AST_LESS, e1,e2) }
	| e1 = annotate(relational_exp) TOK_LESS_EQUAL e2 = annotate(add_exp)		{ AST_compare (AST_LESS_EQUAL, e1,e2) }
	| e1 = annotate(relational_exp) TOK_GREATER e2 = annotate(add_exp)			{ AST_compare (AST_GREATER, e1,e2) }
	| e1 = annotate(relational_exp) TOK_GREATER_EQUAL e2 = annotate(add_exp)	{ AST_compare (AST_GREATER_EQUAL, e1,e2) }

add_exp:
	| e = mul_exp												{ e }
	| e1 = annotate(add_exp) TOK_PLUS e2 = annotate(mul_exp)	{ AST_int_binary (AST_PLUS, e1,e2) }
	| e1 = annotate(add_exp) TOK_MINUS e2 = annotate(mul_exp)	{ AST_int_binary (AST_MINUS, e1,e2) }

mul_exp:
	| e = unary_exp													{ e }
	| e1 = annotate(mul_exp) TOK_STAR e2 = annotate(unary_exp)	{ AST_int_binary (AST_MULTIPLY, e1,e2) }
	| e1 = annotate(mul_exp) TOK_DIVIDE e2 = annotate(unary_exp)	{ AST_int_binary (AST_DIVIDE, e1,e2) }
	| e1 = annotate(mul_exp) TOK_PERCENT e2 = annotate(unary_exp)	{ AST_int_binary (AST_MODULO, e1,e2) }

unary_exp:
	| e = postfix_exp							{ e }
	| TOK_EXCLAIM e = annotate(unary_exp)		{ AST_bool_unary (AST_NOT, e) }
	| o = unary_op e = annotate(unary_exp)		{ AST_int_unary (o,e) }

unary_op:
	| TOK_PLUS	{ AST_UNARY_PLUS }
	| TOK_MINUS	{ AST_UNARY_MINUS }

postfix_exp:
	| e = primary_exp																				{ e }
	| e = call_exp                                                                                  { e }

call_exp:
	| e = annotate(TOK_id) TOK_LPAREN p = separated_list(TOK_COMMA,annotate(exp)) TOK_RPAREN	    { AST_expr_call (e,p) }

primary_exp:
	| TOK_TRUE																				                    		{ AST_bool_const true }
	| TOK_QUESTIONMARK																			                    	{ AST_random }
	| TOK_FALSE																					                    	{ AST_bool_const false }
	| e = annotate(TOK_id)																					            { AST_int_identifier e }
	| e = annotate(TOK_int)																					            { AST_int_const e }
	| TOK_LBRACKET e1 = annotate(sign_int_literal) TOK_COMMA e2 = annotate(sign_int_literal) TOK_RBRACKET	            { AST_int_interval (e1, e2) }
	| TOK_LPAREN e = exp TOK_RPAREN															                    		{ e }

sign_int_literal:
    | i = TOK_int            { i }
    | TOK_PLUS i = TOK_int   { i }
    | TOK_MINUS i = TOK_int  { "-"^i }

%inline annotate(X):
	| x = X		{ x, ($startpos, $endpos) }

%%
