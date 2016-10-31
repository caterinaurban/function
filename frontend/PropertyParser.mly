(*   
            ********* Menhir Property Parser ************
   Copyright (C) 2012-2014 by Caterina Urban. All rights reserved.
*)

%{
open IntermediateSyntax
%}

%token TOK_FALSE;
%token TOK_TRUE;

%token <string> TOK_id

%token TOK_LPAREN;
%token TOK_RPAREN;
%token TOK_LBRACKET;
%token TOK_RBRACKET;
%token TOK_COMMA;
%token TOK_COLON;
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
%token TOK_MINUS;
%token TOK_MULTIPLY;
%token TOK_DIVIDE;
%token TOK_MODULO;

%token <string> TOK_const

%token TOK_EOF

%start <IntermediateSyntax.property IntermediateSyntax.annotated> file

%%

file: t = annotate(exp) TOK_EOF { t }

exp:
	| e = annotate(logical_or_exp)						{ I_universal e }
	| l = TOK_id TOK_COLON e = annotate(logical_or_exp)	{ I_particular (l,e) }

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
	| e = primary_exp							{ e }
	| TOK_NOT e = annotate(unary_exp)			{ I_not e }
	| o = unary_op e = annotate(unary_exp)		{ I_unary (o,e) }

unary_op:
	| TOK_PLUS	{ I_PLUS }
	| TOK_MINUS	{ I_MINUS }

primary_exp:
	| TOK_TRUE																										{ I_TRUE }
	| TOK_FALSE																										{ I_FALSE }
	| e = TOK_id																									{ I_id e }
	| e = TOK_const																									{ I_const e }
	| TOK_LBRACKET o1 = option(unary_op) e1 = TOK_const TOK_COMMA o2=option(unary_op) e2 = TOK_const TOK_RBRACKET	{ I_interval (o1,e1,o2,e2) }
	| TOK_LPAREN e = logical_or_exp TOK_RPAREN																		{ e }

annotate(X): 
	| x = X  { x, ($startpos, $endpos) }

%%
