(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2015
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)


(*
  Parser for a very simple C-like "curly bracket" language.
  
  There should be exactly one shift/reduce conflict, due to nested 
  if-then-else constructs. The resolution picked by menhir should be correct.
 *)

%{
open Abstract_syntax_tree
%}

/* tokens */
/**********/

%token TOK_VOID
%token TOK_INT
%token TOK_TRUE
%token TOK_FALSE
%token TOK_RAND
%token TOK_BRAND
%token TOK_FOR
%token TOK_WHILE
%token TOK_RETURN
%token TOK_BREAK  
%token TOK_IF
%token TOK_ELSE
%token TOK_GOTO  
%token TOK_ASSERT

%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LCURLY
%token TOK_RCURLY
%token TOK_STAR
%token TOK_PLUS
%token TOK_MINUS
%token TOK_EXCLAIM
%token TOK_DIVIDE
%token TOK_PERCENT
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_AND_AND
%token TOK_BAR_BAR
%token TOK_SEMICOLON
%token TOK_COLON
%token TOK_COMMA
%token TOK_EQUAL
%token TOK_PLUS_PLUS
%token TOK_MINUS_MINUS  
%token TOK_PLUS_EQUAL
%token TOK_MINUS_EQUAL
%token TOK_STAR_EQUAL
%token TOK_DIVIDE_EQUAL
%token TOK_PERCENT_EQUAL

%token <string> TOK_id
%token <string> TOK_int

%token TOK_EOF

/* priorities of binary operators (lowest to highest) */
%left TOK_BAR_BAR
%left TOK_AND_AND
%left TOK_EXCLAIM
%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_DIVIDE TOK_PERCENT


/* entry-points */
/****************/

%start<Abstract_syntax_tree.toplevel list Abstract_syntax_tree.ext> file


%%


/* toplevel */
/************/

file: t=ext(list(toplevel)) TOK_EOF { t }

toplevel:
| d=ext(var_decl)           { AST_global_decl d }
| d=ext(fun_decl)           { AST_fun_decl d }

    

/* expressions */
/***************/

bool_expr:
| TOK_LPAREN e=bool_expr TOK_RPAREN
    { e }
    
| TOK_TRUE
    { AST_bool_const true }
    
| TOK_FALSE
    { AST_bool_const false }
    
| o=bool_unary_op e=ext(bool_expr)
    { AST_bool_unary (o,e) }
    
| e1=ext(bool_expr) o=bool_binary_op e2=ext(bool_expr)
    { AST_bool_binary (o,e1,e2) }
    
| e1=ext(int_expr) o=compare_op e2=ext(int_expr)
    { AST_compare (o,e1,e2) }
    
| TOK_BRAND
    { AST_bool_rand }

    
int_expr:    
| TOK_LPAREN e=int_expr TOK_RPAREN
    { e }
    
| e=ext(TOK_int)
    { AST_int_const e }
    
| e=ext(TOK_id)
    { AST_int_identifier e }
    
| o=int_unary_op e=ext(int_expr)
    { AST_int_unary (o,e) }
    
| e1=ext(int_expr) o=int_binary_op e2=ext(int_expr)
    { AST_int_binary (o,e1,e2) }
    
| TOK_RAND TOK_LPAREN e1=ext(sign_int_literal)  
           TOK_COMMA  e2=ext(sign_int_literal) TOK_RPAREN
    { AST_int_rand (e1, e2) }

| e=ext(TOK_id) TOK_LPAREN l=int_expr_list TOK_RPAREN
    { AST_expr_call (e, l) }


int_expr_list:
    l=separated_list(TOK_COMMA,ext(int_expr))  { l }
    
    
/* integer with optional sign */
sign_int_literal:
| i=TOK_int            { i }
| TOK_PLUS i=TOK_int   { i }
| TOK_MINUS i=TOK_int  { "-"^i }

%inline int_unary_op:
| TOK_PLUS           { AST_UNARY_PLUS }
| TOK_MINUS          { AST_UNARY_MINUS }

%inline bool_unary_op:
| TOK_EXCLAIM        { AST_NOT }

%inline int_binary_op:
| TOK_STAR           { AST_MULTIPLY }
| TOK_DIVIDE         { AST_DIVIDE }
| TOK_PERCENT        { AST_MODULO }
| TOK_PLUS           { AST_PLUS }
| TOK_MINUS          { AST_MINUS }

%inline assign_op:
| TOK_STAR_EQUAL           { AST_MULTIPLY }
| TOK_DIVIDE_EQUAL         { AST_DIVIDE }
| TOK_PERCENT_EQUAL        { AST_MODULO }
| TOK_PLUS_EQUAL           { AST_PLUS }
| TOK_MINUS_EQUAL          { AST_MINUS }

%inline compare_op:
| TOK_LESS           { AST_LESS }
| TOK_GREATER        { AST_GREATER }
| TOK_LESS_EQUAL     { AST_LESS_EQUAL }
| TOK_GREATER_EQUAL  { AST_GREATER_EQUAL }
| TOK_EQUAL_EQUAL    { AST_EQUAL }
| TOK_NOT_EQUAL      { AST_NOT_EQUAL }

%inline bool_binary_op:
| TOK_AND_AND        { AST_AND }
| TOK_BAR_BAR        { AST_OR }



/* declarations */
/****************/

var_decl:
| s=ext(typ) i=separated_list(TOK_COMMA,init_declarator) TOK_SEMICOLON
  { s, i }

init_declarator:
| v=ext(TOK_id)                             { v, None }
| v=ext(TOK_id) TOK_EQUAL i=ext(int_expr)   { v, Some i }

fun_decl:
| t=ext(typ_or_void) i=ext(TOK_id)
        TOK_LPAREN p=separated_list(TOK_COMMA,param_decl) TOK_RPAREN 
         TOK_LCURLY l=list(ext(stat)) TOK_RCURLY
  { { Abstract_syntax_tree.fun_name = i;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = p;
      Abstract_syntax_tree.fun_body = l;
      Abstract_syntax_tree.fun_ext = ($startpos, $endpos); }
  }

param_decl:
| s=ext(typ) v=ext(TOK_id) { s, v }

typ:
| TOK_INT    { AST_TYP_INT }

%inline typ_or_void:
| t=typ      { Some t }
| TOK_VOID   { None }



    
/* statements */
/**************/


assign:
| e=ext(TOK_id) TOK_EQUAL f=ext(int_expr)
    { AST_assign (e, f) }

| e=ext(TOK_id) o=assign_op f=ext(int_expr)
    { AST_assign_op (e, o, f) }

| e=ext(TOK_id) TOK_PLUS_PLUS
    { AST_increment (e,1) }

| e=ext(TOK_id) TOK_MINUS_MINUS
    { AST_increment (e,-1); }


assign_list:
    l=separated_list(TOK_COMMA,ext(assign))  { l }


stat:
| a=assign TOK_SEMICOLON
    { a }

| d=ext(var_decl)
    { AST_local_decl d }

| TOK_LCURLY l=list(ext(stat)) TOK_RCURLY
    { AST_block l }

| TOK_IF TOK_LPAREN e=ext(bool_expr) TOK_RPAREN s=ext(stat)
    { AST_if (e, s, None) }
    
| TOK_IF TOK_LPAREN e=ext(bool_expr) TOK_RPAREN s=ext(stat) TOK_ELSE t=ext(stat) 
    { AST_if (e, s, Some t) }
    
| TOK_WHILE TOK_LPAREN e=ext(bool_expr) TOK_RPAREN s=ext(stat)
    { AST_while (e, s) }
    
| TOK_FOR TOK_LPAREN a=assign_list TOK_SEMICOLON b=option(ext(bool_expr)) TOK_SEMICOLON c=assign_list TOK_RPAREN s=ext(stat)
    { AST_for (a,b,c,s) }
    
| TOK_ASSERT TOK_LPAREN e=ext(bool_expr) TOK_RPAREN TOK_SEMICOLON
    { AST_assert e }
    
| e=ext(TOK_BREAK) TOK_SEMICOLON
    { AST_break e }
    
| e=ext(TOK_id) TOK_LPAREN l=int_expr_list TOK_RPAREN TOK_SEMICOLON
    { AST_stat_call (e, l) }

| TOK_RETURN e=option(ext(int_expr)) TOK_SEMICOLON
    { AST_return e }

| TOK_SEMICOLON
    { AST_SKIP }

| TOK_GOTO e=ext(TOK_id) TOK_SEMICOLON
    { AST_goto e }

| e=ext(TOK_id) TOK_COLON
    { AST_label e }    


    
/* utilities */
/*************/

/* adds extent information to rule */
%inline ext(X): 
| x=X { x, ($startpos, $endpos) }


%%
