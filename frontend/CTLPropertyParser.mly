%{
open CTLProperty
%}

%token <string> ATOMIC
%token AX
%token AF
%token AG
%token AU
%token AND
%token OR
%token LEFT_BRACE
%token RIGHT_BRACE

%start <string CTLProperty.generic_formula> prog
%%

prog:
  | e = ATOMIC { Atomic e }
  | AX; LEFT_BRACE; e = prog; RIGHT_BRACE {AX e}
  | AF; LEFT_BRACE; e = prog; RIGHT_BRACE {AF e}
  | AG; LEFT_BRACE; e = prog; RIGHT_BRACE {AG e}
  | AU; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE;
        { AU (e1, e2) }
  | AND; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE;
        { AND (e1, e2) }
  | OR; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE;
        { OR (e1, e2) }
  ;

(*
exp:
  | s = ATOMIC
    { Atomic s }
  | AX; LEFT_BRACE; e = exp; RIGHT_BRACE
    { AX e }
  | AF; LEFT_BRACE; e = exp; RIGHT_BRACE
    { AF e }
  | AG; LEFT_BRACE; e = exp; RIGHT_BRACE
    { AG e }
  | AU; LEFT_BRACE; e1 = exp; RIGHT_BRACE; LEFT_BRACE; e2 = exp; RIGHT_BRACE
    { AU (e1, e2) }
  | e1 = exp; AND; e2 = exp;
    { AND (e1, e2) }
  | e1 = exp; OR; e2 = exp;
    { OR (e1, e2) }
  ;
  *)
