%{
open CTLProperty

let label_name l = String.sub l 0 (String.length l - 1)

%}

%token <string> ATOMIC
%token <string> LABEL
%token AX
%token AF
%token AG
%token AU
%token EX
%token EF
%token EG
%token EU
%token AND
%token OR
%token NOT
%token LEFT_BRACE
%token RIGHT_BRACE

%start <string CTLProperty.generic_property> prog
%%

prog:
  | e = ATOMIC { Atomic (e, None) }
  | l = LABEL; e = ATOMIC { Atomic (e, Some (label_name l)) }
  | AX; LEFT_BRACE; e = prog; RIGHT_BRACE {AX e}
  | AF; LEFT_BRACE; e = prog; RIGHT_BRACE {AF e}
  | AG; LEFT_BRACE; e = prog; RIGHT_BRACE {AG e}
  | AU; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE; { AU (e1, e2) }
  | EX; LEFT_BRACE; e = prog; RIGHT_BRACE {EX e}
  | EF; LEFT_BRACE; e = prog; RIGHT_BRACE {EF e}
  | EG; LEFT_BRACE; e = prog; RIGHT_BRACE {EG e}
  | EU; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE; { EU (e1, e2) }
  | AND; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE; { AND (e1, e2) }
  | OR; LEFT_BRACE; e1 = prog; RIGHT_BRACE; LEFT_BRACE; e2 = prog; RIGHT_BRACE; { OR (e1, e2) }
  | NOT; LEFT_BRACE; e = prog; RIGHT_BRACE; { NOT e}
  ;
