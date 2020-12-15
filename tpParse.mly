%{
open Ast
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP (* je ne comprends pas pourquoi ce serait pas <opComp> tout court... *)
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN

%token IF THEN ELSE

(* Rajouter tous les nouveaux tokens + verifier sil faut leur donner des 'champs' *)
%token CLASS
%token EXTENDS
%token IS
%token VAR
%token DEF
%token OBJECT
%token OVERRIDE
%token THIS (* %token <string> THIS *) (* pour stocker la classe en VC *)
%token SUPER (* inutile sauf si on le gere comme THIS -> super. ou constructeur super(...) *)
%token RESULT (* A verifier *)
%token NEW
%token AS
%token RETURN
%token LACOLADE
%token RACOLADE
%token DEUXPTS
%token COMMA
%token DOT


(* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*)
%token UMINUS

%token EOF

%right ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */

%type <expType> expr bExpr
(* %type <decl> declaration *)



(*%type <list_paramCCOpt> lparamCCOpt
%type <list_paramCC> lparamCC
%type <param_CC> paramCC

%type <list_paramMOpt> lparamMOpt
%type <list_paramM> lparamM
%type <param_M> paramM

%type <list_argOpt> largOpt
%type <list_arg> larg


%type <assign_Opt> assignOpt
%type <extends_Opt> extendsOpt
%type <super_Opt> superOpt


%type <decl_var> declVar

%type <decl_class> declClass
%type <bloc_class> blocClass
%type <bloc_intern> blocIntern

%type <decl_objetIsole> declObjetIsole

%type <decl_constr> declConstr*)

%start<Ast.progType> prog
%%
prog: ld = list(definition) optBlock = delimited(LACOLADE, option(block), RACOLADE) EOF
  { ld, optBlock }

definition: OBJECT x = ID IS lOptDecl = delimited(LACOLADE, list(declaration), RACOLADE) { (* To do *) }
  | CLASS x = ID lParamClasse = delimited(LPAREN, separated_list(COMMA, param_classe), RPAREN)
    optExtends = option(extends) IS blockClasse = delimited(LACOLADE, block_classe, RACOLADE) { (* To do *) }

block: lInstr = nonempty_list(instruction) { (* To do *) }
  | lDecBlock = nonempty_list(declaration_block) IS lInstr = nonempty_list(instruction) { (* To do *) }

declaration: VAR x = ID DEUXPTS y = ID affectationExpr = option(affectation_expr) SEMICOLON { (* To do *) }
  | DEF o = option(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) 
    fin = fin_decl_methode { (* To do *) }

param_classe: option(VAR) x = ID DEUXPTS y = ID { (* To do *) }

extends: EXTENDS x = ID { (* To do *) }

block_classe: decl1 = list(declaration) DEF x = ID lParamClasse = delimited(LPAREN, list(param_classe), RPAREN)
  s = option(super) IS blockConstr = delimited(LACOLADE, option(block_constr), RACOLADE) decl2 = list(declaration) { (* To do *) }

super: DEUXPTS x = ID listArguments = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { (* To do *) }

declaration_block: x = ID DEUXPTS y = ID affectationExpr = option(affectation_expr) SEMICOLON { (* To do *) }

affectation_expr: ASSIGN e = expr { (* To do *) }

param_methode: x = ID DEUXPTS y = ID { (* To do *) }

fin_decl_methode: DEUXPTS x = ID ASSIGN e = expr { (* To do *) }
  | typeRetour = option(type_retour) IS block = delimited(LACOLADE, option(block), RACOLADE) { (* To do *) }

type_retour: DEUXPTS x = ID { (* To do *) }

block_constr: lDeclBlock = nonempty_list(declaration_block) blockConstr = option(block_constr) { (* To do *) }
  | i = instruction blockConstr = option(block_constr) { (* To do *) }

instruction: e = expr SEMICOLON { (* To do *) }
  | optBlock = option(block) { (* To do *) }
  | RETURN optExpr = option(expr) SEMICOLON { (* To do *) }
  | c = cible ASSIGN e = expr SEMICOLON { (* To do *) }
  | IF if = bExpr THEN then = instruction ELSE else = instruction { (* To do *) }

cible: x = ID { (* To do *) }
  | c =  delimited(LPAREN, cible, RPAREN) { (* To do *) }
  | LPAREN AS x = ID DEUXPTS c = cible RPAREN { (* To do *) }
  | c = cible DOT l = separated_nonempty_list(DOT, ID) { (* To do *) }
  | af = appel_fonction DOT l = separated_nonempty_list(DOT, ID) { (* To do *) }

appel_fonction: e = expr DOT x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { (* To do *) }

(* declaration : x = ID ASSIGN e = expr SEMICOLON
  { { lhs = x; rhs = e; } } *)

expr:
    x = ID                        { (* To do *) }
  | v = CSTE                      { (* To do *) }
  | e = delimited (LPAREN, expr, RPAREN) { (* To do *) }
  | LPAREN AS x = ID DEUXPTS e = expr RPAREN { (* To do *) }
  | s = selection { (* To do *) }
  | af = appel_fonction { (* To do *) }
  | NEW x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { (* To do *) }
  | e = exprOperator { (* To do *) }

exprOperator:
  | g = expr PLUS d = expr        { (* To do *) }
  | g = expr MINUS d = expr       { (* To do *) }
  | g = expr TIMES d = expr       { (* To do *) }
  | g = expr DIV d = expr         { (* To do *) }
  | PLUS e = expr                 { (* To do *) }
  | MINUS e = expr %prec UMINUS   { (* To do *) }

selection: e = expr DOT x = ID { (* To do *) }

bexpr :
    g = expr op = RELOP d = expr  { (* To do *) }
  | e = delimited (LPAREN, bexpr, RPAREN) { (* To do *) }




(*lparamCCOpt : l=option(lparamCC) { LParamCCOpt(l) }
lparamCC :
	a=paramCC { LParamCC(a) }
	|a=paramCC COMMA b=lparamCC { LParamsCC(a,b) }
paramCC : v=option(ID) x=ID DEUXPTS y=ID { ParamCC(v,x,y) }
(* lparamCCOpt : l=option(list(lparamCC)) { LParamCCOpt(l) }
lparamCC :
	a=paramCC { LParamCC(a) }
	|a=paramCC COMMA { LParamsCC(a) }
paramCC : v=option(ID) x=ID DEUXPTS y=ID { ParamCC(v,x,y) }
*)

lparamMOpt : l=option(lparamM) { LParamMOpt(l) }
lparamM :
	a=paramM { LParam(a) }
	|a=paramM COMMA b=lparamM { LParams(a,b) }
paramM : x=ID DEUXPTS y=ID { ParamM(x,y) }

largOpt : l=option(larg) { LArgOpt(l) }
larg :
	a=expr { LArg(a) }
	|a=expr COMMA b=larg {LArgs(a,b) }


assignOpt : ASSIGN x=expr { AssignOpt(x) }
extendsOpt : EXTENDS x=ID { ExtendsOpt(x) }
superOpt : DEUXPTS x=ID l=delimited(LPAREN, largOpt, RPAREN) { SuperOpt(x,l) }


declVar : VAR x=ID DEUXPTS y=ID z=option(assignOpt) SEMICOLON { DeclVar(x,y,z) }


declClass :
	CLASS x=ID l=delimited(LPAREN, lparamCCOpt, RPAREN) y=option(extendsOpt) IS b=delimited(LACOLADE, blocClass, RACOLADE) { DeclClass(x,l,y,b) }

blocClass :
	a=option(list(blocIntern)) x=declConstructor b=option(list(blocIntern)) { BlocClass(a,x,y) }

blocIntern :
	x=declVar { BlocInternV(x) }
	|x=declMethod { BlocInternM(x) }


declObjetIsole : OBJECT x=ID IS y=delimited(LACOLADE, option(list(blocIntern)), RACOLADE) { DeclObjetIsole(x,y) }


declConstr : DEF x=ID l=delimited(LPAREN, lparamCCOpt, RPAREN) s=option(superOpt) IS b=option(delimited(LACOLADE, list(instr), RACOLADE)) { DeclConstr(x,l,s,b) }*)
