%{
open Ast
%}

%token <string> ID
%token <string> IDC
%token <int> CSTE
%token <string> STRING
%token <Ast.opCompType> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN

%token IF THEN ELSE

/* Rajouter tous les nouveaux tokens + verifier s'il faut leur donner des 'champs' */
%token CLASS
%token <bool> EXTENDS
%token IS
%token VAR
%token DEF
%token OBJECT
%token OVERRIDE
%token THIS /* %token <string> THIS */ /* pour stocker la classe en VC */
%token SUPER /* inutile sauf si on le gere comme THIS -> super. ou constructeur super(...) */
%token RESULT /* A verifier */
%token NEW
%token AS
%token RETURN
%token LACOLADE
%token RACOLADE
%token DEUXPTS
%token COMMA
%token DOT


/* Il faudra pour block_  */

/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF


%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS  UPLUS     /* highest precedence */
%right DOT

/* Correspondance des types avec l'AST */

/* prog/progType déjà compris dans start */
%type <defType> definition
%type <optBlocType> block
%type <declObjetType> declaration_objet
%type <declClasseType> declaration_classe
%type <paramCType> param_classe
%type <extendsType> extends
%type <optSuperType> super
%type <declVarType> declarationVar_block
%type <affectationType> affectation_expr
%type <paramMethodeType> param_methode
%type <finDeclMethodeType> fin_decl_methode
%type <retourType> type_retour 
%type <optBlocConstr> block_constr
%type <instrType> instruction
//%type <cibleType> cible
//%type <appelFonctionType> appel_fonction
//%type <selectionType> selection

%start<Ast.progType> prog
%%
prog: ld = list(definition) optBlock = delimited(LACOLADE, option(block), RACOLADE) EOF
  { Prog(ld, optBlock) }


definition: OBJECT x = IDC IS lOptDecl = delimited(LACOLADE, list(declaration_objet), RACOLADE) { DefObj(x, lOptDecl) }
  | CLASS x = IDC lParamClasse = delimited(LPAREN, separated_list(COMMA, param_classe), RPAREN)
    optExtends = option(extends) IS blockClasse = delimited(LACOLADE, list(declaration_classe), RACOLADE) { DefClasse(x, lParamClasse, optExtends, blockClasse) }

declaration_objet: VAR x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclAttrObjet(x, y, affectationExpr) }
  | DEF o = option(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) 
    fin = fin_decl_methode { DeclMethodeObjet(o, x, lParamMethode, fin) }

declaration_classe: VAR x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclAttrClasse(x, y, affectationExpr) }
  | DEF o = option(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) 
    fin = fin_decl_methode { DeclMethodeClasse(o, x, lParamMethode, fin) }
  | DEF x = IDC lParamClasse = delimited(LPAREN, list(param_classe), RPAREN) s = option(super) IS blockConstr = delimited(LACOLADE, option(block_constr), RACOLADE) { DeclConstrClasse(x, lParamClasse, s, blockConstr) }

affectation_expr: ASSIGN e = expr { Affectation e }


param_methode: x = ID DEUXPTS y = IDC { ParamMethode (x,y) }


fin_decl_methode: DEUXPTS x = IDC ASSIGN e = expr { FinDeclMethodExpr(x, e) }
  | typeRetour = option(type_retour) IS block = delimited(LACOLADE, option(block), RACOLADE) { FinDeclMethodBloc(typeRetour, block) }

type_retour: DEUXPTS x = IDC { TypeRetour x }

block: lInstr = nonempty_list(instruction) { OptBlocInstr(lInstr) }
  | lDecBlock = nonempty_list(declarationVar_block) IS lInstr = nonempty_list(instruction) { OptBlocDeclAndInstr(lDecBlock, lInstr) }

declarationVar_block: x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclVar(x, y, affectationExpr) }


param_classe: option(VAR) x = ID DEUXPTS y = IDC { ParamC(x, y) }

extends: EXTENDS x = IDC { Extends x }


super: DEUXPTS x = IDC listArguments = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { OptSuper(x, listArguments) }

block_constr: lDeclBlock = nonempty_list(declarationVar_block) blockConstr = option(block_constr) { OptBlocConstrDecl(lDeclBlock, blockConstr) }
  | i = instruction blockConstr = option(block_constr) { OptBlocConstrInstr(i, blockConstr) }


instruction: e = expr SEMICOLON { InstrExpr e }
  | block = delimited(LACOLADE, option(block), RACOLADE) { InstrBloc block }
  | RETURN optExpr = option(expr) SEMICOLON { InstrReturnExpr optExpr }
//  | c = cible ASSIGN e = expr SEMICOLON { InstrAffect(c, e) }
  | IF si = expr THEN alors = instruction ELSE sinon = instruction { InstrITE(si, alors, sinon) }

// TO DO : faire appel_fonction et la dernière regle ci-dessous
/*cible: x = ID { CibleId x }
  | c =  delimited(LPAREN, cible, RPAREN) { CibleId c }
  | LPAREN AS x = ID DEUXPTS c = cible RPAREN { CibleCast(x, c) }
  | c = cible DOT ci = cible { CibleLId(c, ci) }
  | af = appel_fonction DOT l = separated_nonempty_list(DOT, ID) { CibleAppelFonction(af, l) }
*/


expr: x = ID { ExprId x }
  | v = CSTE { ExprCste v }
  | s = STRING {ExprString s}
  | e = delimited (LPAREN, expr, RPAREN) { e }
  | LPAREN AS x = ID DEUXPTS e = expr RPAREN { ExprCast(x,e) }
//  | s = selection { ExprSelection s }
  | NEW x = IDC args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { ExprInstanciation(x, args) }
  //| af = appel_fonction { ExprAppelFonction af }
  | g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | g = expr PLUS d = expr { Plus(g, d) }
  | g = expr MINUS d = expr       { Minus(g, d) }
  | g = expr TIMES d = expr       { Times(g, d) }
  | g = expr DIV d = expr         { Div(g, d) }
  | PLUS e = expr  %prec UPLUS    { UPlus e }
  | MINUS e = expr %prec UMINUS   { UMinus e }


// TO DO : cible appel_fonction selection à faire (responsables des reduce/reduce)

//selection: e = expr DOT c = cible { Selection(e, x) }


//appel_fonction: e = expr DOT x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { AppelFonction(e, x, args) }
