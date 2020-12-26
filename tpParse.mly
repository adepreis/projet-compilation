%{
open Ast
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP /* je ne comprends pas pourquoi ce serait pas <opComp> tout court... */
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN

%token IF THEN ELSE

/* Rajouter tous les nouveaux tokens + verifier s'il faut leur donner des 'champs' */
%token CLASS
%token EXTENDS
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


/* utilise pour donner une precedence maximale au - unaire
* L'analyseur lexical ne renvoie jamais ce token !
*/
%token UMINUS

%token EOF

%right ELSE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left UMINUS            /* highest precedence */

/* Correspondance des types avec l'AST */

/* prog/progType déjà compris dans start */
%type <defType> definition  /* lOptDefType défini dans prog, pose un souci ?? */
%type <optBlocType> block
%type <declType> declaration
%type <lOptParamCType> param_classe /* pas sur car lOptParamCType=liste alors que param_classe=unParam.. */
%type <> extends  /* pas de type extends dans l'AST mais représenté par une string dans defType ?? */
%type <blocClasseType> block_classe
%type <optSuperType> super /* pas sur car opt dans AST, option(..super..) dans "grammaire" */
%type <lDeclType> declaration_block /* pas sur car lDeclType=liste alors que declaration_block=uneDecl.. */
%type <> affectation_expr /* pas de type "affectation" dans l'AST ? Compris dans instrType ? */
%type <lOptParamMethodeType> param_methode /* pas sur car lOptParamMethodeType=liste alors que param_methode=unParam.. */
%type <finDeclMethodeType> fin_decl_methode
%type <> type_retour  /* pas de type "type_de_retour" dans l'AST ? Compris dans instrType ? */
%type <optBlocConstr> block_constr
%type <instrType> instruction
%type <cibleType> cible
%type <appelFonctionType> appel_fonction
%type <selectionType> selection
%type <exprOpType> exprOperator
/* bexpr compris dans expr, opComp ? */
/* expr/bexpr déjà associés à expType */


/* Types restant dans l'AST
type lOptDefType
type lOptArgType
*/

%type <expType> expr bExpr

%start<Ast.progType> prog
%%
prog: ld = list(definition) optBlock = delimited(LACOLADE, option(block), RACOLADE) EOF
  { Prog(ld, optBlock) }


definition: OBJECT x = ID IS lOptDecl = delimited(LACOLADE, list(declaration), RACOLADE) { DefObj(x, lOptDecl) }
  | CLASS x = ID lParamClasse = delimited(LPAREN, separated_list(COMMA, param_classe), RPAREN)
    optExtends = option(extends) IS blockClasse = delimited(LACOLADE, block_classe, RACOLADE) { DefClasse(x, lParamClasse, optExtends, blockClasse) }


declaration: VAR x = ID DEUXPTS y = ID affectationExpr = option(affectation_expr) SEMICOLON { DeclAttr(x, y, affectationExpr) }
  | DEF o = option(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) 
    fin = fin_decl_methode { DeclMethode(o, x, lParamMethode, fin) }

affectation_expr: ASSIGN e = expr { (* To do *) }


param_methode: x = ID DEUXPTS y = ID { (* To do *) }


fin_decl_methode: DEUXPTS x = ID ASSIGN e = expr { FinDeclMethodExpr(x, e) }
  | typeRetour = option(type_retour) IS block = delimited(LACOLADE, option(block), RACOLADE) { FinDeclMethodBloc(typeRetour, block) }

type_retour: DEUXPTS x = ID { ExprId x }

block: lInstr = nonempty_list(instruction) { OptBlocInstr(lInstr) }
  | lDecBlock = nonempty_list(declaration_block) IS lInstr = nonempty_list(instruction) { OptBlocDeclAndInstr(lDecBlock, lInstr) }

declaration_block: x = ID DEUXPTS y = ID affectationExpr = option(affectation_expr) SEMICOLON { LDecl(x, y, affectationExpr) }


param_classe: option(VAR) x = ID DEUXPTS y = ID { (* To do *) }

extends: EXTENDS x = ID { ExprId x }


block_classe: decl1 = list(declaration) DEF x = ID lParamClasse = delimited(LPAREN, list(param_classe), RPAREN)
  s = option(super) IS blockConstr = delimited(LACOLADE, option(block_constr), RACOLADE) decl2 = list(declaration) { BlocClasse(decl1, x, lParamClasse, s, blockConstr, decl2) }

super: DEUXPTS x = ID listArguments = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { OptSuper(x, listArguments) }

block_constr: lDeclBlock = nonempty_list(declaration_block) blockConstr = option(block_constr) { OptBlocConstrDecl(lDeclBlock, blockConstr) }
  | i = instruction blockConstr = option(block_constr) { OptBlocConstrInstr(i, blockConstr) }


instruction: e = expr SEMICOLON { InstrExpr e }
  | optBlock = option(block) { InstrBloc optBlock }
  | RETURN optExpr = option(expr) SEMICOLON { InstrReturnExpr optExpr }
  | c = cible ASSIGN e = expr SEMICOLON { InstrAffect(c, e) }
  | IF iff = bExpr THEN thenn = instruction ELSE elsee = instruction { InstrITE(iff, thenn, elsee) }


cible: x = ID { CibleId x }
  | c =  delimited(LPAREN, cible, RPAREN) { CibleId c }
  | LPAREN AS x = ID DEUXPTS c = cible RPAREN { CibleCast(x, c) }
  | c = cible DOT l = separated_nonempty_list(DOT, ID) { CibleLId(c, l) }
  | af = appel_fonction DOT l = separated_nonempty_list(DOT, ID) { CibleAppelFonction(af, l) }


expr: x = ID { ExprId x }
  | v = CSTE { ExprCste v }
  | e = delimited (LPAREN, expr, RPAREN) { e }
  | LPAREN AS x = ID DEUXPTS e = expr RPAREN { ExprCast(x,e) }
  | s = selection { ExprSelection s }
  | NEW x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { ExprInstanciation(x, args) }
  | af = appel_fonction { ExprAppelFonction af }
  | e = exprOperator { ExprOperator e }

exprOperator: g = expr PLUS d = expr { Plus(g, d) }
  | g = expr MINUS d = expr       { Minus(g, d) }
  | g = expr TIMES d = expr       { Times(g, d) }
  | g = expr DIV d = expr         { Div(g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }


selection: e = expr DOT x = ID { Selection(e, x) }


appel_fonction: e = expr DOT x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { AppelFonction(e, x, args) }


/* declaration : x = ID ASSIGN e = expr SEMICOLON
  { { lhs = x; rhs = e; } } */




bexpr: g = expr op = RELOP d = expr  { (* To do *) }
  | e = delimited (LPAREN, bexpr, RPAREN) { (* To do *) }
