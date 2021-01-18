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

%token BEGIN
%token END


%token EOF

%nonassoc RELOP
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left High     /* highest precedence */
%right DOT

/* Correspondance des types avec l'AST */

/* prog/progType déjà compris dans start */
%type <defType> definition
%type <blocType> block
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
%type <instrType> instruction
//%type <cibleType> cible
//%type <appelFonctionType> appel_fonction
//%type <selectionType> selection

%start<Ast.progType> prog
%%
prog: ld = list(definition) bloc = block EOF
  { Prog(ld, bloc) }


definition: OBJECT x = IDC IS lOptDecl = delimited(LACOLADE, list(declaration_objet), RACOLADE) { DefObj(x, lOptDecl) }
  | CLASS x = IDC lParamClasse = delimited(LPAREN, separated_list(COMMA, param_classe), RPAREN)
    optExtends = option(extends) IS blockClasse = delimited(LACOLADE, list(declaration_classe), RACOLADE) { DefClasse(x, lParamClasse, optExtends, blockClasse) }

declaration_objet: VAR x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclAttrObjet(x, y, affectationExpr) }
  | DEF o = boption(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) 
    fin = fin_decl_methode { DeclMethodeObjet(o, x, lParamMethode, fin) }

declaration_classe: VAR x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclAttrClasse(x, y, affectationExpr) }
  | DEF o = boption(OVERRIDE) x = ID lParamMethode = delimited(LPAREN, separated_list(COMMA, param_methode), RPAREN) (* to do : regarder photos, considérer une meme regle d ast pour les 3 *)
    fin = fin_decl_methode { DeclMethodeClasse(o, x, lParamMethode, fin) }
  | DEF x = IDC lParamClasse = delimited(LPAREN, list(param_classe), RPAREN) s = option(super) IS bloc = block { DeclConstrClasse(x, lParamClasse, s, bloc) }


affectation_expr: ASSIGN e = expr { Affectation e }

param_methode: x = ID DEUXPTS y = IDC { ParamMethode (x,y) }

fin_decl_methode: DEUXPTS x = IDC ASSIGN e = expr { FinDeclMethodExpr(x, e) }
  | typeRetour = option(type_retour) IS bloc = block { FinDeclMethodBloc(typeRetour, bloc) }

type_retour: DEUXPTS x = IDC { TypeRetour x }


block: LACOLADE lInstr = list(instruction) RACOLADE { BlocInstr(lInstr) }
  | LACOLADE lDecBlock = nonempty_list(declarationVar_block) IS lInstr = nonempty_list(instruction) RACOLADE { BlocDeclAndInstr(lDecBlock, lInstr) }


declarationVar_block: x = ID DEUXPTS y = IDC affectationExpr = option(affectation_expr) SEMICOLON { DeclVar(x, y, affectationExpr) }


param_classe: o = boption(VAR) x = ID DEUXPTS y = IDC { ParamC(o, x, y) }


extends: EXTENDS x = IDC { Extends x }

super: DEUXPTS x = IDC listArguments = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { OptSuper(x, listArguments) }


instruction: e = expr SEMICOLON { InstrExpr e }
  | bloc = block { InstrBloc bloc }
  | RETURN optExpr = option(expr) SEMICOLON { InstrReturnExpr optExpr }
  | c = cible ASSIGN e = expr SEMICOLON { InstrAffect(c, e) }
  | IF si = expr THEN alors = instruction ELSE sinon = instruction { InstrITE(si, alors, sinon) }

cible: x = ID { CibleId x }
    | e = expr DOT x = ID { CibleLId(e, x) }
//  | c =  delimited(LPAREN, cible, RPAREN) { CibleId c } // PAS TRES UTILE
//  | LPAREN AS x = IDC DEUXPTS c = cible RPAREN { CibleCast(x, c) } // A ENLEVER SELON PROF


(* IL MANQUE LES APPELS (selection, appels de fonctions) avec les Objets !!! cad avec les IDC *)

expr: x = ID { ExprId x }
  | v = CSTE { ExprCste v }
  | s = STRING {ExprString s}
  | e = delimited (LPAREN, expr, RPAREN) { e } (* On peut renvoyer e sans nom ? Et pas sous cette une forme comme ExprParen(e) ? *)
  | LPAREN AS x = IDC DEUXPTS e = expr RPAREN { ExprCast(x,e) }
  | s = selection { ExprSelection s }

  | NEW x = IDC args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { ExprInstanciation(x, args) }
  | e = expr DOT x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { ExprAppelFonction(e, x, args) }
  | y = IDC DOT x = ID args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { ExprAppelFonctionObjet(y, x, args) }

  | g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | g = expr PLUS d = expr 	{ Plus(g, d) }
  | g = expr MINUS d = expr       { Minus(g, d) }
  | g = expr TIMES d = expr       { Times(g, d) }
  | g = expr DIV d = expr         { Div(g, d) }
  | PLUS e = expr  %prec High     { UPlus e } // vérifier partie droite
  | MINUS e = expr %prec High     { UMinus e } // vérifier partie droite


selection: e = expr DOT x = ID { Selection(e, x) }
