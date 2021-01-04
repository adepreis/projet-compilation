(* CODE DU PROF :
type opCompType =
  Eq | Neq | Lt | Le | Gt | Ge
*)

(* Selon moi, on devrait rajouter un *string dans chaque Noeud of des differentes regles car pour le cast, on a besoin de savoir la classe actuelle de l'expression afin de s'assurer qu'on puisse bien cast. Il faut sauvegarder la classe quelque part! *)

(* CODE DU PROF
type expType =
  Id of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| UMinus of expType
| Comp of opCompType*expType*expType
| Ite of expType*expType*expType
*)

(*
| Cast of string*expType (* pb voir commentaire au dessus *)
| Select of selection
| Object of objet
| Instanciation of string*list_arg
| Message of ??????? (* pb *)
(* | ExpTypeOperator of expTypeOperator , puis separer les Plus, Minus, ... dans expTypeOperator ? *)

*)

(*
type selection = Selection of objet*string
(* Forme de ce qu'on appellera un Objet, soit une instance d'une classe *)
type objet =
ObjetId of string*objet_Opt list option
|ObjetMethod of string*list_arg*objet_Opt list option
(* Forme des diverses possibilites d'objets *)
type objet_Opt = ObjetIdOpt of string | ObjetMethodOpt of string*list_arg
*)



(* CODE DU PROF
type decl = {
    lhs: string;
    rhs: expType;
  }
*)

type progType = Prog of defType list option *optBlocType (* Type du programme *)


type defType =
DefObj of string*declType list option
|DefClasse of string*paramCType list option * extendsType option*blocClasseType



type declType =
DeclAttr of string*string*exprType option
|DeclMethode of string*string*paramMethodeType option *finDeclMethodeType

type affectationType =
Affectation of exprType


type paramMethodeType = (* à ne pas confondre avec paramCType, meme définition mais type différent *)
ParamMethode of string*string



type finDeclMethodeType =
FinDeclMethodExpr of string*exprType
|FinDeclMethodBloc of string option *optBlocType option

type optBlocType =
OptBlocInstr of instrType list
|OptBlocDeclAndInstr of declVarType list*instrType list

type retourType =
TypeRetour of string


type declVarType = DeclVar of string*string*exprType option (* ce sont des variables locales *)



type paramCType =
ParamC of string*string


type extendsType = Extends of string



type blocClasseType = BlocClasse of declType list option *string*paramCType list option *optSuperType option *optBlocConstrType option *declType list option

type optSuperType = OptSuper of string*exprType list option


type optBlocConstrType = 
OptBlocConstrDecl of declVarType list * optBlocConstrType option
|OptBlocConstrInstr of instrType * optBlocConstrType option




type instrType =
InstrExpr of exprType
|InstrBloc of optBlocType
|InstrReturnExpr of (* exprReturnType *) exprType option
|InstrAffect of cibleType*exprType
|InstrITE of exprType*instrType*instrType

(* type exprReturnType =
ExprReturnExpr of exprType
| ExprReturnId of string  *) (* SI InstrReturn of None, on renvoie result (lire p2 enonce) #Remy *)

type cibleType =
CibleId of string
|CibleCast of string*cibleType
|CibleLId of cibleType*string list 
|CibleAppelFonction of exprType*string*exprType list option* string list


type exprType =
ExprId of string
|ExprCste of int
|ExprCast of string*exprType
|ExprSelection of selectionType
|ExprInstanciation of string*exprType list option
|ExprAppelFonction of appelFonctionType(*exprType*string*lOptArgType option*)* string list
|ExprOperator of exprOpType

type exprOpType =
Plus of exprType*exprType
| Minus of exprType*exprType
| Times of exprType*exprType
| Div of exprType*exprType
| UMinus of exprType
| Comp of opCompType*exprType*exprType

type opCompType =
  Eq | Neq | Lt | Le | Gt | Ge


type selectionType = Selection of exprType*string

type appelFonctionType = AppelFonction of exprType*string*exprType list option





(*



(* RQ : peut etre que le mot cle option ne marche pas comme ca, dans ce cas on peut dire que ca renvoie None of *)


(* RQ : Est ce que c'est intelligent de faire des listes au lieu de faire des arbres d'arbres ? ... *)

(* Regrouper les types qui peuvent l'etre !!!!!, comme pour le type expType *)

(* On va stocker le mot cle 'var' pour chaque Id car la definition du constructeur d'une classe utilise les Id precede du mot cle 'var' dans la liste des parametres d'une declaration de classe *)

(* Forme des parametres lors d'une declaration de classes et_ou constructeurs *)
type list_OptParamC = LOptParamC of list_paramCC option
type list_ParamC = LParamC of param_C | LParamsC of param_C*list_ParamC
(* Un parametre dans une declaration de classes et_ou constructeurs *)
type param_C = ParamC of string option*string*string

(* Forme des parametres lors d'une declaration de methodes *)
type list_OptParamM = LOptParamM of list_ParamM option
type list_ParamM = LParamM of param_M | LParamsM of param_M*list_ParamM
(* Un parametre dans une declaration de methodes *)
type param_M = ParamM of string*string

(* Forme des arguments passes en parametres *)
type list_OptArg = LOptArg of list_Arg option
type list_arg = LArg of expType | LArgs of expType*list_Arg


(* Forme des affections et heritages optionnels qu'on a pas le choix d'expliciter dans la grammaire pour pouvoir assurer le bon fonctionnement dans l'analyse syntaxique *)
type assign_Opt = AssignOpt of expType
type extends_Opt = ExtendsOpt of string

(* Forme d'une declaration de variable (ou attribut si dans une classe) a l'interieur d'un corps *)
type decl_var = DeclVar of string*string*expType option


(* Forme d'une declaration de classe *)
type decl_class = DeclClass of string*list_paramCCOpt*string*bloc_class
(* Forme d'un bloc entre {} d'une classe *)
type bloc_class = BlocClass of bloc_intern list option*decl_constructor*bloc_intern list option
(* Forme des declarations possibles (attributs et_ou methodes) a l'interieur d'un bloc de classe *)
type bloc_intern = BlocInternV of DeclVar | BlocInternM of DeclMethod


(* Forme d'une declaration d'objet isole *)
type decl_objetIsole = DeclObjetIsole of string*bloc_intern list option


(* Forme d'une declaration de constructeur *)
type decl_constr = DeclConstr of string*list_paramCCOpt*super_Opt option*instruction list option
(* Forme de la super classe optionnelle *)
type super_Opt = SuperOpt of string*list_argOpt


(* Forme d'une declaration de methode *)
type decl_method =
DeclMethodExpr of override_Opt*string*list_paramMOpt*string*expType
|DeclMethodBloc of override_Opt*string*list_paramMOpt*string option*bloc_instr
(* Sert a retenir si la methode declaree redefinie la methode de sa super classe. La valeur sera 1 pour true, 0 pour false *)
type override_Opt = OverrideOpt of int
(* Forme d'un bloc d'une methode *)
type bloc_instr = Bloc of list_instrOpt option
(* Forme de ce que peut contenir un bloc: une liste de declarations et une liste d'instructions *)
type list_instrOpt = LInstrOpt of decl_var list*instructionBloc list
type instructionBloc =
InstrBlocExpr of expType
|InstrBlocReturn of exptype option
|InstrBlocAffect of cible_affect*expType
|InstrBlocITE of expType*instruction*instruction



(* Forme d'une declaration d'instruction *)
type instruction =
InstrExpr of expType
|InstrBloc of bloc_instr
|InstrReturn of expType option
|InstrAffect of cible_affect*expType
|InstrITE of expType*instruction*instruction

(* Forme de la partie gauche d'une affectation *)
type cible_affect =
CibleId of string
| CibleCast of string*cible_affect
| CibleSelect of selection

 *)



