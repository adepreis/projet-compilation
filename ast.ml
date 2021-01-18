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


(* CODE DU PROF
type decl = {
    lhs: string;
    rhs: expType;
  }
*)


type opCompType =
  Eq | Neq | Lt | Le | Gt | Ge

type exprType =
ExprId of string
|ExprCste of int
|ExprString of string
|ExprCast of string * exprType
|ExprSelection of selectionType
|ExprInstanciation of string * exprType list
|ExprAppelFonction of exprType * string * exprType list
|ExprAppelFonctionObjet of string * string * exprType list
| Plus of exprType * exprType
| Minus of exprType * exprType
| Times of exprType * exprType
| Div of exprType * exprType
| UPlus of exprType
| UMinus of exprType
| Comp of opCompType * exprType * exprType
and selectionType = Selection of exprType * string



type cibleType =
CibleId of string
|CibleLId of exprType * string
(*|CibleCast of string*cibleType // A ENLEVER SELON PROF *)

(* type exprReturnType =
ExprReturnExpr of exprType
| ExprReturnId of string  *) (* SI InstrReturn of None, on renvoie result (lire p2 enonce) #Remy *)


type optSuperType = OptSuper of string * exprType list

type extendsType = Extends of string


type paramCType =
ParamC of bool * string * string


type affectationType =
Affectation of exprType

type declVarType = DeclVar of string * string * affectationType option (* ce sont des variables locales *)


type instrType =
InstrExpr of exprType
|InstrBloc of blocType
|InstrReturnExpr of (* exprReturnType *) exprType option  (* Discuter de ce cas special ! *)
|InstrAffect of cibleType * exprType
|InstrITE of exprType * instrType * instrType
and blocType =
BlocInstr of instrType list
|BlocDeclAndInstr of declVarType list * instrType list


type retourType =
TypeRetour of string

type finDeclMethodeType =
FinDeclMethodExpr of string * exprType
|FinDeclMethodBloc of retourType option * blocType

type paramMethodeType = 
ParamMethode of string * string (* à ne pas confondre avec paramCType, meme définition mais type différent *)


type declClasseType =
DeclAttrClasse of string * string * affectationType option
|DeclMethodeClasse of bool * string * paramMethodeType list * finDeclMethodeType (* remplacer override par un booléen ça sera plus pratique à gérer (boption) *)
|DeclConstrClasse of string * paramCType list * optSuperType option * blocType

type declObjetType =
DeclAttrObjet of string * string * affectationType option
|DeclMethodeObjet of bool * string * paramMethodeType list * finDeclMethodeType

type defType =
DefObj of string * declObjetType list
|DefClasse of string * paramCType list * extendsType option * declClasseType list


type progType = Prog of defType list * blocType


(* J'ai enleve les "option" des list, car on a dit qu'on considere qu'une liste peut etre vide et que si on fait : "option(list(A))", ce ne sera jamais optionnel puisqu'il y aura tjs au minimum une liste VIDE [] *)

(* Ca peut marcher si on fait "option(nonempty_list(A))" ce qui serait optionnel si la liste est VIDE, mais pour je ne sais quelle raison, quand je compile, les erreurs different... *)





































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



