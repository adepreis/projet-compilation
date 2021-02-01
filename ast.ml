(* On considere qu'une liste peut etre vide et donc qu'elle ne sera jamais optionnelle en faisant : "option(list(A))". Il y aura toujours au minimum une liste VIDE [] *)


(* Differents types de comparaison *)
type opCompType =
  Eq | Neq | Lt | Le | Gt | Ge

(* Expression *)
type exprType =
ExprId of string
| ExprCste of int
| ExprString of string
| ExprCast of string * exprType
| ExprSelection of selectionType
| ExprInstanciation of string * exprType list
| ExprAppelFonction of exprType * string * exprType list
| ExprAppelFonctionObjet of string * string * exprType list
| Plus of exprType * exprType
| Minus of exprType * exprType
| Times of exprType * exprType
| Div of exprType * exprType
| Concat of exprType * exprType
| UPlus of exprType
| UMinus of exprType
| Comp of opCompType * exprType * exprType
and selectionType = Selection of exprType * string


(* Partie gauche d'une affectation *)
type cibleType =
CibleId of string
| CibleLId of exprType * string


(* Heritage optionnel du Constructeur *)
type optSuperType = OptSuper of string * exprType list

(* Heritage optionnel d'une Classe *)
type extendsType = Extends of string


(* Parametre dans une declaration de classes et/ou constructeurs *)
type paramCType =
ParamC of bool * string * string


(* Affectation optionnelle *)
type affectationType =
Affectation of exprType

(* Declaration d'une variable locale *)
type declVarType = DeclVar of string * string * affectationType option 


(* Instruction *)
type instrType =
InstrExpr of exprType
| InstrBloc of blocType
| InstrReturnExpr of exprType option (* SI InstrReturnExpr of None, on renvoie result *)
| InstrAffect of cibleType * exprType
| InstrITE of exprType * instrType * instrType
and blocType =
BlocInstr of instrType list
| BlocDeclAndInstr of declVarType list * instrType list

(* Type de retour d'une methode *)
type retourType =
TypeRetour of string

(* Differentes possibilites de declarer une methode *)
type finDeclMethodeType =
FinDeclMethodExpr of string * exprType
| FinDeclMethodBloc of retourType option * blocType

(* Parametre dans une declaration de methodes *)
type paramMethodeType = 
ParamMethode of string * string 


(* Declaration d'une classe *)
type declClasseType =
DeclAttrClasse of string * string * affectationType option
| DeclMethodeClasse of bool * string * paramMethodeType list * finDeclMethodeType 
| DeclConstrClasse of string * paramCType list * optSuperType option * blocType

(* Declaration d'un objet isole *)
type declObjetType =
DeclAttrObjet of string * string * affectationType option
| DeclMethodeObjet of bool * string * paramMethodeType list * finDeclMethodeType

(* Definition en tout debut de programme *)
type defType =
DefObj of string * declObjetType list
| DefClasse of string * paramCType list * extendsType option * declClasseType list


(* Le programme *)
type progType = Prog of defType list * blocType


