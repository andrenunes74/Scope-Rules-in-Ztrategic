{-Module that contains the functions that reverse the parser-}
{-------------------------------------------------------------
********* Just use your parsed expression with the 
reverseP function ********************************************
--------------------------------------------------------------}
module Reverse where
import Core
{-------------------------------------------------------------
*********************** Expressions **************************
--------------------------------------------------------------}
rExp :: Exp -> String
rExp (Const a) = (show a)
rExp (Var a) = a
rExp (Inc a) = (rExp a) ++ "++"
rExp (Dec a) = (rExp a) ++ "++"
rExp (Not a) = "!" ++ (rExp a)
rExp (Or a b) = (rExp a) ++ "or" ++ (rExp b)
rExp (And a b) = (rExp a) ++ "and" ++ (rExp b)
rExp (LTE a b) = (rExp a) ++ "<=" ++ (rExp b)
rExp (GTE a b) = (rExp a) ++ ">=" ++ (rExp b)
rExp (Equals a b) = (rExp a) ++ "==" ++ (rExp b)
rExp (Greater a b) = (rExp a) ++ ">" ++ (rExp b)
rExp (Less a b) = (rExp a) ++ "<" ++ (rExp b)
rExp (Mul a b) = (rExp a) ++ "*" ++ (rExp b)
rExp (Div a b) = (rExp a) ++ "/" ++ (rExp b)
rExp (Sub a b) = (rExp a) ++ "-" ++ (rExp b)
rExp (Add a b) = (rExp a) ++ "+" ++ (rExp b)
rExp (Return a) = "return " ++ (rExp a)
rExp (Bool a) = (show a)

{-------------------------------------------------------------
************************ Items *******************************
--------------------------------------------------------------}
rItem :: Item -> String
rItem (Decl a b) = a ++ "=" ++ (rExp b)
rItem (Arg a) = (rExp a)
rItem (Increment a) = (rExp a) ++ "++"
rItem (Decrement a) = (rExp a) ++ "--"
rItem (NestedIf a) = (rIf a)
rItem (OpenIf a) = (rIf a)
rItem (NestedWhile a) = (rWhile a)
rItem (OpenWhile a) = (rWhile a)
rItem (NestedLet b) =(rLet b)
rItem (OpenLet a) = (rLet a)
rItem (NestedFuncao a) = (rFuncao a)
rItem (OpenFuncao a) = (rFuncao a)
rItem (NestedReturn a) = (rExp a)

rItems :: Items -> String
rItems [] = ""
rItems [h] = (rItem h) ++ ";"
rItems (h:t) = (rItem h) ++";"++ (rItems t) 

{-------------------------------------------------------------
*************************** If *******************************
--------------------------------------------------------------}
rIf :: If -> String
rIf (If a b) = "if"++"("++(rExp a)++")"++"{"++(rItems b)++"}"
rIf (Else a b c) = "if"++"("++(rExp a)++")"++"{"++(rItems b)++"}"++"else"++ "{"++(rItems c)++"}"

{-------------------------------------------------------------
************************ While *******************************
--------------------------------------------------------------}
rWhile :: While -> String
rWhile (While a b) = "while"++"("++(rExp a)++")"++"{"++(rItems b)++"}"

{-------------------------------------------------------------
************************** Let *******************************
--------------------------------------------------------------}
rLet :: Let -> String
rLet (Let a b) = "let"++"{"++(rItems a)++"}in "++(rExp b)

{-------------------------------------------------------------
************************ Functions ***************************
--------------------------------------------------------------}
rName :: Name -> String
rName (Name a) = a

rArgs :: Args -> String
rArgs [] = ""
rArgs [h] = (rItem h)
rArgs (h:t) = (rItem h)++","++(rArgs t)

rFuncao :: Funcao -> String
rFuncao (Funcao a b) = "func "++(rName a)++"("++(rArgs b)++")"
rFuncao (DefFuncao a b c) = "def "++(rName a)++"("++(rArgs b)++")"++"{"++(rItems c)++"}"

{-------------------------------------------------------------
************************** Main *****************************
--------------------------------------------------------------}
reverseP :: [(Item,String)] -> String
reverseP [] = ""
reverseP [(a,b)] = (rItem a)
reverseP ((a,b):t) = (rItem a) ++ (reverseP t)