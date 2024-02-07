{-# LANGUAGE DeriveDataTypeable #-}
{-Module that contains the datatypes of the parser-}
module Core where
import Data.Data
{--------------------------------
********** Expressions **********
---------------------------------}
data Exp = Add Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Mul Exp Exp
         | Less Exp Exp
         | Greater Exp Exp
         | Equals Exp Exp
         | GTE Exp Exp
         | LTE Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Const Int
         | Var String
         | Inc Exp
         | Dec Exp
         | Return Exp
         | Bool Bool
         deriving (Show,Eq,Data)

{--------------------------------
************* Item **************
---------------------------------}
data Item = Decl String Exp
          | Arg Exp
          | Increment Exp
          | Decrement Exp
          | NestedIf If
          | OpenIf If
          | NestedWhile While
          | OpenWhile While
          | NestedLet Let
          | OpenLet Let
          | NestedFuncao Funcao
          | OpenFuncao Funcao
          | NestedReturn Exp
          deriving (Show,Eq,Data)

type Items = [Item] 

{--------------------------------
************* Let ***************
---------------------------------}
data Let = Let Items Exp
        deriving (Show,Eq,Data)

{--------------------------------
********** Functions ************
---------------------------------}
data Funcao = Funcao Name Args
            | DefFuncao Name Args Items
            deriving (Show,Eq,Data)

data Name = Name String 
          deriving (Show,Eq,Data)

type Args = [Item] 

{--------------------------------
************** If ***************
---------------------------------}
data If = If Exp Items
        | Else Exp Items Items
        deriving (Show,Eq,Data)
        
{--------------------------------
************ While **************
---------------------------------}
data While = While Exp Items
        deriving (Show,Eq,Data)