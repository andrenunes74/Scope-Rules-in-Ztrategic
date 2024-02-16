{-# LANGUAGE DeriveDataTypeable #-}

module Core where
import Data.Data

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

data Items = ConsIts Item Items
           | NilIts
           deriving (Show, Data,Eq)

data Let = Let Items Exp
        deriving (Show,Eq,Data)

data Funcao = Funcao Name Items
            | DefFuncao Name Items Items
            deriving (Show,Eq,Data)

data Name = Name String 
          deriving (Show,Eq,Data)

data If = If Exp Items
        deriving (Show,Eq,Data)
        
data While = While Exp Items
        deriving (Show,Eq,Data)