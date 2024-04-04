{-# LANGUAGE DeriveDataTypeable #-}

module Toy_Java where
import Data.Data
import Data.Generics.Zipper

{- 
Class Class1 {
        Global x = 5 
        Global def func () {
                d = 100
                x = a + x
        }
}

Class Class2 {
        Class1 x
}
-}

rootInstance = (ConsIts 
                    (DefClass "Class1" 
                        (ConsIts (Global (Decl "x" (Const 5))) 
                            (ConsIts 
                                (Global (DefFuncao "func" NilIts
                                    (ConsIts 
                                        (Decl "d" (Const 100))
                                        (ConsIts 
                                            (Decl "x" (Add (Var "a") (Var "x"))) 
                                            NilIts
                                        )
                                    )))
                                NilIts)
                        ) 
                    ) 
                    (ConsIts 
                        (DefClass "Class2" 
                            (ConsIts (Class "Class1" (Decl "x" (Var ""))) NilIts)
                        ) 
                        NilIts))

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
         | Const Int
         | Var String
         | Return Exp
         | Bool Bool
         deriving (Show,Eq,Data)

data Item = Decl String Exp
          | Arg Exp
          | Global Item
          | Let Items Exp
          | Funcao String Items
          | DefFuncao String Items Items
          | If Exp Items
          | While Exp Items
          | DefClass String Items
          | Class String Item
          deriving (Show,Eq,Data)

data Root = Root Items deriving (Show,Eq,Data)

data Items = ConsIts Item Items
           | NilIts
           deriving (Show, Data,Eq)

data Constructor = CAdd
                 | CSub
                 | CDiv
                 | CMul 
                 | CLess 
                 | CGreater 
                 | CEquals 
                 | CGTE 
                 | CLTE 
                 | CAnd 
                 | COr 
                 | CConst 
                 | CVar 
                 | CReturn 
                 | CBool 
                 | CDecl 
                 | CArg 
                 | CLet
                 | CGlobal
                 | CFuncao
                 | CDefFuncao
                 | CName
                 | CIf
                 | CWhile
                 | CConsIts
                 | CNilIts
                 | CClass
                 | CDefClass
                 | CRoot
                 deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case (getHole a :: Maybe Exp) of
    Just (Add _ _)        -> CAdd
    Just (Sub _ _)        -> CSub
    Just (Div _ _)        -> CDiv
    Just (Mul _ _)        -> CMul
    Just (Less _ _)       -> CLess
    Just (Greater _ _)    -> CGreater
    Just (Equals _ _)     -> CEquals
    Just (GTE _ _)        -> CGTE
    Just (LTE _ _)        -> CLTE
    Just (And _ _)        -> CAnd
    Just (Or _ _)         -> COr
    Just (Const _)        -> CConst
    Just (Var _)          -> CVar
    Just (Return _)       -> CReturn
    Just (Bool _)         -> CBool
    otherwise -> case (getHole a :: Maybe Item) of
        Just (Decl _ _)         -> CDecl
        Just (Arg _)            -> CArg
        Just (Let _ _)          -> CLet
        Just (Funcao _ _)       -> CFuncao
        Just (DefFuncao _ _ _)  -> CDefFuncao
        Just (If _ _)           -> CIf
        Just (While _ _)        -> CWhile
        Just (Class _ _)          -> CClass
        Just (DefClass _ _)     -> CDefClass
        Just (Global _)         -> CGlobal
        otherwise -> case (getHole a :: Maybe Items) of
                                Just (ConsIts _ _)    -> CConsIts
                                Just (NilIts)         -> CNilIts
                                otherwise -> case (getHole a :: Maybe Root) of
                                        Just (Root _)    -> CRoot
                                        otherwise -> error "Error in constructor"

lexeme a = case (getHole a :: Maybe Exp) of
            Just (Var a)          -> a
            otherwise             -> case (getHole a :: Maybe Item) of
                Just (Decl a _)         -> a
                Just (Funcao a _)         -> a
                Just (DefFuncao a _ _)    -> a
                Just (DefClass a _ )    -> a
                Just (Class a _)    -> a
                Just (Global (Decl a _))    -> a
                Just (Global (DefFuncao a _ _))    -> a
                otherwise                 -> error "Error in lexeme!"